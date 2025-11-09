# mrg_backend.R
# Full R backend for the MRG frontend (complete file)
# Exposes:
#   preview_plot(baseline_path, sample_path, out_path)
#   batch_save_plots(baseline_path, sample_paths, out_dir, ext)
#
# Notes:
# - x axis label set to "Wavelength cm-1"
# - legend keys use short filenames (basename)
# - y-axis breaks set to increments of 0.5
# - two-space indentation throughout

suppressMessages({
  library(tidyverse)
  library(stringr)
  library(ggpubr)
  library(cowplot)
})

# ----- helper: parse CSV -----
parseCSV <- function(csvFile = NULL){
  stopifnot(!is.null(csvFile), file.exists(csvFile))
  # read with header = TRUE and skip = 1 to match your source data layout
  df <- read.csv(csvFile, header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 1)
  # convert columns to numeric where possible (coerce non-numeric to NA)
  df[] <- lapply(df, function(col) as.numeric(as.character(col)))
  return(as.data.frame(df))
}

# ----- small helper to draw a path layer from parsed data -----
ggplotDPath <- function(parsed = NULL, label = "Comparee"){
  if (is.null(parsed)) return(geom_path())
  # ensure label column exists for aesthetics (but caller usually sets it)
  parsed$label <- ifelse(is.null(parsed$label), label, parsed$label)
  geom_path(data = parsed,
            aes(x = cm.1, y = A, colour = label),
            show.legend = FALSE)
}

# ----- baseline setter (keeps baselinePlot and defaults in global env) -----
setBaseLine <- function(fileInputRaw = NULL){
  stopifnot(!is.null(fileInputRaw), file.exists(fileInputRaw))
  p <- parseCSV(fileInputRaw)

  # Attach a short label (filename only) to the data so legend shows filenames
  short_label <- basename(fileInputRaw)
  p$label <- short_label

  # store original path and short label in global env for reuse
  defaultFileName <<- fileInputRaw
  defaultFileLabel <<- short_label

  # record baseline min/max for later synchronized axis scaling
  defaultMin <<- min(p$A, na.rm = TRUE)
  defaultMax <<- max(p$A, na.rm = TRUE)

  # compute y-axis start/end snapped to 0.5 increments
  y_start <- floor(defaultMin / 0.5) * 0.5
  y_end   <- ceiling(defaultMax / 0.5) * 0.5

  baselinePlot <<- ggplot(p, aes(x = cm.1, y = A, colour = label)) +
    geom_path(show.legend = TRUE) +
    scale_x_continuous(breaks = pretty(p$'cm.1', n = 20), trans = "reverse") +
    scale_y_continuous(breaks = seq(y_start, y_end, by = 0.5)) +
    xlab("Wavelength cm-1") +
    ylab("Amplitude (A)") +
    theme_pubr() +
    theme(axis.text.x = element_text(size = 8), legend.title = element_blank())

  invisible(baselinePlot)
}

# ----- add a sample graph onto baselinePlot -----
addGraph <- function(base = baselinePlot, fileInput = NULL){
  stopifnot(!is.null(fileInput), file.exists(fileInput))

  # Parse the sample and attach short filename label
  parsed_sample <- parseCSV(fileInput)
  parsed_sample$label <- basename(fileInput)

  # Build a named color vector keyed by short filenames so legend shows short names
  cols <- c()
  cols[basename(defaultFileName)] <- "#008000"   # baseline color (green)
  cols[basename(fileInput)]       <- "#0000FF"   # sample color (blue)

  # determine combined y-range (baseline global defaults + sample)
  ymin <- defaultMin
  ymax <- defaultMax
  # try to safely extend using sample values if present
  if (!all(is.na(parsed_sample$A))){
    ymin <- min(ymin, min(parsed_sample$A, na.rm = TRUE))
    ymax <- max(ymax, max(parsed_sample$A, na.rm = TRUE))
  }
  # snap to 0.5 grid
  y_start <- floor(ymin / 0.5) * 0.5
  y_end   <- ceiling(ymax / 0.5) * 0.5

  # Add the sample path layer and apply the manual color scale and axis labels
  newGraph <- base +
    geom_path(data = parsed_sample, aes(x = cm.1, y = A, colour = label), show.legend = TRUE) +
    scale_colour_manual(values = cols, name = "Name:") +
    scale_y_continuous(breaks = seq(y_start, y_end, by = 0.5)) +
    xlab("Wavelength cm-1") +
    ylab("Amplitude (A)")

  return(newGraph)
}

# ----- convenience wrapper to return a single plot image for preview -----
preview_plot <- function(baseline_path, sample_path = "", out_path){
  stopifnot(!is.null(baseline_path), file.exists(baseline_path))
  # set baseline globally (used by addGraph)
  setBaseLine(baseline_path)

  # produce plot (either baseline-only or baseline+sample)
  if (!is.null(sample_path) && nzchar(sample_path) && file.exists(sample_path)){
    g <- addGraph(baselinePlot, sample_path)
  } else {
    g <- baselinePlot
  }

  # ensure output dir exists and save
  out_dir <- dirname(out_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ext <- tolower(tools::file_ext(out_path))
  if (ext == "") ext <- "png"

  ggsave(filename = basename(out_path),
         plot = g,
         device = ext,
         path = out_dir,
         dpi = 150,
         width = 6, height = 4, units = "in")
  invisible(TRUE)
}

# ----- batch saver for many sample files -----
batch_save_plots <- function(baseline_path, sample_paths, out_dir = getwd(), ext = "png"){
  stopifnot(!is.null(baseline_path), file.exists(baseline_path))
  setBaseLine(baseline_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  for (s in sample_paths){
    if (!file.exists(s)){
      warning(paste("Skipping missing file:", s))
      next
    }
    g <- addGraph(baselinePlot, s)
    fname <- paste0(tools::file_path_sans_ext(baseline_path), "_vs_", tools::file_path_sans_ext(s), ".", ext)
    ggsave(filename = fname, plot = g, device = ext, path = out_dir, dpi = 320, width = 8, height = 5, units = "in")
  }
  invisible(TRUE)
}
