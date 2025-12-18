#' Build cumulative pressure rasters from analyzed layers
#'
#' Sums all .tif files in each analyzed subfolder (excluding specified folders),
#' normalizes each sum to 0-1, writes outputs to figures/graphicalabstract/,
#' then creates a cumulative sum across all normalized outputs.
#'
#' @param analyzed_dir Character path to analyzed data folder
#' @param output_dir Character path for output rasters
#' @param excluded_folders Character vector of folder names to exclude
#' @param exclude_pattern Character pattern to exclude folders (case-insensitive)
#' @param overwrite Logical, whether to overwrite existing rasters
#' @return List with output paths for per-folder rasters and cumulative raster
#'
#' @export
create_cumulative_pressure <- function(
    analyzed_dir = "workspace/data/analyzed",
    output_dir = "figures/graphicalabstract",
    excluded_folders = c("fisheries_intensity", "offshore_wind_farm", "shipping_night_light_intensity_density"),
    exclude_pattern = "atlantic",
    overwrite = TRUE) {
  if (!dir.exists(analyzed_dir)) {
    stop("Analyzed directory not found: ", analyzed_dir)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  all_folders <- list.dirs(analyzed_dir, full.names = TRUE, recursive = FALSE)
  if (length(all_folders) == 0) {
    stop("No subfolders found in analyzed directory: ", analyzed_dir)
  }

  folder_names <- basename(all_folders)
  exclusion_regex <- paste(c(excluded_folders, exclude_pattern), collapse = "|")
  include_mask <- !grepl(exclusion_regex, folder_names, ignore.case = TRUE)
  target_folders <- all_folders[include_mask]

  if (length(target_folders) == 0) {
    stop("No folders to process after exclusions in: ", analyzed_dir)
  }

  output_paths <- character(0)

  for (folder in target_folders) {
    summed <- .sum_tifs_in_folder(folder)
    if (is.null(summed)) {
      next
    }

    log_transformed <- .log_transform_raster(summed)
    normalized <- .normalize_raster_01(log_transformed)
    out_name <- paste0(basename(folder), "_sum_norm.tif")
    out_path <- file.path(output_dir, out_name)

    terra::writeRaster(normalized, out_path, overwrite = overwrite)
    output_paths <- c(output_paths, out_path)
  }

  if (length(output_paths) == 0) {
    stop("No output rasters were created from analyzed folders.")
  }

  cumulative <- terra::app(terra::rast(output_paths), sum, na.rm = TRUE)
  cumulative[cumulative == 0] <- NA
  cumulative_path <- file.path(output_dir, "cumulative_pressure.tif")
  terra::writeRaster(cumulative, cumulative_path, overwrite = overwrite)

  list(
    folder_outputs = output_paths,
    cumulative_path = cumulative_path
  )
}

#' Export cumulative pressure as a static figure
#'
#' @param cumulative_path Character path to cumulative pressure .tif
#' @param output_path Character path for the exported figure
#' @param width Numeric width in inches
#' @param height Numeric height in inches
#' @param dpi Numeric resolution for export
#' @return Character path to the saved figure
#'
#' @export
export_cumulative_pressure_figure <- function(
    cumulative_path = "figures/graphicalabstract/cumulative_pressure.tif",
    output_path = "figures/graphicalabstract/cumulative_pressure.png",
    width = 8,
    height = 6,
    dpi = 300) {
  if (!file.exists(cumulative_path)) {
    stop("Cumulative pressure raster not found: ", cumulative_path)
  }

  raster <- terra::rast(cumulative_path)
  raster <- log(log(raster + 1) + 1)

  tmap::tmap_mode("plot")
  tmap::tmap_options(raster.max_cells = Inf)

  map <- tmap::tm_shape(raster) +
    tmap::tm_raster(
      palette = "magma",
      style = "cont",
      n = 100,
      col_alpha = 0.85,
      col.legend = tm_legend_hide()
    ) +
    tmap::tm_basemap("CartoDB.Positron")


  tmap::tmap_save(map, output_path, width = width, height = height, dpi = dpi)

  output_path
}

.sum_tifs_in_folder <- function(folder) {
  tif_files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE, ignore.case = TRUE)
  if (length(tif_files) == 0) {
    warning("No .tif files found in: ", folder)
    return(NULL)
  }

  raster_stack <- terra::rast(tif_files)
  terra::app(raster_stack, sum, na.rm = TRUE)
}

.normalize_raster_01 <- function(raster) {
  stats <- terra::global(raster, c("min", "max"), na.rm = TRUE)
  min_val <- stats[1, "min"]
  max_val <- stats[1, "max"]

  if (is.na(min_val) || is.na(max_val) || max_val == min_val) {
    return(raster * 0)
  }

  (raster - min_val) / (max_val - min_val)
}

.log_transform_raster <- function(raster) {
  # Log-transform normalized data, then re-normalize to 0-1.
  logged <- log(raster + 1)
}
