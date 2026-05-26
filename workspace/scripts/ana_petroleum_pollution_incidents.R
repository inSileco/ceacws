ana_petroleum_pollution_incidents_neec <- function(input_files, output_path) {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # NEEC data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  input_files <- unlist(input_files)
  neec <- input_files[stringr::str_detect(input_files, "neec.csv")] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  substances <- input_files[stringr::str_detect(input_files, "substances.csv")] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)
  # ----------------------------------------------------------------
  # Filter NEEC data based on specified constraints
  # Remove empty coordinates
  neec <- neec |>
    dplyr::filter(!is.na(latitude))

  # ----------------------------------------------------------------
  # Use data from “Notification Date Time” when not available from “Incident Local Date Time” field
  neec <- neec |>
    dplyr::mutate(
      datetime = ifelse(!is.na(incident_local_date_time_local_time), incident_local_date_time_local_time, notification_date_time),
      date = as.Date(datetime, format = "%Y-%m-%d %H:%M"),
      year = format(date, "%Y")
    ) |>
    dplyr::filter(datetime > lubridate::as_date("2016-01-01"))

  # ----------------------------------------------------------------
  # Remove records containing "covid", "istop", "sipps"
  neec <- neec |>
    dplyr::filter(!stringr::str_detect(tolower(incident_name), "covid")) |>
    dplyr::filter(!stringr::str_detect(tolower(incident_name), "istop")) |>
    dplyr::filter(!stringr::str_detect(tolower(incident_name), "sipps"))

  # ----------------------------------------------------------------
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # TO VERIFY
  # Set all NA quantities to minimum observed quantity
  neec <- neec |>
    dplyr::mutate(
      quantity = ifelse(is.na(quantity_converted), min(quantity_converted, na.rm = TRUE), quantity_converted) # ,
      # quantity = ifelse(quantity < 1, 1, quantity)
    )
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  # ----------------------------------------------------------------
  # Select greatest quantity for individual spills with multiple substances
  neec <- neec |>
    dplyr::group_by(incident_name) |>
    dplyr::filter(quantity == max(quantity, na.rm = TRUE)) |>
    dplyr::ungroup()

  # ----------------------------------------------------------------
  # Based on email exhange with Robert Ronconi on 2024-02-23
  # Remove Incident Name containing "MVI" or "furnace oil"
  # This is to remove a few dozen incidents that generally won't impact marine birds
  neec <- neec |>
    dplyr::filter(!stringr::str_detect(tolower(incident_name), "mvi")) |>
    dplyr::filter(!stringr::str_detect(tolower(incident_name), "furnace oil"))

  # ----------------------------------------------------------------
  # Group by substance type
  neec <- neec |>
    dplyr::left_join(substances[, c("name", "chemical_state", "oil_sheen")], by = c("substance" = "name")) |>
    dplyr::mutate(
      chemical_state = ifelse(is.na(chemical_state), "unknown", chemical_state),
      oil_sheen = ifelse(is.na(oil_sheen), "unknown", oil_sheen)
    ) |>
    dplyr::mutate(
      oil_sheen = dplyr::case_when(
        oil_sheen == "yes" ~ "with_oil_sheen",
        oil_sheen == "no" ~ "without_oil_sheen",
        .default = oil_sheen
      ),
      substance = paste0(chemical_state, "_", oil_sheen),
      substance = stringr::str_replace(substance, "unknown_unknown", "unknown")
    )

  # ----------------------------------------------------------------
  # # Based on email exhange with Robert Ronconi on 2024-02-23
  # # Keep all incidents within 1km of the coastline
  # # Coastal outline
  fetch_data <- function(url) {
    # Temporary file
    tmp <- tempfile(fileext = ".json")

    # Download file
    curl::curl_download(url, tmp)

    # Import
    sf::st_read(tmp, quiet = TRUE)
  }

  coastlines <- c(
    "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_USA_0.json",
    "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CAN_0.json"
  ) |>
    lapply(fetch_data)

  terre <- sf::st_union(coastlines[[1]], coastlines[[2]]) |>
    sf::st_transform(32198) |> # To get units in meters
    smoothr::fill_holes(threshold = units::set_units(1000, km^2))

  # 1km buffer inland
  terre_buf <- sf::st_buffer(terre, -1000) |>
    sf::st_transform(4326)

  # Filter data spatially
  neec <- neec |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  iid <- sf::st_disjoint(terre_buf, neec) |> unlist()
  neec <- neec[iid, ]

  # ----------------------------------------------------------------
  # Function to categorize a numeric vector into n categories by percentile
  categorize_by_percentile <- function(x) {
    # Calculate quantiles for dividing into n categories
    probs <- c(0.0, 0.7, 0.9, 1.0)
    breaks <- quantile(x, probs = probs, na.rm = TRUE)

    # Use cut to assign each value in x to a percentile category
    categories <- cut(x, breaks = breaks, include.lowest = TRUE, labels = paste0("Q", 1:(length(probs) - 1)))

    return(categories)
  }


  # Categorize quantities
  # neec <- neec |>
  #   dplyr::mutate(quantity_category = categorize_by_percentile(neec$quantity))
  neec <- categorize_by_quantity(neec)

  # Add buffer and remove terrestrial zones
  neec <- sf::st_buffer(neec, neec$quantity_category) |>
    sf::st_difference(sf::st_transform(terre, 4326))

  # Select columns that are going to be useful for the next step only to reduce file size
  neec <- neec |>
    dplyr::mutate(
      date = as.Date(date, format = "%Y-%m-%d %H:%M"),
      year = format(date, "%Y"),
      uid = sprintf("neec_%04d", dplyr::row_number())
    ) |>
    dplyr::select(uid, date, year, substance)

  # Export partial data for use in report (this should be revisited)
  sf::st_write(
    neec,
    dsn = file.path(output_path, "petroleum_pollution_incidents_neec.gpkg"),
    quiet = TRUE,
    delete_dsn = TRUE
  )

  # ----------------------------------------------------------------
  # To explore:
  # Impact to Waterbody field.  Consider if we should treat “actual” vs. “potential” spills differently.  Data on “potential” spills only started being recorded in April 2021 onward (no differentiation before that).  Some “Potential” spills may report very large amounts of substance that was not actually spilled (e.g.  Incident Number NL-20200611-03045-20 was a disabled vessel with 400000 L of fuel on board, but nothing spilled).  These “Potential” spills still represent risk even if no true pollution occurred.
}

ana_petroleum_pollution_incidents_nasp <- function(input_files, output_path) {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # NASP data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  input_files <- unlist(input_files)
  nasp <- input_files[stringr::str_detect(input_files, "nasp.gpkg")] |>
    sf::st_read(quiet = TRUE) |>
    dplyr::filter(!is.na(volume_l)) |>
    dplyr::rename(geometry = geom, quantity = volume_l)

  # Add buffer based on volumes reported
  # Categorize quantities
  # neec <- neec |>
  #   dplyr::mutate(quantity_category = categorize_by_percentile(neec$quantity))
  nasp <- categorize_by_quantity(nasp)

  # Fetch data to remove terrestrial areas
  fetch_data <- function(url) {
    # Temporary file
    tmp <- tempfile(fileext = ".json")

    # Download file
    curl::curl_download(url, tmp)

    # Import
    sf::st_read(tmp, quiet = TRUE)
  }

  coastlines <- c(
    "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_USA_0.json",
    "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CAN_0.json"
  ) |>
    lapply(fetch_data)

  terre <- sf::st_union(coastlines[[1]], coastlines[[2]]) |>
    sf::st_transform(32198) |> # To get units in meters
    smoothr::fill_holes(threshold = units::set_units(1000, km^2))

  # Add buffer and remove terrestrial zones
  nasp <- sf::st_buffer(nasp, nasp$quantity_category) |>
    sf::st_difference(sf::st_transform(terre, 4326))

  # Final format
  nasp <- nasp |>
    dplyr::mutate(
      date = as.Date(date, format = "%Y-%m-%d %H:%M"),
      year = format(date, "%Y"),
      uid = sprintf("nasp_%04d", dplyr::row_number())
    ) |>
    dplyr::select(uid, date, year)

  # Export
  sf::st_write(nasp, dsn = file.path(output_path, "petroleum_pollution_incidents_nasp.gpkg"), quiet = TRUE, delete_dsn = TRUE)
}

ana_petroleum_pollution_incidents_istop <- function(input_files, output_path) {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # ISTOP data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load & Filter ISTOP data based on specified constraints
  istop <- input_files[stringr::str_detect(unlist(input_files), "istop.gpkg")] |>
    sf::st_read(quiet = TRUE) |>
    dplyr::filter(!is.na(date)) |>
    dplyr::distinct() |>
    dplyr::filter(category %in% c("1A", "1B", "2")) |>
    dplyr::rename(geometry = geom) |>
    dplyr::mutate(
      area = sf::st_area(geometry),
      area = units::set_units(area, km^2),
      area = as.numeric(area)
    ) |>
    dplyr::mutate(
      date = as.Date(date, format = "%Y-%m-%d %H:%M"),
      year = format(date, "%Y"),
      uid = sprintf("istop_%04d", dplyr::row_number())
    ) |>
    dplyr::select(uid, date, year)

  # Export partial data for use in report (this should be revisited)
  sf::st_write(istop, dsn = file.path(output_path, "petroleum_pollution_incidents_istop.gpkg"), quiet = TRUE, delete_dsn = TRUE)
}


categorize_by_quantity <- function(dat) {
  dat |>
    dplyr::mutate(quantity_category = dplyr::case_when(
      quantity < 100 ~ 250,
      quantity >= 100 & quantity < 1000 ~ 500,
      quantity >= 1000 & quantity < 10000 ~ 1000,
      quantity >= 10000 ~ 2000,
      .default = NA_real_
    ))
}

ana_petroleum_pollution_incidents <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/petroleum_pollution_incidents-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/analyzed/petroleum_pollution_incidents-1.0.0/petroleum_pollution_incidents_istop.gpkg",
  #   "workspace/data/analyzed/petroleum_pollution_incidents-1.0.0/petroleum_pollution_incidents_nasp.gpkg",
  #   "workspace/data/analyzed/petroleum_pollution_incidents-1.0.0/petroleum_pollution_incidents_neec.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/aoi.gpkg",
  #   "workspace/data/harvested/aoi-1.0.0/processed/grid.tif"
  # )

  input_files <- unlist(input_files)
  grid <- terra::rast(input_files[basename(input_files) == "grid.tif"])
  aoi <- sf::st_read(input_files[basename(input_files) == "aoi.gpkg"], quiet = TRUE)

  # ISTOP
  istop <- input_files[stringr::str_detect(unlist(input_files), "istop.gpkg")] |>
    sf::st_read(quiet = TRUE) |>
    sf::st_transform(4326) |>
    dplyr::mutate(month = lubridate::month(date))

  group_rasterize_export(
    istop,
    grouping_vars = "month",
    fun = "count",
    field = NA,
    norm = diff(range(as.numeric(istop$year))),
    grid = grid,
    aoi = aoi,
    dataset_name = "petroleum_pollution_incidents_istop",
    output_path = output_path
  )


  # NASP
  nasp <- input_files[stringr::str_detect(unlist(input_files), "nasp.gpkg")] |>
    sf::st_read(quiet = TRUE) |>
    sf::st_transform(4326) |>
    dplyr::mutate(month = lubridate::month(date))

  group_rasterize_export(
    nasp,
    grouping_vars = "month",
    fun = "count",
    field = NA,
    norm = diff(range(as.numeric(nasp$year))),
    grid = grid,
    aoi = aoi,
    dataset_name = "petroleum_pollution_incidents_nasp",
    output_path = output_path
  )

  # NEEC
  neec <- input_files[stringr::str_detect(unlist(input_files), "neec.gpkg")] |>
    sf::st_read(quiet = TRUE) |>
    sf::st_transform(4326) |>
    dplyr::mutate(month = lubridate::month(date))

  group_rasterize_export(
    neec,
    grouping_vars = c("month", "substance"),
    fun = "count",
    field = NA,
    norm = diff(range(as.numeric(neec$year))),
    grid = grid,
    aoi = aoi,
    dataset_name = "petroleum_pollution_incidents_neec",
    output_path = output_path
  )

  # All
  fls <- data.frame(
    path = dir(output_path, full.names = TRUE, pattern = ".tif")
  ) |>
    dplyr::mutate(
      month = basename(path) |>
        stringr::str_extract_all("\\d+") |>
        unlist()
    ) |>
    dplyr::group_by(month) |>
    dplyr::group_split()
  for (i in seq_len(length(fls))) {
    # Import
    dat <- lapply(fls[[i]]$path, terra::rast)
    dat <- do.call(c, dat)

    # Sum
    dat <- terra::app(dat, sum, na.rm = TRUE)

    # Export
    month <- fls[[i]]$month[1]
    terra::writeRaster(
      dat,
      filename = file.path(output_path, glue::glue("petroleum_pollution_incidents_all_{month}.tif")),
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
    )
  }
}
