prc_istop <- function(input_files, output_path) {
  # Unzip
  tmp <- file.path(output_path, "tmp")
  lapply(input_files, function(x) archive::archive_extract(x, tmp))

  # Import
  dat <- dir(tmp, pattern = ".shp$", full.names = TRUE) |>
    lapply(sf::st_read, quiet = TRUE)

  # Format data
  mod_nm1 <- data.frame(
    from = c(
      "Img_Cat_Id",
      "Category",
      "Comments",
      "Slick_Deta",
      "Other_Deta",
      "ANOMDATE",
      "geometry"
    ),
    to = c(
      "slick_id",
      "category",
      "comments",
      "length_km",
      "details",
      "date",
      "geometry"
    )
  )

  mod_nm2 <- data.frame(
    from = c(
      "OILSLICKID",
      "CATEGORY",
      "LENGTH",
      "COMMENTS",
      "SLICKDETAI",
      "ANALYST",
      "MODDATE",
      "geometry"
    ),
    to = c(
      "slick_id",
      "category",
      "length_km",
      "comments",
      "details",
      "analyst",
      "date",
      "geometry"
    )
  )

  # Make modifs
  dat <- lapply(dat, \(x) {
    nm <- colnames(x)
    if ("Img_Cat_Id" %in% nm) {
      # Select and rename columns
      x <- x[, mod_nm1$from]
      colnames(x)[match(mod_nm1$from, colnames(x))] <- mod_nm1$to

      # Make length numeric
      x$length_km <- gsub(" km", "", x$length_km)
      x$length_km <- gsub(" long", "", x$length_km)
      x$length_km <- as.numeric(x$length_km)
    } else if ("OILSLICKID" %in% nm) {
      # Select and rename columns
      x <- x[, mod_nm2$from]
      colnames(x)[match(mod_nm2$from, colnames(x))] <- mod_nm2$to

      # Rename categories
      x$category <- gsub("1A - Slick attached to target / Nappe de pétrole attenant à une cible radar", "1A", x$category)
      x$category <- gsub("1B - Slick with target in area / Nappe de pétrole avec la cible dans la zone", "1B", x$category)
      x$category <- gsub("2 - Slick without source / Nappe de pétrole sans source", "2", x$category)
      x$category <- gsub("3 - Possible oil / Possible Nappe de pétrole", "3", x$category)
    }
    x
  }) |>
    dplyr::bind_rows()

  # Export
  sf::st_write(dat, dsn = file.path(output_path, "istop.gpkg"), quiet = TRUE, delete_dsn = TRUE)

  # Remove unzipped files
  fs::dir_delete(tmp)
}
