#' Bepaalt de aangrenzende VHAG's (VHAG2) voor een subset van punten.
#'
#' Let op: Deze functie is ontworpen om in een batch-lus te worden aangeroepen.
#'
#' @param data_batch Een subset van de hoofddata met puntgeometrie (sf object, CRS 31370).
#' @param polyline De gemergde waterloopgeometrie (waterloop_merge).
#' @param waterloop Het originele waterloop sf object voor de join.
#'
#' @return De ingevoerde data_batch met de toegevoegde VHAG2 kolom.
calculate_vhag2_batch <- function(data_batch, polyline, waterloop) {
  
  # 1. Bepaal welke punten de complexe logica nodig hebben (Projectie)
  
  # Koppel de waterlooplijnen aan de batch om te projecteren
  data_linked <- data_batch %>%
    dplyr::filter(!is.na(VHAG)) %>%
    # Koppel de volledige gemergde VHAG-lijn aan elk punt
    sf::st_join(polyline, join = sf::st_nearest_feature, suffix = c("", "_poly"))
  
  # Zoek het projectiepunt op de lijn (st_line_project werkt op sf/sfc)
  # st_line_project berekent de afstand langs de lijn vanaf het begin.
  measure_data <- sf::st_line_project(
    data_linked$geometry_poly, 
    data_linked$geometry
  )
  
  waterloop_length <- sf::st_length(data_linked$geometry_poly)
  
  # Bereken de afstand tot het dichtstbijzijnde eindpunt
  min_end_dist <- pmin(
    as.numeric(measure_data),
    as.numeric(waterloop_length) - as.numeric(measure_data)
  )
  
  # 2. Selecteer ALLE punten die aan de 2km criteria voldoen (Batch-selectie)
  data_needs_vhag2 <- data_linked %>%
    dplyr::mutate(min_end_dist = units::set_units(min_end_dist, "m")) %>%
    dplyr::filter(min_end_dist < units::set_units(2000, "m"))
  
  # Als er geen punten zijn, return de batch zonder wijzigingen
  if (nrow(data_needs_vhag2) == 0) {
    data_batch$VHAG2 <- NA_character_
    return(data_batch)
  }
  
  # ------------------------------------------------------------------
  # 3. Voer QGIS-bewerkingen uit op de HELE subset (Eén keer Buffer/Join)
  # Dit is de prestatieverbetering!
  # ------------------------------------------------------------------
  
  # Buffer ALLE benodigde punten tegelijk (QGIS: 1 aanroep)
  buffer_points <- qgisprocess::qgis_run_algorithm(
    "native:buffer",
    INPUT = data_needs_vhag2,
    DISTANCE = 1000 # De 1 km nabijheidsbuffer
  ) %>% sf::st_as_sf()
  
  # Join de gebufferde polygoon met de originele waterlopen om alle 
  # aangrenzende VHAG's te vinden (QGIS: 1 aanroep)
  # Gebruik st_intersects/cross via st_join in R voor vectorisatie
  data_vhag2_results <- sf::st_join(
    data_needs_vhag2,
    buffer_points,
    join = sf::st_intersects,
    left = FALSE, # Join alleen waar er een match is
    suffix = c("", "_buffer")
  )
  
  # Join de punten met de gevonde gebufferde regio's om de VHAG2 codes te verzamelen
  data_final_join <- sf::st_join(
    data_vhag2_results,
    waterloop, # Gebruik de originele, getransformeerde waterloop
    join = sf::st_is_within_distance,
    dist = 1000,
    suffix = c("", "_2")
  ) %>%
    # Groepeer op het oorspronkelijke ID en verzamel alle gevonden VHAG2 codes
    dplyr::group_by(locID) %>%
    dplyr::summarise(
      VHAG2 = paste(unique(VHAG_2[VHAG_2 != VHAG]), collapse = "; "),
      .groups = "drop"
    ) %>%
    # Vervang lege strings (geen VHAG2 gevonden) door NA
    dplyr::mutate(VHAG2 = ifelse(VHAG2 == "", NA_character_, VHAG2)) %>%
    sf::st_drop_geometry()
  
  # 4. Koppel resultaten terug naar de oorspronkelijke batch
  data_batch_output <- data_batch %>%
    dplyr::left_join(data_final_join, by = "locID")
  
  # Zorg ervoor dat de VHAG2-kolom bestaat voor punten die geen match hadden
  if (!"VHAG2" %in% names(data_batch_output)) {
    data_batch_output$VHAG2 <- NA_character_
  }
  
  return(data_batch_output)
}