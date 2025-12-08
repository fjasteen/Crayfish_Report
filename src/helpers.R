################################################################################
#' Bepaalt de aangrenzende VHAG's (VHAG2) voor een subset van punten.
#'
#' Let op: Deze functie is ontworpen om in een batch-lus te worden aangeroepen.
#'
#' @param data_batch Een subset van de hoofddata met puntgeometrie (sf object, CRS 31370).
#' @param polyline De gemergde waterloopgeometrie (waterloop_merge).
#' @param waterloop Het originele waterloop sf object voor de join.
#'
#' @return De ingevoerde data_batch met de toegevoegde VHAG2 kolom.
#' ################################################################################

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

################################################################################
#' Match punten aan waterlopen en watervlakken
#' 
#' @param points_sf SF object met de te koppelen punten
#' @param rivers SF object (Lijnen/Segmenten) met VHAG en VHAS kolommen
#' @param lakes SF object (Polygonen) met WVLC kolom
#' @param watergang SF object (GRB Polygonen) voor breedtebepaling
#' @param buffer_m Numeriek, de maximale basisafstand (uit config)
#' 
#' @return SF object met toegevoegde kolommen: VHAG, VHAS, WVLC, distance_linked, link_status
################################################################################

match_points_to_water <- function(points_sf, rivers, lakes, watergang, buffer_m) {
  
  message("--- Start centrale match-functie ---")
  
  # 1. Nearest feature bepalen voor rivieren en meren
  message("   > Berekenen dichtstbijzijnde features...")
  idx_riv <- st_nearest_feature(points_sf, rivers)
  idx_wat <- st_nearest_feature(points_sf, lakes)
  
  # 2. Afstanden berekenen
  # Let op: by_element = TRUE zorgt voor paarsgewijze afstand (snel)
  dist_riv <- st_distance(points_sf, rivers[idx_riv, ], by_element = TRUE)
  dist_wat <- st_distance(points_sf, lakes[idx_wat, ], by_element = TRUE)
  
  # 3. Data voorbereiden met kandidaten
  # We zetten units om naar numeriek om warnings te vermijden
  data_linked <- points_sf %>%
    mutate(
      # Rivier kandidaten
      VHAG_cand = rivers$VHAG[idx_riv],
      VHAS_cand = rivers$VHAS[idx_riv],
      dist_riv_m = as.numeric(dist_riv),
      
      # Meer kandidaten
      WVLC_cand = lakes$WVLC[idx_wat],
      dist_wat_m = as.numeric(dist_wat),
      
      # Bepaal winnaar (wie is dichterbij?)
      type_water = if_else(dist_riv_m <= dist_wat_m, "open", "gesloten"),
      dist_actual = pmin(dist_riv_m, dist_wat_m)
    )
  
  # 4. Validatie Logica
  message("   > Validatie en GRB-breedte check uitvoeren...")
  
  data_validated <- data_linked %>%
    mutate(
      VHAG = NA_character_,
      VHAS = NA_character_,
      WVLC = NA_character_,
      link_status = "niet gekoppeld",
      distance_linked = NA_real_
    )
  
  # A. Binnen harde buffer (Standaard validatie)
  # --------------------------------------------
  # Alles binnen 'buffer_m' is automatisch goed.
  mask_buffer <- data_validated$dist_actual <= buffer_m
  
  data_validated$link_status[mask_buffer] <- "buffer"
  data_validated$distance_linked[mask_buffer] <- data_validated$dist_actual[mask_buffer]
  
  # Vul ID's in op basis van type
  is_open <- data_validated$type_water == "open"
  
  # Open water binnen buffer
  data_validated$VHAG[mask_buffer & is_open] <- as.character(data_validated$VHAG_cand[mask_buffer & is_open])
  data_validated$VHAS[mask_buffer & is_open] <- as.character(data_validated$VHAS_cand[mask_buffer & is_open])
  
  # Gesloten water binnen buffer
  data_validated$WVLC[mask_buffer & !is_open] <- as.character(data_validated$WVLC_cand[mask_buffer & !is_open])
  
  
  # B. GRB Breedte Check (Alleen voor Open Water buiten buffer)
  # --------------------------------------------
  # Selecteer de twijfelgevallen: Buiten buffer, maar wel 'open' water
  mask_check_grb <- (!mask_buffer) & (data_validated$type_water == "open")
  
  if (any(mask_check_grb)) {
    
    # We isoleren de twijfelgevallen voor de zware ruimtelijke join
    candidates_grb <- data_validated[mask_check_grb, ]
    
    # Spatial join naar GRB watergang (dichtstbijzijnde)
    # We doen dit apart om performance te sparen
    candidates_grb <- st_join(candidates_grb, watergang, join = st_nearest_feature, suffix = c("", "_wg"))
    
    # Validatie logica
    # 1. De GRB polygoon moet matchen met de VHAG van de aslijn
    # 2. De afstand moet binnen (breedte/2 + buffer) vallen
    
    candidates_grb <- candidates_grb %>%
      mutate(
        breedteschatting = OPPERVL / LENGTE,
        max_allowed_dist = (breedteschatting / 2) + buffer_m,
        
        # Check match (VHAG moet bestaan in GRB dataset en overeenkomen)
        vhag_match = !is.na(VHAG) & (as.character(VHAG_cand) == as.character(VHAG)),
        
        is_valid_grb = vhag_match & (dist_actual <= max_allowed_dist)
      )
    
    # Terugzetten in de hoofd-dataset
    # We gebruiken row-indices of ID matching indien beschikbaar. 
    # Hier simpelweg via logische vectoren updaten als de volgorde klopt (wat zo is bij filtering).
    
    valid_indices <- which(mask_check_grb)[candidates_grb$is_valid_grb]
    
    if (length(valid_indices) > 0) {
      data_validated$link_status[valid_indices] <- "GRB_corrected"
      data_validated$distance_linked[valid_indices] <- data_validated$dist_actual[valid_indices]
      data_validated$VHAG[valid_indices] <- as.character(data_validated$VHAG_cand[valid_indices])
      data_validated$VHAS[valid_indices] <- as.character(data_validated$VHAS_cand[valid_indices])
    }
  }
  
  # 5. Opkuis
  message("   > Afronden...")
  data_final <- data_validated %>%
    select(-VHAG_cand, -VHAS_cand, -WVLC_cand, -dist_riv_m, -dist_wat_m, -type_water, -dist_actual)
  
  return(data_final)
}

