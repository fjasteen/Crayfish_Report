# ====================================================
# Scriptnaam: src/05_load_aq_sbz.R
# Beschrijving: 
# - Laadt alle ruimtelijke beschermingslagen (N2000, SBP, HBTRL)
# - Laadt en verwerkt de kreeftendata (CF_presence)
# - Transformeert alles naar CRS Lambert (31370)
# ====================================================

# We gaan ervan uit dat config.R al geladen is door het moederscript
if (!exists("file_n2000_habitats")) {
  stop("Configuratie niet geladen. Run eerst source('./src/config.R')")
}

message("--- Start laden ruimtelijke data (Lagen & Soorten) ---")

# 1. Natura 2000
# ----------------------------------------------------
if (!exists("natura_2000_aq")) { 
  message("Laden Natura 2000...")
  natura_2000 <- st_read(file_n2000_habitats, quiet = TRUE) %>%
    st_transform(crs_lambert)
  
  natura_2000_aq <- natura_2000 %>%
    filter(str_detect(HAB1, paste0("\\b(", paste(aqua_habcodes, collapse = "|"), ")\\b"))) %>%
    mutate(habitat_id = row_number()) 
}

# 2. Habitatrichtlijngebieden (HBTRL)
# ----------------------------------------------------
if (!exists("hbtrl_aq")) {
  message("Laden HBTRL...")
  hbtrl <- st_read(url_hbrl, quiet = TRUE)
  
  if (is.na(st_crs(hbtrl))) st_crs(hbtrl) <- crs_lambert
  hbtrl <- st_transform(hbtrl, crs_lambert)
  
  # Filteren op overlap met aquatische N2000
  hbtrl_aq <- hbtrl %>%
    st_filter(natura_2000_aq, .predicate = st_intersects) %>%
    mutate(habitat_id = row_number())
}

# 3. Soortenbeschermingsprogramma's (SBP)
# ----------------------------------------------------
if (!exists("sbp_pgs_aq")) {
  message("Laden SBP (PGS)...")
  sbp_pgs <- st_read(url_sbp_pgs, quiet = TRUE)
  if (is.na(st_crs(sbp_pgs))) st_crs(sbp_pgs) <- crs_lambert
  
  sbp_pgs_aq <- sbp_pgs %>%
    st_transform(crs_lambert) %>%
    filter(sbp %in% aquatische_sbp) %>%
    mutate(habitat_id = row_number())
}

if (!exists("sbp_vissen")) {
  message("Laden SBP (Vissen)...")
  sbp_vissen <- st_read(url_sbp_pls, quiet = TRUE)
  if (is.na(st_crs(sbp_vissen))) st_crs(sbp_vissen) <- crs_lambert
  
  sbp_vissen <- sbp_vissen %>%
    st_transform(crs_lambert) %>%
    mutate(habitat_id = row_number())
}

# 4. Kreeftendata (Analysedataset)
# ----------------------------------------------------
if (!exists("CF_presence")) {
  message("Laden Kreeftendata & Transformatie...")
  
  # Lees CSV
  CF_data_raw <- readr::read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)
  
  # FIX: Forceer alle kolomnamen naar kleine letters
  # Dit lost het probleem op als de kolom 'Date', 'DATE' of 'date' heet
  names(CF_data_raw) <- tolower(names(CF_data_raw))
  
  # Check of 'date' kolom bestaat (eventueel hernoemen van 'datum')
  if(!"date" %in% names(CF_data_raw) && "datum" %in% names(CF_data_raw)) {
    CF_data_raw <- CF_data_raw %>% rename(date = datum)
  }
  
  if(!"date" %in% names(CF_data_raw)) {
    stop("Fout: Geen kolom 'date' of 'datum' gevonden in de dataset!")
  }
  
  # Zorg dat kolomnamen matchen met config
  species_columns <- tolower(gbif_species)
  
  # Van Wide naar Long format
  CF_long <- CF_data_raw %>%
    # Selecteer kolommen (nu zeker met kleine letters)
    select(
      dat.source,
      date,
      longitude, latitude,
      all_of(species_columns)
    ) %>%
    # Datum conversie (veilig)
    mutate(
      # Als read_csv het al als Date herkende, doe niets. Anders parse.
      date = if(inherits(date, "Date")) date else lubridate::ymd(date)
    ) %>%
    pivot_longer(
      cols      = all_of(species_columns),
      names_to  = "species",
      values_to = "presence_raw"
    ) %>%
    mutate(
      presence = case_when(
        presence_raw == 1 ~ 1,
        presence_raw == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(-presence_raw)
  
  # Omzetten naar SF object (Punten) en transformeren naar Lambert
  CF_presence <- CF_long %>%
    filter(presence == 1) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs_lambert)
}

message("--- Alle data succesvol geladen ---")