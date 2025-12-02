# ====================================================
# Scriptnaam: 03_merge_craywatch-gbif.R
# Auteur: Frédérique Steen
# Datum: 26-11-2025
# Beschrijving:
# - Voegt verwerkte Craywatch en GBIF data samen
# - Verwijdert dubbele waarnemingen (GBIF records die al in Craywatch zitten)
# - Koppelt ruimtelijke info (Bekkens, waterlopen, watervlakken)
# - Exporteert de finale dataset voor analyse 
# ====================================================

# --- 0. Instellingen laden ---
source("./src/config.R")
required_species <- tolower(gbif_species)

# --- 1. Inputs Controleren & Laden ---

# Check: Craywatch processed data (uit script 01)
if (!file.exists(file_inter_craywatch_clean)) {
  message("Bewerkte craywatch data niet gevonden. Script 01 wordt uitgevoerd...")
  source("./01_prepare_craywatch_data.R")
}
craywatch_wide <- readRDS(file_inter_craywatch_clean)

# Check: GBIF processed data (uit script 02)
if (!file.exists(file_inter_gbif_processed)) {
  message("Bewerkte GBIF data niet gevonden. Script 02 wordt uitgevoerd...")
  source("./02_prepare_gbif_data.R")
}
gbif_clean <- readRDS(file_inter_gbif_processed)

# Check: Raw craywatch data (nodig voor ontdubbeling op ID niveau)
if (!file.exists(file_craywatch_validated)) stop("Craywatch bronbestand niet gevonden!")
craywatch_raw <- read_csv(file_craywatch_validated, show_col_types = FALSE)

# --- 2. Shapefiles laden ---
watervlakken  <- st_read(file_watervlakken, quiet = TRUE)
target_crs <- st_crs(watervlakken)

vha_catc     <- st_read(file_vha_catc, quiet = TRUE) %>% st_transform(target_crs)
bekken       <- st_read(file_bekken, quiet = TRUE) %>% st_transform(target_crs)

# --- 3. Data ontdubbelen ---
# Verwijder GBIF records afkomstig van 'Natuurpunt:Waarnemingen' 
# én waarvan het ID al voorkomt in de craywatch dataset.

cray_ids <- as.character(craywatch_raw$id)

ids_to_discard <- gbif_clean %>%
  select(occurrenceID) %>% 
  filter(str_detect(occurrenceID, "^Natuurpunt:Waarnemingen")) %>% 
  mutate(extracted_ids = str_remove(occurrenceID, "^Natuurpunt:Waarnemingen:")) %>%
  separate_rows(extracted_ids, sep = ":") %>%
  filter(extracted_ids %in% cray_ids) %>%
  pull(occurrenceID) %>%
  unique()

gbif_clean <- gbif_clean %>%
  filter(!occurrenceID %in% ids_to_discard)

print(paste("GBIF records verwijderd (al in craywatch):", length(ids_to_discard)))

# --- 4. GBIF naar wide ---
# Maak structuur compatibel met craywatch (presence=1, absences = NA, CPUE=NA)
gbif_wide <- gbif_clean %>%
  transmute( 
    locID      = NA_character_,
    session_nr = NA_character_,
    dat.source = "gbif_data",
    year       = year,
    date       = make_date(year, month, day),
    Latitude   = decimalLatitude,
    Longitude  = decimalLongitude,
    trapdays   = NA_real_,
    found_species = tolower(species) 
  )

# Vul kolommen aan voor elke soort
for (sp in required_species) {
  gbif_wide[[sp]] <- if_else(gbif_wide$found_species == sp, 1, NA_real_)
  gbif_wide[[paste0("CPUE_", sp)]] <- NA_real_
}
gbif_wide <- gbif_wide %>% select(-found_species)


# --- 5. Samenvoegen craywatch & GBIF ---
full_dataset <- bind_rows(craywatch_wide, gbif_wide)
message(paste("Totaal aantal records in merged dataset:", nrow(full_dataset)))

# --- 6. Ruimtelijke koppeling met WVLC, CATC en VHAG---

# transformeer craywatch data
full_dataset_sf <- st_as_sf(full_dataset, 
                            coords = c("Longitude", "Latitude"),
                            crs = 4326, 
                            remove = FALSE) %>% 
  st_transform(target_crs)     # Lambert voor berekeningen met afstanden

# nearest neighbour berekeningen
idx_river <- st_nearest_feature(full_dataset_sf, vha_catc)
idx_water <- st_nearest_feature(full_dataset_sf, watervlakken)


dataset_analyse <- full_dataset_sf %>%
  mutate(
    # Haal info op van dichtstbijzijnde features
    VHAG_cand  = vha_catc$VHAG[idx_river],
    CATC_cand  = vha_catc$CATC[idx_river],
    dist_river = st_distance(geometry, vha_catc[idx_river, ], by_element = TRUE),
    
    WVLC_cand  = watervlakken$WVLC[idx_water],
    dist_water = st_distance(geometry, watervlakken[idx_water, ], by_element = TRUE),
    
    # Bepaal minimale afstand en zet om naar numeriek (meters)
    min_dist_unit = pmin(dist_river, dist_water),
    distances     = as.numeric(min_dist_unit),
    # Check tegen de drempelwaarde uit config.R (bv. 50m)
    is_too_far    = distances > max_link_distance_m,
    
    # Ken ID's toe (NA als het te ver is)
    VHAG = if_else(!is_too_far & (dist_river <= dist_water), as.character(VHAG_cand), NA_character_),
    CATC = if_else(!is_too_far & (dist_river <= dist_water), as.character(CATC_cand), NA_character_),
    WVLC = if_else(!is_too_far & (dist_water < dist_river), as.character(WVLC_cand), NA_character_)
  ) %>%
  
  # Koppel bekkens (punt in polygoon)
  st_join(
    bekken %>% select(BEKNR, BEKNAAM), 
    join = st_intersects, 
    left = TRUE
  ) %>%
  
  # Terug naar dataframe
  st_drop_geometry() %>%
  
  # Selecteer de uiteindelijke kolommen
  select(
    dat.source, locID, year, date, Latitude, Longitude, 
    VHAG, CATC, WVLC, BEKNR, BEKNAAM, distances,
    all_of(required_species), starts_with("CPUE_")
  )


# --- 7. Checks & rapport ---
# check afstanden
summary(dataset_analyse$distances)

# check percentage gekoppeld aan WVLC of CATC 
n_gekoppeld <- sum(!is.na(dataset_analyse$VHAG) | !is.na(dataset_analyse$WVLC))
n_totaal    <- nrow(dataset_analyse)
percentage  <- round(n_gekoppeld / n_totaal * 100, 1)

print(paste("Succesvol gekoppeld:", n_gekoppeld, "van de", n_totaal, paste0("(", percentage, "%)")))


# --- 8. Export CSV ---
if (!dir.exists(dirname(file_analyse_dataset_rapport))) dir.create(dirname(file_analyse_dataset_rapport), recursive = TRUE)
write.csv(dataset_analyse, file = file_analyse_dataset_rapport, quote = TRUE, row.names = FALSE)
message(paste("Dataset opgeslagen in rapport map:", file_analyse_dataset_rapport))

# --- 9. Visualisatie: kijk toch even de 50m koppeling na--

# Snelle kaart om de koppeling visueel te checken

map_data_long <- dataset_analyse %>%
  select(
    any_of(c("dat.source", "year", "date", 
             "Latitude", "Longitude", "VHAG", "WVLC", "distances")),
    any_of(required_species) 
  ) %>%
  pivot_longer(
    cols = any_of(required_species), 
    names_to = "species_col", 
    values_to = "pres_val"
  )

# Groeperen 
map_data <- map_data_long %>%
  group_by(dat.source, date, Latitude, Longitude, VHAG, WVLC, distances) %>%
  mutate(
    # Is er iéts gevangen op deze plek+datum?
    any_catch = any(pres_val == 1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  # Filter: Toon soort als aanwezig (1), OF toon 1 rij "Geen vangst" als niks gevangen is
  filter(pres_val == 1 | (!any_catch & species_col == required_species[1])) %>%
  
  # Zal "Afwezig" zijn bij true absence, en NA indien geen 12 trapdays
  mutate(
    species_display = if_else(pres_val == 1, species_col, "Afwezig"),
    
    # Bepaal status voor styling (Wel of niet gekoppeld aan water)
    is_linked = !is.na(VHAG) | !is.na(WVLC)
  )

# map
leaflet(data = map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    radius = 5,
    
    color = "black", 
    weight = 1,
    opacity = 1,
    
    fillColor = "black", 
    fillOpacity = ~ifelse(is_linked, 0.8, 0), # 0.8 = vol: gekoppeld), 0 = leeg: niet gekoppeld
    
    popup = ~paste0(
      "<b>Soort:</b> ", species_display, "<br>",
      "<b>Datum:</b> ", date, "<br>",
      "<b>Dataset:</b> ", dat.source, "<br>",
      "<hr>",
      "<b>Koppeling:</b> ", ifelse(is_linked, "Ja", "Nee (te ver)"), "<br>",
      "<b>Afstand:</b> ", round(distances, 1), "m<br>",
      "<b>VHAG:</b> ", ifelse(is.na(VHAG), "-", VHAG), "<br>",
      "<b>WVLC:</b> ", ifelse(is.na(WVLC), "-", WVLC)
    )
  ) %>%
  addControl(
    html = "<div style='background:white;padding:5px;border:1px solid #ccc;'>
              <b>Legende</b><br>
              <i style='background:black;width:10px;height:10px;display:inline-block;border-radius:50%;'></i> Gekoppeld<br>
              <i style='border:1px solid black;width:10px;height:10px;display:inline-block;border-radius:50%;'></i> Niet gekoppeld
            </div>",
    position = "bottomright"
  )


