# ====================================================
# Scriptnaam: 18_Prepare_fc_data.R
# Auteur: Stien Mertens
# Refactored door: Frédérique Steen
# Datum: 04-12-2025
# Beschrijving: 
# - Ruimtelijke validatie (koppelen aan VHAG/WVLC)
# - Data aggregatie: 
#     - selecteert fc van mei tot oktober
#     - aggregeert tot mediaan over alle meetjaren
# - Output: fc_data_aggregated.Rdata 
# ====================================================

# --- 0. Instellingen & libraries ---
source("./src/config.R") # Laadt alle libraries en variabelen (max_link_distance_m, fc_parameter_map, etc.)

library(mapview)
library(units)
# --- 1. Data & shapefiles laden ---
message("Laden FC data en referentielagen...")

# FC data (reeds in Lambert 31370)
load(file = here("data", "input", "fc_data", "fc_fc_data_breed.Rdata")) 

# GIS Referentielagen transformeren naar Lambert (31370)
waterloopsegmenten <- read_sf(file_waterloopsegmenten) %>% st_transform(31370)
watervlakken       <- read_sf(file_watervlakken) %>% st_transform(31370)
watergang          <- read_sf(file_watergang) %>% st_transform(31370) 

# --- 3. Ruimtelijke Koppeling ---
message("--- Start Ruimtelijke Koppeling ---")

# Kolommen te behouden uit de fc-dataset
columns_to_keep <-c("sample_point", "sample_datum_monstername", "sample_tijdstip_monstername",
  "sample_point_omschrijving", "lambert_x", "lambert_y", "waterloop",  
  "bekken", "gemeente", "geometry")

# Vind de dichtste waterlopen en watervlakken voor elk uniek meetpunt
fc_loc <- fc_breed %>%
  select(any_of(columns_to_keep)) %>%
  distinct(sample_point, .keep_all = TRUE) %>%
  mutate(
    # A. Bepaal dichtstbijzijnde waterloopsegmenten
    nearest_river_index = st_nearest_feature(., waterloopsegmenten),
    VHAS = waterloopsegmenten$VHAS[nearest_river_index],
    VHAG = waterloopsegmenten$VHAG[nearest_river_index],
    distances_vhag = st_distance(geometry, waterloopsegmenten[nearest_river_index, ], by_element = TRUE),
    
    # B. Bepaal dichtstbijzijnde watervlak
    nearest_waterbody_index = st_nearest_feature(., watervlakken),
    WVLC = watervlakken$WVLC[nearest_waterbody_index],
    distances_wv = st_distance(geometry, watervlakken[nearest_waterbody_index, ], by_element = TRUE)
  )

# Selecteer type waterlichaam en bereken definitieve afstand
fc_loc <- fc_loc %>%
  mutate(
    # Gebruik NA_real_ om type mismatch te voorkomen
    VHASFinal = if_else(distances_vhag <= distances_wv, VHAS, NA),
    VHAGFinal = if_else(distances_vhag <= distances_wv, VHAG, NA),
    WVLCFinal = if_else(distances_wv < distances_vhag, WVLC, NA),
    
    distances = pmin(distances_vhag, distances_wv),
    distances = set_units(distances, "m"), 
    is_river  = distances_vhag <= distances_wv 
  )

# --- 4. Verificatie van de koppeling ---
message("--- Verificatie Initiële Koppeling ---")

# Check statistieken
summary(fc_loc$distances)

# Plot histogram
fc_loc_no_units <- fc_loc %>% 
  mutate(m = drop_units(distances))

plot_histogram <- ggplot(fc_loc_no_units, aes(x = m)) +
  geom_histogram(bins = 100) +
  xlim(0, 20) + 
  labs(
    title = "Distributie van Dichtstbijzijnde Afstanden (< 20m)", 
    x = "Afstand (m)"
  )

print(plot_histogram) 

# --- 4. Filteren & Breedtecorrectie ---

# Split de data
fc_loc_10 <- fc_loc %>%
  filter(distances <= set_units(max_link_distance_m, "m")) # Harde drempel: binnen 10m is OK

fc_loc_large <- fc_loc %>%
  filter(distances > set_units(max_link_distance_m, "m"))

# Breedtecorrectie (Enkel voor punten >10m gelinkt aan rivier)
fc_loc_large_river <- fc_loc_large %>%
  filter(is_river == TRUE) %>%
  
  # Koppel aan GRB watergang voor breedteschatting
  st_join(watergang, join = st_nearest_feature, suffix = c("", "wg")) %>%
  
  # Validatie 1: Is de GRB polygoon op dezelfde VHAG?
  filter(VHAGFinal == VHAGwg) %>% 
  
  # Bereken toegestane buffer o.b.v. geschatte breedte
  mutate(
    breedteschatting = OPPERVL / LENGTE,
    max_allowed_dist = set_units(breedteschatting / 2 + max_link_distance_m, "m")
  ) %>%
  
  # Validatie 2: Valt punt binnen de berekende buffer?
  filter(distances <= max_allowed_dist) %>%
  
  # Behoud enkel kolommen consistent met fc_loc_10
  select(any_of(colnames(fc_loc_10)))

# Combineer de gevalideerde sets
fc_loc_final <- bind_rows(fc_loc_10, fc_loc_large_river) %>%
  dplyr::select(-VHAS, -VHAG, -WVLC, -sample_datum_monstername, -sample_tijdstip_monstername) %>%
  dplyr::rename(VHAS = VHASFinal, VHAG = VHAGFinal,WVLC = WVLCFinal
  )

message(paste0("Totaal unieke FC-locaties na validatie: ", nrow(fc_loc_final)))

# --- 5. Visuele controle & Validatie ---
message("--- Visuele Controle Validatie ---")

# A. Definieer sets
gevalideerde_ids <- fc_loc_final$sample_point

gefilterde_punten <- fc_loc %>%
  dplyr::filter(!sample_point %in% gevalideerde_ids) 

print(paste("Totaal aantal verwijderde punten:", nrow(gefilterde_punten)))
message(paste0("Totaal unieke FC-locaties na validatie: ", nrow(fc_loc_final)))

if (interactive()) {
# B. Basiskaart
basis_kaart_water <- mapview(waterloopsegmenten, layer.name = "Waterlopen (VHAG)") +
  mapview(watervlakken, 
          col.regions = "lightblue", 
          alpha.regions = 0.5, 
          layer.name = "Watervlakken (WVLC)")

# C. Plot resultaten
# Rood = Verwijderd
map_verwijderd <- basis_kaart_water + 
  mapview(gefilterde_punten, 
          cex = 4, 
          col.region = "red", 
          layer.name = "Verwijderde Punten")

# Groen = Behouden
behouden_punten_kaart <- mapview(fc_loc_final, 
                                 cex = 2, 
                                 col.region = "green", 
                                 layer.name = "Behouden Punten")

map_totaal <- basis_kaart_water + behouden_punten_kaart + 
  mapview(gefilterde_punten, 
          cex = 4, 
          col.region = "red", 
          layer.name = "Verwijderde Punten")
} else {
  message("Interactieve kaart overgeslagen (niet in interactieve modus).")
}

 print(map_totaal) 

# --- 6. Export ---
save(fc_loc_final, file = here("data", "intermediate", "fysicochemie", "fc_locaties_validated.Rdata"))
message(paste("FC-sampling punten opgeslagen in:", here("data", "intermediate", "fysicochemie", "fc_locaties_validated.Rdata")))

# ==============================================================================
# DEEL 2: DATA AGGREGATIE (Mediaan berekenen)
# ==============================================================================
message("--- Start Data Aggregatie (Mediaan & Mei-Okt) ---")

# We nemen de originele data (fc_breed) en filteren deze
# Vervolgens koppelen we enkel de gevalideerde locaties eraan

fc_summary_table <- fc_breed %>%
  st_drop_geometry() %>%
  # 1. Datum parsing
  mutate(
    date  = as.Date(sample_datum_monstername),
    year  = year(date),
    month = month(date)
  ) %>%
  
  # 2. Filter op seizoen (uit config)
  filter(month %in% fc_season_months) %>%
  
  # 3. Selecteer kolommen (Mapping uit config)
  select(
    sample_point, year, 
    any_of(fc_parameter_map), 
    contains("Clfyl") 
  ) %>%
  
  # 4. Aggregeer naar mediaan per punt/jaar
  group_by(sample_point, year) %>%
  summarise(across(where(is.numeric), \(x) median(x, na.rm = TRUE)), .groups = "drop")

# Koppel de data aan de ruimtelijke locaties
fc_aggregated <- fc_loc_final %>%
  inner_join(fc_summary_table, by = "sample_point")

# --- 3. Export ---

# Sla de geaggregeerde data op (klein, snel, bevat alles wat je nodig hebt)
file_export_fc <- file.path(dir_data_intermediate, "fysicochemie", "fc_data_aggregated.Rdata")

message(paste("Totaal aantal rijen (Locatie x Jaar):", nrow(fc_aggregated)))

# Check of map bestaat
if(!dir.exists(dirname(file_export_fc))) dir.create(dirname(file_export_fc), recursive = TRUE)

save(fc_aggregated, file = file_export_fc)

message(paste("Klaar! Geaggregeerde dataset opgeslagen"))
