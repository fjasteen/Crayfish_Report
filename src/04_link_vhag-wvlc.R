# ==============================================================================
# Script: 04_spatial_link_core.R
# Auteur: Stien Mertens
# Refactored by: Frédérique Steen
# Doel:   Koppeling masterdataset aan VHAG/WVLC met dynamische validatie
# ==============================================================================

# --- 0. Setup ---
source("./src/config.R")
library(lwgeom) 
library(purrr)

# --- 1. Data Laden ---
message("Laden masterdataset en referentielagen...")

if (!file.exists(file_analyse_dataset_rapport)) stop("Input file (stap 03) niet gevonden.")

# Masterdataset omzetten naar SF (Lambert72)
dataset_sf <- read_csv(file_analyse_dataset_rapport) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(31370) %>%
  select(-any_of(c("VHAG", "CATC", "WVLC", "distances"))) 

# Shapefiles laden
# Gebruik de gedetailleerde segmenten voor de koppeling!
waterloopsegmenten <- read_sf(file_waterloopsegmenten, quiet = TRUE) %>% st_transform(31370)
vha_waterloop      <- read_sf(file_vha_catc, quiet = TRUE) %>% st_transform(31370)
watervlakken       <- read_sf(file_watervlakken, quiet = TRUE) %>% st_transform(31370)
watergang          <- read_sf(file_watergang, quiet = TRUE) %>% st_transform(31370)

message(paste("Analysedataset geladen:", nrow(dataset_sf), "rijen."))

# --- 2. Initiële Koppeling (Nearest Feature) ---
message("Uitvoeren koppeling met WVLC/VHAS...")

# We zoeken nu in de GEDETAILLEERDE segmenten
idx_riv <- st_nearest_feature(dataset_sf, waterloopsegmenten)
idx_wat <- st_nearest_feature(dataset_sf, watervlakken)

dataset_linked <- dataset_sf %>%
  mutate(
    # Kandidaat open (uit waterloopsegmenten)
    VHAG_cand = waterloopsegmenten$VHAG[idx_riv],
    VHAS_cand = waterloopsegmenten$VHAS[idx_riv],
    # Check of CATC aanwezig is in segmenten file, anders later joinen
    CATC_cand = if("CATC" %in% names(waterloopsegmenten)) waterloopsegmenten$CATC[idx_riv] else NA,
    dist_riv  = as.numeric(st_distance(geometry, waterloopsegmenten[idx_riv, ], by_element = TRUE)),
    
    # Kandidaat gesloten
    WVLC_cand = watervlakken$WVLC[idx_wat],
    dist_wat  = as.numeric(st_distance(geometry, watervlakken[idx_wat, ], by_element = TRUE)),
    
    # Bepaal type en afstand
    type_water  = if_else(dist_riv <= dist_wat, "open", "gesloten"),
    dist_actual = pmin(dist_riv, dist_wat)
  )

# --- 3. Validatie & filtering ---
message("Toepassen validatie logica...")

buffer <- max_link_distance_m

# A. VHAS/WVLC binnen afstand buffer
data_strict <- dataset_linked %>%
  filter(dist_actual <= buffer) %>%
  mutate(
    valid_link = TRUE, # We nemen aan dat binnen buffer altijd OK is (optie 'benefit of doubt')
    link_method = "<buffer"
  )

# B. VHAS buiten afstand buffer -> naar GRB logica
data_candidates <- dataset_linked %>%
  filter(dist_actual > buffer & type_water == "open")

# C. WVLC niet binnen afstand buffer 
data_invalid <- dataset_linked %>%
  filter(dist_actual > buffer & type_water == "gesloten") %>%
  mutate(valid_link = FALSE, link_method = "not linked")

# Verwerking GRB kandidaten
if(nrow(data_candidates) > 0) {
  
  data_grb_checked <- data_candidates %>%
    st_join(watergang, join = st_nearest_feature, suffix = c("", "_wg")) %>%
    mutate(
      vhag_from_grb = VHAG, 
      is_same_vhag = if_else(!is.na(vhag_from_grb), 
                             as.character(VHAG_cand) == as.character(vhag_from_grb), 
                             TRUE), 
      # Buffer berekening
      breedteschatting = OPPERVL / LENGTE,
      max_buffer       = (breedteschatting / 2) + buffer,
      # Dubbele validatie: afstand binnen buffer
      valid_link  = (dist_actual <= max_buffer) & is_same_vhag,
      link_method = if_else(valid_link, "GRB/2 + buffer", "not linked")
    ) %>%
    # Ruim GRB kolommen op (incl de tijdelijke vhag_from_grb)
    select(any_of(colnames(dataset_linked)), valid_link, link_method)
  
} else {
  data_grb_checked <- data_candidates %>% mutate(valid_link = FALSE, link_method = NA)
}

# --- 4. Finaliseren ---
message("Samenvoegen en opschonen...")

dataset_final <- bind_rows(data_strict, data_grb_checked, data_invalid) %>%
  arrange(locID, date) %>%
  mutate(
    VHAG = if_else(valid_link & type_water == "open", as.character(VHAG_cand), NA_character_),
    VHAS = if_else(valid_link & type_water == "open", as.character(VHAS_cand), NA_character_),
    CATC = if_else(valid_link & type_water == "open", as.character(CATC_cand), NA_character_),
    WVLC = if_else(valid_link & type_water == "gesloten", as.character(WVLC_cand), NA_character_),
    distances = if_else(valid_link, dist_actual, NA_real_)
  ) %>%
  select(-ends_with("_cand"), -dist_riv, -dist_wat, -dist_actual, -type_water)

# --- 5. Export ---
# Stats printen
cat(paste0("Totaal records: ", nrow(dataset_final), "\n"))
cat(paste0("Succesvol gekoppeld: ", sum(dataset_final$valid_link), "\n"))
print(table(dataset_final$link_method[dataset_final$valid_link]))

# Visuele check (Histogram)
p <- ggplot(dataset_final %>% filter(valid_link), aes(x = distances)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  geom_vline(xintercept = buffer, color = "red", linetype = "dashed") +
  labs(title = "Afstanden gevalideerde koppelingen", x = "Afstand (m)")
ggsave(file.path(dir_data_intermediate, "check_spatial_link_distances.png"), p, width = 8, height = 6)

# Opslaan
# Indien output paden niet in config stonden, definieer ze hier even tijdelijk:
if (!dir.exists(dirname(file_analyse_dataset_rapport))) dir.create(dirname(file_analyse_dataset_rapport), recursive = TRUE)
write.csv(dataset_final, file = file_analyse_dataset_rapport, quote = TRUE, row.names = FALSE)
message(paste("Dataset opgeslagen in rapport map:", file_analyse_dataset_rapport))


#Visuele check
library(mapview)
library(tidyverse)
library(sf)

map_sf <- dataset_final %>%
  # 1. Soorten samenvoegen tot één kolom voor weergave
  pivot_longer(
    cols = c("procambarus clarkii", "procambarus virginalis", "procambarus acutus", 
             "faxonius limosus", "pacifastacus leniusculus", "faxonius virilis", 
             "pontastacus leptodactylus"),
    names_to = "Soort",
    values_to = "Aanwezig"
  ) %>%
  filter(Aanwezig == 1) %>%
  
  mutate(
    # 2. Status bepalen
    Status = case_when(
      !is.na(VHAG) ~ "VHAG (Waterloop)",
      !is.na(WVLC) ~ "WVLC (Watervlak)",
      TRUE ~ "Niet gekoppeld"
    ),
    # 3. Afstand afronden voor nette popup (1 decimaal)
    Afstand_m = round(distances, 1)
  ) %>%
  
  # 4. Omzetten naar SF
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  
  # 5. SELECTEER HIER de kolommen voor de popup
  # De naam die je hier kiest, wordt de label in de popup
  select(Soort, date, Status, VHAG, WVLC, Afstand_m)

# 6. Kaart genereren
mapview(
  map_sf, 
  zcol = "Status", 
  col.regions = c("gray", "red", "orange"), 
  layer.name = "Kreeften",
  legend = TRUE
)
