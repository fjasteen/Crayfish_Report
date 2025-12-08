# ==============================================================================
# Script: 04_spatial_link_core.R
# Auteur: Stien Mertens
# Refactored by: Frédérique Steen
# Doel:   Koppeling masterdataset aan VHAG/WVLC met dynamische validatie
# ==============================================================================

# --- 0. Setup ---
source("./src/config.R")
source("./src/helpers.R")
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

# 2. Uitvoeren van de functie
# Dit vervangt regels 40 t/m 115 van je oude script!
dataset_linked <- match_points_to_water(
  points_sf = dataset_sf,
  rivers = waterloopsegmenten,
  lakes = watervlakken,
  watergang = watergang,
  buffer_m = max_link_distance_m # Uit config.R
)

# 3.  CATC erbij halen 
vha_waterloop <- read_sf(file_vha_catc) %>% st_transform(31370) %>% st_drop_geometry()

dataset_final <- dataset_linked %>%
  left_join(
    waterloopsegmenten %>% 
      st_drop_geometry() %>% 
      mutate(VHAS = as.character(VHAS)) %>%  # <--- HIER: Converteer naar tekst
      select(VHAS, CATC), 
    by = "VHAS"
  )


# 4. Export
write.csv(st_drop_geometry(dataset_final), file_analyse_dataset_rapport, row.names = FALSE)
message("Script 04 Klaar.")

# Stats printen
cat(paste0("Totaal records: ", nrow(dataset_final), "\n"))
cat(paste0("Succesvol gekoppeld: ", sum(dataset_final$valid_link), "\n"))
print(table(dataset_final$link_method[dataset_final$valid_link]))

# Visuele check (Histogram)
p <- ggplot(dataset_final %>% filter(valid_link), aes(x = distance_linked)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  geom_vline(xintercept = max_link_distance_m, color = "red", linetype = "dashed") +
  labs(title = "Afstanden gevalideerde koppelingen", x = "Afstand (m)")
ggsave(file.path(dir_data_intermediate, "check_spatial_link_distances.png"), p, width = 8, height = 6)



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
    Afstand_m = round(distance_linked, 1)
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
