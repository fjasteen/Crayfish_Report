# ====================================================
# Scriptnaam: 18_craywatch_report_complete.R
# Beschrijving: 
# 1. Laadt data en Riparias Shapefile.
# 2. Filtert data RUIMTELIJK (enkel punten binnen de shapefile).
# 3. Genereert statistiek tabellen voor rapportage.
# 4. Genereert de overzichtskaart.
# ====================================================

# --- 0. Instellingen & Packages ---
source("./src/config.R")

# Hulpfunctie package check
check_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
check_install("ggspatial")
check_install("kableExtra")

library(tidyverse)
library(sf)
library(ggspatial)
library(knitr)
library(kableExtra)
library(lubridate)

# Paden instellen
dir_output_riparias <- file.path(dir_data_output, "riparias")
dir_output_riparias <- file.path(dir_data_output, "maps", "craywatch_osm")
if(!dir.exists(dir_output_riparias)) dir.create(dir_output_riparias, recursive = TRUE)

# --- 1. Data & Shapefile Inlezen ---
message("--- Stap 1: Data laden ---")

# A. Dataset
if (!file.exists(file_analyse_dataset_rapport)) stop("Dataset niet gevonden.")
df <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)

# Filter eerst grof op bron
cw_raw <- df %>% filter(dat.source == "craywatch_data")

# B. Shapefile
path_riparias <- "./data/input/shapefiles/riparias.shp"
if(!file.exists(path_riparias)) stop("Shapefile niet gevonden!")

riparias_sf <- st_read(path_riparias, quiet = TRUE) %>% 
  st_transform(crs_lambert) # Lambert72

# --- 2. Ruimtelijke Filtering ---
message("--- Stap 2: Ruimtelijke filter toepassen ---")

# --- Stap 2: Ruimtelijke Filtering ---
message("--- Stap 2: Ruimtelijke filter toepassen ---")

# 1. Maak meteen een SF object van de ruwe data
# We hoeven remove=FALSE niet eens te gebruiken als we hierna in SF blijven werken
cw_sf_raw <- cw_raw %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs_lambert) # Lambert72

# 2. Ruimtelijke filter (behoudt geometry!)
cw_sf_filtered <- st_filter(cw_sf_raw, riparias_sf)

# 3. Voor de tabellen (stap 3) hebben we wel een gewone tabel nodig
# Maar we noemen die anders om verwarring te voorkomen
cw_data_table <- cw_sf_filtered %>% st_drop_geometry()

if(nrow(cw_sf_filtered) == 0) stop("Geen data over na ruimtelijke filter!")


# --- 3. Statistieken Genereren (Tabellen) ---
message("--- Stap 3: Tabellen berekenen ---")

# Tabel 1: Algemeen
stats_general <- tibble(
  Metric = c(
    "Startdatum", "Einddatum",
    "Aantal Unieke Locaties (Binnen Riparias)", 
    "Aantal Vrijwilligers (Binnen Riparias)", 
    "Totaal Aantal Sessies",
    "Totaal Aantal Records"
  ),
  Waarde = c(
    as.character(min(cw_data_final$date, na.rm = TRUE)),
    as.character(max(cw_data_final$date, na.rm = TRUE)),
    as.character(n_distinct(cw_data_final$locID)),
    as.character(n_distinct(cw_data_final$vrijwillID)),
    as.character(n_distinct(cw_data_final$session_nr)),
    as.character(nrow(cw_data_final))
  )
)

# Tabel 2: Per Jaar
target_species <- tolower(gbif_species)
species_cols <- names(cw_data_final)[names(cw_data_final) %in% target_species]

stats_year <- cw_data_final %>%
  mutate(Year = as.character(year(date))) %>%
  group_by(Year) %>%
  summarise(
    Vrijwilligers = n_distinct(vrijwillID),
    Locaties = n_distinct(locID),
    Sessies = n_distinct(session_nr),
    across(all_of(species_cols), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  bind_rows(
    cw_data_final %>%
      summarise(
        Year = "TOTAAL",
        Vrijwilligers = n_distinct(vrijwillID), # Let op: unieke vrijwilligers over totaal, niet som van jaren
        Locaties = n_distinct(locID),
        Sessies = n_distinct(session_nr),
        across(all_of(species_cols), \(x) sum(x, na.rm = TRUE))
      )
  )

# Printen en Opslaan Tabellen
print(kable(stats_general, format = "simple", caption = "Project Samenvatting"))
print(kable(stats_year, format = "simple", caption = "Details per Jaar"))

write_csv(stats_general, file.path(dir_output_tables, "riparias_summary_general.csv"))
write_csv(stats_year, file.path(dir_output_tables, "riparias_summary_year.csv"))


# --- 4. Kaart Genereren ---
message("--- Stap 4: Kaart maken ---")

# Data voorbereiden voor plot (Presences vs Absences)
cw_long <- cw_data_final %>%
  select(locID, Latitude, Longitude, any_of(target_species)) %>%
  pivot_longer(cols = any_of(target_species), names_to = "species", values_to = "present")

# Presences (Rood/Kleur)
df_pres <- cw_long %>% filter(present == 1)
sf_pres <- df_pres %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(crs_lambert)

# Absences (Grijs) - Locaties in de gefilterde set zonder vangst
locs_met_vangst <- unique(df_pres$locID)
df_abs <- cw_data_final %>% filter(!locID %in% locs_met_vangst)
sf_abs <- df_abs %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(crs_lambert)

# Kleuren en Labels
if(!exists("species_colors")) species_colors <- setNames(rainbow(length(target_species)), target_species)
labeller_fun <- function(x) str_to_sentence(gsub("_", " ", x))

# Plot
p <- ggplot() +
  # Extent & Achtergrond
  geom_sf(data = riparias_sf, fill = NA, color = NA) +
  annotation_map_tile(type = "cartolight", zoom = NULL, progress = "none") +
  
  # Riparias Grens
  geom_sf(data = riparias_sf, fill = NA, color = "black", size = 0.5) +
  
  # Punten
  geom_sf(data = sf_abs, color = "white", size = 3.5) + 
  geom_sf(data = sf_abs, color = "grey60", size = 2, alpha = 0.8) +
  
  geom_sf(data = sf_pres, color = "white", size = 4) +
  geom_sf(data = sf_pres, aes(color = species), size = 2.5) +
  
  scale_color_manual(values = species_colors, labels = labeller_fun, name = "Soort") +
  
  annotation_scale_bar(location = "br", width_hint = 0.2, style = "ticks") +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_minimal()) +
  
  labs(title = "Craywatch Waarnemingen (Riparias Gebied)",
       subtitle = paste0("Totaal ", nrow(cw_data_final), " records op ", n_distinct(cw_data_final$locID), " locaties"),
       caption = "Grijs = Geen vangst\n© OpenStreetMap contributors") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())

# Opslaan Kaart
output_map_file <- file.path(dir_output_riparias, "craywatch_riparias_filtered.png")
ggsave(output_map_file, p, width = 25, height = 18, units = "cm", dpi = 300, bg = "white")

message("Klaar! Tabellen en kaart zijn opgeslagen.")