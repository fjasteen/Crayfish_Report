# ====================================================
# Scriptnaam: 15_craywatch_maps.R
# Auteur: Frédérique Steen (Geoptimaliseerd)
# Beschrijving: 
# - Genereert statische kaarten voor Craywatch 2024
# - STANDAARD ACHTERGROND: VHA Categorie 0 en 1 (Uniform voor alle kaarten)
# - Kaart 1: Totaal overzicht
# - Kaart 2: Gemeente overzicht
# - Kaart 3: Focus met Jitter
# ====================================================

# --- 0. Instellingen laden ---
source("./src/config.R")
library(ggspatial) 

# Definieer soorten en output map
required_species <- tolower(gbif_species)
dir_cw_maps <- file.path(dir_data_output, "craywatch_maps")
if(!dir.exists(dir_cw_maps)) dir.create(dir_cw_maps, recursive = TRUE)

# --- 1. Data Inlezen ---

# A. Analyse Dataset
if (!file.exists(file_analyse_dataset_rapport)) stop("Run eerst script 03!")
df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)

cw_data <- df_analyse %>% filter(dat.source == "craywatch_data")

# B. Shapefiles (Laden & Voorbereiden)
message("Shapefiles laden (Vlaanderen & VHA CATC 0/1)...")

# 1. Vlaanderen (Masker)
vlaanderen <- st_read(file_vlaanderen_grenzen, quiet = TRUE) %>% 
  st_transform(crs_lambert)

# 2. Gemeenten (voor kaart 2)
gemeenten_clip <- st_read(file_gemeenten, quiet = TRUE) %>%
  st_transform(crs_lambert) %>%
  st_intersection(vlaanderen)

# 3. VHA Waterlopen (De nieuwe standaard achtergrond)
# We laden de VHA shapefile en filteren direct op CATC 0 en 1
vha_raw <- st_read(file_vha_catc, quiet = TRUE) %>% 
  st_transform(crs_lambert)

# CATC 0: Bevaarbare waterlopen (De 'hoofdaders')
cat0_lines <- vha_raw %>% 
  filter(CATC == 0) %>% 
  st_intersection(vlaanderen)

# CATC 1: Onbevaarbare waterlopen cat. 1 (De belangrijke zijlopen)
cat1_lines <- vha_raw %>% 
  filter(CATC == 1) %>% 
  st_intersection(vlaanderen)

message("Shapefiles verwerkt. Gebruik CATC 0 en 1 als basislaag.")

# --- 2. Data Preparatie (Punten) ---

# Pivot naar long format
cw_long <- cw_data %>%
  select(locID, Latitude, Longitude, CATC, any_of(required_species)) %>%
  pivot_longer(cols = any_of(required_species), names_to = "species", values_to = "present")

# A. Aanwezigheden
sf_aanwezig <- cw_long %>%
  filter(present == 1) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs_lambert)

# B. Afwezigheden
locs_met_vangst <- unique(sf_aanwezig$locID)

sf_afwezig <- cw_data %>%
  filter(!locID %in% locs_met_vangst) %>%
  mutate(species = "Afwezigheid") %>%
  select(locID, species, CATC, Latitude, Longitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs_lambert)

# --- 3. Kaart Generatie ---

# Styling definities
point_size_abs  <- 1.0
point_size_pres <- 1.0

# Kleuren voor de waterlopen (Uniform over alle kaarten)
col_cat0 <- "#004C99" # Donkerblauw (Hoofd)
col_cat1 <- "#6BA1D3" # Lichter blauw (Secundair)
size_cat0 <- 0.4
size_cat1 <- 0.3

theme_kaart <- theme_void() +
  theme(
    legend.title = element_blank(), 
    legend.text = element_text(size = 8, face = "italic"), 
    legend.key.size = unit(0.4, "cm"),
    legend.position = "bottom",
    plot.title = element_text(face = "italic", hjust = 0.5)
  )

# --- KAART 1: Totaal Overzicht ---
message("Genereren Kaart 1: Totaal (Basis: CATC 0+1)...")

p_totaal <- ggplot() +
  # Achtergrond Vlaanderen
  geom_sf(data = vlaanderen, fill = "#EEEEEE", size = 0.2, colour = "black") +
  
  # Waterlopen (De nieuwe standaard)
  geom_sf(data = cat1_lines, size = size_cat1, colour = col_cat1) +
  geom_sf(data = cat0_lines, size = size_cat0, colour = col_cat0) +
  
  # Punten
  geom_sf(data = sf_afwezig, aes(color = species), size = point_size_abs) +
  geom_sf(data = sf_aanwezig, aes(color = species), size = point_size_pres) +
  
  scale_color_manual(values = species_colors, labels = species_labels_dutch) +
  guides(color = guide_legend(override.aes = list(size = 3), ncol = 2)) +
  theme_kaart

ggsave(filename = file.path(dir_cw_maps, "kaart_1_totaal_overzicht.png"), 
       plot = p_totaal, width = 15, height = 8, units = "cm", dpi = 400)


# --- KAART 2: Gemeenten ---
message("Genereren Kaart 2: Gemeenten (Basis: CATC 0+1)...")

p_gemeente <- ggplot() +
  geom_sf(data = vlaanderen, fill = "#EEEEEE", size = 0.2, colour = "black") +
  geom_sf(data = gemeenten_clip, size = 0.1, colour = "lightgrey", fill = NA) +
  
  # Waterlopen (Dezelfde als kaart 1)
  geom_sf(data = cat1_lines, size = size_cat1, colour = col_cat1) +
  geom_sf(data = cat0_lines, size = size_cat0, colour = col_cat0) +
  
  # Punten
  geom_sf(data = sf_afwezig, aes(color = species), size = point_size_abs) +
  geom_sf(data = sf_aanwezig, aes(color = species), size = point_size_pres) +
  
  scale_color_manual(values = species_colors, labels = species_labels_dutch) +
  guides(color = guide_legend(override.aes = list(size = 3), ncol = 2)) +
  theme_kaart

ggsave(filename = file.path(dir_cw_maps, "kaart_2_gemeenten.png"), 
       plot = p_gemeente, width = 15, height = 8, units = "cm", dpi = 400)


# --- KAART 3: Focus met Jitter ---
message("Genereren Kaart 3: Focus met Jitter...")

# 1. Filter Data (We plotten hier enkel punten die ook effectief op deze waterlopen liggen)
#    Dit is optioneel, als je alle punten wilt zien, gebruik sf_afwezig/sf_aanwezig
sf_afwezig_cat <- sf_afwezig %>% filter(CATC %in% c(0, 1))
sf_aanwezig_cat <- sf_aanwezig %>% filter(CATC %in% c(0, 1))

# 2. Handmatige toevoeging (Punt Clarkii IJzer - correctie)
punt_ijzer <- st_as_sf(
  data.frame(
    long = c(2.81524, 2.83965), 
    lat = c(50.98940, 51.00636), 
    species = "procambarus clarkii" 
  ),
  coords = c("long", "lat"), crs = 4326
) %>% st_transform(crs_lambert)

sf_aanwezig_cat_total <- bind_rows(sf_aanwezig_cat, punt_ijzer)

# 3. Jitter (500m)
sf_aanwezig_jitter <- st_jitter(sf_aanwezig_cat_total, amount = 500)

p_catc <- ggplot() +
  geom_sf(data = vlaanderen, fill = "#EEEEEE", size = 0.2, colour = "black") +
  geom_sf(data = gemeenten_clip, size = 0.1, colour = "lightgrey", fill = NA) +
  
  # Waterlopen (Dezelfde styling)
  geom_sf(data = cat1_lines, size = size_cat1, colour = col_cat1) +
  geom_sf(data = cat0_lines, size = size_cat0, colour = col_cat0) +
  
  # Punten
  geom_sf(data = sf_afwezig_cat, aes(color = species), size = point_size_abs, alpha = 0.8) +
  geom_sf(data = sf_aanwezig_jitter, aes(color = species), size = point_size_pres) +
  
  scale_color_manual(values = species_colors, labels = species_labels_dutch) +
  guides(color = guide_legend(override.aes = list(size = 3), ncol = 2)) +
  theme_kaart

ggsave(filename = file.path(dir_cw_maps, "kaart_3_cat0_1_focus.png"), 
       plot = p_catc, width = 15, height = 8, units = "cm", dpi = 400)

message("Klaar! Alle kaarten (gebaseerd op CATC 0/1) opgeslagen in: ", dir_cw_maps)