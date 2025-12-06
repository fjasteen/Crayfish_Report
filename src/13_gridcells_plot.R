# ====================================================
# Scriptnaam: 13_gridcell_trend_plot.R
# Beschrijving: 
# - Expansie-analyse (toename bezette km-hokken)
# - Filtert data < 2010 weg (start_pre)
# - Filtert soorten zonder historie of expansie
# - Sorteert op totaal aantal (meeste links)
# - FIX: Robuuste arcering
# Gebaseerd op: Concepten uit script [stats] van M. Vermeylen
# ====================================================

source("./src/config.R")
if (!requireNamespace("ggpattern", quietly = TRUE)) install.packages("ggpattern")
library(ggpattern)

# --- 1. Data Inlezen & Filteren ---
if (!file.exists(file_analyse_dataset_rapport)) stop("Run eerst script 03!")

# Dataset
occurrences <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE) %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  # Filter op watergebonden waarnemingen (toch niet nodig)
  # filter(!is.na(VHAG) | !is.na(WVLC)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = crs_wgs84) %>%
  st_transform(crs_lambert)

# Shapefile Vlaanderen
vlaanderen <- st_read(file_vlaanderen_grenzen, quiet = TRUE) %>% 
  st_transform(crs_lambert)

# --- 2. Grid & Koppeling ---
message("Starten met grid berekening...")

grid <- st_make_grid(vlaanderen, cellsize = 1000, square = TRUE) %>% 
  st_sf() %>% mutate(grid_id = 1:n())

occ_grid <- st_join(occurrences, grid, left = FALSE) %>% 
  st_drop_geometry()

# Datums instellen
occ_grid$date <- as.Date(occ_grid$date)
start_pre     <- as.Date("2010-01-01") 
end_pre       <- as.Date("2024-05-31") 

# ==============================================================================
# 3. BEREKENING 
# ==============================================================================

# Stap A: Aggregatie
grid_summary <- occ_grid %>%
  filter(date >= start_pre) %>% 
  pivot_longer(cols = all_of(required_species), names_to = "species", values_to = "present") %>%
  filter(present == 1) %>%
  group_by(species, grid_id) %>%
  summarise(
    is_pre = any(date <= end_pre),                
    is_cw  = any(dat.source == "craywatch_data"), 
    .groups = "drop"
  )

# Stap B: Classificeren
df_trends <- grid_summary %>%
  mutate(Categorie = case_when(
    is_pre ~ "n_basis",                           
    is_cw  ~ "n_cw_winst",                        
    TRUE   ~ "n_rest_winst"                       
  )) %>%
  count(species, Categorie, name = "Aantal") %>%
  mutate(dutch_name = species_labels_dutch[species])

# Stap C: Totalen, Filters & Factoren
df_plot_ready <- df_trends %>%
  group_by(dutch_name) %>%
  mutate(
    n_basis = sum(Aantal[Categorie == "n_basis"]),
    total_winst = sum(Aantal[Categorie != "n_basis"]),
    n_totaal = sum(Aantal),
    toename_pct = if_else(n_basis > 0, (total_winst / n_basis), Inf)
  ) %>%
  ungroup() %>%
  
  # Filters
  filter(n_basis > 0) %>%
  filter(total_winst > 0) %>%
  
  mutate(
    # Factors (Volgorde van stapelen: Basis onderaan)
    Component = factor(Categorie, 
                       levels = c("n_cw_winst", "n_rest_winst", "n_basis"),
                       labels = c("Door Craywatch", "Na Craywatch (GBIF)", "Voor Craywatch")),
    
    # Label
    label_pct = paste0("+", round(toename_pct * 100, 0), "%")
  )

if(nrow(df_plot_ready) == 0) stop("Geen data om te plotten na filtering.")

# --- SORTEREN ---
order_sp <- df_plot_ready %>%
  select(dutch_name, n_totaal) %>% 
  distinct() %>%
  arrange(desc(n_totaal)) %>% 
  pull(dutch_name)

df_plot_ready$dutch_name <- factor(df_plot_ready$dutch_name, levels = order_sp)

# ==============================================================================
# 4. PLOTTEN 
# ==============================================================================

p_trend <- ggplot(df_plot_ready, aes(x = dutch_name, y = Aantal)) +
  
  geom_col_pattern(
    # FIX: Map pattern direct op Component
    aes(fill = Component, pattern = Component),
    
    # Patroon instellingen voor zichtbaarheid
    pattern_fill = "black", 
    pattern_color = "black",
    pattern_alpha = 0.5,     # Iets donkerder voor betere zichtbaarheid
    pattern_density = 0.1,   # Dichtheid van de streepjes
    pattern_spacing = 0.05,  # Afstand tussen streepjes
    pattern_angle = 45,
    
    color = "black",         # Rand om de balk
    linewidth = 0.2
  ) +
  
  # Kleuren
  scale_fill_manual(
    values = c(
      "Voor Craywatch"      = "#6BA1D3",
      "Na Craywatch (GBIF)" = "#33A02C",
      "Door Craywatch"      = "#33A02C"
    )
  ) +
  
  # Arcering: Wijs specifiek toe welk level een streep krijgt
  scale_pattern_manual(
    values = c(
      "Door Craywatch"      = "stripe", 
      "Na Craywatch (GBIF)" = "none", 
      "Voor Craywatch"      = "none"
    )
  ) +
  
  # Labels
  geom_text(
    data = unique(df_plot_ready[, c("dutch_name", "n_totaal", "label_pct")]),
    aes(x = dutch_name, y = n_totaal, label = label_pct),
    vjust = -0.5, size = 3, fontface = "bold"
  ) +
  
  labs(
    title = "Aantal bezette kilometerhokken per soort",
    subtitle = "voor en na Craywatch",
    y = "Aantal bezette 1x1 km grid cellen", x = ""
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  ) +
  
  # Verberg de patroon-legenda (de kleurenlegenda volstaat)
  guides(pattern = "none")

# Opslaan
file_plot <- file.path(dir_gridcell_output, "gridcell_plot.png")
if(!dir.exists(dirname(file_plot))) dir.create(dirname(file_plot), recursive = TRUE)

ggsave(file_plot, p_trend, width = 10, height = 7, bg = "white")

message(paste("Klaar! Plot opgeslagen in:", file_plot))