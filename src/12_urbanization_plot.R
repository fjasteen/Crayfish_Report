# ====================================================
# Scriptnaam: 12_urbanisation_plot.R
# Auteur: Frédérique Steen
# Datum: 01-12-2025
# Beschrijving: 
# - Koppelt waarnemingen aan verstedelijkingsgraad
# - Plot frequentie per type verstedelijking 
# - Plot soorten per type 
# - Plot vangstsucces per type verstedelijking 
# Gebaseerd op: Concepten uit script [voorkomen_urbanisatie] van M. Vermeylen
# ====================================================

# --- 0. Instellingen laden ---
source("./src/config.R")

# --- 1. Data Inlezen ---

# A. laad analyse dataset
if (!file.exists(file_analyse_dataset_rapport)) stop("Run eerst script 03!")

df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE) #%>%
  # filter op gekoppelde data voor consistentie (niet nodig)
  #filter(!is.na(VHAG) | !is.na(WVLC))

# B. Verstedelijking shapefile
file_urban_map <- file.path(dir_shapefiles, "verstedelijking.gpkg") 
if (!file.exists(file_urban_map)) stop(paste("Bestand niet gevonden:", file_urban_map))

layers <- st_layers(file_urban_map)
urban_sf <- st_read(file_urban_map, layer = layers$name[1], quiet = TRUE) %>%
  st_transform(crs_lambert) %>%
  select(geom, type_urban = type)

# --- 2. Ruimtelijke koppeling ---
cray_sf <- df_analyse %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = crs_wgs84) %>%
  st_transform(crs_lambert)

# spatial join met statistische sectoren
cray_urban <- st_join(cray_sf, urban_sf, join = st_intersects) %>%
  filter(!is.na(type_urban)) %>% 
  st_drop_geometry()

print("Aantal punten per omgevingstype:")
print(table(cray_urban$type_urban))

# --- 3. Data voorbereiding ---

cray_urban$type_urban <- factor(
  cray_urban$type_urban, 
  levels = urban_levels
)

# Omzetten naar long format
cray_long <- cray_urban %>%
  pivot_longer(
    cols = all_of(tolower(gbif_species)), 
    names_to = "species",
    values_to = "presence"
  ) %>%
  filter(!is.na(presence))

# ==============================================================================
# 4. VISUALISATIES
# ==============================================================================

# ------------------------------------------------------------------------------
# Plot frequentie per type verstedelijking
# Data = GBIF + Craywatch
# ------------------------------------------------------------------------------

# 1. Data voorbereiden 
data_distributie <- cray_long %>%
  filter(presence == 1) %>% 
  group_by(species, type_urban) %>%
  summarise(n = n(), .groups = "drop") %>% 
  complete(species, type_urban, fill = list(n = 0)) %>%
  group_by(species) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(dutch_name = factor(species_labels_dutch[species], levels = species_labels_dutch))

# 2. Plotten 
p_distributie <- ggplot(data_distributie, aes(x = dutch_name, y = percentage, fill = type_urban)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(
    aes(label = ifelse(n > 0, n, "")), 
    position = position_stack(vjust = 0.5), 
    size = 2.5,
    color = "white",
    fontface = "bold"
  ) +
  
  scale_fill_manual(values = urban_colors) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(limits = c(0, 101), expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Relatieve frequentie van de waarnemingen",
    subtitle = "per type verstedelijking en soort",
    y = "Percentage van de waarnemingen (%)",
    x = "", fill = ""
  ) +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black")
  )

print(p_distributie)
ggsave(file.path(dir_urbanisation_output, "plot_urban_distributie.png"), p_distributie, width=10, height=6)

# ------------------------------------------------------------------------------
# Plot soorten per type
# Data = GBIF + Craywatch
# Als ik in een type verstedelijking vang, welke soort kom ik dan tegen?
# ------------------------------------------------------------------------------
data_community <- cray_long %>%
  filter(presence == 1) %>%
  group_by(type_urban, species) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(dutch_name = factor(species_labels_dutch[species], levels = species_labels_dutch))

p_community <- ggplot(data_community, aes(x = type_urban, y = percentage, fill = species)) + 
  geom_bar(stat = "identity", position = "stack") + 
    scale_fill_manual(
    values = species_colors,      
    labels = species_labels_dutch  
  ) +      
    labs(
    title = "Soortensamenstelling per type verstedelijking",
    subtitle = "",
    y = "Aandeel in de gemeenschap (%)",
    x = "", fill = "Soort"
  ) +
  theme_minimal() + 
  theme(legend.position = "right")

print(p_community)
ggsave(file.path(dir_urbanisation_output, "plot_urban_community.png"), p_community, width=8, height=6)


# ------------------------------------------------------------------------------
# Plot vangstsucces per type verstedelijking
# Data = Craywatch (kan enkel indien totaal aantal bemonsteringen wordt in 
# rekening gebracht)
# Hoe groot is de pakkans per bemonstering?
# ------------------------------------------------------------------------------
# 1. Data voorbereiden
data_vangstsucces <- cray_long %>%
  filter(dat.source == "craywatch_data") %>% 
  filter(!is.na(presence)) %>%
  group_by(species, type_urban) %>%
  summarise(
    n_traps = n(),
    percentage = mean(presence) * 100,
    .groups = "drop"
  ) %>%
  # Filter soorten eruit die overal 0% hebben (Californisch, Turks)
  group_by(species) %>%
  filter(sum(percentage) > 0) %>% # Behoud alleen als er tenminste ergens iets gevangen is
  ungroup() %>%
  complete(species, type_urban, fill = list(percentage = 0, n_traps = 0)) %>%
  
  # Namen toevoegen
  mutate(dutch_name = factor(species_labels_dutch[species], levels = species_labels_dutch))

# 2. Bepaal de maximale waarde voor de Y-as limiet
max_y <- max(data_vangstsucces$percentage, na.rm = TRUE)

# 3. Plot
p_vangstsucces <- ggplot(data_vangstsucces, aes(x = dutch_name, y = percentage, fill = type_urban)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  
  # label toont percentage 
  geom_text(
    aes(label = ifelse(percentage > 0, paste0(round(percentage, 1), "%"), "")), 
    position = position_dodge(width = 0.8), 
    vjust = -0.5, 
    size = 2.5
  ) +
  scale_fill_manual(values = urban_colors) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(
    limits = c(0, max_y * 1.1), 
    expand = expansion(mult = c(0, 0)) 
  ) +
  labs(
    title = "Vangstkans voor een soort per type verstedelijking",
    subtitle = "op basis van de gestandaardiseerde monitoring",
    y = "Vangstskans (%)",
    x = "", fill = ""
  ) +
  theme_minimal() + 
  theme(legend.position = "bottom")

print(p_vangstsucces)
ggsave(file.path(dir_urbanisation_output, "plot_urban_vangstsucces.png"), p_vangstsucces, width=10, height=6)
