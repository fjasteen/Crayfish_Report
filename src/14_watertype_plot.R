# ====================================================
# Scriptnaam: 14_watertype_plot_glmm.R
# Project: Craywatch
# Datum: 01-12-2025
# Beschrijving: 
# - Classificeert waarnemingen als gesloten of open
# - Plot frequentie per watertype  
# - Plot soorten per watertype 
# - Plot vangstsucces per watertype  
# - Statistiek: GLMM
# Gebaseerd op: Concepten uit script [voorkomen_lentisch_lotisch] van M. Vermeylen
# ====================================================

# --- 0. Instellingen laden ---
source("./src/config.R")

library(lme4)   # Voor GLMM
library(DHARMa)
library(emmeans)
library(car) # Voor Type III Anova

# --- 1. Data Inlezen ---
if (!file.exists(file_analyse_dataset_rapport)) stop("Run eerst script 03!")

df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)

# 2. Check op Ruimtelijke Koppeling (Script 04)
if (!"valid_link" %in% names(df_analyse)) {
  stop(
    "\n\n======================================================================",
    "\nFOUT: De dataset mist de ruimtelijke koppeling (VHAG/WVLC).",
    "\n\nDe kolommen 'valid_link' en 'link_method' ontbreken.",
    "\n-> Voer eerst script '04_spatial_link_core.R' uit om dit toe te voegen.",
    "\n======================================================================\n"
  )
} else {
  message("Ruimtelijke koppeling geverifieerd. Analyse start...")
}

# --- 2. Data voorbereiden ---

cray_water <- df_analyse %>%
  mutate(
    # logica: WVLC (gesloten), anders (open)
    water_type = if_else(!is.na(WVLC), "gesloten", "open"),
    water_type = factor(water_type, levels = water_levels)
  )

print("Aantal waarnemingen per watertype:")
print(table(cray_water$water_type))

# Omzetten naar long format
cray_long <- cray_water %>%
  pivot_longer(
    cols = all_of(tolower(gbif_species)), 
    names_to = "species",
    values_to = "presence"
  ) %>%
  filter(!is.na(presence)) 

# ==============================================================================
# 3. VISUALISATIES
# ==============================================================================

# ------------------------------------------------------------------------------
# Plot frequentie per watertype
# Op basis van het totale aantal waarnemingen, welk % bevindt zich in welk type water?"
# Data: GBIF + Craywatch (presence == 1)
# ------------------------------------------------------------------------------

data_distributie <- cray_long %>%
  filter(presence == 1) %>% 
  group_by(species, water_type) %>%
  summarise(n = n(), .groups = "drop") %>% 
  complete(species, water_type, fill = list(n = 0)) %>%
  group_by(species) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(dutch_name = factor(species_labels_dutch[species], levels = species_labels_dutch))

p_distributie <- ggplot(data_distributie, aes(x = dutch_name, y = percentage, fill = water_type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(
    aes(label = ifelse(n > 0, n, "")), 
    position = position_stack(vjust = 0.5), 
    size = 2.5, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = water_colors) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(limits = c(0, 101), expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Relatieve frequentie van de waarnemingen",
    subtitle = "per type systeem en soort",
    y = "Percentage van de populatie (%)",
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
ggsave(file_plot_water_distributie, p_distributie, width=10, height=6)


# ------------------------------------------------------------------------------
# Plot soorten per type
# Data = GBIF + Craywatch
# Als ik in een type systeem vang, welke soort kom ik dan tegen?
# ------------------------------------------------------------------------------

data_community <- cray_long %>%
  filter(presence == 1) %>%
  group_by(water_type, species) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(species = factor(species, levels = names(species_labels_dutch)))

p_community <- ggplot(data_community, aes(x = water_type, y = percentage, fill = species)) + 
  geom_bar(stat = "identity", position = "stack", width = 0.7) + 
  
  scale_fill_manual(
    values = species_colors,      
    labels = species_labels_dutch  
  ) +      
  
  labs(
    title = "Soortensamenstelling per watertype",
    subtitle = "",
    y = "Aandeel in de gemeenschap (%)",
    x = "", fill = "Soort"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black")
  )

print(p_community)
ggsave(file_plot_water_community, p_community, width=8, height=6)


# ------------------------------------------------------------------------------
# Plot vangstsucces per type systeem
# Data = Craywatch (deler is totaal aantal succesvolle bemonsteringen, 
# niet beschikbaar voor GBIF data)
# Hoe groot is de pakkans per bemonsteringsessie?
# ------------------------------------------------------------------------------

data_vangstsucces <- cray_long %>%
  filter(dat.source == "craywatch_data") %>% 
  filter(!is.na(presence)) %>%
  group_by(species, water_type) %>%
  summarise(
    n_traps = n(),
    n_succes = sum(presence),   
    percentage = mean(presence) * 100,
    .groups = "drop"
  ) %>%
  # Filter soorten eruit die nergens gevangen zijn
  group_by(species) %>%
  filter(sum(percentage) > 0) %>% 
  ungroup() %>%
  
  # Maak compleet voor vaste balkbreedte
  complete(species, water_type, fill = list(percentage = 0, n_traps = 0)) %>%
  mutate(dutch_name = factor(species_labels_dutch[species], levels = species_labels_dutch))

max_y <- max(data_vangstsucces$percentage, na.rm = TRUE)

p_vangstsucces <- ggplot(data_vangstsucces, aes(x = dutch_name, y = percentage, fill = water_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  
  geom_text(
    aes(
      label = ifelse(
        percentage > 0, 
        paste0(round(percentage, 1), "%\n(", n_succes, ")"), 
        ""
      )
    ), 
    position = position_dodge(width = 0.8), 
    vjust = -0.5, 
    size = 2.5,
    lineheight = 0.8 
  ) +
  
  scale_fill_manual(values = water_colors) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(limits = c(0, max_y * 1.1), expand = expansion(mult = c(0, 0))) +
  
  labs(
    title = "Vangstkans voor een soort per type systeem",
    subtitle = "op basis van de gestandaardiseerde monitoring",
    y = "Vangstkans (%)",
    x = "", fill = ""
  ) +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black")
  )

print(p_vangstsucces)
ggsave(file_plot_water_vangstsucces, p_vangstsucces, width=10, height=6)


