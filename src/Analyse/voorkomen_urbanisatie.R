############################################################
# Script:    Voorkeur_urbanisatie.R
# Doel:      Nagaan of de verschillende soorten invasieve rivierkreeften voorkomen in meer stedelijke of natuurlijkere omgevingen
# Auteur:    Margot Vermeylen
# Datum:     21-08-2025
############################################################

library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lme4) 
library(broom) 
library(purrr) 
library(rlang) 
library(DHARMa) 
library(mapview)
library(lmerTest)
library(leaflet)
library(tidyverse)
library(effects)
library(jtools)
library(ROCR)


# ==============================================================================
# 1. DATA INLEZEN EN VOORBEREIDING
# ==============================================================================

# Data en lagen inlezen
# Zorg ervoor dat de paden correct zijn op jouw systeem
verstedelijking <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/verstedelijking.gpkg")
CF <- read.csv("~/GitHub/Craywatch-Rapport/R/data/output/analyse_dataset.csv")

# Verstedelijking laag: filter op relevante kolommen en transformeer
verstedelijking <- st_transform(verstedelijking, 31370)
verstedelijking <- verstedelijking[, c("geom", "type")] # Behoud enkel geom en 'type'

# mapview(verstedelijking)

# Craywatch data: transformeer naar sf en projecteer
CF_sf <- CF %>%
  # Filter waarnemingen zonder coördinaten
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(31370)

# Maak een intersect tussen de craywatch data en verstedelijking
# Dit voegt de kolom 'type' (van verstedelijking) toe aan de CF data
CF_verstedelijking <- st_join(CF_sf, verstedelijking, join = st_intersects)

# Behoud enkel waarnemingen waar 'type' (verstedelijking) is ingevuld
CF_verstedelijking_schoon <- CF_verstedelijking %>%
  filter(!is.na(type))

# Hoeveel van elk type bemonsterd (punten)
type_bemonsterd_punten <- CF_verstedelijking_schoon %>%
  st_drop_geometry() %>%
  group_by(type) %>%
  count(name = "aantal_punten")
print("Aantal bemonsterde punten per verstedelijkingstype:")
print(type_bemonsterd_punten)

# ==============================================================================
# 2. CLUSTERING PER VERSTEDELIJKINGSCATEGORIE
# ==============================================================================

# CF_verstedelijking_clustered <- CF_verstedelijking_schoon %>%
#   filter(!(is.na(WVLC) & is.na(VHAG) & is.na(CATC))) %>%
#   mutate(
#     cluster_id = coalesce(as.character(VHAG), as.character(WVLC), "Overig")
#   ) %>%
#   # zet de dataframe om naar een sf-object
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
#   st_transform(31370)
# 
# # Tel het aantal unieke locaties (clusters) per type water
# type_bemonsterd <- CF_verstedelijking_clustered %>%
#   distinct(cluster_id, type) %>% 
#   count(type, name = "aantal_locaties") 
# print("Aantal unieke locaties (clusters) per type verstedelijking:")
# print(type_bemonsterd)
# 
# # maak definitieve cluster_id's
# CF_verstedelijking_clustered <- CF_verstedelijking_clustered %>%
#   mutate(
#     # Combineer de 'type' groep met het cluster-ID-binnen-de-groep
#     cluster = paste(type, cluster_id, sep = "_")
#   ) %>%
#   select(-cluster_id)

# ==============================================================================
# 3. TELLINGEN PER SOORT EN TYPE VERSTEDELIJKING
# ==============================================================================

CF_verstedelijking_schoon$type <- factor(CF_verstedelijking_schoon$type, 
                            levels = c("landelijk", "randstedelijk", "verstedelijkt"))

# Volledige lijst van alle soorten in de dataset
alle_soorten_kolommen <- c(
  "faxonius.limosus",
  "procambarus.virginalis",
  "procambarus.acutus",
  "faxonius.virilis",
  "procambarus.clarkii",
  "pontastacus.leptodactylus",
  "pacifastacus.leniusculus"
)

# 1. Herschep de CF_clustered data van breed naar lang formaat
species_presence_long <- CF_verstedelijking_schoon %>%
  st_drop_geometry() %>%
  select(type, all_of(alle_soorten_kolommen)) %>%
  pivot_longer(
    cols = all_of(alle_soorten_kolommen),
    names_to = "soort",
    values_to = "Aanwezig"
  )

# aanwezigheidskolom als numeriek zetten om te kunnen groeperen
species_presence_long <- species_presence_long %>%
  mutate(
    aanwezigheid = as.numeric(Aanwezig)) %>%
  select(-Aanwezig)

# waarnemingen per type en soort (zonder clustering)
species_counts_full <- species_presence_long %>%
  filter(aanwezigheid == 1) %>%
  select(-aanwezigheid)

species_counts_full <- species_counts_full %>%
  group_by(soort, type) %>%
  summarise(aantal = n(), .groups = 'drop')
print(species_counts_full)


# # data groeperen per cluster
# species_long <- species_presence_long %>%
#   group_by(cluster, soort, type) %>%
#   summarise(
#     aanwezigheid_clustered = case_when(
#       any(aanwezigheid == 1, na.rm = TRUE) ~ 1,       # Als er minstens een 1 is = 1
#       any(aanwezigheid == 0, na.rm = TRUE) ~ 0,       # Als er GEEN 1 is en minstens een 0 = 0
#       sum(!is.na(aanwezigheid)) == 0 ~ NA_real_      # Als er geen 1 en geen 0 is (alleen NA's) = NA
#   ))
# 
# species_counts <- species_long %>%
#   filter(aanwezigheid_clustered == 1) %>%
#   group_by(soort, type) %>%
#   # Tel het aantal unieke clusters per soort
#   summarise(
#     Aantal_Locaties = n_distinct(cluster),
#     .groups = 'drop'
#   ) %>%
#   
#   pivot_wider(
#     names_from = type,
#     values_from = Aantal_Locaties,
#     values_fill = 0 # Vul 0 in waar de soort niet voorkwam in dat type water
#   )
# 
# print("Aantal geclusterde locaties (unieke cluster_id's) met aanwezigheid per soort:")
# print(species_counts)
# 
# # Voeg de kolommen geometry en BKNR toe aan species_long om de modellen te kunnen runnen
# populatie_clusters <- CF_verstedelijking_clustered %>%
#   group_by(cluster) %>%
#   summarise(
#     BEKNR = first(BEKNR),
#     geometry = st_centroid(st_union(geometry)),
#     .groups = 'drop'
#   )
# 
# aggregated <- species_long %>%
#   left_join(populatie_clusters, by = "cluster") %>%
#   st_as_sf()
# # ==============================================================================
# # 4. MODELLEN DEFINIËREN EN UITVOEREN (GLM & GLMM) - **ALLE SOORTEN**
# # ==============================================================================

# soorten_voor_analyse <- alle_soorten_kolommen
# 
# analyse_data <- aggregated %>%
#   filter(!soort == "faxonius.virilis") %>%
#   filter(!soort == "pontastacus.leptodactylus") %>%
#   filter(!soort == "pacifastacus.leniusculus") %>%
#   filter(!aanwezigheid_clustered == "NA")
# 
# #model
# Model_1 <- glmer(aanwezigheid_clustered ~ type * soort + (1|BEKNR),
#                           family = binomial("logit"),
#                           data = analyse_data,
#                           control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
# 
# summary(Model_1)
# 
# # fit checken
# sim_residuen <- simulateResiduals(
#   fittedModel = Model_1,
#   n = 250,
#   plot = TRUE
# )
# 
# testDispersion(sim_residuen)
# testZeroInflation(sim_residuen)
# 
# # model 2
# Model_2 <- glmer(aanwezigheid_clustered ~ type * soort + (type|BEKNR),
#                  family = binomial("logit"),
#                  data = analyse_data,
#                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
# 
# summary(Model_2)
# 
# # fit checken
# sim_residuen <- simulateResiduals(
#   fittedModel = Model_2,
#   n = 250,
#   plot = TRUE
# )
# 
# testDispersion(sim_residuen)
# testZeroInflation(sim_residuen)
# 
# # model 3 (zonder op voorhand clusteren van de data)
# 
# ongeclusterd <- species_presence_long %>%
#   filter(!soort == "faxonius.virilis") %>%
#   filter(!soort == "pontastacus.leptodactylus") %>%
#   filter(!soort == "pacifastacus.leniusculus") %>%
#   filter(!aanwezigheid == "NA")
# 
# Model_3 <- glmer(aanwezigheid ~ type * soort + (1|cluster),
#                  family = binomial("logit"),
#                  data = ongeclusterd,
#                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
# 
# summary(Model_3)
# 
# # fit checken
# sim_residuen <- simulateResiduals(
#   fittedModel = Model_3,
#   n = 250,
#   plot = TRUE
# )
# 
# testDispersion(sim_residuen)
# testZeroInflation(sim_residuen)

################ Plot de data ######################################


# --- 2. Data verwerking en proporties berekenen ---
name_map_urban <- c(
  "faxonius.limosus" = "gevlekte Amerikaanse rivierkreeft",
  "procambarus.clarkii" = "rode Amerikaanse rivierkreeft",
  "procambarus.acutus" = "gestreepte Amerikaanse rivierkreeft",
  "procambarus.virginalis" = "marmerkreeft",
  "pontastacus.leptodactylus" = "Turkse rivierkreeft",
  "faxonius.virilis" = "geknobbelde Amerikaanse rivierkreeft",
  "pacifastacus.leniusculus" = "Californische rivierkreeft")

gewenste_volgorde <- c(
  "gevlekte Amerikaanse rivierkreeft",            
  "rode Amerikaanse rivierkreeft",             
  "gestreepte Amerikaanse rivierkreeft",
  "marmerkreeft",
  "Turkse rivierkreeft",
  "geknobbelde Amerikaanse rivierkreeft",
  "Californische rivierkreeft")

# Bereken het totale aantal waarnemingen per soort
species_total <- species_presence_long %>%
  filter(!is.na(aanwezigheid)) %>%
  group_by(soort, type) %>%
  summarise(
    N_totaal = n(),
    N_aanwezig = sum(aanwezigheid),
    percentage = (N_aanwezig / N_totaal) * 100) %>%
  ungroup()

df_plot <- species_total %>%
  mutate(soort = factor(name_map_urban[soort], levels = gewenste_volgorde))

# --- Visualisatie genereren met ggplot2 ---
library(stringr) 

# maximale breedte van de labeltekst
MAX_LABEL_WIDTH <- 15

# Definieer kleurenpalet (optioneel, voor consistentie met eerdere suggestie)
kleuren <- c("landelijk" = "#1B5E20", "randstedelijk" = "#A29B63", "verstedelijkt" = "#707070")

plot_frequentie_urban <- ggplot(df_plot, aes(x = soort, y = percentage, fill = type)) +
  
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  
  geom_text(
    aes(label = ifelse(N_aanwezig > 0, as.character(N_aanwezig), "")),
    position = position_dodge(width = 0.8), vjust = -0.5, size = 3, color = "black"
  ) +
  
  scale_x_discrete(labels = function(x) str_wrap(x, width = MAX_LABEL_WIDTH)) +
  scale_fill_manual(values = kleuren) +
  
  labs(
    y = "relatieve aanwezigheden (%)",
    fill = "Verstedelijking"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(plot_frequentie_urban)

# Sla de plot op (optioneel)
ggsave("~/GitHub/Craywatch-Rapport/R/data/output/craywatch_maps/urbanisatie_plot_R.png", width = 12, height = 7, dpi = 1200)