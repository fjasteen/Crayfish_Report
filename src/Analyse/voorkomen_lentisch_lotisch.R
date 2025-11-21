############################################################
# Script:    Voorkeur_lentisch_lotisch.R
# Doel:      Nagaan of de verschillende soorten invasieve rivierkreeften een habitatvoorkeur hebben 
#             voor stromende versus niet-stromende waterlichamen in Vlaanderen op basis van Craywatch en gbif waarnemingen
# Auteur:    Margot Vermeylen
# Datum:     21-08-2025 (update 29/10/25)
############################################################

library(sf)
library(lme4)
library(lmerTest)
library(broom)
library(leaflet)
library(tidyverse)
library(effects)
library(jtools)
library(ROCR)
library(DHARMa)

# Upload crayfish data (met geupdate toekenning voor stilstaand vs stromend)
load("~/GitHub/Craywatch-Rapport/R/data/input/fc_data/analyses_dataset_VHAG_WVLC_CATC_rivier_breedte.Rdata")
CF <- data


# ----------- cluster waarnemingen in hetzelfde waterlichaam samen ------------------------
# Filter de datapunten zonder watertype en de dubbele waarnemingen op dezelfde locatie er uit. Maak een kolom met type water
CF_zonder_water <- CF %>%
  filter(is.na(WVLC) & is.na(VHAG) & is.na(CATC))

CF_water <- CF %>%
  filter(!(is.na(WVLC) & is.na(VHAG) & is.na(CATC))) %>%
  mutate(
    type = if_else(!is.na(WVLC), "stilstaand", "stromend"),
    # Creëer een ID voor de clustering
    cluster_id = coalesce(as.character(VHAS), as.character(WVLC), "Overig")
  ) %>%
  # zet de dataframe om naar een sf-object
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(31370)

# Hoeveel van elk type water bemonsterd (voor clustering)
type_bemonsterd <- CF_water %>%
  group_by(type) %>%
  count()
print(type_bemonsterd)

# Check VHAG segmenten
vhag_segmenten <- st_read("~/GitHub/Craywatch-Rapport/R/data/input/shapefiles/Vhag.shp")

# leaflet() %>%
#   addTiles(data = "OSM") %>%
#   addPolylines(data = vhag_segmenten %>% st_transform(4326), 
#               color = "red",
#               label = ~as.character(VHAG))


# Tel het aantal unieke locaties (clusters) per type water
type_bemonsterd_per_locatie <- CF_water %>%
  distinct(cluster_id, type) %>% 
  count(type, name = "aantal_locaties") 
print("Aantal unieke locaties (clusters) per type water (met VHAG/WVLC-voorwaarde):")
print(type_bemonsterd_per_locatie)

# definieer de soorten
soortenkolommen <- c(
  "faxonius.limosus", 
  "procambarus.virginalis", 
  "procambarus.acutus", 
  "faxonius.virilis", 
  "procambarus.clarkii", 
  "pontastacus.leptodactylus", 
  "pacifastacus.leniusculus"
)

# ------------ Hoeveel locaties van elke soort in stilstaand/stromend water -----------------------------
# 1. Herschep de CF_clustered data van breed naar lang
species_long <- CF_water %>%
  st_drop_geometry() %>% 
  # Behoud alleen de relevante kolommen
  select(cluster_id, type, BEKNR, all_of(soortenkolommen)) %>%
  # Transformatie: zet elke soortenkolom om naar rijen
  pivot_longer(
    cols = all_of(soortenkolommen),
    names_to = "soort",
    values_to = "Aanwezig"
  ) 

# aanwezigheidskolom als numeriek zetten om te kunnen groeperen
species_long <- species_long %>%
  mutate(
    aanwezigheid = as.numeric(Aanwezig),
    BEKNR = as.factor(BEKNR)) %>%
  select(-Aanwezig)

class(species_long$BEKNR)

# aantal waarnemingen per soort in lentisch en lotisch
species_counts <- species_long %>%
  filter(aanwezigheid == 1) %>%
  group_by(soort, type) %>%
  # Tel het aantal unieke clusters per soort
  summarise(
    Aantal_Locaties = n(),
    .groups = 'drop'
  ) %>%
  
  pivot_wider(
    names_from = type,
    values_from = Aantal_Locaties,
    values_fill = 0 # Vul 0 in waar de soort niet voorkwam in dat type water
  )

print("Aantal locaties met aanwezigheid per soort:")
print(species_counts)

# data groeperen per cluster
species_long_clustered <- species_long %>%
  group_by(cluster_id, soort, type) %>%
  summarise(
    aanwezigheid_clustered = case_when(
      any(aanwezigheid == 1, na.rm = TRUE) ~ 1,       # Als er minstens een 1 is = 1
      any(aanwezigheid == 0, na.rm = TRUE) ~ 0,       # Als er GEEN 1 is en minstens een 0 = 0
      sum(!is.na(aanwezigheid)) == 0 ~ NA_real_      # Als er geen 1 en geen 0 is (alleen NA's) = NA
  ))

# 2. Tel het aantal unieke geclusterde locaties per soort en type water
species_location_counts <- species_long_clustered %>%
  filter(aanwezigheid_clustered == 1) %>%
  group_by(soort, type) %>%
  # Tel het aantal unieke clusters per soort
  summarise(
    Aantal_Locaties = n_distinct(cluster_id),
    .groups = 'drop'
  ) %>%
  
  pivot_wider(
    names_from = type,
    values_from = Aantal_Locaties,
    values_fill = 0 # Vul 0 in waar de soort niet voorkwam in dat type water
  )

print("Aantal geclusterde locaties (unieke cluster_id's) met aanwezigheid per soort:")
print(species_location_counts)

# Voeg de kolommen geometry en BKNR toe aan species_long om de modellen te kunnen runnen
cluster_locaties <- CF_water %>%
  group_by(cluster_id) %>%
  summarise(
    BEKNR = first(BEKNR),
    geometry = st_centroid(st_union(geometry)),
    .groups = 'drop'
  )

aggregated_data <- species_long_clustered %>%
  left_join(cluster_locaties, by = "cluster_id") %>%
  st_as_sf()

# soorten met te weinig data weglaten uit het model + NA's er uit filteren
aggregated_data <- aggregated_data %>%
  filter(!soort == "faxonius.virilis") %>%
  filter(!soort == "pontastacus.leptodactylus") %>%
  filter(!soort == "pacifastacus.leniusculus") %>%
  filter(!aanwezigheid_clustered == "NA")


# ----------------- Voer GLM en GLMM uit ------------------------------------  
# test <- meanDecompose(aanwezigheid_clustered ~ type, data = aggregated_data)
# str(test, nchar.max = 30)

# simpel model
glm_1 <- glm(aanwezigheid_clustered ~ type, data = aggregated_data, family = binomial(link = "logit"))
summary(glm_1)
summ(glm_1, exp = T)
plot(allEffects(glm_1))

# data exploratie
Pred <- predict(glm_1, type = "response")
Pred <- if_else(Pred > 0.5, 1, 0)
ConfusionMatrix <- table(Pred, pull(aggregated_data, aanwezigheid_clustered)) #`pull` results in a vector
#correct classification rate
sum(diag(ConfusionMatrix))/sum(ConfusionMatrix)
ConfusionMatrix

# Compute AUC for predicting Class with the model
Prob <- predict(glm_1, type="response")
Pred <- prediction(Prob, as.vector(pull(aggregated_data, aanwezigheid_clustered)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC

aggregated_data %>%
  group_by(BEKNR) %>%
  summarise(PROP = sum(aanwezigheid_clustered)/n()) %>%
  plot()

aggregated_data %>%
  mutate(type = if_else(type == "stromend", 1, 0)) %>%
  ggplot(aes(x = type, y = aanwezigheid_clustered, color = as.factor(BEKNR))) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1))


# Interactieterm soort toevoegen en random effect (sterkte van het effect wordt anders geschat per bekken door bekken als random effect toe te voegen)
Model_1 <- glmer(aanwezigheid_clustered ~ type * soort + (1|BEKNR),
                               family = binomial("logit"),
                               data = aggregated_data,
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(Model_1)


# Fit checken
plot(Model_1)
sim_residuen <- simulateResiduals(
  fittedModel = Model_1,
  n = 250,
  plot = TRUE
)

testDispersion(sim_residuen)
testZeroInflation(sim_residuen)

# ander model om fit te verbeteren (sterkte en richting van het effect wordt anders geschat per bekken)
Model_2 <- glmer(aanwezigheid_clustered ~ type * soort + (type|BEKNR),
                          family = binomial("logit"),
                          data = aggregated_data,
                          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(Model_2)

# Fit opnieuw checken
plot(Model_2)

sim_residuen <- simulateResiduals(
  fittedModel = Model_2,
  n = 250,
  plot = TRUE
)

testDispersion(sim_residuen)
testZeroInflation(sim_residuen)

# model in de 'porbit' schaal ipv 'logit' schaal proberen
Model_3 <- glmer(aanwezigheid_clustered ~ type * soort + (type|BEKNR),
                               family = binomial("probit"),
                               data = aggregated_data,
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(Model_3)

# Fit opnieuw checken
plot(Model_3)

sim_residuen <- simulateResiduals(
  fittedModel = Model_3,
  n = 250,
  plot = TRUE
)

testDispersion(sim_residuen)
testZeroInflation(sim_residuen)

# Deze drie modellen allemaal vrij gelijkaardig.AIC van model 2 iets beter, de resultaten en DHARMa fit gelijkaardig.

# Locatie ipv bekken als random effect
Model_4 <- glmer(aanwezigheid_clustered ~ type * soort + (1|cluster_id),
                            family = binomial("logit"),
                            data = aggregated_data,
                            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(Model_4)

# Fit opnieuw checken
plot(Model_4)
sim_residuen <- simulateResiduals(
  fittedModel = Model_4,
  n = 250,
  plot = TRUE
)

testDispersion(sim_residuen)
testZeroInflation(sim_residuen)
# fit zeer slecht dus deze al niet!

# Model uitvoeren op ongeclusterde data met WVLC en VHAG code als random effect (zien of dit een betere fit geeft)
ongeclusterd <- species_long %>%
  filter(!soort == "faxonius.virilis") %>%
  filter(!soort == "pontastacus.leptodactylus") %>%
  filter(!soort == "pacifastacus.leniusculus") %>%
  filter(!aanwezigheid == "NA")

Model_5 <- glmer(aanwezigheid ~ type * soort + (1|cluster_id),
                          family = binomial("logit"),
                          data = ongeclusterd,
                          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(Model_5)


# fit
plot(Model_5)
sim_residuen <- simulateResiduals(
  fittedModel = Model_5,
  n = 250,
  plot = TRUE
)

testDispersion(sim_residuen)
testZeroInflation(sim_residuen)
# fit niet zo goed

# Ongeclusterde data en bekken als random effect
Model_6 <- glmer(aanwezigheid ~ type * soort + (1|BEKNR/cluster_id),
                 family = binomial("logit"),
                 data = ongeclusterd)

summary(Model_6)

# fit
plot(Model_6)
sim_residuen <- simulateResiduals(
  fittedModel = Model_6,
  n = 250,
  plot = TRUE
)

testDispersion(sim_residuen)
testZeroInflation(sim_residuen)
# fit niet zo goed

# beste modellen vergelijken
anova(Model_5)
anova(Model_6)

# anova(Model_5, Model_6, test = "Chisq")

# post-hoc analyse voor de soorten
library("lsmeans")
lsm=lsmeans(Model_5,~type|soort)
pairs(lsm)

summary(pairs(lsm), by = NULL, adjust = "bonf")

newData <- data.frame(soort=unique(ongeclusterd$soort),type=rep(unique(ongeclusterd$type),each=4),aanwezigheid=1)

newData$prediction_odds <- predict(Model_5,newdata=newData, re.form = NA)
newData$prediction_prob <- predict(Model_5,newdata=newData, re.form = NA, type="response")


ggplot(ongeclusterd, aes(x=type,fill=as.factor(aanwezigheid)))+geom_bar(position = "fill")+facet_wrap(~soort)

ggplot(ongeclusterd, aes(x=type,fill=as.factor(aanwezigheid)))+geom_bar()+facet_wrap(~soort)

######## plot maken ###########################
# 1. Groeperen en Aggregeren
species_abs <- species_long %>%
  filter(!is.na(aanwezigheid))

name_map <- c(
  "faxonius.limosus" = "gevlekte Amerikaanse rivierkreeft",
  "procambarus.clarkii" = "rode Amerikaanse rivierkreeft",
  "procambarus.acutus" = "gestreepte Amerikaanse rivierkreeft",
  "procambarus.virginalis" = "marmerkreeft",
  "pontastacus.leptodactylus" = "Turkse rivierkreeft",
  "faxonius.virilis" = "geknobbelde Amerikaanse rivierkreeft")

# Bereken het totale aantal waarnemingen per soort
species_full <- species_abs %>%
  group_by(soort, type) %>%
  summarise(
    N_aanwezig = sum(aanwezigheid),
    N_totaal = n(),
    .groups = 'drop')

# bereken het percentage
proportioneel <- species_full %>%
  mutate(
    # Bereken het percentage: (aantal per type) / (totaal per soort)
    percentage = (N_aanwezig / N_totaal) * 100,
    naam = name_map[soort]
  ) %>%
  filter(!soort == "pacifastacus.leniusculus")
  
volgorde <- c(
  "gevlekte Amerikaanse rivierkreeft", "rode Amerikaanse rivierkreeft", 
  "gestreepte Amerikaanse rivierkreeft", "marmerkreeft", 
  "Turkse rivierkreeft", "geknobbelde Amerikaanse rivierkreeft")

df_plot <- proportioneel %>%
  mutate(soort = factor(naam, levels = volgorde))

# --- 3. Visualisatie genereren met ggplot2 ---
library(stringr) 

# maximale breedte van de labeltekst
MAX_LABEL_WIDTH <- 15

# Definieer kleurenpalet (optioneel, voor consistentie met eerdere suggestie)
kleuren <- c("stromend" = "#2A8C8C", "stilstaand" = "#2A5959")

plot_frequentie <- ggplot(df_plot, aes(x = soort, y = percentage, fill = type)) +
  
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  
  geom_text(
    aes(label = ifelse(N_aanwezig > 0, as.character(N_aanwezig), "")),
    position = position_dodge(width = 0.8), vjust = -0.5, size = 3, color = "black"
  ) +
  
  scale_x_discrete(labels = function(x) str_wrap(x, width = MAX_LABEL_WIDTH)) +
  scale_fill_manual(values = kleuren) +
  
  labs(
    y = "relatieve aanwezigheden (%)",
    fill = "Watertype"
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

print(plot_frequentie)


# Sla de plot op
ggsave("~/GitHub/Craywatch-Rapport/R/data/output/craywatch_maps/lentisch_lotisch_plot_R.png", width = 12, height = 7, dpi = 1200)

