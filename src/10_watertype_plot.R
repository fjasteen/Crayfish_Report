# ====================================================
# Scriptnaam: 10_watertype_plot_glmm.R
# Auteur: Frédérique Steen
# Datum: 01-12-2025
# Beschrijving: 
# - Classificeert waarnemingen als gesloten of open
# - Plot frequentie per watertype  
# - Plot soorten per watertype 
# - Plot vangstsucces per watertype  
# - Statistiek: GLMM

# ====================================================

# --- 0. Instellingen laden ---
source("./src/config.R")

library(lme4)   # Voor GLMM
library(DHARMa)
library(emmeans)
library(car) # Voor Type III Anova

# --- 1. Data Inlezen ---
if (!file.exists(file_analyse_dataset_rapport)) stop("Run eerst script 03!")

df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE) %>%
  # We hebben een ruimtelijke koppeling nodig om het watertype te bepalen
  filter(!is.na(VHAG) | !is.na(WVLC))

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
    cols = all_of(required_species), 
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


# ==============================================================================
# 4. GLMM 
# ==============================================================================

# Selecteer de dataset (craywatch (1/0)  
valid_species <- cray_long %>%
  filter(dat.source == "craywatch_data") %>%
  filter(!is.na(presence)) %>%
  group_by(species, water_type) %>%
  summarise(n_vangsten = sum(presence), .groups = "drop") %>%
  group_by(species) %>%
  filter(all(n_vangsten > 0)) %>% 
  pull(species) %>%
  unique()

print("Soorten geschikt voor GLMM (komen voor in beide types):")
print(valid_species)

# Maak model 
model_data <- cray_long %>%
  filter(dat.source == "craywatch_data") %>% 
  filter(!is.na(presence)) %>%
  filter(species %in% valid_species) %>% 
  mutate(
    Waterbody_ID = coalesce(as.character(VHAG), as.character(WVLC), as.character(locID))
  )

# Draai het model
if(nrow(model_data) > 0) {
  message("Start GLMM analyse (Stilstaand vs Stromend) - Cleaned Data...")
  
  glmm_water <- glmer(
    presence ~ water_type * species + (1 | Waterbody_ID),
    family = binomial("logit"),
    data = model_data,
    control = glmerControl(optimizer = "bobyqa")
  )
  
  print(summary(glmm_water))
  
  
} else {
  warning("Te weinig data.")
}

# ==============================================================================
# 5. MODEL VALIDATIE & POST-HOC ANALYSE
# ==============================================================================
# --- Model validatie (DHARMa) ---
# simuleren residuen om te checken voor model fit
simulationOutput <- simulateResiduals(fittedModel = glmm_water, plot = TRUE)

# extra test (dispersie / zero-inflation)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)

# --- Type III ANOVA ---
# Omdat we een interactie hebben, kijken we naar de Type III Anova voor de globale effecten
print(car::Anova(glmm_water, type = "III"))

# --- C. Post-hoc analyse (Bonferroni) ---
# Is het verschil tussen open en gesloten significant per soort?

# Bereken de geschatte gemiddelden (estimated marginal means)
emm_results <- emmeans(glmm_water, ~ water_type | species)

# paarsgewijze vergelijking met Bonferroni correctie
# type = "response" geeft de kansen weer (probabiliteiten) in plaats van log-odds
pairwise_comp <- pairs(emm_results, adjust = "bonferroni")
conf_int      <- confint(pairwise_comp)

print("Resultaten per soort (open vs gesloten) met Bonferroni correctie:")
print(pairwise_comp)

# Als je de geschatte kansen (probabiliteiten) wilt zien per groep:
print(as.data.frame(emmeans(glmm_water, ~ water_type | species, type = "response")))

# ==============================================================================
# 6. RAPPORTAGE: EXPORT NAAR WORD (.docx) 
# ==============================================================================

library(officer)      
library(flextable)    
library(broom.mixed)  
library(dplyr)

# --- A. Voorbereiding ---
doc <- read_docx() %>%
  body_add_par("Statistisch rapport: watertype analyse", style = "heading 1") %>%
  body_add_par(paste("Gegenereerd op:", Sys.Date()), style = "Normal")

# --- B. Model Validatie Plot ---
doc <- doc %>% 
  body_add_par("1. Model validatie (DHARMa)", style = "heading 2") %>%
  body_add_par("Figuur 1: Analyse van de residuen.", style = "Image Caption")

temp_plot_file <- tempfile(fileext = ".png")
png(filename = temp_plot_file, width = 1000, height = 500)
plot(simulationOutput)
dev.off()

doc <- doc %>% 
  body_add_img(src = temp_plot_file, width = 6.5, height = 3.5) 

# --- C. Model Resultaten (Tabel) ---
res_model <- broom.mixed::tidy(glmm_water, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
  mutate(
    term = dplyr::recode(term, 
                         "(Intercept)" = "Gevlekte (Gesloten)",
                         "water_typeopen" = "Type: Open",
                         "speciesprocambarus acutus" = "Soort: Gestreepte",
                         "speciesprocambarus clarkii" = "Soort: Rode",
                         "water_typeopen:speciesprocambarus acutus" = "Interactie: Open x Gestreepte",
                         "water_typeopen:speciesprocambarus clarkii" = "Interactie: Open x Rode"),
    across(where(is.numeric), ~ round(., 3)),
    p_label = ifelse(p.value < 0.001, "< 0.001", as.character(p.value))
  ) %>%
  select(term, estimate, conf.low, conf.high, statistic, p_label)

ft_model <- flextable(res_model) %>%
  set_header_labels(
    term = "variabele", estimate = "odds Ratio", 
    conf.low = "CI Laag", conf.high = "CI Hoog", 
    statistic = "z-waarde", p_label = "p-waarde"
  ) %>%
  autofit() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  bold(part = "header") 

doc <- doc %>%
  body_add_break() %>%
  body_add_par("2. Model Resultaten (GLMM)", style = "heading 2") %>%
  body_add_flextable(ft_model) %>%
  body_add_par("Tabel 1: Fixed effects van het GLMM (odds Ratios).", style = "table title")

# --- D. ANOVA type III ---
df_anova <- broom::tidy(car::Anova(glmm_water, type = "III")) %>%
  mutate(
    across(where(is.numeric), ~ round(., 3)),
    p_label = ifelse(p.value < 0.001, "< 0.001", as.character(p.value))
  )

ft_anova <- flextable(df_anova, col_keys = c("term", "statistic", "df", "p_label")) %>%
  set_header_labels(term = "Effect", statistic = "Chi-sq", df = "Df", p_label = "P-waarde") %>%
  autofit() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  bold(part = "header")

doc <- doc %>%
  body_add_par("3. Globale effecten (ANOVA)", style = "heading 2") %>%
  body_add_flextable(ft_anova) %>%
  body_add_par("Tabel 2: Type III Analysis of Deviance.", style = "table title")

# --- E. Post-hoc (Tabel) ---
raw_contrasts <- summary(pairs(emmeans(glmm_water, ~ water_type | species), adjust="bonf", type="response"))

df_posthoc <- as.data.frame(raw_contrasts) %>%
  mutate(
    species = species_labels_dutch[as.character(species)],
    across(where(is.numeric), ~ round(., 3)),
    p_label = ifelse(p.value < 0.001, "< 0.001", as.character(p.value))
  ) %>%
  select(species, contrast, odds.ratio, SE, z.ratio, p_label)

ft_posthoc <- flextable(df_posthoc) %>%
  set_header_labels(
    species = "soort", contrast = "vergelijking", 
    odds.ratio = "odds Ratio", SE = "std. Error", 
    z.ratio = "z-ratio", p_label = "p-waarde (Bonf.)"
  ) %>%
  autofit() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  bold(part = "header")

doc <- doc %>%
  body_add_par("4. Post-hoc vergelijkingen per soort", style = "heading 2") %>%
  body_add_flextable(ft_posthoc) %>%
  body_add_par("Tabel 3: Paarsgewijze vergelijkingen (open vs gesloten).", style = "table title")

# --- F. Opslaan ---
output_word <- file.path(dir_watertype_output, "statistiek_watertype_rapport.docx")
print(doc, target = output_word)

message(paste("Clean word rapport opgeslagen:", output_word))
shell.exec(output_word)