
data_fc_cray_combined

# Alle data per jaar per sample point, reeds voor de zomer
fc_aggregated

# ! in 19 gemiddelde pakken voor een jaar & samplepoint ipv median


# ==============================================================================
# Script: Equivalence Test (TOST) - 1-jaar vs 3-jaar Methodologie
# ==============================================================================

# --- 0. Setup ---
source("./src/config.R") # Zorg dat je config geladen is voor fc_parameter_map
library(dplyr)
library(tidyr)
library(purrr)
library(TOSTER)      # Voor de equivalence test
library(wrappedtools)# Voor de 'medianse' functie (zoals in M&M)


# VEILIGE FUNCTIE DEFINITIE (Gecorrigeerd)
medianse_custom <- function(x, na.rm = TRUE) {
  
  # STAP 1: Filter de NA's hier al weg (indien gevraagd)
  # Zo hoeven we 'na.rm' niet door te geven aan de package functie
  if (na.rm) {
    x <- na.omit(x)
  }
  
  # Veiligheidscheck: als vector leeg is of te klein, geef NA
  if (length(x) < 2) return(NA)
  
  # STAP 2: Probeer het officiële pakket
  if (requireNamespace("wrappedtools", quietly = TRUE)) {
    # HIER ZAT DE FOUT: we geven 'x' door (die is nu al schoon), zonder extra argumenten
    return(wrappedtools::medianse(x)) 
  }
  
  # STAP 3: Fallback (als wrappedtools er niet is)
  # Formule: 1.253 * (SD / wortel(n))
  return(1.2533 * (sd(x) / sqrt(length(x))))
}
# --- 1. Data Inladen ---
# We gaan ervan uit dat deze objecten in je environment staan. 
# Zo niet, laad ze hier in (pas paden aan indien nodig).

# A. De volledige FC dataset (voor bepalen grenzen)
# load(here("data", "intermediate", "fc_data_aggregated.Rdata")) 
# Of gebruik de variabele 'fc_aggregated' die je al hebt

# B. De gekoppelde Kreeft-FC data (voor de vergelijking)
# dataset_fc_cray_combined <- read.table(...) 

# --- 2. Stap A: Bepaal Equivalentie Grenzen (Bounds) ---
# "De grenzen... werden bepaald door de standaard error op de mediaan... tussen 2010 en 2024"

message("Berekenen van equivalentie-grenzen (SESOI) per parameter...")

# Selecteer de relevante kolommen uit fc_aggregated
fc_cols <- names(fc_parameter_map) # O2, pH, etc.
# Zorg dat namen matchen met je dataframe (check names(fc_aggregated))
available_cols <- intersect(fc_cols, names(fc_aggregated))

# Zet om naar long format voor berekening
bounds_calc <- fc_aggregated %>%
  select(sample_point, year, all_of(available_cols)) %>%
  filter(year >= 2010 & year <= 2024) %>%
  pivot_longer(cols = all_of(available_cols), 
               names_to = "Parameter", 
               values_to = "Waarde") %>%
  filter(!is.na(Waarde))

# Bereken SE per meetpunt per parameter
se_per_point <- bounds_calc %>%
  group_by(Parameter, sample_point) %>%
  summarise(
    se_median = medianse_custom(Waarde, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(se_median)) # Verwijder punten met te weinig data

# "Vervolgens de mediaan van al deze standaard errors berekenen"
equivalence_bounds <- se_per_point %>%
  group_by(Parameter) %>%
  summarise(
    # Dit is je 'Theta' (de toegestane afwijking)
    bound_delta = median(se_median, na.rm = TRUE)
  )

print("Berekende Equivalentie Grenzen (Delta):")
print(equivalence_bounds)

# --- 3. Stap B: Bereken 1-jaar en 3-jaar waarden per Kreeft ---

message("Data voorbereiden voor vergelijking...")

# Zet combined data om naar long format
# Aanname: dataset_fc_cray_combined bevat 'ID', 'yearGroup' (vangst), 'year' (meting), en fc-kolommen
cray_long <- data_fc_cray_combined %>%
  select(vangstID, yearGroup, year, all_of(available_cols)) %>%
  pivot_longer(cols = all_of(available_cols), 
               names_to = "Parameter", 
               values_to = "Waarde") %>%
  filter(!is.na(Waarde))

# 1. Bereken 3-Jaar Zomer Mediaan (Huidige methode)
# Filter: measurement year zit tussen (Capture Year - 2) en (Capture Year)
df_3jaar <- cray_long%>%
  filter(yearGroup >= (year - 2) & yearGroup <= year) %>%
  group_by(vangstID, Parameter) %>%
  summarise(Val_3yr = median(Waarde, na.rm = TRUE), .groups = "drop")

# 2. Bereken 1-Jaar Zomer Mediaan (Alternatieve methode)
# Filter: measurement year == Capture Year
df_1jaar <- cray_long%>%
  filter(yearGroup == year) %>%
  group_by(vangstID, Parameter) %>%
  summarise(Val_1yr = median(Waarde, na.rm = TRUE), .groups = "drop")

df_compare <- inner_join(df_3jaar, df_1jaar, by = c("vangstID", "Parameter"))

# Samenvoegen
# --- ROBUUSTE TOST FUNCTIE (Workaround voor TOSTER bug) ---
# --- AANGEPASTE FUNCTIE: Wilcoxon TOST (Conform M&M en Collega) ---
perform_tost <- function(param_name, data, bounds_df) {
  
  # 1. Filter data en haal grens op
  subset_data <- data %>% filter(Parameter == param_name)
  delta <- bounds_df$bound_delta[bounds_df$Parameter == param_name]
  
  # Bereken verschillen (voor validatie en N)
  diffs <- subset_data$Val_3yr - subset_data$Val_1yr
  diffs <- na.omit(diffs)
  
  # 2. Checks: Hebben we genoeg data en een grens?
  if(length(diffs) < 3 || is.na(delta) || length(delta) == 0) return(NULL)
  
  # 3. Voer Wilcoxon TOST uit (Non-parametrisch, robuust tegen uitschieters)
  result <- tryCatch({
    
    TOSTER::wilcox_TOST(
      x = subset_data$Val_3yr,
      y = subset_data$Val_1yr,
      paired = TRUE,           # Het gaat om dezelfde kreeften
      low_eqbound = -delta,    # Ondergrens
      high_eqbound = delta,    # Bovengrens
      plot = FALSE
    )
    
  }, error = function(e) {
    message(paste("Fout bij parameter:", param_name, "-", e$message))
    return(NULL)
  })
  
  if(is.null(result)) return(NULL)
  
  # 4. Resultaten extraheren
  # Wilcoxon werkt met rangordes, dus we rapporteren de MEDIAAN van het verschil, niet het gemiddelde.
  
  # TOSTER output structuur voor wilcox_TOST:
  # Rij 2 = TOST Lower, Rij 3 = TOST Upper (meestal, we pakken veilig de max p-waarde)
  max_p <- max(result$TOST$p.value[2:3], na.rm = TRUE) 
  
  return(data.frame(
    Parameter = param_name,
    N = length(diffs),
    # We tonen de mediaan van het verschil (Dit zal veel lager zijn dan jouw 544!)
    Median_Diff = median(diffs), 
    Bound_Delta = delta,
    Max_P_TOST = max_p,
    # Conclusie: Als P < 0.05, dan is het equivalent
    Resultaat = ifelse(max_p < 0.05, "Equivalent", "Niet Equivalent"),
    stringsAsFactors = FALSE
  ))
}

# --- Herstart de berekening ---
message("Start TOST analyse (Workaround mode)...")

# Run de map functie
results_list <- map(available_cols, ~perform_tost(.x, df_compare, equivalence_bounds))

# Samenvoegen
final_tost_results <- bind_rows(results_list[!sapply(results_list, is.null)])

print(final_tost_results)

