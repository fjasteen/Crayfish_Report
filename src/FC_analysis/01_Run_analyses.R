# R/src/analysis_fysicochemie/_RUN_ANALYSIS.R

# 1. Setup
cat("=== START FYSICOCHEMIE ANALYSE ===\n")
source(here::here("R/src/analysis_fysicochemie/00_config_fc.R"))

# 2. Workflow stappen
# Stap 1: Schoonmaken & Filteren (o.a. GBIF onzekerheid)
source(here("R/src/analysis_fysicochemie/01_data_prep.R"))

# Stap 2: Ruimtelijke koppeling (Het zware rekenwerk)
# Check of dit al gedraaid is, want dit duurt lang.
if(!file.exists(file.path(dir_fc_inter, "spatial_link_raw.rds"))){
  source(here("R/src/analysis_fysicochemie/02_spatial_link.R"))
} else {
  cat("Stap 2 overgeslagen (bestand bestaat al). Verwijder file om opnieuw te draaien.\n")
}

# Stap 3: Validatie [FS2]
# Dit script stopt als er nieuwe twijfelgevallen zijn die jij moet checken!
source(here("R/src/analysis_fysicochemie/03_validatie.R"))

# Stap 4: Tijdskoppeling (Medianen & Overstorten)
source(here("R/src/analysis_fysicochemie/04_temporal_match.R"))

# Stap 5: Statistiek & Figuren
source(here("R/src/analysis_fysicochemie/05_stats_pca.R"))

cat("\n=== ANALYSE VOLTOOID ===\n")
cat("Resultaten staan in:", dir_fc_output)