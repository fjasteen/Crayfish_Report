# ============================================================
# Scriptnaam: 09_physchem_correlation_boxplots.R
# Project: Craywatch
# Datum: 2025-12-24
#
# Doel:
# - Bereken correlaties tussen fysisch-chemische parameters en CPUE
# - Visualiseer significante correlaties (corrplot)
# - Maak scatterplot voorbeeld en boxplots per parameter per soort
# - Sla automatisch boxplots op naar ./data/output/fysicochemie/boxplots
#
# Verwachte input:
# - R-object/dataframe `data_fc_cray` (in de global env) of:
#   - laad eerst met bijvoorbeeld:
#       data_fc_cray <- readr::read_csv("data/processed/fc_cray.csv")
# - Verwachte kolomnamen (exact zoals hieronder genoemd):
#   - CPUE_faxonius limosus, CPUE_procambarus clarkii
#   - Cl, Nt, O2, EC20, T, pH, Secchi, Pt, ZS
#   - Voor boxplots: kolommen met soort-aanwezigheid: faxonius limosus, procambarus clarkii
#
# Output:
# - Corrplot in active R-plot window 
# - Scatterplot in R-plot window
# - PNG-bestanden per parameter opgeslagen in:
#     ./data/output/fysicochemie/boxplots/<PARAM>_boxplot.png
#
# Benodigde packages:
# - corrplot, ggplot2, dplyr, tidyr, rlang, readr (indien je CSV laadt)
## ============================================================


# Laad het pakket
library(corrplot)

# Lees data
FC_data <- data_fc_cray

# duid de nodige kolommen aan
cor_kolommen <- c("CPUE_faxonius limosus", "CPUE_procambarus clarkii","Cl", "Nt", "O2", "EC20", "T", "pH", "Secchi", "Pt", "ZS")

cor_data <- FC_data[, cor_kolommen]

# Definiëer een realistisch maximum (bijv. 1000). Alles daarboven is een fout.
MAX_O2_REALISTISCH <- 1000 

# Vervang alle waarden die boven deze drempel liggen door NA
cor_data$O2[cor_data$O2 > MAX_O2_REALISTISCH] <- NA

# Controleer de summary opnieuw
print(summary(cor_data$O2))
print(summary(cor_data$Cl))
print(summary(cor_data$Nt))
print(summary(cor_data$EC20))
print(summary(cor_data$T))
print(summary(cor_data$pH))
print(summary(cor_data$Secchi))
print(summary(cor_data$Pt))
print(summary(cor_data$ZS))

# Functie om de p-waarden matrix te maken
get_p_matrix <- function(mat, method) {
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      mat_sub <- na.omit(mat[,c(i,j)])
      # Bereken cor.test alleen voor unieke paren
      result <- cor.test(mat_sub[, 1], mat_sub[, 2], method = method,  exact=FALSE)
      p.mat[i, j] <- p.mat[j, i] <- result$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}

get_rho_matrix <- function(mat, method) {
  n <- ncol(mat)
  rho.mat <- matrix(NA, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      mat_sub <- na.omit(mat[,c(i,j)])
      # Bereken cor.test alleen voor unieke paren
      result <- cor.test(mat_sub[, 1], mat_sub[, 2], method = method,  exact=FALSE)
      rho.mat[i, j] <- rho.mat[j, i] <- result$estimate
    }
  }
  colnames(rho.mat) <- rownames(rho.mat) <- colnames(mat)
  return(rho.mat)
}

cor_data <- as.data.frame(cor_data)

# Bereken de p-waarden matrix
rho_matrix <- get_rho_matrix(cor_data, method = "spearman")
p_matrix <- get_p_matrix(cor_data, method = "spearman")

p_adjusted_matrix <- matrix(p.adjust(p_matrix, method="bonferroni"),ncol=11,nrow=11)
colnames(p_adjusted_matrix) <- rownames(p_adjusted_matrix) <- colnames(p_matrix)

# Optioneel: Creëer een correlatiematrix waar insignificante waarden NA zijn
cor_matrix_significant <- rho_matrix
cor_matrix_significant[p_adjusted_matrix > 0.05] <- NA 
colnames(cor_matrix_significant) <- rownames(cor_matrix_significant) <- gsub("CPUE_","",colnames(cor_matrix_significant))

# Creëer de uiteindelijke plot
corrplot(
  cor_matrix_significant, 
  method = "circle",
  type = "upper",
  order = "original",
  tl.col = "black",
  tl.srt = 45,
  na.label=" "
)

# Creëer de scatterplot
ggplot(cor_data, aes(x = cor_data$Cl, y = `CPUE_faxonius limosus`)) +
  geom_point(position = position_jitter(width = 0.05, height = 0.05), 
             alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relatie tussen CPUE van P. clarkii en Chloride (Cl.)",
       x = "Chloride (Cl.)",
       y = "Faxonius limosus") 

# Maak boxplots voor aan- of afwezigheid P. acutus
cor_kolommen <- c("faxonius limosus", "procambarus clarkii","procambarus acutus","Cl", "Nt", "O2", "EC20", "T", "pH", "Secchi", "Pt", "ZS")
cor_data <- FC_data[, cor_kolommen]

# filter NA's er uit
cor_data_filterd_PA <- cor_data %>%
  filter(!is.na(`procambarus acutus`))

ggplot(cor_data_filterd_PA, aes(x = as.factor(`procambarus acutus`), y = ZS)) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "red", "1" = "lightblue")) + 
  labs(title = "Zwevende stoffen bij aan- of afwezigheid van P. acutus",
       x = "P. acutus Aanwezigheid (0 = Afwezig, 1 = Aanwezig)",
       y = "zwevende stoffen (mg/l)") +
  theme_minimal()


# ==============================================================================
# TOEVOEGING: Automatische Boxplots Genereren & Opslaan
# ==============================================================================

# 0. Definieer de output map en maak deze aan indien nodig
output_dir <- "./data/output/fysicochemie/boxplots"

if (!dir.exists(output_dir)) {
  # recursive = TRUE zorgt dat ook tussenniveaus (bv. fysicochemie) worden aangemaakt
  dir.create(output_dir, recursive = TRUE)
  message(paste("Map aangemaakt:", output_dir))
} else {
  message(paste("Map bestaat al:", output_dir))
}

# 1. Data omvormen naar long format
if(inherits(cor_data, "tbl_df")) cor_data <- as.data.frame(cor_data)

data_long_species <- cor_data %>%
  pivot_longer(
    cols = c(`faxonius limosus`, `procambarus clarkii`), 
    names_to = "Soort", 
    values_to = "Aanwezigheid" 
  ) %>%
  filter(!is.na(Aanwezigheid)) %>%
  mutate(Aanwezigheid = as.factor(Aanwezigheid))

# 2. Labels definiëren 
species_labels_italic <- c(
  "faxonius limosus" = "italic(F.~limosus)", 
  "procambarus clarkii" = "italic(P.~clarkii)"
)

# 3. Parameters definiëren
fc_parameters <- c("Cl", "Nt", "O2", "EC20", "T", "pH", "Secchi", "Pt", "ZS") 

# 4. De loop uitvoeren en opslaan
message("Genereren en opslaan van boxplots...")

for (param in fc_parameters) {
  
  if (param %in% names(data_long_species)) {
    
    # Maak de plot
    p <- ggplot(data_long_species, aes(x = Aanwezigheid, y = !!sym(param))) +
      geom_boxplot(aes(fill = Aanwezigheid), alpha = 0.7) + 
      facet_wrap(~ Soort, 
                 labeller = labeller(Soort = as_labeller(species_labels_italic, 
                                                         default = label_parsed))
      ) +
      scale_x_discrete(labels = c("0" = "Afwezig", "1" = "Aanwezig")) +
      scale_fill_manual(values = c("0" = "lightblue", "1" = "darkmagenta")) +
      labs(
        title = paste("Verdeling van", param, "per soort"),
        y = param,
        x = NULL
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14)
      )
    
    print(p)
    
    # Sla de plot op
    # Bestandsnaam samenstellen: bv. "Cl_boxplot.png"
    file_name <- paste0(param, "_boxplot.png")
    full_save_path <- file.path(output_dir, file_name)
    
    # Opslaan met ggsave 
    tryCatch({
      ggsave(filename = full_save_path, plot = p, width = 8, height = 6, dpi = 300)
      message(paste("Opgeslagen:", full_save_path))
    }, error = function(e) {
      message(paste("Fout bij opslaan van", param, ":", e$message))
    })
    
  } else {
    message(paste("Waarschuwing: Kolom", param, "niet gevonden in dataset. Sla over."))
  }
}