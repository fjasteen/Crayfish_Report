
# Laad het pakket
library(corrplot)
library(ggplot2)
library(dplyr)

# Lees data
FC_data <- data_fc_cray

# duid de nodige kolommen aan
cor_kolommen <- c("CPUE_faxonius.limosus", "CPUE_procambarus.clarkii","Cl", "Nt", "O2", "EC20", "T", "pH", "Secchi", "Pt", "ZS")

cor_data <- FC_data[, cor_kolommen]

# Definiëer een realistisch maximum (bijv. 1000). Alles daarboven is een fout.
MAX_O2_REALISTISCH <- 1000 

# Vervang alle waarden die boven deze drempel liggen door NA
cor_data$O2[cor_data$O2 > MAX_O2_REALISTISCH] <- NA

# Controleer de summary opnieuw: de Max. en Mean moeten nu correct zijn.
print(summary(cor_data$O2))
print(summary(cor_data$Cl))
print(summary(cor_data$Nt))
print(summary(cor_data$EC20))
print(summary(cor_data$T))
print(summary(cor_data$pH))
print(summary(cor_data$Secchi))
print(summary(cor_data$Pt))
print(summary(cor_data$ZS))

# # Correlatiematrix
# cor_matrix <- cor(
#   cor_data,
#   method = "spearman",
#   use = "pairwise.complete.obs"
# )
# 



# Functie om de p-waarden matrix te maken (identiek aan de cor.mtest, maar zonder de list/extra checks die faalden)
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
rho_matrix <- get_rho_matrix(
  cor_data, 
  method = "spearman"
)


p_matrix <- get_p_matrix(
  cor_data, 
  method = "spearman"
)

p_adjusted_matrix <- matrix(p.adjust(p_matrix, method="bonferroni"),ncol=11,nrow=11)
colnames(p_adjusted_matrix) <- rownames(p_adjusted_matrix) <- colnames(p_matrix)


# Optioneel: Creëer een NIEUWE correlatiematrix waar insignificante waarden NA zijn
cor_matrix_significant <- rho_matrix
cor_matrix_significant[p_adjusted_matrix > 0.05] <- NA 
colnames(cor_matrix_significant) <- rownames(cor_matrix_significant) <- gsub("CPUE_","",colnames(cor_matrix_significant))

# Creëer de uiteindelijke plot
corrplot(
  cor_matrix_significant, # Gebruik de gemaskerde matrix met NA's
  method = "circle",
  type = "upper",
  order = "original",
  tl.col = "black",
  tl.srt = 45,
  na.label=" "
)


# Creëer de scatterplot
ggplot(cor_data, aes(x = cor_data$Cl, y = CPUE_faxonius.limosus)) +
  # Gebruik geom_point om de datapunten te plotten
  geom_point(position = position_jitter(width = 0.05, height = 0.05), # Voeg jitter toe om overlapping te vermijden
             alpha = 0.6, color = "darkblue") +
  
  # Voeg een trendlijn toe
  # 'method = "lm"' voor een lineaire lijn (Pearson)
  # 'method = "loess"' voor een gladde curve (vaak beter voor ecologische data)
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  
  # Labels en titel
  labs(title = "Relatie tussen CPUE van P. clarkii en Chloride (Cl.)",
       x = "Chloride (Cl.)",
       y = "Faxonius limosus") 


# Maak boxplots voor aan- of afwezigheid
cor_kolommen <- c("faxonius.limosus", "procambarus.clarkii","procambarus.acutus","Cl", "Nt", "O2", "EC20", "T", "pH", "Secchi", "Pt", "ZS")

cor_data <- FC_data[, cor_kolommen]

# filter NA's er uit
cor_data_filterd_FL <- cor_data %>%
  filter(!is.na(faxonius.limosus))

cor_data_filterd_PC <- cor_data %>%
  filter(!is.na(procambarus.clarkii))

cor_data_filterd_PA <- cor_data %>%
  filter(!is.na(procambarus.acutus))

ggplot(cor_data_filterd_PA, aes(x = as.factor(procambarus.acutus), y = ZS)) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "red", "1" = "lightblue")) + 
  labs(title = "Zwevende stoffen bij aan- of afwezigheid van P. acutus",
       x = "P. acutus Aanwezigheid (0 = Afwezig, 1 = Aanwezig)",
       y = "zwevende stoffen (mg/l)") +
  theme_minimal()



# ==============================================================================
# TOEVOEGING: Automatische Boxplots Genereren (zonder P. acutus)
# ==============================================================================

# We hebben de 'tidyr' library nodig voor pivot_longer
if (!require(tidyr)) install.packages("tidyr")
library(tidyr)

# 1. Data omvormen naar 'Long' format voor de loop
# We gebruiken de 'cor_data' die hierboven is aangemaakt
# Zorg dat cor_data een dataframe is (voor de zekerheid, ivm eerdere tibble error)
if(inherits(cor_data, "tbl_df")) cor_data <- as.data.frame(cor_data)

data_long_species <- cor_data %>%
  pivot_longer(
    # AANGEPAST: P. acutus is hier verwijderd
    cols = c(faxonius.limosus, procambarus.clarkii), 
    names_to = "Soort", 
    values_to = "Aanwezigheid" 
  ) %>%
  filter(!is.na(Aanwezigheid)) %>%
  mutate(Aanwezigheid = as.factor(Aanwezigheid))

# 2. Labels definiëren (AANGEPAST: P. acutus verwijderd)
species_labels_italic <- c(
  "faxonius.limosus" = "italic(F.~limosus)", 
  "procambarus.clarkii" = "italic(P.~clarkii)"
)

# 3. Parameters definiëren
# Let op: zorg dat deze namen exact matchen met je kolommen (bijv. "Cl" vs "Cl.")
fc_parameters <- c("Cl", "Nt", "O2", "EC20", "T", "pH", "Secchi", "Pt", "ZS") 

# 4. De Loop uitvoeren
message("Genereren van boxplots voor alle parameters...")

for (param in fc_parameters) {
  
  # Check of de parameter bestaat in de data om errors te voorkomen
  if (param %in% names(data_long_species)) {
    
    p <- ggplot(data_long_species, aes(x = Aanwezigheid, y = !!sym(param))) +
      
      geom_boxplot(aes(fill = Aanwezigheid), alpha = 0.7) + 
      
      # Facet per soort
      facet_wrap(~ Soort, 
                 labeller = labeller(Soort = as_labeller(species_labels_italic, 
                                                         default = label_parsed))
      ) +
      
      # Styling
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
    
  } else {
    message(paste("Waarschuwing: Kolom", param, "niet gevonden in dataset. Sla over."))
  }
}