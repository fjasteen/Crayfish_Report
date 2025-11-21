# Laad het pakket
library(corrplot)
library(ggplot2)
library(dplyr)

# Lees data
FC_data <- read.table("~/GitHub/Craywatch-Rapport/R/data/output/data_fc_cray_rivierbreedte.txt", header = TRUE, sep = "", quote = "\"")

# duid de nodige kolommen aan
cor_kolommen <- c("CPUE_faxonius.limosus", "CPUE_procambarus.clarkii","Cl.", "N.t", "O2", "EC.20", "T", "pH", "Secchi", "P.t", "ZS")

cor_data <- FC_data[, cor_kolommen]

# Definiëer een realistisch maximum (bijv. 1000). Alles daarboven is een fout.
MAX_O2_REALISTISCH <- 1000 

# Vervang alle waarden die boven deze drempel liggen door NA
cor_data$O2[cor_data$O2 > MAX_O2_REALISTISCH] <- NA

# Controleer de summary opnieuw: de Max. en Mean moeten nu correct zijn.
print(summary(cor_data$O2))
print(summary(cor_data$Cl.))
print(summary(cor_data$N.t))
print(summary(cor_data$EC.20))
print(summary(cor_data$T))
print(summary(cor_data$pH))
print(summary(cor_data$Secchi))
print(summary(cor_data$P.t))
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
ggplot(cor_data, aes(x = cor_data$Cl., y = CPUE_faxonius.limosus)) +
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
cor_kolommen <- c("faxonius.limosus", "procambarus.clarkii","procambarus.acutus","Cl.", "N.t", "O2", "EC.20", "T", "pH", "Secchi", "P.t", "ZS")

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
