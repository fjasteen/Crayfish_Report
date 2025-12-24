# Script: io_inventory.R
# Doel: scan scripts voor file_* variabelen en IO-calls, controleer of paden bestaan
# Gebruik: Rscript src/io_inventory.R
suppressPackageStartupMessages({
  pkgs <- c("stringr","dplyr","readr","purrr")
  lapply(pkgs, function(p) if(!requireNamespace(p, quietly=TRUE)) install.packages(p))
  lapply(pkgs, library, character.only = TRUE)
})

# helper: alle script-bestanden
files <- list.files(".", pattern = "\\.R$|\\.Rmd$|\\.rmd$", recursive = TRUE, full.names = TRUE)
# filter buiten .git etc.
files <- files[!grepl("^\\.git/", files)]
files <- files[!grepl("/\\.Rproj.user/", files)]

# patterns
filevar_pat <- "file_[A-Za-z0-9_]+"
io_pat <- paste0("(", paste(c("st_read\\(", "read_csv\\(", "read_sf\\(", "read.table\\(", "read\\.csv\\(", 
                              "write\\.csv\\(", "saveRDS\\(", "write_csv\\(", "occ_download_get\\(", "occ_download_import\\("), collapse="|"), ")")

out <- list()

# Try to safely source config.R to collect defined file_* values (if possible)
config_vars <- data.frame(var=character(), path=character(), exists=logical(), stringsAsFactors = FALSE)
try({
  if (file.exists("src/config.R")) {
    # source quietly in separate env
    e <- new.env()
    sys.source("src/config.R", envir = e)
    vars <- ls(envir = e, pattern = "^file_")
    if (length(vars)>0) {
      for (v in vars) {
        p <- tryCatch(get(v, envir = e), error = function(e) NA_character_)
        config_vars <- rbind(config_vars, data.frame(var=v, path=as.character(p), exists = file.exists(as.character(p)), stringsAsFactors = FALSE))
      }
    }
  } else if (file.exists("config.R")) {
    e <- new.env()
    sys.source("config.R", envir = e)
    vars <- ls(envir = e, pattern = "^file_")
    if (length(vars)>0) {
      for (v in vars) {
        p <- tryCatch(get(v, envir = e), error = function(e) NA_character_)
        config_vars <- rbind(config_vars, data.frame(var=v, path=as.character(p), exists = file.exists(as.character(p)), stringsAsFactors = FALSE))
      }
    }
  }
}, silent = TRUE)

# scan files for patterns
for (f in files) {
  lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
  for (i in seq_along(lines)) {
    line <- lines[i]
    # file var references
    fv <- str_extract_all(line, filevar_pat)[[1]]
    if (length(fv)>0) {
      for (v in fv) {
        out <- append(out, list(data.frame(script = f, line = i, match = v, type = "filevar_ref", literal_path = NA_character_, stringsAsFactors = FALSE)))
      }
    }
    # literal paths inside read/write calls (e.g., read_csv("data/..."))
    if (str_detect(line, "read_csv\\(|read\\.csv\\(|st_read\\(|read_sf\\(|write\\.csv\\(|saveRDS\\(|write_csv\\(")) {
      # extract quoted strings
      quotes <- str_match_all(line, "\"([^\"]+)\"|'([^']+)'")[[1]]
      if (nrow(quotes)>0) {
        for (r in seq_len(nrow(quotes))) {
          path <- if (!is.na(quotes[r,2])) quotes[r,2] else quotes[r,3]
          path_clean <- path
          out <- append(out, list(data.frame(script = f, line = i, match = basename(path_clean), type = "literal_path", literal_path = path_clean, stringsAsFactors = FALSE)))
        }
      } else {
        out <- append(out, list(data.frame(script = f, line = i, match = NA_character_, type = "io_call_no_literal", literal_path = NA_character_, stringsAsFactors = FALSE)))
      }
    }
  }
}

if (length(out)==0) {
  cat("Geen IO-verwijzingen gevonden.\n")
  quit(save="no", status=0)
}

df <- bind_rows(out)

# Combine with config vars where possible
df2 <- df %>%
  mutate(filevar = ifelse(type=="filevar_ref", match, NA_character_)) %>%
  left_join(config_vars %>% rename(filevar=var, config_path=path, config_exists=exists), by="filevar")

# For literal paths, check existence relative to repo root and absolute
df2 <- df2 %>%
  mutate(path_checked = ifelse(!is.na(literal_path), literal_path, config_path),
         path_checked = ifelse(is.na(path_checked), NA_character_, path_checked),
         exists = ifelse(!is.na(path_checked), file.exists(path_checked), NA))

# save manifest
if (!dir.exists("data/output")) dir.create("data/output", recursive = TRUE)
manifest_path <- "data/output/io_manifest.csv"
write.csv(df2, manifest_path, row.names = FALSE)
cat("IO manifest written to:", manifest_path, "\n\nSummary:\n")
print(df2 %>% group_by(type) %>% summarize(n = n(), missing = sum(is.na(exists) | exists==FALSE)))
cat("\nConfig file_* variables (from config.R):\n")
if (nrow(config_vars)>0) print(config_vars) else cat("Geen config vars gedetecteerd (config.R misschien ontbreekt of faalde tijdens source)\n")