#!/usr/bin/env Rscript

packages_to_install <- c(
  # Datenmanipulation und -visualisierung
  "tidyverse",
  "janitor",
  "naniar",
  "rio",
  "datawizard",
  "MASS",


  # Statistik und Modellierung
  "easystats",
  "rstatix",
  "infer",
  "pwr",
  "robustbase",
  "emmeans",
  "WRS2",

  # Reporting und Entwicklung
  "quarto",
  "see",
  "flextable",
  "kableExtra",
  "plotly",
  "languageserver",
  "servr"
)


installed_packages <- rownames(utils::installed.packages())
miss_pack <- packages_to_install[!packages_to_install %in% installed_packages]

if (length(miss_pack) > 0) {
  cat("Die folgenden Pakete fehlen und werden jetzt installiert:\n")
  cat(paste(" -", miss_pack, collapse = "\n"))
  cat("\n")

  utils::install.packages(miss_pack)
} else {
  # Informiere den Benutzer, dass alles auf dem neuesten Stand ist.
  cat("✅ Alle benötigten Pakete sind bereits installiert.\n")
}


cat("\n🚀 Setup-Skript abgeschlossen.\n")
