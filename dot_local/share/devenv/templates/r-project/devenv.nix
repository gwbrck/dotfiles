{ pkgs, ... }:

let
  rPackages = with pkgs.rPackages; [
    # Datenimport/-aufbereitung
    tidyverse
    janitor
    naniar
    rio
    datawizard
    MASS

    # Statistik und Modellierung
    easystats
    rstatix
    infer
    pwr
    robustbase
    emmeans
    WRS2
    tidycomm
    icr

    # Reporting und Entwicklung
    rempsyc
    quarto
    knitr
    rmarkdown
    see
    flextable
    kableExtra
    plotly
    languageserver

    # Beispieldatensätze
    titanic
    gss

    # Dev
    servr
    dotenv
    rix
  ];

  # Die Wrapper tragen die Paket-Library in sich (R_LIBS_SITE).
  # Immer die Wrapper verwenden.
  r-with-packages = pkgs.rWrapper.override { packages = rPackages; };
  radian-with-packages = pkgs.radianWrapper.override { packages = rPackages; };
in
{
  languages.r.enable = true;
  languages.r.package = r-with-packages;
  languages.r.lsp.enable = false;
  languages.r.radian.enable = true;
  languages.r.radian.package = radian-with-packages;

  packages = [
    # `rstudio` aus dieser Shell starten: nutzt das Wrapper-R mit allen
    # Paketen.
    (pkgs.rstudioWrapper.override { packages = rPackages; })
  ];

  files.".vscode/settings.json".json = {
    "r.rpath.mac" = "${r-with-packages}/bin/R";
    "r.rpath.linux" = "${r-with-packages}/bin/R";
    "r.rterm.mac" = "${radian-with-packages}/bin/radian";
    "r.rterm.linux" = "${radian-with-packages}/bin/radian";
  };

  integrations.gitnr.".gitignore" = {
    templates = [
      "tt:r"
      "tt:macos"
      "tt:linux"
      "tt:vim"
      "tt:visualstudiocode"
    ];
    content = [
      # devenv
      ".devenv/"
      "devenv.local.nix"
      "devenv.local.yaml"
      # von devenv generiert (maschinenlokale Store-Pfade)
      ".vscode/settings.json"
      # Quarto
      ".quarto/"
      "/.quarto/"
      "_site/"
      "_book/"
      "/_output/"
      "**/*.quarto_ipynb"
      # sonstiges
      "/.luarc.json"
      ".env"
    ];
  };

  # Quarto-Cli muss im Path liegen

  # Nutzung:
  #   devenv allow        → einmalig: Projekt für Auto-Aktivierung vertrauen
  #   cd <projekt>        → Umgebung lädt automatisch (devenv-Hook in fish),
  #                         raus aus dem Ordner = Umgebung weg. Manuell: `devenv shell`
  #   code .              → VS Code aus der Umgebung: erbt R, radian, languageserver
  #   rstudio             → RStudio mit Projekt-R
  #   git                 → kommt aus deinem User-Profil, Config gilt wie immer
}
