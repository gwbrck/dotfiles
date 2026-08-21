{ config, pkgs, ... }:

{
  languages.python = {
    enable = true;
    venv.enable = true;
    lsp = {
      enable = true;
      package = pkgs.ty;
    };

    uv = {
      enable = true;
      sync.enable = true;
    };
  };

  # Python-Code in einem Unterverzeichnis? Dann `directory` setzen, devenv
  # sucht pyproject.toml/uv.lock dort und legt die venv passend an:
  #   languages.python.directory = "./src";
  # Siehe https://devenv.sh/languages/python/#changing-the-project-directory

  packages = with pkgs; [
    ruff
  ];

  files.".vscode/settings.json".json = {
    "python.defaultInterpreterPath" = "${config.devenv.state}/venv/bin/python";
  };

  integrations.gitnr.".gitignore" = {
    templates = [
      "tt:python"
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
      # von devenv generiert (maschinenlokale Pfade)
      ".vscode/settings.json"
      # uv / Python
      ".venv/"
      "__pycache__/"
      "*.py[cod]"
      "*.egg-info/"
      ".pytest_cache/"
      ".mypy_cache/"
      ".ruff_cache/"
      # sonstiges
      ".env"
    ];
  };

  # Nutzung:
  #   uv init             → vor der ersten Shell pyproject.toml anlegen
  #   devenv allow        → einmalig: Projekt für Auto-Aktivierung vertrauen
  #   cd <projekt>        → Umgebung lädt automatisch (devenv-Hook in fish),
  #                         raus aus dem Ordner = Umgebung weg. Manuell: `devenv shell`
  #   uv add <paket>      → Paket ins pyproject.toml + uv.lock aufnehmen
  #   uv add --dev <p>    → Dev-Dependency
  #   code .              → VS Code findet den Interpreter immer: aus der Shell
  #                         via VIRTUAL_ENV, sonst via generierter
  #                         .vscode/settings.json
  #   pytest / ruff / …   → Tools aus der venv direkt nutzbar
}
