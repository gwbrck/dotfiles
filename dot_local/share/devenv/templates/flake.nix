{
  description = "Local devenv project templates";

  outputs = { self }: {
    templates = {
      r-project = {
        path = ./r-project;
        description = "R project with devenv, RStudio, radian, and Quarto";
      };

      python-project = {
        path = ./python-project;
        description = "Python project with devenv, uv, ruff, and ty";
      };
    };
  };
}
