# ================================================
# Curso–Taller (El Salvador) - Setup paquetes v2
# Windows: fuerza instalación BINARIA (sin compilar)
# Uso: abrir en RStudio y click en "Source"
# ================================================

options(repos = c(CRAN = "https://cloud.r-project.org"))
options(pkgType = "binary")
options(install.packages.check.source = "no")

message("=== Configurando repositorios y modo binario ===")
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (tolower(Sys.info()[["sysname"]]) == "windows") {
  options(pkgType = "binary")
  options(install.packages.check.source = "no")
}

# Paquetes requeridos (sin duplicados, y sin 'covr')
pkgs <- c(
  "rprojroot","rstudioapi","telegram.bot","data.table","readr","readxl",
  "dplyr","fs","purrr","stringr","terra","gdata","ggplot2","raster","rlang",
  "sf","svDialogs","tibble","tictoc","tidyterra","tidyverse","tidyr",
  "tmap","classInt","scales","forcats","openxlsx", "readODS"
)
pkgs <- unique(pkgs)

# Instalar faltantes (binarios en Windows)
installed <- rownames(installed.packages())
to_install <- setdiff(pkgs, installed)
if (length(to_install) > 0) {
  message("Instalando paquetes (puede tardar unos minutos):")
  print(to_install)
  install.packages(to_install, dependencies = TRUE)
} else {
  message("Todos los paquetes ya estaban instalados.")
}

# Verificación de carga
failed <- character(0)
for (p in pkgs) {
  ok <- suppressWarnings(suppressMessages(require(p, character.only = TRUE)))
  if (!ok) failed <- c(failed, p)
}

message("\n=== RESUMEN ===")
if (length(failed) == 0) {
  message("✅ Todo listo: TODOS los paquetes se instalaron y cargaron correctamente.")
} else {
  message("⚠️ No se pudieron cargar estos paquetes:\n  - ", paste(failed, collapse = ", "))
  if ("sf" %in% failed) {
    message("\nSugerencia para 'sf':\n",
            "  1) Cierra y vuelve a abrir RStudio.\n",
            "  2) Ejecuta de nuevo este script (se intentará el binario).\n",
            "  3) Si aún falla, ver 'Plan B' abajo (RTools).\n")
  }
}





