# ============================================================================
# read_data.R — Chargement des sources de données Excel (répertoire data/)
# ============================================================================
# Conformément aux nouvelles directives, les données de l'enquête sont stockées
# dans plusieurs fichiers Excel placés dans le répertoire data/. Ce fichier
# centralise la lecture de ces fichiers via le package {readxl}.
#
# NB : le package {readxl} est nécessaire pour lire les fichiers .xlsx.
#
# Usage typique :
#   medoc_reg <- read_medoc_reg()
# ============================================================================

library(readxl)

# --- Chemins --------------------------------------------------------------
#' Répertoire contenant les fichiers Excel sources de données.
#' @return Chemin absolu vers le répertoire data/.
data_dir <- function() {
  file.path(getwd(), "data")
}

#' Chemin complet vers le fichier Excel MEDOC_REG.
#' @return Chemin absolu vers data/MEDOC_REG.xlsx.
medoc_reg_path <- function() {
  file.path(data_dir(), "MEDOC_REG.xlsx")
}

# --- Lecture MEDOC_REG ------------------------------------------------------
#' Importe le fichier data/MEDOC_REG.xlsx dans une dataframe.
#'
#' Le fichier MEDOC_REG.xlsx possède un onglet unique dont les trois premières
#' lignes servent d'en-têtes imbriqués (libellé des questions, modalités de
#' réponse, type de donnée). On ignore donc ces lignes (skip = 3) afin d'obtenir
#' une dataframe exploitable : une ligne par (Typologie, DCI, Type_donnee) et une
#' colonne par question / modalité de réponse.
#'
#' @return Une dataframe (tibble) issue de readxl::read_excel().
#' @export
read_medoc_reg <- function(path = medoc_reg_path()) {
  if (!file.exists(path)) {
    stop("Fichier Excel MEDOC_REG introuvable : ", path)
  }
  readxl::read_excel(path, sheet = 1L, skip = 3L, .name_repair = "unique")
}

# --- Lecture générique (futurs fichiers Excel) ------------------------------
#' Importe tous les fichiers Excel du répertoire data/ dans une liste de dataframes.
#'
#' Chaque element de la liste est nommé par le nom du fichier (sans extension).
#' Cette fonction anticipe l'ajout futur d'autres fichiers Excel, conformément aux
#' directives (plusieurs fichiers Excel chargés selon le même principe).
#'
#' @return Une liste nommée de dataframes, un element par fichier .xlsx de data/.
#' @export
read_all_excel <- function(dir = data_dir()) {
  files <- list.files(dir, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)
  if (length(files) == 0) {
    return(list())
  }

  out <- lapply(files, function(f) {
    readxl::read_excel(f)
  })
  names(out) <- tools::file_path_sans_ext(basename(files))
  out
}
