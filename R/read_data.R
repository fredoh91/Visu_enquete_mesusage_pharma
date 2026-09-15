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
#' Le fichier MEDOC_REG.xlsx possède un onglet unique dont les deux premières
#' lignes constituent des en-têtes imbriqués :
#'   - ligne 1 : grandes catégories (ex : "S0. Genre du patient :") ;
#'   - ligne 2 : libellés complets des colonnes (ex : "périmètre", "DCI",
#'     "Type_donnee", "Total", "Homme", "Femme", ...).
#'
#' On ignore donc la première ligne et on utilise la seconde comme noms de
#' colonnes (skip = 1). Le paramètre .name_repair = "unique" garantit des noms
#' de colonnes sans doublons (les intitulés répétés tels que "Oui", "Non", ...
#' sont suffixés automatiquement).
#'
#' De cette façon, la dataframe obtenue contient une ligne par (périmètre, DCI,
#' Type_donnee) et une colonne par question / modalité de réponse.
#'
#' @return Une dataframe (tibble) issue de readxl::read_excel().
#' @export
read_medoc_reg <- function(path = medoc_reg_path()) {
  if (!file.exists(path)) {
    stop("Fichier Excel MEDOC_REG introuvable : ", path)
  }
  readxl::read_excel(path, sheet = 1L, skip = 1L, .name_repair = "unique")
}

#' Chemin complet vers le fichier Excel LIB_CODE_ATC_OXOMEMAZINE.
#' @return Chemin absolu vers data/LIB_CODE_ATC_OXOMEMAZINE.xlsx.
lib_code_atc_path <- function() {
  file.path(data_dir(), "LIB_CODE_ATC_OXOMEMAZINE.xlsx")
}

#' Chemin complet vers le fichier Excel des facteurs.
#' @return Chemin absolu vers data/principaux_facteurs.xlsx.
principaux_facteurs_path <- function() {
  file.path(data_dir(), "principaux_facteurs.xlsx")
}

# --- Lecture LIB_CODE_ATC_OXOMEMAZINE ---------------------------------------
#' Importe le fichier data/LIB_CODE_ATC_OXOMEMAZINE.xlsx dans une dataframe.
#'
#' Comme pour MEDOC_REG.xlsx, ce fichier possède un onglet unique dont les deux
#' premières lignes constituent des en-têtes imbriqués : on ignore donc la
#' première ligne et on utilise la seconde comme noms de colonnes (skip = 1).
#' La structure est analogue à celle de MEDOC_REG, à la différence près que la
#' colonne B contient le code ATC et la colonne C le libellé ATC : un libellé
#' supplémentaire (« Lib ATC ») est donc inséré entre le code et le type de
#' donnée, ce qui décale d'un cran toutes les colonnes de données par rapport
#' au fichier MEDOC_REG.
#'
#' La dataframe obtenue contient une ligne par (périmètre, Code ATC, Type_donnee)
#' et une colonne par question / modalité de réponse.
#'
#' @return Une dataframe (tibble) issue de readxl::read_excel().
#' @export
read_lib_code_atc <- function(path = lib_code_atc_path()) {
  if (!file.exists(path)) {
    stop("Fichier Excel LIB_CODE_ATC_OXOMEMAZINE introuvable : ", path)
  }
  readxl::read_excel(path, sheet = 1L, skip = 1L, .name_repair = "unique")
}

# --- Lecture principaux_facteurs ----------------------------------------------
#' Importe le fichier data/principaux_facteurs.xlsx dans une dataframe.
#'
#' Contrairement aux fichiers MEDOC_REG et LIB_CODE_ATC, l'onglet de ce fichier
#' est organisé « en transposé » : chaque ligne correspond à un facteur (libellé
#' en colonne B, type de donnée « effectif » / « pourcentage » / « significativite »
#' en colonne C, effectif global en colonne D « Total »), et chaque DCI occupe une
#' colonne dédiée (EE..EQ, c'est-à-dire les indices 135 à 147) dont l'en-tête
#' (ligne 2) donne le nom de la DCI.
#'
#' On lit donc le fichier SANS ignorer de ligne et sans nommer les colonnes
#' (col_names = FALSE) : la ligne 1 contient les grandes catégories, la ligne 2
#' les en-têtes de colonnes et les lignes suivantes les données. On accède aux
#' colonnes par indice numérique, ce qui garantit la robustesse face aux
#' libellés contenant des espaces insécables.
#'
#' @return Une dataframe brute issue de readxl::read_excel() (col_names = FALSE).
#' @export
read_principaux_facteurs <- function(path = principaux_facteurs_path()) {
  if (!file.exists(path)) {
    stop("Fichier Excel principaux_facteurs introuvable : ", path)
  }
  readxl::read_excel(path, sheet = 1L, col_names = FALSE, .name_repair = "minimal")
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
