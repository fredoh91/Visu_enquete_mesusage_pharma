# ============================================================================
# colors.R — Couleurs métier de l'application (référence pour Plotly et R)
# ============================================================================
# Ce fichier centralise les couleurs associées aux notions « genre du patient »
# (Homme / Femme / Autre) utilisées par les graphiques Plotly et, d'une manière
# générale, par tout code R qui doit colorer ces notions.
#
# NB IMPORTANT : les valeurs ci-dessous doivent rester en phase avec les
# variables CSS homonymes définies dans www/custom.css (bloc `:root`) :
#     --couleur-homme : rgb(0,70,103)
#     --couleur-femme : rgb(45,131,14)
#     --couleur-autre : rgb(255,149,43)
# Le CSS sert de référence pour tout rendu HTML/CSS natif (badges, cellules,
# fonds...), tandis que ce helper R les reflète pour les graphiques Plotly,
# qui ne lisent pas les variables CSS directement.
# ============================================================================

#' Couleurs officielles des genres (Homme, Femme, Autre).
#'
#' Renvoie un vecteur nommé de couleurs hexadécimales, une par modalité du
#' genre du patient, dans l'ordre canonique `Homme`, `Femme`, `Autre`.
#' Ces valeurs correspondent aux variables CSS `--couleur-*` de `custom.css`.
#'
#' @return Un vecteur nommé : c(Homme = ..., Femme = ..., Autre = ...).
#' @export
genres_colors <- function() {
  c(
    Homme = "#004667",   # rgb(0, 70, 103)  — bleu foncé
    Femme = "#2d830e",   # rgb(45, 131, 14) — vert
    Autre = "#ff952b"    # rgb(255, 149, 43) — orange
  )
}

#' Couleurs officielles des barres d'âge (niveau 1 = groupes, niveau 2 = détails).
#'
#' Renvoie un vecteur nommé de deux couleurs hexadécimales utilisées pour la
#' série de barres horizontales des âges :
#'   * `groupe` : barres de niveau 1 (ex. "ST ENFANTS, ADOLESCENTS", "ST ADULTES") ;
#'   * `detail` : barres de niveau 2 (tranches d'âge détaillées).
#'
#' Il s'agit de nuances de bordeaux, volontairement distinctes des couleurs du
#' genre (notamment du vert "Femme") pour éviter toute confusion visuelle.
#' Ces valeurs correspondent aux variables CSS `--couleur-groupe` et
#' `--couleur-detail` de `custom.css`.
#'
#' @return Un vecteur nommé : c(groupe = ..., detail = ...).
#' @export
age_colors <- function() {
  c(
    groupe = "#8b1e2d",   # var(--couleur-groupe) — bordeaux soutenu
    detail = "#c98792"    # var(--couleur-detail) — bordeaux clair
  )
}

#' Couleurs officielles des barres d'âge « ensemble des cas » (2 niveaux).
#'
#' Renvoie un vecteur nommé de deux couleurs hexadécimales utilisées pour la
#' seconde barre de la série horizontale des âges, celle exprimée « par rapport
#' à l'ensemble des cas » (le dénominateur est alors l'effectif total de
#' l'enquête, et non celui de la molécule sélectionnée) :
#'   * `groupe` : barres de niveau 1 (ex. "ENFANTS, ADOLESCENTS", "ADULTES") ;
#'   * `detail` : barres de niveau 2 (tranches d'âge détaillées).
#'
#' Il s'agit de nuances d'orange, volontairement distinctes des bordeaux du
#' mode « molécule » (age_colors()) afin de bien différencier les deux modes
#' de calcul des pourcentages.
#' Ces valeurs correspondent aux variables CSS `--couleur-groupe-ensemble` et
#' `--couleur-detail-ensemble` de `custom.css`.
#'
#' @return Un vecteur nommé : c(groupe = ..., detail = ...).
#' @export
age_colors_ensemble <- function() {
  c(
    groupe = "#e2800f",   # var(--couleur-groupe-ensemble) — orange soutenu
    detail = "#e4b279"    # var(--couleur-detail-ensemble) — orange clair
  )
}

#' Couleurs officielles de la répartition « enceinte » (Oui / Non / Non renseigné).
#'
#' Renvoie un vecteur nommé de trois nuances de vert, déclinées à partir du vert
#' "Femme" (--couleur-femme). Elles colorent les sous-segments "enceinte" qui
#' découpent le secteur Femme du camembert (sunburst).
#'
#' Ces valeurs correspondent aux variables CSS `--couleur-enceinte-*` de
#' `custom.css`.
#'
#' @return Un vecteur nommé : c(Oui = ..., Non = ..., `Non renseigné` = ...).
#' @export
enceinte_colors <- function() {
  c(
    Oui            = "#2d830e",   # var(--couleur-enceinte-oui) — vert femme
    Non            = "#1b5e09",   # var(--couleur-enceinte-non) — vert foncé
    `Non renseigné` = "#6fae48"   # var(--couleur-enceinte-nr)  — vert clair
  )
}
