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

#' Découpe un libellé en plusieurs lignes pour les graphiques Plotly.
#'
#' Si un libellé (ex. "Manque d'info. ou com. interprofessionnelle") est trop
#' long pour être lisible sur une seule ligne sur l'axe Y d'un graphique en
#' barres, on insère des retours à la ligne HTML (`<br>`) afin de l'afficher sur
#' **2 lignes, voire 3** maximum. La découpe se fait proprement sur les espaces.
#'
#' La fonction préserve :
#'   * l'indentation initiale (espaces) utilisée pour la hiérarchie (niveau 2) ;
#'   * les balises `<b>...</b>` encadrant les libellés de niveau 1 (groupes) ;
#'   * le caractère invisible `\u200B` ajouté en fin de libellé pour distinguer
#'     sur l'axe Y les deux barres « molécule » et « ensemble des cas ».
#'
#' @param x Vecteur de chaînes (les libellés à afficher sur l'axe Y).
#' @param max_width Largeur (en caractères) maximale d'une ligne, au-delà de
#'   laquelle on passe à la ligne suivante.
#' @param max_lines Nombre maximal de lignes autorisé (2 ou 3) ; au-delà, les
#'   lignes excédentaires sont recompressées dans la dernière.
#'
#' @return Un vecteur de chaînes où les libellés trop longs sont séparés par
#'   `<br>` (Parser par Plotly comme des retours à la ligne).
#' @export
wrap_libelle <- function(x, max_width = 28L, max_lines = 3L) {
  vapply(x, function(s) {
    s <- as.character(s)
    if (length(s) == 0 || is.na(s) || !nzchar(s)) {
      return(s)
    }
    # Indentation initiale (espaces) — préservée en tête de libellé.
    m <- regexpr("^ *", s)
    indent <- regmatches(s, m)
    corps <- sub("^ *", "", s)

    # Balises de gras (niveau 1 / groupes).
    bold <- grepl("^<b>", corps) && grepl("</b>$", corps)
    if (bold) {
      corps <- sub("^<b>", "", corps)
      corps <- sub("</b>$", "", corps)
    }

    # Caractère invisible de distinction des deux barres (molécule / ensemble).
    zw <- grepl("\u200B$", corps)
    if (zw) {
      corps <- sub("\u200B$", "", corps)
    }

    # Découpe sur les espaces si le libellé dépasse la largeur autorisée.
    if (nchar(corps) <= max_width) {
      lignes <- corps
    } else {
      mots <- strsplit(corps, " ")[[1]]
      lignes <- character(0)
      ligne <- ""
      for (mo in mots) {
        test <- if (nzchar(ligne)) paste(ligne, mo) else mo
        if (nchar(test) > max_width && nzchar(ligne)) {
          lignes <- c(lignes, ligne)
          ligne <- mo
        } else {
          ligne <- test
        }
      }
      if (nzchar(ligne)) {
        lignes <- c(lignes, ligne)
      }
      # Limite le nombre de lignes à max_lines.
      if (length(lignes) > max_lines) {
        lignes <- c(
          lignes[seq_len(max_lines - 1L)],
          paste(lignes[seq(from = max_lines, to = length(lignes))], collapse = " ")
        )
      }
    }

    out <- paste(lignes, collapse = "<br>")
    if (bold) {
      out <- paste0("<b>", out, "</b>")
    }
    if (zw) {
      out <- paste0(out, "\u200B")
    }
    paste0(indent, out)
  }, character(1), USE.NAMES = FALSE)
}

# ============================================================================
# Helpers pour les TITRES de graphiques Plotly
# ============================================================================
# Les titres des graphiques (ex. « Répartition par âge — Au moment de la
# dispensation en pharmacie ») peuvent être trop longs pour la largeur de la
# carte : Plotly les tronque alors sur une seule ligne. On les découpe donc sur
# 2 à 3 lignes (via <br>) pour en afficher l'intégralité, selon leur longueur.

#' Découpe un titre de graphique Plotly sur 2 à 3 lignes.
#'
#' Réutilise la logique de [wrap_libelle()] (retours à la ligne HTML `<br>`,
#' découpe propre aux espaces, lignes regroupées dans la limite imposée) avec
#' des réglages adaptés aux titres : largeur de ligne plus généreuse que pour
#' les ticks d'axe Y et nombre de lignes maximal fixé à 3.
#'
#' @param x Vecteur de chaînes (les titres à afficher au-dessus des graphiques).
#' @param max_width Largeur (en caractères) maximale d'une ligne.
#' @param max_lines Nombre maximal de lignes autorisé (2 ou 3).
#' @return Un vecteur de chaînes où les titres trop longs sont séparés par `<br>`.
#' @export
wrap_titre <- function(x, max_width = 30L, max_lines = 3L) {
  vapply(x, function(s) {
    s <- as.character(s)
    if (length(s) == 0 || is.na(s) || !nzchar(s)) {
      return(s)
    }
    wrap_libelle(s, max_width = max_width, max_lines = max_lines)
  }, character(1), USE.NAMES = FALSE)
}

#' Calcule la marge supérieure d'un graphique pour accueillir son titre multi-lignes.
#'
#' Détermine une marge `t` (en px) suffisante pour que le titre — éventuellement
#' découpé sur plusieurs lignes par [wrap_titre()] — soit intégralement visible
#' sans chevaucher la zone de tracé. La marge de base correspond à un titre sur
#' une seule ligne ; chaque ligne supplémentaire ajoute une hauteur fixe.
#'
#' @param x Vecteur de chaînes (titres, contenant éventuellement `<br>`).
#' @param base Hauteur (en px) réservée pour une seule ligne de titre.
#' @param par_ligne Hauteur (en px) supplémentaire pour chaque ligne additionnelle.
#' @return Un vecteur numérique des marges supérieures, en px.
#' @export
titre_margin_top <- function(x, base = 50L, par_ligne = 20L) {
  n <- lengths(strsplit(as.character(x), "<br>", fixed = TRUE))
  base + (pmax(n, 1L) - 1L) * par_ligne
}
