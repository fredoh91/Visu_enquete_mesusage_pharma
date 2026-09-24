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

# ============================================================================
# Couleurs d'identité de l'application (primary / secondary)
# ============================================================================
# Ces couleurs correspondent aux variables Bootstrap `--bs-primary` et
# `--bs-secondary` définies dans app.R via bs_theme(). Elles sont reflétées ici
# pour être utilisables côté R / Plotly (qui ne lisent pas les variables CSS).

#' Couleur « primary » de l'application (reflète `--bs-primary`).
#' @export
primary_color <- function() {
  "#18bc9c"   # vert turquoise (thème clair) — voir app.R
}

#' Couleur « secondary » de l'application (reflète `--bs-secondary`).
#' @export
secondary_color <- function() {
  "#3C1400"   # marron très foncé — voir app.R
}

# ============================================================================
# Couleurs du thème (clair / sombre) pour les graphiques Plotly
# ============================================================================
# Plotly dessine ses graphiques dans un canvas (SVG) : il ne lit ni les
# variables CSS du thème Bootstrap ni le thème bslib. Il faut donc lui fournir
# explicitement les fonds, la couleur du texte et celle de la grille/des axes,
# et ce pour chacun des deux modes (clair / sombre) de l'application.
#
# La bascule de thème est pilotée via la valeur réactive `theme_courant()`
# définie dans app.R (valeurs possibles : "clair" / "sombre"). Chaque
# renderPlotly en dépend pour être re-rendu à chaud, puis applique ces couleurs
# via apply_plotly_theme().

#' Palette thème (fond, texte, grille) pour un graphique Plotly.
#'
#' Renvoie une liste nommée décrivant l'habillage Plotly pour le mode demandé :
#'   * `bg`    : fond de la carte (paper_bgcolor) ;
#'   * `bgplot`: fond de la zone de tracé (plot_bgcolor) ;
#'   * `text`  : couleur du texte/des labélisations (font) ;
#'   * `grid`  : couleur de la grille et des axes.
#'
#' @param theme "clair" ou "sombre".
#' @return Une liste nommée.
#' @export
plotly_theme_colors <- function(theme = c("clair", "sombre")) {
  theme <- match.arg(theme)
  if (identical(theme, "sombre")) {
    list(
      bg     = "#303030",   # var(--bs-secondary-bg) — fond carte
      bgplot = "#303030",   # même fond pour la zone de tracé
      text   = "#ffffff",   # var(--bs-body-color)
      grid   = "#444444"    # var(--bs-border-color)
    )
  } else {
    list(
      bg     = "#ffffff",   # fond carte (thème clair)
      bgplot = "#ffffff",
      text   = NULL,        # garde la couleur de police par défaut de plotly
      grid   = "#e3e8ec"    # grille discrète (proche du défaut plotly)
    )
  }
}

#' Applique l'habillage du thème (clair/sombre) à un graphique Plotly.
#'
#' Ajoute (ou écrase) sur un graphique déjà construit : le fond de la carte
#' (`paper_bgcolor`), le fond de la zone de tracé (`plot_bgcolor`), la couleur
#' du texte global (`font`), et la couleur de la grille / des axes. À utiliser en
#' dernier dans chaque `renderPlotly` pour garantir un rendu cohérent avec le
#' thème actif.
#'
#' @param p Un objet plotly (éventuellement NULL pour un graphique vide).
#' @param theme "clair" ou "sombre".
#' @return L'objet plotly habillé, ou `p` inchangé s'il est NULL.
#' @export
apply_plotly_theme <- function(p, theme = c("clair", "sombre")) {
  if (is.null(p)) {
    return(p)
  }
  theme <- match.arg(theme)
  cols  <- plotly_theme_colors(theme)

  args <- list(
    paper_bgcolor = cols$bg,
    plot_bgcolor  = cols$bgplot,
    xaxis = list(
      gridcolor     = cols$grid,
      zerolinecolor = cols$grid,
      tickcolor     = cols$grid
    ),
    yaxis = list(
      gridcolor     = cols$grid,
      zerolinecolor = cols$grid,
      tickcolor     = cols$grid
    )
  )
  # On ne force la couleur de police que si elle est définie (mode sombre) afin
  # de préserver exactement le rendu par défaut du thème clair (notamment la
  # couleur des étiquettes "inside" des camemberts).
  if (!is.null(cols$text)) {
    args$font <- list(color = cols$text)
  }

  do.call(plotly::layout, c(list(p), args))
}

#' Construit un `renderPlotly` appliquant automatiquement le thème actif.
#'
#' Remplace `plotly::renderPlotly({ ... })` : l'expression `expr` (le corps du
#' render) est simplement évaluée, puis le graphique obtenu est habillé avec
#' [apply_plotly_theme()] selon la valeur réactive `theme` (arguments "clair" /
#' "sombre" pilotés par la bascule de thème).
#'
#' Grâce à ce wrapper, **il n'est pas nécessaire** de réécrire chacun des
#' `plotly::layout(...)` des modules : l'habillage du thème (fonds, texte,
#' grille) est appliqué *après* la construction, sur le résultat final.
#'
#' L'expression est évaluée dans l'environnement de l'appelant (le module), ce
#' qui laisse tous les objets réactifs locaux (dataframes, valeurs sélectionnées,
#' etc.) accessibles comme dans un `renderPlotly` classique. La dépendance à
#' `theme()` force le re-rendu du graphique à chaque bascule de thème.
#'
#' @param theme Une valeur réactive (appelable) renvoyant "clair" ou "sombre".
#' @param expr L'expression du corps du `renderPlotly` (non évaluée).
#' @param env Environnement dans lequel évaluer `expr` (par défaut : l'environnement
#'   de l'appelant, c.-à-d. la fonction serveur du module). À ne pas préciser.
#' @return Un objet `shiny.render.function` (renderPlotly).
#' @export
render_plotly_theme <- function(theme, expr, env = parent.frame()) {
  expr <- substitute(expr)
  # Important : forcer immédiatement l'évaluation de `parent.frame()` afin de
  # capturer l'environnement du module au moment de l'appel. Sans `force()`,
  # la promesse n'est évaluée qu'à l'exécution du render (bien plus tard), où
  # `parent.frame()` renverrait alors le contexte d'exécution de Shiny et non
  # l'environnement du module (d'où « objet/fonction introuvable » pour les
  # réactives locales comme `selected_age_court`).
  env <- force(env)
  plotly::renderPlotly({
    th <- theme()
    p  <- eval(expr, envir = env)
    apply_plotly_theme(p, th)
  })
}
