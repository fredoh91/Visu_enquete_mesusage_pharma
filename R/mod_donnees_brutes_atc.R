# ============================================================================
# mod_donnees_brutes_atc.R — Module Shiny « DONNÉES BRUTES ATC »
# Affiche un tableau récapitulatif des codes ATC (libellé, code, effectif)
# issus de PMI_ANSM_Livraison_Données_brutes.xlsx, puis, au clic sur une
# ligne, plusieurs graphiques interactifs (plotly) pour le code ATC
# sélectionné. Cet écran est largement analogue à celui du module
# LIB_CODE_ATC, mais repose sur un fichier « brut » : chaque ligne du fichier
# correspond à UN CAS (une fiche signalée par un pharmacien), et non à une
# combinaison effectif/pourcentage/significativité.
#
# La première passe effectue un comptage des occurrences pour chaque code /
# libellé ATC (colonne Q, format "CODE (Libellé)"). Les graphiques agrégeront
# ensuite les modalités des colonnes D (genre), E (enceinte), F (âge),
# G (origine) et K/L/M (types de mésusage) pour l'ATC sélectionné.
#
# NB IMPORTANT : ce fichier est lu via read_pmi_brutes() avec col_names = FALSE
# (accès par index numérique). La première ligne du fichier contient l'en-tête
# de catégorie (ex. "S0. Genre du patient :", "Libellé Code ATC") : elle est
# volontairement ignorée, les données commencent à la ligne 2.

library(shiny)
library(dplyr)
library(DT)

# ---------------------------------------------------------------------------
# SEUIL D'AFFICHAGE DU TABLEAU ATC
# ---------------------------------------------------------------------------
# Variable clairement identifiée en tête de module : seules les lignes dont
# l'effectif est au moins égal à ce seuil seront affichées dans le tableau
# supérieur (et serviront de base au calcul de l'effectif « ensemble des cas »).
SEUIL_ATC <- 5

# --- UI du module -------------------------------------------------------------
#' UI du module Données brutes ATC.
#'
#' L'écran reprend l'organisation en deux grandes zones des autres modules :
#'   * Zone supérieure (.zone-tableau)  : le tableau récapitulatif des ATC
#'     (Libellé ATC / Code ATC / Effectif).
#'   * Zone inférieure (.zone-graphiques) : les graphiques relatifs au code ATC
#'     sélectionné, chacun dans sa carte (.graph-card) disposée de façon
#'     responsive via la grille Bootstrap 5 (col-12 / col-md-6 / col-xl-4).
#'
#' @param id Identifiant unique du module.
mod_donnees_brutes_atc_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # --- Zone supérieure : tableau des codes ATC ------------------------------
    tags$div(
      class = "zone-tableau",
      uiOutput(ns("etat")),
      DTOutput(ns("table_atc"))
    ),

    # --- Zone inférieure : graphiques (responsive) ----------------------------
    tags$div(
      class = "zone-graphiques",
      tags$h4("Détail du code ATC sélectionné"),
      uiOutput(ns("plot_msg")),

      fluidRow(
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_genre"), height = "640px")
          )
        ),
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_enceinte"), height = "640px")
          )
        ),
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_age"), height = "640px")
          )
        ),
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_origine"), height = "640px")
          )
        ),
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_type"), height = "640px")
          )
        ),
        # Carte 6 : barres horizontales des facteurs de mésusage.
        # Les données proviennent directement du fichier brut (colonnes S, T, U
        # = indices 19, 20, 21 pour les libellés prédéfinis, et V = index 22
        # pour le texte libre « Autre »). Le grand nombre de facteurs (29)
        # justifie un dépliage : par défaut on n'affiche que le nombre paramétré
        # de facteurs (n_facteurs, par défaut 10) aux plus grands pourcentages,
        # une case à cocher permet d'afficher l'ensemble des facteurs.
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            shiny::tags$div(
              class = "facteur-controls",
              shiny::checkboxInput(
                ns("facteur_afficher_tous"),
                label = "Afficher les 29 facteurs (déplier)",
                value = FALSE
              ),
              shiny::checkboxInput(
                ns("facteur_afficher_ens"),
                label = "Afficher l'ensemble des cas",
                value = FALSE
              )
            ),
            # compacte pour la vue "n_facteurs premiers", agrandie après dépliage.
            shiny::uiOutput(ns("plot_facteur_ui"))
          )
        ),
        # Carte 7 : barres horizontales du type d'obtention (colonne H, index 8).
        # Les données occupent 2 niveaux hiérarchiques (2 groupes de niveau 1,
        # 5 modalités de niveau 2), comme les graph âge et type de mésusage.
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_type_obtention"), height = "840px")
          )
        ),
        # Carte 8 : barres horizontales du type d'ordonnance (colonne I, index 9).
        # 4 modalités de niveau « detail » (pas de hiérarchie).
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_type_ordonnance"), height = "640px")
          )
        ),
        # Carte 9 : barres horizontales de la nature du prescripteur (colonne J,
        # index 10). 4 modalités de niveau « detail » (pas de hiérarchie).
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_nature_prescripteur"), height = "640px")
          )
        )
      )
    )
  )
}

# --- Server du module ---------------------------------------------------------
#' Server du module Données brutes ATC.
#' @param id Identifiant unique du module (doit correspondre à l'UI).
#' @param theme Une valeur réactive (appelable) renvoyant "clair" ou "sombre",
#'   utilisée pour habiller les graphiques (render_plotly_theme).
#' @param n_facteurs Nombre de facteurs de mésusage affichés par défaut dans le
#'   graphique des facteurs (barres horizontales) avant dépliage. Par défaut,
#'   seul le mode molécule est tracé : le nombre de barres vaut donc
#'   \code{n_facteurs}. Lorsque la case « Afficher l'ensemble des cas » est cochée,
#'   chaque facteur génère 2 barres (molécule + ensemble des cas), soit
#'   \code{n_facteurs * 2}. Paramétrable :
#'   \code{mod_donnees_brutes_atc_server("donnees_brutes_atc", theme, n_facteurs = 10)}.
mod_donnees_brutes_atc_server <- function(id, theme, n_facteurs = 10) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Données brutes importées depuis le fichier Excel -------------------
    # La dataframe brute est chargée une seule fois au démarrage du module
    # (accès par index, voir read_data.R). La première ligne (en-tête de
    # catégorie) est retirée : dans ce fichier brut, les données commencent à
    # la ligne 2.
    pmi_brutes <- reactive({
      tryCatch({
        d <- read_pmi_brutes()
        if (is.null(d) || nrow(d) == 0) {
          return(NULL)
        }
        d[-1L, , drop = FALSE]
      }, error = function(e) NULL)
    })

    # --- Extraction du code / libellé ATC à partir de la colonne Q ----------
    # La colonne Q (index 17) est formatée "CODE (Libellé)". On nettoie les
    # valeurs vides / NA, puis on sépare le code (avant la première parenthèse)
    # du libellé (entre parenthèses).
    colonne_atc <- reactive({
      d <- pmi_brutes()
      req(d)
      vals <- as.character(d[[17]])
      vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
      code <- sub("\\s*\\(.*$", "", vals)
      lib  <- sub("^.*\\(", "", vals)
      lib  <- sub("\\)$", "", lib)
      ok   <- nzchar(trimws(code)) & nzchar(trimws(lib))
      data.frame(
        Code = trimws(code[ok]),
        Lib  = trimws(lib[ok]),
        stringsAsFactors = FALSE
      )
    })

    # --- Dataframe du tableau récapitulatif des ATC --------------------------
    # Compte le nombre d'occurrences pour chaque (Code, Libellé) ATC, filtre sur
    # le SEUIL_ATC et trie par effectif décroissant. Colonnes affichées :
    # Libellé ATC, Code ATC, Effectif.
    atc_df <- reactive({
      ca <- colonne_atc()
      req(ca)
      # `table()` sur les 2 colonnes (Code puis Lib) renvoie un croisement dont
      # la 1ère colonne est le Code et la 2e le Libellé : on les nomme ici dans
      # cet ordre, avant de réordonner l'affichage (Libellé ATC en 1re colonne).
      tbl <- as.data.frame(table(ca), stringsAsFactors = FALSE)
      names(tbl) <- c("Code ATC", "Lib ATC", "Effectif")
      tbl$Effectif <- as.integer(tbl$Effectif)
      tbl <- tbl[tbl$Effectif >= SEUIL_ATC, , drop = FALSE]
      tbl <- tbl[order(-tbl$Effectif), , drop = FALSE]
      tbl <- tbl[, c("Lib ATC", "Code ATC", "Effectif"), drop = FALSE]
      rownames(tbl) <- NULL
      tbl
    })

    # --- Effectif « ensemble des cas » ----------------------------------------
    total_ens <- reactive({
      df <- atc_df()
      req(df)
      sum(df$Effectif, na.rm = TRUE)
    })



    # --- Code ATC sélectionné dans le tableau ---------------------------------
    selected_code <- reactive({
      idx <- input$table_atc_rows_selected
      if (is.null(idx) || length(idx) == 0 || idx < 1 || idx > nrow(atc_df())) {
        return(NULL)
      }
      atc_df()[["Code ATC"]][idx]
    })

    # Libellé ATC de la ligne sélectionnée (pour les titres de graphiques).
    selected_lib <- reactive({
      idx <- input$table_atc_rows_selected
      if (is.null(idx) || length(idx) == 0 || idx < 1 || idx > nrow(atc_df())) {
        return(NULL)
      }
      atc_df()[["Lib ATC"]][idx]
    })

    # --- Effectif de l'ATC sélectionné (« molécule ») -------------------------
    # Dénominateur des pourcentages en mode « molécule » (nombre de cas de
    # l'ATC sélectionné = son effectif dans le tableau).
    total_mol <- reactive({
      code <- selected_code()
      df <- atc_df()
      req(code, df)
      hit <- df[["Code ATC"]] == code
      if (!any(hit)) {
        return(NULL)
      }
      sum(df$Effectif[hit], na.rm = TRUE)
    })

    # --- Sous-ensemble des cas de l'ATC sélectionné ---------------------------
    # Toutes les lignes du fichier brut dont la colonne Q (nettoyée) vaut le
    # code ATC sélectionné : base de tous les graphiques.
    cas_atc <- reactive({
      code <- selected_code()
      d <- pmi_brutes()
      req(code, d)
      vals <- as.character(d[[17]])
      code_row <- sub("\\s*\\(.*$", "", vals)
      d[!is.na(code_row) & trimws(code_row) == code, , drop = FALSE]
    })

    # --- Données du camembert du genre des patients (colonne D, index 4) -----
    genre_values <- reactive({
      cas <- cas_atc()
      req(cas)
      v <- as.character(cas[[4]])
      lbls <- c("Homme", "Femme", "Autre")
      eff <- vapply(lbls, function(l) sum(v %in% l, na.rm = TRUE), integer(1))
      total <- sum(eff)
      if (total <= 0) {
        return(NULL)
      }
      df <- data.frame(Label = lbls, Effectif = as.numeric(eff), stringsAsFactors = FALSE)
      df$Pct <- df$Effectif / total * 100
      df$Pct_fr <- formatC(df$Pct, format = "f", digits = 1,
                           big.mark = " ", decimal.mark = ",")
      df[df$Effectif > 0, ]
    })

    # --- Données du camembert des données "enceinte" (colonne E, index 5) -----
    # Pourcentage sur le TOTAL des données "enceinte" (Non + Oui + Non
    # renseigné), conformément aux directives. Les NA/vides sont écartés.
    enceinte_values <- reactive({
      cas <- cas_atc()
      req(cas)
      v <- as.character(cas[[5]])
      lbls <- c("Non", "Oui", "Non renseigné")
      eff <- vapply(lbls, function(l) sum(v %in% l, na.rm = TRUE), integer(1))
      total <- sum(eff)   # Non + Oui + Non renseigné
      if (total <= 0) {
        return(NULL)
      }
      df <- data.frame(Label = lbls, Effectif = as.numeric(eff), stringsAsFactors = FALSE)
      df$Pct <- df$Effectif / total * 100
      df$Pct_fr <- formatC(df$Pct, format = "f", digits = 1,
                           big.mark = " ", decimal.mark = ",")
      df[df$Effectif > 0, ]
    })



    # --- Données des barres d'âge (colonne F, index 6) ------------------------
    # Compte les tranches d'âge parmi les cas de l'ATC et reorganise selon la
    # hierarchie des niveaux 1 / 2 (comme les autres onglets). La valeur
    # d'en-tête de colonne est exclue car elle ne figure pas dans les valeurs.
    # Chaque libellé genere DEUX barres (modes "molécule" et "ensemble des cas") :
    #   * "molécule"        : denominateur = total_mol (effectif de l'ATC) ;
    #   * "ensemble des cas": denominateur = total_ens (somme des ATC >= seuil).
    age_values <- reactive({
      cas <- cas_atc()
      mol <- total_mol()
      ens <- total_ens()
      req(cas, mol, ens)

      v <- as.character(cas[[6]])
      v <- v[!is.na(v) & nzchar(trimws(v))]
      # Uniformise les espaces insécables (présents dans le fichier brut, ex.
      # "Nouveau né ou nourrisson\u00A0(0-23 mois)") en espaces normaux pour un
      # appariement fiable des tranches d'âge.
      v <- gsub("\u00A0", " ", v)

      # Tranches fines de la colonne F (hors groupe "ST ..." qui n'apparaît pas).
      tranches <- c(
        "Nouveau né ou nourrisson (0-23 mois)",
        "Entre 2 et 11 ans (enfant)",
        "Entre 12 et 17 ans (adolescent)",
        "Entre 18 et 34 ans",
        "Entre 35 et 49 ans",
        "Entre 50 et 64 ans",
        "65 ans et plus"
      )
      eff_t <- vapply(tranches, function(t) sum(v %in% t, na.rm = TRUE), integer(1))

      # Libellés affichés (niveau 1 en gras, niveau 2 indentés) et hiérarchie.
      defs <- data.frame(
        Libelle_brut = c(
          "Total ENFANTS et ADOLESCENTS",        # groupe (niveau 1)
          "Nouveau né ou nourrisson (0-23 mois)",
          "Entre 2 et 11 ans (enfant)",
          "Entre 12 et 17 ans (adolescent)",
          "Total ADULTES",                        # groupe (niveau 1)
          "Entre 18 et 34 ans",
          "Entre 35 et 49 ans",
          "Entre 50 et 64 ans",
          "65 ans et plus"
        ),
        Index = c(1L, 1L, 2L, 3L, 4L, 4L, 5L, 6L, 7L),
        Niveau = c(1L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L),
        stringsAsFactors = FALSE
      )
      # Effectif des groupes = somme des tranches fines correspondantes.
      # (NB : un défaut d'index faisait auparavant ne sommer qu'une seule
      #  tranche par groupe. On somme désormais explicitement les tranches
      #  fines de chaque groupe, à l'instar des graph Types / Type d'obtention.)
      #   Total ENFANTS et ADOLESCENTS -> tranches 1, 2, 3
      #                                   (Nouveau né, 2-11 ans, 12-17 ans)
      #   Total ADULTES                -> tranches 4, 5, 6, 7
      #                                   (18-34, 35-49, 50-64, 65 ans et plus)
      groupe_tranches <- list(
        `1` = c(1L, 2L, 3L),
        `4` = c(4L, 5L, 6L, 7L)
      )
      eff <- vapply(seq_len(nrow(defs)), function(k) {
        if (defs$Niveau[k] == 1L) {
          sum(eff_t[groupe_tranches[[as.character(defs$Index[k])]]])
        } else {
          eff_t[defs$Index[k]]
        }
      }, integer(1))

      Type  <- ifelse(defs$Niveau == 1L, "groupe", "detail")
      Libelle_aff <- ifelse(
        defs$Niveau == 1L,
        paste0("<b>", defs$Libelle_brut, "</b>"),
        paste0("    ", defs$Libelle_brut)
      )

      # Deux rangées par libellé (molécule puis ensemble des cas).
      out <- do.call(rbind, lapply(seq_along(Libelle_aff), function(k) {
        rbind(
          data.frame(
            Libelle      = Libelle_aff[k],
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "molécule",
            Type         = Type[k],
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(Libelle_aff[k], "\u200B"),
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "ensemble des cas",
            Type         = Type[k],
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Données des barres horizontales de l'origine du mésusage (col G) -----
    # Compte les 3 modalités d'origine parmi les cas de l'ATC en ramenant les
    # libellés longs du fichier à leurs libellés courts. Deux barres par
    # modalité (molécule / ensemble des cas), comme les graph Âge et Type.
    origine_values <- reactive({
      cas <- cas_atc()
      mol <- total_mol()
      ens <- total_ens()
      req(cas, mol, ens)

      v <- as.character(cas[[7]])
      v <- v[!is.na(v) & nzchar(trimws(v))]
      # Uniformise les espaces insécables du fichier brut en espaces normaux.
      v <- gsub("\u00A0", " ", v)

      origin_bruts <- c(
        "Au moment de la prise du médicament",
        "Au moment de la prescription médicale",
        "Au moment de la dispensation en pharmacie"
      )
      eff <- vapply(origin_bruts, function(o) sum(grepl(o, v, fixed = TRUE)), integer(1))

      # Deux rangées par modalité (toutes de niveau "detail").
      out <- do.call(rbind, lapply(seq_along(origin_bruts), function(k) {
        rbind(
          data.frame(
            Libelle      = origin_bruts[k],
            Libelle_brut = origin_bruts[k],
            Mode         = "molécule",
            Type         = "detail",
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(origin_bruts[k], "\u200B"),
            Libelle_brut = origin_bruts[k],
            Mode         = "ensemble des cas",
            Type         = "detail",
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })



    # --- Données des barres horizontales du type de mésusage (K, L, M) --------
    # Compte les types de mésusage parmi les cas de l'ATC. Les données sont
    # réparties sur 3 colonnes (K=11, L=12, M=13) : un cas peut avoir jusqu'à 3
    # types. On fusionne ces 3 colonnes puis on attribue chaque valeur à un
    # libellé affiché court, réparti sur 2 groupes de niveau 1. Deux barres par
    # libellé (molécule / ensemble des cas), comme les graph Âge et Origine.
    type_values <- reactive({
      cas <- cas_atc()
      mol <- total_mol()
      ens <- total_ens()
      req(cas, mol, ens)

      # Empile les valeurs des 3 colonnes de types de mésusage.
      vals <- unlist(lapply(c(11L, 12L, 13L), function(i) as.character(cas[[i]])), use.names = FALSE)
      vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
      # Uniformise les espaces insécables du fichier brut en espaces normaux.
      vals <- gsub("\u00A0", " ", vals)

      if (length(vals) == 0) {
        return(NULL)
      }

      # Motifs de détection (regex, uniques entre eux) -> libellé affiché.
      motifs <- c(
        "Schéma posologique non conforme"        = "Schéma posologique non conforme",
        "Arrêt prématuré"                         = "Arrêt prématuré",
        "Prolongation de la durée du traitement"  = "Prolongation durée TT",
        "Voie d'administration non conforme"      = "Voie d'administration non conforme",
        "pour une indication hors AMM"            = "Indication hors AMM",
        "par une population non prévue"           = "Population non prévue",
        "contre-indications connues"              = "Contre-indications",
        "interaction médicamenteuse contre-indiquée" = "Interaction médicamenteuse",
        "Autre"                                   = "Autre"
      )

      # Compte chaque type en testant les motifs (le 1er motif qui matche).
      compter_type <- function(pattern, valeurs) {
        sum(sapply(valeurs, function(x) grepl(pattern, x, fixed = TRUE)))
      }
      eff_noms <- names(motifs)
      eff <- vapply(eff_noms, function(m) compter_type(m, vals), integer(1))
      names(eff) <- NULL

      # Libellés affichés courts (niveau 2) + groupe auquel ils appartiennent.
      details1 <- c("Schéma posologique non conforme", "Arrêt prématuré",
                    "Prolongation durée TT", "Voie d'administration non conforme")
      details2 <- c("Indication hors AMM", "Population non prévue",
                    "Contre-indications", "Interaction médicamenteuse", "Autre")

      lib_d <- unname(motifs)   # libellés affichés des 9 types
      grp <- ifelse(lib_d %in% details1, 1L, 2L)   # groupe (1 ou 2)

      # Effectif des 2 groupes = somme des types du groupe.
      eff_g1 <- sum(eff[grp == 1L])
      eff_g2 <- sum(eff[grp == 2L])

      defs <- data.frame(
        Libelle_brut = c(
          "Total Posologie, Fréquence, durée",   # groupe 1 (niveau 1)
          lib_d[grp == 1L],
          "Total Indication, population, CI",    # groupe 2 (niveau 1)
          lib_d[grp == 2L]
        ),
        Niveau = c(1L, rep(2L, sum(grp == 1L)), 1L, rep(2L, sum(grp == 2L))),
        Effectif = c(eff_g1, eff[grp == 1L], eff_g2, eff[grp == 2L]),
        stringsAsFactors = FALSE
      )

      Type <- ifelse(defs$Niveau == 1L, "groupe", "detail")
      Libelle_aff <- ifelse(
        defs$Niveau == 1L,
        paste0("<b>", defs$Libelle_brut, "</b>"),
        paste0("    ", defs$Libelle_brut)
      )

      out <- do.call(rbind, lapply(seq_len(nrow(defs)), function(k) {
        rbind(
          data.frame(
            Libelle      = Libelle_aff[k],
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "molécule",
            Type         = Type[k],
            Effectif     = defs$Effectif[k],
            Pct          = if (defs$Effectif[k] > 0) defs$Effectif[k] / mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(Libelle_aff[k], "\u200B"),
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "ensemble des cas",
            Type         = Type[k],
            Effectif     = defs$Effectif[k],
            Pct          = if (defs$Effectif[k] > 0) defs$Effectif[k] / ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })



    # --- Message / état de l'import des données --------------------------------
    output$etat <- renderUI({
      data <- pmi_brutes()
      if (is.null(data)) {
        return(
          div(class = "alert alert-danger",
              icon("exclamation-triangle"),
              strong("Fichier PMI_ANSM_Livraison_Données_brutes introuvable ou illisible dans data/."))
        )
      }
      NULL
    })

    # --- Tableau récapitulatif des ATC -----------------------------------------
    output$table_atc <- renderDT({
      df <- atc_df()
      if (is.null(df) || nrow(df) == 0) {
        return(
          DT::datatable(
            data.frame(Avertissement = "Aucune donnée ATC disponible."),
            options = list(dom = "t"), rownames = FALSE
          )
        )
      }
      DT::datatable(
        df,
        rownames = FALSE,
        colnames = c("Libellé ATC" = "Lib ATC",
                     "Code ATC" = "Code ATC",
                     "Effectif" = "Effectif"),
        # Une seule ligne sélectionnable à la fois (mode "single").
        selection = "single",
        options = list(
          pageLength = 15,
          language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json")
        )
      )
    })

    # --- Normalisation des libellés du fichier brut --------------------------
    # Le fichier brut contient des espaces insécables (U+00A0, ex. « cabinet  ou
    # clinique privée »), des apostrophes courbes (’) ou droites (') selon les
    # lignes, ainsi que des variantes de libellés facteurs avec suffixe " *".
    # Cette fonction uniformise tout cela pour un appariement fiable des
    # modalités (comparaison par égalité exacte après normalisation).
    norm_brut <- function(x) {
      x <- as.character(x)
      x <- gsub("\u00A0", " ", x)          # espace insécable -> espace normal
      x <- gsub("\u2019", "'", x)          # apostrophe courbe fermante -> droite
      x <- gsub("\u2018", "'", x)          # apostrophe courbe ouvrante -> droite
      x <- sub(" *\\*$", "", x)            # retire le suffixe " *" éventuel
      x <- gsub(" +", " ", x)             # replie les espaces multiples
      trimws(x)
    }

    # --- Données des barres horizontales des facteurs (S, T, U, V) -----------
    # Les facteurs sont directement dans le fichier brut : les libellés
    # prédéfinis occupent les colonnes S, T, U (indices 19, 20, 21) ; la colonne
    # V (index 22) contient le texte libre « Autre, préciser » : toute cellule
    # non-vide y est comptabilisée comme « Autre ». Chaque libellé génère DEUX
    # barres (molécule / ensemble des cas), comme les graph Âge, Origine et
    # Type. Les barres sont triées par pourcentage décroissant (critère = barre
    # molécule).
    facteurs_map <- c(
      "Manque d'informations et/ou de communication interprofessionnelle" = "Manque d'info. ou com. interprofessionnelle",
      "Entourage (famille, proches)"                                        = "Entourage",
      "A déjà reçu un traitement dans des conditions similaires d'un médicament gardé dans son armoire à pharmacie" = "A déjà reçu un méd. dans des cnd. sim.",
      "Survenue d'effets indésirables gênants, autres que ceux nécessitant un arrêt du traitement" = "Survenue d'effets indésirables",
      "Médicament accessible sans ordonnance ou en accès direct"            = "Médicament accessible sans ordonnance",
      "Double prescription ou chevauchement de traitement"                   = "Double prescription ou chevauchement TT",
      "Défaut de transmission de l'information au patient lors de la dispensation" = "Défaut de transmission de l'information",
      "Forme galénique ou voie d'administration mal adaptée"                 = "Forme ou voie mal adaptée",
      "Influence d'un discours promotionnel autour du médicament (ex. : recommandation média, réseaux sociaux, laboratoire, publicité, forum de patients, etc.)" = "Influence d'un discours promotionnel",
      "Rupture de stock entrainant un remplacement"                          = "Rupture de stock entraînant un remp.",
      "Volonté du patient"                                                   = "Volonté du patient",
      "Manquements du médecin (manque de formation, n'a pas posé de questions, ne s'est pas informé…)" = "Manquements du médecin",
      "Volonté du médecin, passe outre les recommandations"                  = "Volonté du médecin",
      "Accoutumance, dépendance"                                             = "Accoutumance",
      "Douleurs persistantes, non soulagées"                                 = "Douleurs persistantes",
      "Amélioration des symptômes"                                           = "Amélioration des symptômes",
      "Manque d'alternatives, échec d'autres molécules, essais du médecin"   = "Manque d'alternatives",
      "Manque d'observance du patient"                                       = "Manque d'observance patient",
      "Médecin trop permissif, complaisance, insistance patient"             = "Médecin trop permissif",
      "Médicament obtenu via une pharmacie en ligne"                         = "Pharmacie en ligne",
      "Médecin (sans précision)"                                             = "Médecin (sans précision)",
      "Objectif esthétique"                                                  = "Objectif esthétique",
      "Environnement du patient (déficit intellectuel, classe sociale…)"     = "Environnement du patient",
      "Manque de médecins, difficultés à avoir un rendez-vous"                = "Manque de médecins",
      "Manque de connaissance du traitement"                                 = "Manque de connaissance traitement",
      "Manquement du pharmacien, mauvaises habitudes"                         = "Manquement du pharmacien",
      "Financier, ne veut pas payer, faire passer par la Sécurité Sociale"   = "Financier",
      "Autre"                                                                = "Autre",
      "(Non renseigné)"                                                      = "Non renseigné"
    )

    facteur_values <- reactive({
      cas <- cas_atc()
      mol <- total_mol()
      ens <- total_ens()
      req(cas, mol, ens)

      # Libellés prédéfinis sur les colonnes S, T, U (indices 19, 20, 21).
      vals <- unlist(lapply(c(19L, 20L, 21L), function(i) as.character(cas[[i]])), use.names = FALSE)
      vals <- norm_brut(vals)
      vals <- vals[!is.na(vals) & nzchar(vals)]

      # Effectif de chaque facteur : comparaison par égalité exacte après
      # normalisation (les clés sont aussi normalisées pour ce faire).
      noms <- norm_brut(names(facteurs_map))
      eff <- vapply(noms, function(n) sum(vals == n, na.rm = TRUE), integer(1))

      # Colonne V (index 22) : texte libre « Autre, préciser » -> « Autre ».
      autre_txt <- norm_brut(cas[[22]])
      autre_txt <- autre_txt[!is.na(autre_txt) & nzchar(autre_txt)]
      eff[["Autre"]] <- eff[["Autre"]] + length(autre_txt)

      lib_disp <- unname(facteurs_map)

      # Tri par pourcentage décroissant (critère = barre molécule) : on réordonne
      # les indices des libellés avant de générer les 2 barres de chacun.
      ordre <- order(-eff)

      out <- do.call(rbind, lapply(ordre, function(k) {
        rbind(
          data.frame(
            Libelle      = lib_disp[k],
            Libelle_brut = lib_disp[k],
            Mode         = "molécule",
            Type         = "detail",
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(lib_disp[k], "\u200B"),
            Libelle_brut = lib_disp[k],
            Mode         = "ensemble des cas",
            Type         = "detail",
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Hauteur du graphique facteur (responsive au dépliage) ----------------
    facteur_height <- reactive({
      if (isTRUE(input$facteur_afficher_tous)) {
        "1000px"
      } else {
        # Nombre de barres affichées : n_facteurs si seul le mode molécule est
        # tracé, 2 * n_facteurs si la case « ensemble des cas » est cochée.
        n_barres <- n_facteurs * (if (isTRUE(input$facteur_afficher_ens)) 2L else 1L)
        paste0(n_barres * 55, "px")
      }
    })

    # --- Jeu de données affiché (filtrage n_facteurs premiers / tous) ---------
    facteur_plot_data <- reactive({
      fv <- facteur_values()
      if (is.null(fv) || nrow(fv) == 0) {
        return(NULL)
      }
      # Modes tracés : par défaut, seul le mode molécule est conservé ;
      # la case « Afficher l'ensemble des cas » y ajoute le mode ensemble des cas.
      modes <- if (isTRUE(input$facteur_afficher_ens)) {
        c("molécule", "ensemble des cas")
      } else {
        "molécule"
      }
      fv <- fv[fv$Mode %in% modes, , drop = FALSE]
      n_lignes <- if (isTRUE(input$facteur_afficher_tous)) nrow(fv) else n_facteurs * length(modes)
      fv[seq_len(min(n_lignes, nrow(fv))), ]
    })

    # --- Données des barres horizontales du type d'obtention (col. H) ---------
    # Colonne H (index 8) du fichier brut. Les données occupent 2 niveaux
    # hiérarchiques : 2 groupes de niveau 1 (« Prise avec/sans ordonnance ») et
    # leurs 5 modalités de niveau 2, comme les graph Âge et Type. Deux barres
    # par libellé (molécule / ensemble des cas).
    type_obtention_values <- reactive({
      cas <- cas_atc()
      mol <- total_mol()
      ens <- total_ens()
      req(cas, mol, ens)

      v <- as.character(cas[[8]])
      v <- norm_brut(v)
      v <- v[!is.na(v) & nzchar(v)]

      src_niv2 <- c(
        "D'une primo-prescription",
        "D'un renouvellement d'ordonnance",
        "D'un médicament sans ordonnance en automédication",
        "D'un médicament sans ordonnance sur conseil du pharmacien",
        "Je ne sais pas"
      )
      eff2 <- vapply(src_niv2, function(s) sum(v == s, na.rm = TRUE), integer(1))

      defs <- data.frame(
        Libelle_brut = c(
          "Prise avec ordonnance",        # groupe 1 (niveau 1)
          "Primo prescription",
          "Renouvellement",
          "Prise sans ordonnance",        # groupe 2 (niveau 1)
          "Sans ordo en automédication",
          "Sur conseil du pharmacien",
          "Je ne sais pas"
        ),
        Niveau = c(1L, 2L, 2L, 1L, 2L, 2L, 2L),
        Effectif = c(
          sum(eff2[1:2]),
          eff2[1],
          eff2[2],
          sum(eff2[3:5]),
          eff2[3],
          eff2[4],
          eff2[5]
        ),
        stringsAsFactors = FALSE
      )

      Type <- ifelse(defs$Niveau == 1L, "groupe", "detail")
      Libelle_aff <- ifelse(
        defs$Niveau == 1L,
        paste0("<b>", defs$Libelle_brut, "</b>"),
        paste0("    ", defs$Libelle_brut)
      )

      out <- do.call(rbind, lapply(seq_len(nrow(defs)), function(k) {
        rbind(
          data.frame(
            Libelle      = Libelle_aff[k],
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "molécule",
            Type         = Type[k],
            Effectif     = defs$Effectif[k],
            Pct          = if (defs$Effectif[k] > 0) defs$Effectif[k] / mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(Libelle_aff[k], "\u200B"),
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "ensemble des cas",
            Type         = Type[k],
            Effectif     = defs$Effectif[k],
            Pct          = if (defs$Effectif[k] > 0) defs$Effectif[k] / ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Données des barres horizontales du type d'ordonnance (col. I) --------
    # Colonne I (index 9) du fichier brut, 4 modalités de niveau « detail »
    # (pas de hiérarchie), comme le graph Origine. Deux barres par modalité
    # (molécule / ensemble des cas).
    type_ordonnance_values <- reactive({
      cas <- cas_atc()
      mol <- total_mol()
      ens <- total_ens()
      req(cas, mol, ens)

      mods <- c(
        "Ordonnance simple",
        "Ordonnance sécurisée",
        "Prescription de médicaments d'exception",
        "Autre type d'ordonnance"
      )
      v <- as.character(cas[[9]])
      v <- norm_brut(v)
      v <- v[!is.na(v) & nzchar(v)]

      nmods <- norm_brut(mods)
      eff <- vapply(nmods, function(n) sum(v == n, na.rm = TRUE), integer(1))

      out <- do.call(rbind, lapply(seq_along(mods), function(k) {
        rbind(
          data.frame(
            Libelle      = mods[k],
            Libelle_brut = mods[k],
            Mode         = "molécule",
            Type         = "detail",
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(mods[k], "\u200B"),
            Libelle_brut = mods[k],
            Mode         = "ensemble des cas",
            Type         = "detail",
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Données des barres de la nature du prescripteur (col. J) -------------
    # Colonne J (index 10) du fichier brut, 4 modalités de niveau « detail »
    # (pas de hiérarchie). Deux barres par modalité (molécule / ensemble des cas).
    nature_prescripteur_values <- reactive({
      cas <- cas_atc()
      mol <- total_mol()
      ens <- total_ens()
      req(cas, mol, ens)

      mods <- c(
        "Médecin généraliste",
        "Médecin spécialiste hospitalier",
        "Médecin spécialiste libéral (en cabinet ou clinique privée)",
        "Autres prescripteurs : Sage-Femme, kinésithérapeute, IDE"
      )
      v <- as.character(cas[[10]])
      v <- norm_brut(v)
      v <- v[!is.na(v) & nzchar(v)]

      nmods <- norm_brut(mods)
      eff <- vapply(nmods, function(n) sum(v == n, na.rm = TRUE), integer(1))

      out <- do.call(rbind, lapply(seq_along(mods), function(k) {
        rbind(
          data.frame(
            Libelle      = mods[k],
            Libelle_brut = mods[k],
            Mode         = "molécule",
            Type         = "detail",
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(mods[k], "\u200B"),
            Libelle_brut = mods[k],
            Mode         = "ensemble des cas",
            Type         = "detail",
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Message / titre de la partie détail ----------------------------------
    output$plot_msg <- renderUI({
      code <- selected_code()
      lib  <- selected_lib()
      if (is.null(code)) {
        return(
          tags$p("Cliquez sur un code ATC du tableau ci-dessus pour afficher le détail.")
        )
      }
      tags$p(
        class = "dci-title",
        icon("file-medical"),
        strong(paste("Caractéristiques du code ATC —", code, "·", lib))
      )
    })



    # --- Camembert 1 : genre des patients (partie basse) ----------------------
    # Camembert simple : répartition Homme / Femme / Autre. Chaque secteur
    # affiche l'effectif et le pourcentage (entre parenthèses). L'infobulle
    # mentionne le libellé de la donnée, l'effectif et le pourcentage.
    output$plot_genre <- render_plotly_theme(theme, {
      code <- selected_code()
      gv <- genre_values()
      if (is.null(code) || is.null(gv) || nrow(gv) == 0) {
        return(plotly::plotly_empty())
      }

      # Couleurs officielles du genre (R/colors.R).
      pal_genre <- genres_colors()
      col_segments <- unname(pal_genre[gv$Label])

      txt <- paste0(gv$Label, "<br>", gv$Effectif, " (", gv$Pct_fr, "%)")

      plot_genre_ly <- plotly::plot_ly(
        labels = gv$Label,
        values = gv$Effectif,
        type = "pie",
        customdata = gv$Pct_fr,
        text = txt,
        textinfo = "text",
        textposition = "inside",
        insidetextorientation = "horizontal",
        marker = list(
          colors = col_segments,
          line = list(color = "#ffffff", width = 1)
        ),
        hovertemplate = paste0(
          "%{label}<br>Effectif : %{value}<br>Pourcentage : %{customdata} %<extra></extra>"
        ),
        showlegend = FALSE
      )

      titre <- wrap_titre(paste("Répartition par genre —", selected_lib(), "·", code))
      plot_genre_ly %>%
        plotly::layout(
          title = titre,
          margin = list(l = 20, r = 20, t = titre_margin_top(titre), b = 20)
        )
    })

    # --- Camembert 2 : données "enceinte" (partie basse) ----------------------
    # Camembert simple : répartition Oui / Non / Non renseigné en nuances de
    # vert. Le pourcentage est recalculé sur l'effectif TOTAL des données
    # "enceinte" (Non + Oui + Non renseigné).
    output$plot_enceinte <- render_plotly_theme(theme, {
      code <- selected_code()
      ev <- enceinte_values()
      if (is.null(code) || is.null(ev) || nrow(ev) == 0) {
        return(plotly::plotly_empty())
      }

      pal_enceinte <- enceinte_colors()
      col_segments <- unname(pal_enceinte[ev$Label])

      txt <- paste0(ev$Label, "<br>", ev$Effectif, " (", ev$Pct_fr, "%)")

      plot_enceinte_ly <- plotly::plot_ly(
        labels = ev$Label,
        values = ev$Effectif,
        type = "pie",
        customdata = ev$Pct_fr,
        text = txt,
        textinfo = "text",
        textposition = "inside",
        insidetextorientation = "horizontal",
        marker = list(
          colors = col_segments,
          line = list(color = "#ffffff", width = 1)
        ),
        hovertemplate = paste0(
          "%{label}<br>Effectif : %{value}<br>Pourcentage : %{customdata} %<extra></extra>"
        ),
        showlegend = FALSE
      )

      titre <- wrap_titre(paste("Répartition « enceinte » —", selected_lib(), "·", code))
      plot_enceinte_ly %>%
        plotly::layout(
          title = titre,
          margin = list(l = 20, r = 20, t = titre_margin_top(titre), b = 20)
        )
    })



    # --- Barres horizontales des âges (données S2, colonne F) -----------------
    # Deux barres par libellé : la première (bordeaux) exprime le pourcentage
    # « par rapport à l'ATC concerné » (dénominateur = total_mol), la seconde
    # (orange), juste en dessous, « par rapport à l'ensemble des cas »
    # (dénominateur = total_ens). Les 4 combinaisons (Type : groupe/detail) ×
    # (Mode : molécule/ensemble des cas) forment 4 traces distinctes.
    output$plot_age <- render_plotly_theme(theme, {
      code <- selected_code()
      av <- age_values()
      if (is.null(code) || is.null(av) || nrow(av) == 0) {
        return(plotly::plotly_empty())
      }

      pal_mol <- age_colors()
      pal_ens <- age_colors_ensemble()
      col_vec <- ifelse(
        av$Mode == "molécule",
        ifelse(av$Type == "groupe", unname(pal_mol["groupe"]), unname(pal_mol["detail"])),
        ifelse(av$Type == "groupe", unname(pal_ens["groupe"]), unname(pal_ens["detail"]))
      )
      av$Col <- col_vec

      av$Libelle <- wrap_libelle(av$Libelle)
      categoryarray <- rev(av$Libelle)

      av$TickLabel <- av$Libelle
      av$TickLabel[av$Mode == "ensemble des cas"] <- ""

      groups <- list(
        list(Mode = "molécule",         Type = "groupe", Nom = "Molécule — groupe"),
        list(Mode = "molécule",         Type = "detail", Nom = "Molécule — détail"),
        list(Mode = "ensemble des cas", Type = "groupe", Nom = "Ensemble des cas — groupe"),
        list(Mode = "ensemble des cas", Type = "detail", Nom = "Ensemble des cas — détail")
      )

      p <- plotly::plot_ly()
      for (g in groups) {
        sub <- av[av$Mode == g$Mode & av$Type == g$Type, ]
        if (nrow(sub) == 0) {
          next
        }
        p <- p %>%
          plotly::add_trace(
            type = "bar",
            orientation = "h",
            x = sub$Pct,
            y = sub$Libelle,
            text = paste0(round(sub$Pct, 1), " %"),
            textposition = "auto",
            cliponaxis = FALSE,
            marker = list(color = sub$Col),
            customdata = lapply(seq_len(nrow(sub)), function(i) {
              c(sub$Libelle_brut[i], sub$Effectif[i], round(sub$Pct[i], 1))
            }),
            insidetextfont = list(color = "#ffffff"),
            hovertemplate = paste0(
              "%{customdata[0]}<br>Effectif : %{customdata[1]}",
              "<br>Pourcentage : %{customdata[2]} %<extra></extra>"
            ),
            name = g$Nom,
            showlegend = TRUE
          )
      }

      titre <- wrap_titre(paste("Répartition par âge —", selected_lib(), "·", code))
      p %>%
        plotly::layout(
          title = titre,
          barmode = "overlay",
          xaxis = list(
            title = "Pourcentage (%)",
            range = c(0, 105),
            ticksuffix = "%"
          ),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = categoryarray,
            type = "category",
            automargin = TRUE,
            tickfont = list(size = 11),
            tickmode = "array",
            tickvals = categoryarray,
            ticktext = rev(av$TickLabel)
          ),
          margin = list(l = 20, r = 20, t = titre_margin_top(titre), b = 20),
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.12,
            font = list(size = 11)
          )
        )
    })



    # --- Barres horizontales de l'origine du mésusage (colonne G) -------------
    # Deux barres par modalité d'origine (molécule puis ensemble des cas), avec
    # les mêmes couleurs bordeaux / orange que les graph Âge et Type. Toutes les
    # modalités sont de niveau "detail" (pas de groupe).
    output$plot_origine <- render_plotly_theme(theme, {
      code <- selected_code()
      ov <- origine_values()
      if (is.null(code) || is.null(ov) || nrow(ov) == 0) {
        return(plotly::plotly_empty())
      }

      pal_mol <- age_colors()
      pal_ens <- age_colors_ensemble()
      col_vec <- ifelse(
        ov$Mode == "molécule",
        ifelse(ov$Type == "groupe", unname(pal_mol["groupe"]), unname(pal_mol["detail"])),
        ifelse(ov$Type == "groupe", unname(pal_ens["groupe"]), unname(pal_ens["detail"]))
      )
      ov$Col <- col_vec

      ov$Libelle <- wrap_libelle(ov$Libelle)
      categoryarray <- rev(ov$Libelle)

      ov$TickLabel <- ov$Libelle
      ov$TickLabel[ov$Mode == "ensemble des cas"] <- ""

      groups <- list(
        list(Mode = "molécule",         Type = "groupe", Nom = "Molécule — groupe"),
        list(Mode = "molécule",         Type = "detail", Nom = "Molécule — détail"),
        list(Mode = "ensemble des cas", Type = "groupe", Nom = "Ensemble des cas — groupe"),
        list(Mode = "ensemble des cas", Type = "detail", Nom = "Ensemble des cas — détail")
      )

      p <- plotly::plot_ly()
      for (g in groups) {
        sub <- ov[ov$Mode == g$Mode & ov$Type == g$Type, ]
        if (nrow(sub) == 0) {
          next
        }
        p <- p %>%
          plotly::add_trace(
            type = "bar",
            orientation = "h",
            x = sub$Pct,
            y = sub$Libelle,
            text = paste0(round(sub$Pct, 1), " %"),
            textposition = "auto",
            cliponaxis = FALSE,
            marker = list(color = sub$Col),
            customdata = lapply(seq_len(nrow(sub)), function(i) {
              c(sub$Libelle_brut[i], sub$Effectif[i], round(sub$Pct[i], 1))
            }),
            insidetextfont = list(color = "#ffffff"),
            hovertemplate = paste0(
              "%{customdata[0]}<br>Effectif : %{customdata[1]}",
              "<br>Pourcentage : %{customdata[2]} %<extra></extra>"
            ),
            name = g$Nom,
            showlegend = TRUE
          )
      }

      titre <- wrap_titre(paste("Répartition par origine —", selected_lib(), "·", code))
      p %>%
        plotly::layout(
          title = titre,
          barmode = "overlay",
          xaxis = list(
            title = "Pourcentage (%)",
            range = c(0, 105),
            ticksuffix = "%"
          ),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = categoryarray,
            type = "category",
            automargin = TRUE,
            tickfont = list(size = 11),
            tickmode = "array",
            tickvals = categoryarray,
            ticktext = rev(ov$TickLabel)
          ),
          margin = list(l = 20, r = 20, t = titre_margin_top(titre), b = 20),
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.12,
            font = list(size = 11)
          )
        )
    })



    # --- Barres horizontales du type de mésusage (K, L, M) --------------------
    # Deux barres par libellé (molécule / ensemble des cas), hiérarchie 2
    # groupes de niveau 1 + détails. Mêmes couleurs et mise en forme que les
    # graph Âge et Origine.
    output$plot_type <- render_plotly_theme(theme, {
      code <- selected_code()
      tv <- type_values()
      if (is.null(tv) || nrow(tv) == 0) {
        return(plotly::plotly_empty())
      }

      pal_mol <- age_colors()
      pal_ens <- age_colors_ensemble()
      col_vec <- ifelse(
        tv$Mode == "molécule",
        ifelse(tv$Type == "groupe", unname(pal_mol["groupe"]), unname(pal_mol["detail"])),
        ifelse(tv$Type == "groupe", unname(pal_ens["groupe"]), unname(pal_ens["detail"]))
      )
      tv$Col <- col_vec

      tv$Libelle <- wrap_libelle(tv$Libelle)
      categoryarray <- rev(tv$Libelle)

      tv$TickLabel <- tv$Libelle
      tv$TickLabel[tv$Mode == "ensemble des cas"] <- ""

      groups <- list(
        list(Mode = "molécule",         Type = "groupe", Nom = "Molécule — groupe"),
        list(Mode = "molécule",         Type = "detail", Nom = "Molécule — détail"),
        list(Mode = "ensemble des cas", Type = "groupe", Nom = "Ensemble des cas — groupe"),
        list(Mode = "ensemble des cas", Type = "detail", Nom = "Ensemble des cas — détail")
      )

      p <- plotly::plot_ly()
      for (g in groups) {
        sub <- tv[tv$Mode == g$Mode & tv$Type == g$Type, ]
        if (nrow(sub) == 0) {
          next
        }
        p <- p %>%
          plotly::add_trace(
            type = "bar",
            orientation = "h",
            x = sub$Pct,
            y = sub$Libelle,
            text = paste0(round(sub$Pct, 1), " %"),
            textposition = "auto",
            cliponaxis = FALSE,
            marker = list(color = sub$Col),
            customdata = lapply(seq_len(nrow(sub)), function(i) {
              c(sub$Libelle_brut[i], sub$Effectif[i], round(sub$Pct[i], 1))
            }),
            insidetextfont = list(color = "#ffffff"),
            hovertemplate = paste0(
              "%{customdata[0]}<br>Effectif : %{customdata[1]}",
              "<br>Pourcentage : %{customdata[2]} %<extra></extra>"
            ),
            name = g$Nom,
            showlegend = TRUE
          )
      }

      titre <- wrap_titre(paste("Répartition par type de mésusage —", selected_lib(), "·", code))
      p %>%
        plotly::layout(
          title = titre,
          barmode = "overlay",
          xaxis = list(
            title = "Pourcentage (%)",
            range = c(0, 105),
            ticksuffix = "%"
          ),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = categoryarray,
            type = "category",
            automargin = TRUE,
            tickfont = list(size = 11),
            tickmode = "array",
            tickvals = categoryarray,
            ticktext = rev(tv$TickLabel)
          ),
          margin = list(l = 20, r = 20, t = titre_margin_top(titre), b = 20),
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.12,
            font = list(size = 11)
          )
        )
    })

    # --- Barres horizontales des facteurs (S, T, U, V) ------------------------
    # Conteneur du graphe facteur : renderUI injecte le plotlyOutput avec la
    # bonne hauteur (facteur_height()), qui change selon l'état du dépliage.
    output$plot_facteur_ui <- shiny::renderUI({
      plotly::plotlyOutput(ns("plot_facteur"), height = facteur_height())
    })

    output$plot_facteur <- render_plotly_theme(theme, {
      code <- selected_code()
      fv <- facteur_plot_data()
      if (is.null(code) || is.null(fv) || nrow(fv) == 0) {
        return(plotly::plotly_empty())
      }

      # Couleurs officielles : bordeaux (molécule) / orange (ensemble des cas).
      pal_mol <- age_colors()
      pal_ens <- age_colors_ensemble()
      col_vec <- ifelse(
        fv$Mode == "molécule",
        ifelse(fv$Type == "groupe", unname(pal_mol["groupe"]), unname(pal_mol["detail"])),
        ifelse(fv$Type == "groupe", unname(pal_ens["groupe"]), unname(pal_ens["detail"]))
      )
      fv$Col <- col_vec

      fv$Libelle <- wrap_libelle(fv$Libelle)
      categoryarray <- rev(fv$Libelle)

      fv$TickLabel <- fv$Libelle
      fv$TickLabel[fv$Mode == "ensemble des cas"] <- ""

      groups <- list(
        list(Mode = "molécule", Type = "detail", Nom = "Molécule — détail")
      )
      # La trace « ensemble des cas » n'est ajoutée que si la case
      # « Afficher l'ensemble des cas » est cochée.
      if (isTRUE(input$facteur_afficher_ens)) {
        groups[[length(groups) + 1L]] <- list(Mode = "ensemble des cas", Type = "detail", Nom = "Ensemble des cas — détail")
      }

      p <- plotly::plot_ly()
      for (g in groups) {
        sub <- fv[fv$Mode == g$Mode & fv$Type == g$Type, ]
        if (nrow(sub) == 0) {
          next
        }
        p <- p %>%
          plotly::add_trace(
            type = "bar",
            orientation = "h",
            x = sub$Pct,
            y = sub$Libelle,
            text = paste0(round(sub$Pct, 1), " %"),
            textposition = "outside",
            cliponaxis = FALSE,
            marker = list(color = sub$Col),
            customdata = lapply(seq_len(nrow(sub)), function(i) {
              c(sub$Libelle_brut[i], sub$Effectif[i], round(sub$Pct[i], 1))
            }),
            # textposition = "outside" : le texte des pourcentages n'est plus
            # contraint par l'épaisseur des barres (fines surtout en mode déplié),
            # il reste donc lisible et de grande taille dans les deux modes.
            outsidetextfont = list(size = 17),
            hovertemplate = paste0(
              "%{customdata[0]}<br>Effectif : %{customdata[1]}",
              "<br>Pourcentage : %{customdata[2]} %<extra></extra>"
            ),
            name = g$Nom,
            showlegend = TRUE
          )
      }

      titre <- wrap_titre(paste("Répartition par facteur —", selected_lib(), "·", code))
      p %>%
        plotly::layout(
          title = titre,
          barmode = "overlay",
          xaxis = list(
            title = "Pourcentage (%)",
            range = c(0, 115),
            ticksuffix = "%"
          ),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = categoryarray,
            type = "category",
            automargin = TRUE,
            tickfont = list(size = 11),
            tickmode = "array",
            tickvals = categoryarray,
            ticktext = rev(fv$TickLabel)
          ),
          margin = list(l = 20, r = 45, t = titre_margin_top(titre), b = 20),
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.12,
            font = list(size = 11)
          )
        )
    })

    # --- Barres horizontales du type d'obtention (col. H, 2 niveaux) ----------
    # Comme le graph « type de mésusage » : 4 traces (molécule groupe/detail,
    # ensemble groupe/detail) et couleurs bordeaux / orange.
    output$plot_type_obtention <- render_plotly_theme(theme, {
      code <- selected_code()
      tv <- type_obtention_values()
      if (is.null(code) || is.null(tv) || nrow(tv) == 0) {
        return(plotly::plotly_empty())
      }

      pal_mol <- age_colors()
      pal_ens <- age_colors_ensemble()
      col_vec <- ifelse(
        tv$Mode == "molécule",
        ifelse(tv$Type == "groupe", unname(pal_mol["groupe"]), unname(pal_mol["detail"])),
        ifelse(tv$Type == "groupe", unname(pal_ens["groupe"]), unname(pal_ens["detail"]))
      )
      tv$Col <- col_vec

      tv$Libelle <- wrap_libelle(tv$Libelle)
      categoryarray <- rev(tv$Libelle)

      tv$TickLabel <- tv$Libelle
      tv$TickLabel[tv$Mode == "ensemble des cas"] <- ""

      groups <- list(
        list(Mode = "molécule",         Type = "groupe", Nom = "Molécule — groupe"),
        list(Mode = "molécule",         Type = "detail", Nom = "Molécule — détail"),
        list(Mode = "ensemble des cas", Type = "groupe", Nom = "Ensemble des cas — groupe"),
        list(Mode = "ensemble des cas", Type = "detail", Nom = "Ensemble des cas — détail")
      )

      p <- plotly::plot_ly()
      for (g in groups) {
        sub <- tv[tv$Mode == g$Mode & tv$Type == g$Type, ]
        if (nrow(sub) == 0) {
          next
        }
        p <- p %>%
          plotly::add_trace(
            type = "bar",
            orientation = "h",
            x = sub$Pct,
            y = sub$Libelle,
            text = paste0(round(sub$Pct, 1), " %"),
            textposition = "auto",
            cliponaxis = FALSE,
            marker = list(color = sub$Col),
            customdata = lapply(seq_len(nrow(sub)), function(i) {
              c(sub$Libelle_brut[i], sub$Effectif[i], round(sub$Pct[i], 1))
            }),
            insidetextfont = list(color = "#ffffff"),
            hovertemplate = paste0(
              "%{customdata[0]}<br>Effectif : %{customdata[1]}",
              "<br>Pourcentage : %{customdata[2]} %<extra></extra>"
            ),
            name = g$Nom,
            showlegend = TRUE
          )
      }

      titre <- wrap_titre(paste("Répartition par type d'obtention —", selected_lib(), "·", code))
      p %>%
        plotly::layout(
          title = titre,
          barmode = "overlay",
          xaxis = list(
            title = "Pourcentage (%)",
            range = c(0, 105),
            ticksuffix = "%"
          ),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = categoryarray,
            type = "category",
            automargin = TRUE,
            tickfont = list(size = 11),
            tickmode = "array",
            tickvals = categoryarray,
            ticktext = rev(tv$TickLabel)
          ),
          margin = list(l = 20, r = 20, t = titre_margin_top(titre), b = 20),
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.12,
            font = list(size = 11)
          )
        )
    })

    # --- Barres horizontales du type d'ordonnance (col. I) --------------------
    # Comme le graph Origine : 2 traces en niveaux « detail » (molécule /
    # ensemble des cas), couleurs bordeaux / orange.
    output$plot_type_ordonnance <- render_plotly_theme(theme, {
      code <- selected_code()
      tv <- type_ordonnance_values()
      if (is.null(code) || is.null(tv) || nrow(tv) == 0) {
        return(plotly::plotly_empty())
      }

      pal_mol <- age_colors()
      pal_ens <- age_colors_ensemble()
      col_vec <- ifelse(
        tv$Mode == "molécule",
        unname(pal_mol["detail"]),
        unname(pal_ens["detail"])
      )
      tv$Col <- col_vec

      tv$Libelle <- wrap_libelle(tv$Libelle)
      categoryarray <- rev(tv$Libelle)

      tv$TickLabel <- tv$Libelle
      tv$TickLabel[tv$Mode == "ensemble des cas"] <- ""

      groups <- list(
        list(Mode = "molécule",         Nom = "Molécule"),
        list(Mode = "ensemble des cas", Nom = "Ensemble des cas")
      )

      p <- plotly::plot_ly()
      for (g in groups) {
        sub <- tv[tv$Mode == g$Mode, ]
        if (nrow(sub) == 0) {
          next
        }
        p <- p %>%
          plotly::add_trace(
            type = "bar",
            orientation = "h",
            x = sub$Pct,
            y = sub$Libelle,
            text = paste0(round(sub$Pct, 1), " %"),
            textposition = "auto",
            cliponaxis = FALSE,
            marker = list(color = sub$Col),
            customdata = lapply(seq_len(nrow(sub)), function(i) {
              c(sub$Libelle_brut[i], sub$Effectif[i], round(sub$Pct[i], 1))
            }),
            insidetextfont = list(color = "#ffffff"),
            hovertemplate = paste0(
              "%{customdata[0]}<br>Effectif : %{customdata[1]}",
              "<br>Pourcentage : %{customdata[2]} %<extra></extra>"
            ),
            name = g$Nom,
            showlegend = TRUE
          )
      }

      titre <- wrap_titre(paste("Répartition par type d'ordonnance —", selected_lib(), "·", code))
      p %>%
        plotly::layout(
          title = titre,
          barmode = "overlay",
          xaxis = list(
            title = "Pourcentage (%)",
            range = c(0, 105),
            ticksuffix = "%"
          ),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = categoryarray,
            type = "category",
            automargin = TRUE,
            tickfont = list(size = 11),
            tickmode = "array",
            tickvals = categoryarray,
            ticktext = rev(tv$TickLabel)
          ),
          margin = list(l = 20, r = 20, t = titre_margin_top(titre), b = 20),
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.12,
            font = list(size = 11)
          )
        )
    })


    # --- Barres horizontales de la nature du prescripteur (col. J) ------------
    # Comme le graph Origine : 2 traces en niveaux « detail » (molécule /
    # ensemble des cas), couleurs bordeaux / orange.
    output$plot_nature_prescripteur <- render_plotly_theme(theme, {
      code <- selected_code()
      tv <- nature_prescripteur_values()
      if (is.null(code) || is.null(tv) || nrow(tv) == 0) {
        return(plotly::plotly_empty())
      }

      pal_mol <- age_colors()
      pal_ens <- age_colors_ensemble()
      col_vec <- ifelse(
        tv$Mode == "molécule",
        unname(pal_mol["detail"]),
        unname(pal_ens["detail"])
      )
      tv$Col <- col_vec

      tv$Libelle <- wrap_libelle(tv$Libelle)
      categoryarray <- rev(tv$Libelle)

      tv$TickLabel <- tv$Libelle
      tv$TickLabel[tv$Mode == "ensemble des cas"] <- ""

      groups <- list(
        list(Mode = "molécule",         Nom = "Molécule"),
        list(Mode = "ensemble des cas", Nom = "Ensemble des cas")
      )

      p <- plotly::plot_ly()
      for (g in groups) {
        sub <- tv[tv$Mode == g$Mode, ]
        if (nrow(sub) == 0) {
          next
        }
        p <- p %>%
          plotly::add_trace(
            type = "bar",
            orientation = "h",
            x = sub$Pct,
            y = sub$Libelle,
            text = paste0(round(sub$Pct, 1), " %"),
            textposition = "auto",
            cliponaxis = FALSE,
            marker = list(color = sub$Col),
            customdata = lapply(seq_len(nrow(sub)), function(i) {
              c(sub$Libelle_brut[i], sub$Effectif[i], round(sub$Pct[i], 1))
            }),
            insidetextfont = list(color = "#ffffff"),
            hovertemplate = paste0(
              "%{customdata[0]}<br>Effectif : %{customdata[1]}",
              "<br>Pourcentage : %{customdata[2]} %<extra></extra>"
            ),
            name = g$Nom,
            showlegend = TRUE
          )
      }

      titre <- wrap_titre(paste("Répartition par nature du prescripteur —", selected_lib(), "·", code))
      p %>%
        plotly::layout(
          title = titre,
          barmode = "overlay",
          xaxis = list(
            title = "Pourcentage (%)",
            range = c(0, 105),
            ticksuffix = "%"
          ),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = categoryarray,
            type = "category",
            automargin = TRUE,
            tickfont = list(size = 11),
            tickmode = "array",
            tickvals = categoryarray,
            ticktext = rev(tv$TickLabel)
          ),
          margin = list(l = 20, r = 20, t = titre_margin_top(titre), b = 20),
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.12,
            font = list(size = 11)
          )
        )
    })


    # --- Valeurs exposées (pour d'éventuels affichages extérieurs) ------------
    list(
      atc_code    = selected_code,
      atc_libelle = selected_lib,
      atc_list    = atc_df,
      facteur_values        = facteur_values,
      facteur_plot_data     = facteur_plot_data,
      facteur_height        = facteur_height,
      type_obtention_values = type_obtention_values,
      type_ordonnance_values = type_ordonnance_values,
      nature_prescripteur_values = nature_prescripteur_values
    )
  })
}
