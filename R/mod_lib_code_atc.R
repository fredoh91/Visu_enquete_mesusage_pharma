# ============================================================================
# mod_lib_code_atc.R — Module Shiny « Libellés / codes ATC »
# Affiche un tableau des libellés ATC (Paracétamol, Amoxicilline, ...) avec
# leur code ATC (N02BE01, J01CA04, ...) issus de
# LIB_CODE_ATC_OXOMEMAZINE.xlsx, puis, au clic sur une ligne, plusieurs
# graphiques interactifs (plotly) pour le code ATC sélectionné. Cet écran est
# largement analogue à celui du module MEDOC_REG (même organisation de l'écran,
# mêmes cinq graphiques).
#
# Les données proviennent de data/LIB_CODE_ATC_OXOMEMAZINE.xlsx via le helper
# read_lib_code_atc() de R/read_data.R.
#
# NB IMPORTANT : la structure de ce fichier est analogue à celle de
# MEDOC_REG.xlsx, à ceci près qu'une colonne « Lib ATC » est insérée entre le
# code (colonne B) et le type de donnée (colonne D). Toutes les colonnes de
# données sont donc DÉCALÉES D'UN CRAN par rapport au fichier MEDOC_REG :
#   - genre   : colonnes 6, 7, 8   (au lieu de 5, 6, 7)
#   - enceinte: colonnes 9, 10, 11  (au lieu de 8, 9, 10)
#   - âge     : colonnes 12, 14-21  (au lieu de 11, 13-20)
#   - origine : colonnes 22, 23, 24 (au lieu de 21, 22, 23)
#   - type    : colonnes 41 à 51    (au lieu de 40 à 50)

library(shiny)
library(dplyr)
library(DT)

# --- Helper : libellé ATC lisible sur le titre d'un graphique ----------------
#' Formate le texte d'un titre de graphique avec un libellé ATC éventuellement
#' long.
#'
#' Si le libellé ATC est trop long pour tenir sur une seule ligne (longueur >
#' `max_car`), il est coupé sur DEUX lignes au niveau d'un espace et les deux
#' fragments sont reliés par un retour à la ligne (\n, compris par Plotly).
#'
#' @param prefixe Texte de préfixe du titre (ex. "Répartition par genre —").
#' @param lib Libellé ATC (ex. "Paracétamol en association sauf aux psycholeptiques").
#' @param code Code ATC (ex. "N02BE51").
#' @param max_car Longueur maximale de la première ligne du libellé.
#' @return Une chaîne de caractères prête pour l'argument `title` de Plotly.
atc_titre <- function(prefixe, lib, code, max_car = 28L) {
  lib <- as.character(lib)
  lib <- gsub("\\s+", " ", trimws(lib))  # normalise les espaces
  if (nchar(lib) > max_car) {
    # Découpe au niveau de l'espace le plus proche du seuil.
    pos <- unlist(gregexpr(" ", lib, fixed = TRUE))  # positions des espaces
    pos <- pos[pos > 0 & pos <= max_car]
    if (length(pos) >= 1) {
      cut <- pos[length(pos)] - 1L    # dernier espace avant le seuil
      lib <- paste0(substr(lib, 1L, cut),
                    "\n",
                    substr(lib, cut + 2L, nchar(lib)))
    }
  }
  paste0(prefixe, " ", lib, " (", code, ")")
}

# --- UI du module -------------------------------------------------------------
#' UI du module Libellés / codes ATC.
#'
#' L'écran reprend l'organisation en deux grandes zones du module MEDOC_REG :
#'   * Zone supérieure (.zone-tableau)  : le tableau des codes ATC.
#'   * Zone inférieure (.zone-graphiques) : les graphiques relatifs au code ATC
#'     sélectionné, chacun dans sa carte (.graph-card) disposée de façon
#'     responsive via la grille Bootstrap 5 (col-12 / col-md-6 / col-xl-4).
#'
#' @param id Identifiant unique du module.
mod_lib_code_atc_ui <- function(id) {
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
        # Comme dans l'onglet MEDOC_REG, ce graphique exploite le fichier Excel
        # distinct principaux_facteurs.xlsx, chargé en parallèle de
        # LIB_CODE_ATC_OXOMEMAZINE.xlsx. La liaison se fait par le code ATC
        # (en-têtes de ligne 2 des colonnes EX -> FI = indices 154:165).
        # Le grand nombre de facteurs (29) justifie un dépliage : par défaut on
        # n'affiche que les 15 premiers pourcentages, une case à cocher permet
        # d'afficher l'ensemble des facteurs.
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            shiny::checkboxInput(
              ns("facteur_afficher_tous"),
              label = "Afficher les 29 facteurs (déplier)",
              value = FALSE
            ),
            # La hauteur est pilotée côté serveur (renderUI + facteur_height()) :
            # compacte pour la vue "15 premiers", agrandie après dépliage.
            shiny::uiOutput(ns("plot_facteur_ui"))
          )
        ),

        # Carte 7 : barres horizontales du type de prise (2 niveaux hiérarchiques).
        # Les données proviennent des colonnes Y -> AE de
        # LIB_CODE_ATC_OXOMEMAZINE.xlsx et se répartissent sur 2 niveaux
        # (comme les graphiques âge et type).
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_type_prise"), height = "640px")
          )
        )
      )
    )
  )
}



# --- Server du module ---------------------------------------------------------
#' Server du module Libellés / codes ATC.
#' @param id Identifiant unique du module (doit correspondre à l'UI).
mod_lib_code_atc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Données brutes importées depuis le fichier Excel -------------------
    # La dataframe LIB_CODE_ATC est chargée une seule fois au démarrage du module.
    lib_code_atc <- reactive({
      tryCatch(
        read_lib_code_atc(),
        error = function(e) NULL
      )
    })

    # --- Données brutes des facteurs (principaux_facteurs.xlsx) -------------
    # Chargé au même moment que LIB_CODE_ATC_OXOMEMAZINE.xlsx pour alimenter le
    # 6e graphique de l'onglet (barres horizontales des facteurs de mésusage).
    # Fichier transposé (col_names = FALSE) : chaque ligne = un facteur ; chaque
    # code ATC occupe une colonne dédiée (EX..FI = indices 154:165) dont l'en-tête
    # (ligne 2) porte le libellé au format "CODE (Libellé)".
    principaux_facteurs <- reactive({
      tryCatch(
        read_principaux_facteurs(),
        error = function(e) NULL
      )
    })

    # --- Dataframe des codes ATC (affichage basique) -------------------------
    # Ne conserve que les lignes du type "effectif" et exclut les lignes
    # "Total" (périmètre global) ainsi que la ligne agrégée "Autres codes ATC
    # (inférieurs à 30)" qui ne correspond pas à un code ATC précis. Chaque
    # libellé ATC est présenté avec son code ATC et son effectif total,
    # trié par effectif décroissant.
    atc_df <- reactive({
      data <- lib_code_atc()
      req(data)
      data %>%
        dplyr::filter(.data[["Type_donnee"]] == "effectif") %>%
        dplyr::filter(!.data[["Code ATC"]] %in% c(
          "Total",
          "Autres codes ATC (inférieurs à 30)"
        )) %>%
        dplyr::select(`Lib ATC`, `Code ATC`, Total) %>%
        dplyr::arrange(dplyr::desc(.data$Total))
    })

    # --- Code ATC sélectionné dans le tableau --------------------------------
    # input$table_atc_rows_selected : index de l'unique ligne sélectionnée
    # (NULL tant qu'aucune ligne n'est cliquée, car DT est en mode "single").
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

    # --- Ligne "effectif" du code ATC sélectionné ----------------------------
    # Permet de récupérer les effectifs du genre (colonnes Homme / Femme / Autre).
    selected_genre <- reactive({
      code <- selected_code()
      data <- lib_code_atc()
      req(code, data)

      row <- data %>%
        dplyr::filter(.data[["Code ATC"]] == code,
                      .data[["Type_donnee"]] == "effectif")

      if (nrow(row) != 1) {
        return(NULL)
      }
      row[1, ]
    })

    # --- Données du camembert du genre des patients ----------------------------
    # Construit la dataframe nécessaire au camembert 1 : uniquement la
    # répartition du genre (Homme / Femme / Autre). Les colonnes sont lues par
    # index (robustesse face aux libellés avec espaces insécables) : genre =
    # 6 (Homme), 7 (Femme), 8 (Autre) — décalé d'un cran par rapport à MEDOC_REG.
    # Les éventuelles NA sont traitées comme 0 et les segments d'effectif nul
    # sont écartés. Le pourcentage est recalculé sur le total Homme + Femme + Autre.
    genre_values <- reactive({
      row <- selected_genre()
      req(row)

      h <- suppressWarnings(as.numeric(row[[6]]))
      f <- suppressWarnings(as.numeric(row[[7]]))
      a <- suppressWarnings(as.numeric(row[[8]]))
      v <- c(h, f, a)
      v[is.na(v)] <- 0
      h <- v[1]; f <- v[2]; a <- v[3]

      total <- h + f + a
      if (is.na(total) || total <= 0) {
        return(NULL)
      }

      df <- data.frame(
        Label  = c("Homme", "Femme", "Autre"),
        Effectif = c(h, f, a),
        stringsAsFactors = FALSE
      )
      # Pourcentage sur le total (Homme + Femme + Autre), formaté avec la
      # virgule décimale française.
      df$Pct <- df$Effectif / total * 100
      df$Pct_fr <- formatC(df$Pct, format = "f", digits = 1,
                           big.mark = " ", decimal.mark = ",")

      # On écarte les segments d'effectif nul (pas de secteur vide).
      df[df$Effectif > 0, ]
    })

    # --- Données du camembert des données "enceinte" ---------------------------
    # Construit la dataframe nécessaire au camembert 2 : uniquement la
    # répartition "enceinte" (Oui / Non / Non renseigné), en nuances de vert.
    # Les colonnes sont lues par index : enceinte = 9 (Non), 10 (Oui),
    # 11 (Non renseigné) — décalé d'un cran par rapport à MEDOC_REG.
    #
    # Le pourcentage est recalculé sur le TOTAL des données "enceinte"
    # (Non + Oui + Non renseigné), conformément aux directives.
    enceinte_values <- reactive({
      row <- selected_genre()
      req(row)

      non <- suppressWarnings(as.numeric(row[[9]]))
      oui <- suppressWarnings(as.numeric(row[[10]]))
      nr  <- suppressWarnings(as.numeric(row[[11]]))
      v <- c(non, oui, nr)
      v[is.na(v)] <- 0
      non <- v[1]; oui <- v[2]; nr <- v[3]

      total <- non + oui + nr
      if (is.na(total) || total <= 0) {
        return(NULL)
      }

      df <- data.frame(
        Label  = c("Non", "Oui", "Non renseigné"),
        Effectif = c(non, oui, nr),
        stringsAsFactors = FALSE
      )
      # Pourcentage sur le total "enceinte" (Non + Oui + Non renseigné), formaté
      # avec la virgule décimale française.
      df$Pct <- df$Effectif / total * 100
      df$Pct_fr <- formatC(df$Pct, format = "f", digits = 1,
                           big.mark = " ", decimal.mark = ",")

      # On écarte les segments d'effectif nul (pas de secteur vide).
      df[df$Effectif > 0, ]
    })


    # --- Données des barres d'âge (données S2 — âge du patient) --------------
    # Lecture depuis la ligne "effectif" du code ATC sélectionné. Les colonnes
    # concernées reprennent la même hiérarchie que MEDOC_REG, décalées d'un cran
    # (colonne « Lib ATC » insérée) : on accède par index numérique car certains
    # libellés contiennent des espaces insécables. La colonne "ST JEUNES,
    # ENFANTS" (qui chevauche enfants et adolescents) est volontairement ignorée.
    #
    # Chaque libellé est représenté par DEUX barres, correspondant à deux modes
    # de calcul des pourcentages :
    #   * mode "molécule"        : pourcentage = effectif / Total molécule * 100,
    #     où le Total (colonne E, index 5) provient de la ligne "effectif" du
    #     code ATC sélectionné ;
    #   * mode "ensemble des cas" : pourcentage = effectif / Total enquête * 100,
    #     où le Total (colonne E) provient de la ligne agrégée "Total" du
    #     périmètre (l'ensemble des cas de l'enquête).
    #
    # Les deux barres d'un même libellé sont affichées l'une juste en dessous de
    # l'autre, avec des couleurs différentes (voir plot_age ci-dessous).
    age_values <- reactive({
      code <- selected_code()
      req(code)
      data <- lib_code_atc()
      req(data)

      row <- data %>%
        dplyr::filter(.data[["Code ATC"]] == code,
                      .data[["Type_donnee"]] == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      # Total relatif au code ATC sélectionné (colonne E de la ligne "effectif").
      total_mol <- suppressWarnings(as.numeric(row[[5]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      # Total de l'ensemble des cas (colonne E de la ligne agrégée "Total" du
      # périmètre). En cas d'absence, on retombe sur le total du code ATC (les
      # deux pourcentages seraient alors égaux).
      row_total <- data %>%
        dplyr::filter(.data[["Code ATC"]] == "Total",
                      .data[["Type_donnee"]] == "effectif")
      total_ens <- if (nrow(row_total) >= 1) {
        suppressWarnings(as.numeric(row_total[[5]][1]))
      } else {
        NA
      }
      if (is.na(total_ens) || total_ens <= 0) {
        total_ens <- total_mol
      }

      # Définition hiérarchique : (libellé affiché, index colonne, niveau)
      # Ordre d'affichage de haut en bas. Les indices sont ceux de
      # LIB_CODE_ATC_OXOMEMAZINE (décalés d'un cran vs MEDOC_REG) :
      #   ST ENFANTS, ADOLESCENTS = 12 ; Nouveau né = 14 ; 2-11 ans = 15 ;
      #   12-17 ans = 16 ; ST ADULTES = 17 ; 18-34 = 18 ; 35-49 = 19 ;
      #   50-64 = 20 ; 65 ans et plus = 21.
      # Les libellés de niveau 1 (groupes) sont affichés "Total ENFANTS et
      # ADOLESCENTS" / "Total ADULTES", en gras ; ceux de niveau 2 (tranches
      # d'âge) ne sont pas en gras.
      defs <- data.frame(
        Libelle_brut = c(
          "Total ENFANTS et ADOLESCENTS",         # groupe (niveau 1)
          "Nouveau né ou nourrisson (0-23 mois)",
          "Entre 2 et 11 ans (enfant)",
          "Entre 12 et 17 ans (adolescent)",
          "Total ADULTES",                        # groupe (niveau 1)
          "Entre 18 et 34 ans",
          "Entre 35 et 49 ans",
          "Entre 50 et 64 ans",
          "65 ans et plus"
        ),
        Index = c(12L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L),
        Niveau = c(1L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L),
        stringsAsFactors = FALSE
      )

      eff <- vapply(defs$Index, function(i) suppressWarnings(as.numeric(row[[i]])), numeric(1))
      eff[is.na(eff)] <- 0

      Type  <- ifelse(defs$Niveau == 1L, "groupe", "detail")
      # Libellé AFFICHÉ sur l'axe Y :
      #   * niveau 1 (groupes) : libellé entouré de <b>...</b> (gras, reconnu par
      #     plotly.js) sans indentation ;
      #   * niveau 2 (détails) : indentation de 4 espaces pour matérialiser le
      #     "léger décalage vers la droite" de la hiérarchie, sans gras (pas de
      #     balise HTML afin de conserver le rendu des espaces).
      Libelle_aff <- ifelse(
        defs$Niveau == 1L,
        paste0("<b>", defs$Libelle_brut, "</b>"),
        paste0("    ", defs$Libelle_brut)
      )

      # Deux rangées par libellé, dans l'ordre d'affichage (haut → bas) :
      # d'abord la barre "molécule", puis juste en dessous celle "ensemble des
      # cas". Le caractère invisible (espace de largeur nulle \u200B) ajouté au
      # libellé de la seconde barre permet à Plotly de distinguer deux
      # catégories d'axe Y visuellement identiques.
      # Libelle_brut reste le libellé "propre" (sans gras ni indentation), utilisé
      # pour l'infobulle.
      out <- do.call(rbind, lapply(seq_along(Libelle_aff), function(k) {
        rbind(
          data.frame(
            Libelle      = Libelle_aff[k],
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "molécule",
            Type         = Type[k],
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / total_mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(Libelle_aff[k], "\u200B"),
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "ensemble des cas",
            Type         = Type[k],
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / total_ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })


    # --- Données des barres horizontales de l'origine du mésusage --------------
    # Lecture depuis la ligne "effectif" du code ATC sélectionné. Les colonnes
    # concernées vont de 22 à 24 (décalées d'un cran vs MEDOC_REG) :
    #   - Au moment de la prise du médicament
    #   - Au moment de la prescription médicale
    #   - Au moment de la dispensation en pharmacie
    # On accède par index numérique car certains libellés contiennent des
    # espaces insécables.
    #
    # Comme pour le graphique des âges et du type, DEUX modes de calcul des
    # pourcentages sont produits pour chaque libellé :
    #   * mode "molécule"         : dénominateur = effectif total du code ATC
    #     (colonne E, index 5, de la ligne "effectif" du code sélectionné) ;
    #   * mode "ensemble des cas" : dénominateur = effectif total de l'ensemble
    #     de l'enquête (colonne E de la ligne agrégée "Total").
    # Cela génère donc 2 rangées par modalité d'origine (toutes de niveau
    # "detail", sans hiérarchie), dans le même format que age_values().
    origine_values <- reactive({
      code <- selected_code()
      req(code)
      data <- lib_code_atc()
      req(data)

      row <- data %>%
        dplyr::filter(.data[["Code ATC"]] == code,
                      .data[["Type_donnee"]] == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      # Total relatif au code ATC sélectionné (colonne E).
      total_mol <- suppressWarnings(as.numeric(row[[5]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      # Total de l'ensemble des cas (colonne E de la ligne "Total").
      row_total <- data %>%
        dplyr::filter(.data[["Code ATC"]] == "Total",
                      .data[["Type_donnee"]] == "effectif")
      total_ens <- if (nrow(row_total) >= 1) {
        suppressWarnings(as.numeric(row_total[[5]][1]))
      } else {
        NA
      }
      if (is.na(total_ens) || total_ens <= 0) {
        total_ens <- total_mol
      }

      eff <- vapply(22:24, function(i) suppressWarnings(as.numeric(row[[i]])), numeric(1))
      eff[is.na(eff)] <- 0

      Libelle_brut <- c(
        "Au moment de la prise du médicament",
        "Au moment de la prescription médicale",
        "Au moment de la dispensation en pharmacie"
      )
      # Toutes les modalités d'origine sont de niveau "detail" (pas de groupe).
      Type <- rep("detail", length(Libelle_brut))

      # Deux rangées par modalité (molécule puis ensemble des cas), avec le
      # caractère invisible \u200B pour distinguer les catégories d'axe Y.
      out <- do.call(rbind, lapply(seq_along(Libelle_brut), function(k) {
        rbind(
          data.frame(
            Libelle      = Libelle_brut[k],
            Libelle_brut = Libelle_brut[k],
            Mode         = "molécule",
            Type         = Type[k],
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / total_mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(Libelle_brut[k], "\u200B"),
            Libelle_brut = Libelle_brut[k],
            Mode         = "ensemble des cas",
            Type         = Type[k],
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / total_ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Données des barres horizontales du type de mésusage -------------------
    # Lecture depuis la ligne "effectif" du code ATC sélectionné. Les colonnes
    # concernées vont de 41 à 51 (décalées d'un cran vs MEDOC_REG) et se
    # répartissent sur 2 niveaux hiérarchiques (comme le graphique des âges) :
    #   Niveau 1 « Posologie, Fréquence, Durée de traitement » (colonne 41) :
    #     - Schéma posologique non conforme        (colonne 42)
    #     - Arrêt prématuré et injustifié du traitement (colonne 43)
    #     - Prolongation de la durée du traitement (colonne 44)
    #     - Voie d'administration non conforme     (colonne 45)
    #   Niveau 1 « Indication, population, contre-indications » (colonne 46) :
    #     - Utilisation pour une indication hors AMM          (colonne 47)
    #     - Utilisation par une population non prévue         (colonne 48)
    #     - Utilisation en présence de contre-indications     (colonne 49)
    #     - Utilisation en présence d'une interaction interdite (colonne 50)
    #     - Autre                                             (colonne 51)
    #
    # Comme pour le graphique des âges, le pourcentage est RECALCULÉ sur DEUX
    # dénominateurs distincts :
    #   * mode "molécule" : le Total (colonne E, index 5) de la ligne "effectif"
    #     du code ATC sélectionné ;
    #   * mode "ensemble des cas" : le Total (colonne E) de la ligne agrégée
    #     "Total" de l'ensemble de l'enquête.
    type_values <- reactive({
      code <- selected_code()
      req(code)
      data <- lib_code_atc()
      req(data)

      row <- data %>%
        dplyr::filter(.data[["Code ATC"]] == code,
                      .data[["Type_donnee"]] == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      total_mol <- suppressWarnings(as.numeric(row[[5]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }
      row_total <- data %>%
        dplyr::filter(.data[["Code ATC"]] == "Total",
                      .data[["Type_donnee"]] == "effectif")
      total_ens <- if (nrow(row_total) >= 1) {
        suppressWarnings(as.numeric(row_total[[5]][1]))
      } else {
        NA
      }
      if (is.na(total_ens) || total_ens <= 0) {
        total_ens <- total_mol
      }

      # Définition hiérarchique : (libellé affiché, index colonne, niveau)
      # Ordre d'affichage de haut en bas.
      # Conformément aux directives, les libellés affichés sont ceux précisés
      # entre parenthèses "(Afficher : ...)" : les groupes de niveau 1 sont
      # affichés "Total Posologie, Fréquence, durée" / "Total Indication,
      # population, CI" (en gras), les libellés de niveau 2 ne sont pas en gras
      # (l'arrêt prématuré et injustifié du traitement est affiché
      # « Arrêt prématuré »).
      defs <- data.frame(
        Libelle_brut = c(
          "Total Posologie, Fréquence, durée",        # groupe (niveau 1)
          "Schéma posologique non conforme",
          "Arrêt prématuré",
          "Prolongation durée TT",
          "Voie d'administration non conforme",
          "Total Indication, population, CI",         # groupe (niveau 1)
          "Indication hors AMM",
          "Population non prévue",
          "Contre-indications",
          "Interaction médicamenteuse",
          "Autre"
        ),
        Index = c(41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L),
        Niveau = c(1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L),
        stringsAsFactors = FALSE
      )

      eff <- vapply(defs$Index, function(i) suppressWarnings(as.numeric(row[[i]])), numeric(1))
      eff[is.na(eff)] <- 0

      Type  <- ifelse(defs$Niveau == 1L, "groupe", "detail")
      # Libellé AFFICHÉ sur l'axe Y :
      #   * niveau 1 (groupes) : libellé entouré de <b>...</b> (gras, reconnu par
      #     plotly.js) sans indentation ;
      #   * niveau 2 (détails) : indentation de 4 espaces pour matérialiser le
      #     "léger décalage vers la droite" de la hiérarchie, sans gras (pas de
      #     balise HTML afin de conserver le rendu des espaces).
      Libelle_aff <- ifelse(
        defs$Niveau == 1L,
        paste0("<b>", defs$Libelle_brut, "</b>"),
        paste0("    ", defs$Libelle_brut)
      )

      # Deux rangées par libellé, dans l'ordre d'affichage (haut → bas) :
      # d'abord la barre "molécule", puis juste en dessous celle "ensemble des
      # cas". Le caractère invisible (\u200B) ajouté au libellé de la seconde
      # barre permet à Plotly de distinguer deux catégories d'axe Y
      # visuellement identiques.
      out <- do.call(rbind, lapply(seq_along(Libelle_aff), function(k) {
        rbind(
          data.frame(
            Libelle      = Libelle_aff[k],
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "molécule",
            Type         = Type[k],
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / total_mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(Libelle_aff[k], "\u200B"),
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "ensemble des cas",
            Type         = Type[k],
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / total_ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Données des barres horizontales du type de prise -----------------------
    # Lecture depuis la ligne "effectif" du code ATC sélectionné. Les colonnes
    # concernées vont de Y à AE (index 25 à 31) et se répartissent sur 2 niveaux
    # hiérarchiques (comme les graphiques des âges et du type) :
    #   Niveau 1 « Médicament avec ordonnance » (colonne Y=25) :
    #     - D'une primo-prescription                    (Z=26)
    #     - D'un renouvellement d'ordonnance            (AA=27)
    #   Niveau 1 « Médicament sans ordonnance » (colonne AB=28) :
    #     - D'un médicament sans ordonnance en automédication         (AC=29)
    #     - D'un médicament sans ordonnance sur conseil du pharmacien (AD=30)
    #     - Je ne sais pas                                             (AE=31)
    #
    # Comme les graphiques précédents, le pourcentage est RECALCULÉ sur DEUX
    # dénominateurs distincts : mode "molécule" (total du code ATC sélectionné)
    # et mode "ensemble des cas" (total de la ligne agrégée "Total"). L'affichage
    # est géré en aval par le render plot_type_prise.
    type_prise_values <- reactive({
      code <- selected_code()
      req(code)
      data <- lib_code_atc()
      req(data)

      row <- data %>%
        dplyr::filter(.data[["Code ATC"]] == code,
                      .data[["Type_donnee"]] == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      total_mol <- suppressWarnings(as.numeric(row[[5]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }
      row_total <- data %>%
        dplyr::filter(.data[["Code ATC"]] == "Total",
                      .data[["Type_donnee"]] == "effectif")
      total_ens <- if (nrow(row_total) >= 1) {
        suppressWarnings(as.numeric(row_total[[5]][1]))
      } else {
        NA
      }
      if (is.na(total_ens) || total_ens <= 0) {
        total_ens <- total_mol
      }

      # Définition hiérarchique : (libellé affiché, index colonne, niveau).
      # Ordre d'affichage de haut en bas. Les groupes de niveau 1 sont affichés
      # « Prise avec ordonnance » / « Prise sans ordonnance » (en gras), les
      # libellés de niveau 2 ne sont pas en gras.
      defs <- data.frame(
        Libelle_brut = c(
          "Prise avec ordonnance",       # groupe (niveau 1)
          "Primo prescription",
          "Renouvellement",
          "Prise sans ordonnance",       # groupe (niveau 1)
          "Sans ordo en automédication",
          "Sur conseil du pharmacien",
          "Je ne sais pas"
        ),
        Index = c(25L, 26L, 27L, 28L, 29L, 30L, 31L),
        Niveau = c(1L, 2L, 2L, 1L, 2L, 2L, 2L),
        stringsAsFactors = FALSE
      )

      eff <- vapply(defs$Index, function(i) suppressWarnings(as.numeric(row[[i]])), numeric(1))
      eff[is.na(eff)] <- 0

      Type  <- ifelse(defs$Niveau == 1L, "groupe", "detail")
      # Libellé AFFICHÉ sur l'axe Y : gras (<b>...</b>) pour les groupes de
      # niveau 1, indentation de 4 espaces pour les détails de niveau 2 (pas de
      # balise HTML afin de préserver le rendu des espaces).
      Libelle_aff <- ifelse(
        defs$Niveau == 1L,
        paste0("<b>", defs$Libelle_brut, "</b>"),
        paste0("    ", defs$Libelle_brut)
      )

      # Deux rangées par libellé (molécule puis ensemble des cas). Le caractère
      # invisible (\u200B) distingue les catégories d'axe Y identiques.
      out <- do.call(rbind, lapply(seq_along(Libelle_aff), function(k) {
        rbind(
          data.frame(
            Libelle      = Libelle_aff[k],
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "molécule",
            Type         = Type[k],
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / total_mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(Libelle_aff[k], "\u200B"),
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "ensemble des cas",
            Type         = Type[k],
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / total_ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Données des barres horizontales des facteurs de mésusage --------------
    # Comme dans l'onglet MEDOC_REG, DEUX barres par facteur : bordeaux = « par
    # rapport au code ATC sélectionné », orange = « par rapport à l'ensemble des
    # cas ». Données issues de principaux_facteurs.xlsx (lignes "effectif"),
    # colonnes EX..FI (indices 154:165) dont l'en-tête de ligne 2 est au format
    # "CODE (Libellé)" : la sélection se fait par le code ATC.
    facteur_values <- reactive({
      code <- selected_code()
      data <- lib_code_atc()
      pf <- principaux_facteurs()
      req(code, data, pf)

      # Total du code ATC sélectionné (colonne E : Total).
      row <- data %>%
        dplyr::filter(.data[["Code ATC"]] == code,
                      .data[["Type_donnee"]] == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]
      total_mol <- suppressWarnings(as.numeric(row[[5]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      # Total de l'ensemble des cas (colonne E de la ligne "Total").
      row_total <- data %>%
        dplyr::filter(.data[["Code ATC"]] == "Total",
                      .data[["Type_donnee"]] == "effectif")
      total_ens <- if (nrow(row_total) >= 1) {
        suppressWarnings(as.numeric(row_total[[5]][1]))
      } else {
        NA
      }
      if (is.na(total_ens) || total_ens <= 0) {
        total_ens <- total_mol
      }

      # Colonnes EX..FI (154:165) : une par code ATC ; l'en-tête (ligne 2) y est
      # stocké au format "CODE (Libellé)". On n'en extrait que le code ATC (la
      # partie avant le premier espace) pour comparer à selected_code().
      dci_cols <- 154:165
      en_tetes <- vapply(dci_cols, function(i) {
        v <- pf[[i]][2]
        ifelse(is.na(v), "", as.character(v))
      }, character(1))
      codes_entetes <- sub(" .*$", "", en_tetes)
      hit <- which(trimws(codes_entetes) == trimws(as.character(code)))
      if (length(hit) == 0) {
        return(NULL)   # pas de colonne dédiée à ce code ATC
      }
      dci_col <- dci_cols[hit[1]]

      # Lignes "effectif" ; on écarte l'en-tête puis la ligne agrégée "Total".
      types <- pf[[3]]
      eff_lines <- which(types == "effectif")
      eff_lines <- eff_lines[eff_lines > 2]
      if (length(eff_lines) <= 1) {
        return(NULL)
      }
      eff_lines <- eff_lines[-1]

      # Libellés AFFICHÉS (courts) du cahier des charges, dans l'ordre du fichier.
      lib_aff <- c(
        "Manque d'info. ou com. interprofessionnelle",
        "Entourage",
        "A déjà reçu un méd. dans des cnd. sim.",
        "Survenue d'effets indésirables",
        "Médicament accessible sans ordonnance",
        "Double prescription ou chevauchement TT",
        "Défaut de transmission de l'information",
        "Forme ou voie mal adaptée",
        "Influence d'un discours promotionnel",
        "Rupture de stock entraînant un remp.",
        "Volonté du patient",
        "Manquements du médecin",
        "Volonté du médecin",
        "Accoutumance",
        "Douleurs persistantes",
        "Amélioration des symptômes",
        "Manque d'alternatives",
        "Manque d'observance patient",
        "Médecin trop permissif",
        "Pharmacie en ligne",
        "Médecin (sans précision)",
        "Objectif esthétique",
        "Environnement du patient",
        "Manque de médecins",
        "Manque de connaissance traitement",
        "Manquement du pharmacien",
        "Financier",
        "Autre",
        "Non renseigné"
      )
      n <- min(length(eff_lines), length(lib_aff))
      eff_lines <- eff_lines[seq_len(n)]
      lib_aff <- lib_aff[seq_len(n)]

      eff_dci <- vapply(eff_lines, function(i) {
        suppressWarnings(as.numeric(pf[[dci_col]][i]))
      }, numeric(1))
      eff_dci[is.na(eff_dci)] <- 0
      eff_global <- vapply(eff_lines, function(i) {
        suppressWarnings(as.numeric(pf[[4]][i]))
      }, numeric(1))
      eff_global[is.na(eff_global)] <- 0

      # Tri par pourcentage DÉCROISSANT (critère = barre "molécule").
      pct_mol <- ifelse(eff_dci > 0, eff_dci / total_mol * 100, 0)
      ordre <- order(pct_mol, decreasing = TRUE)
      eff_dci <- eff_dci[ordre]
      eff_global <- eff_global[ordre]
      lib_aff <- lib_aff[ordre]

      # Pas de hiérarchie : toutes les modalités de facteurs sont "detail".
      Type <- rep("detail", n)

      out <- do.call(rbind, lapply(seq_len(n), function(k) {
        rbind(
          data.frame(
            Libelle = lib_aff[k], Libelle_brut = lib_aff[k],
            Mode = "molécule", Type = Type[k], Effectif = eff_dci[k],
            Pct = if (eff_dci[k] > 0) eff_dci[k] / total_mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle = paste0(lib_aff[k], "\u200B"), Libelle_brut = lib_aff[k],
            Mode = "ensemble des cas", Type = Type[k], Effectif = eff_global[k],
            Pct = if (eff_global[k] > 0) eff_global[k] / total_ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Hauteur du graphique facteur (responsive au dépliage) ----------------
    # Vue "15 premiers facteurs" : 2 barres × 15 ≈ 30 barres → hauteur compacte.
    # Vue "tous" (29 facteurs) : 2 barres × 29 ≈ 58 barres → hauteur agrandie.
    # Hauteurs alignées sur celles de l'onglet MEDOC_REG (820 px plié / 1000 px
    # déplié) afin que les pourcentages restent parfaitement lisibles.
    facteur_height <- reactive({
      if (isTRUE(input$facteur_afficher_tous)) {
        "1000px"
      } else {
        "820px"
      }
    })
    # --- Jeu de données affiché (filtrage 15 premiers / tous) -----------------
    # Extrait de facteur_values() le sous-ensemble effectivement tracé : 30 lignes
    # (15 facteurs × 2 barres) par défaut, ou la totalité (58 = 29 × 2) quand la
    # case "Afficher les 29 facteurs" est cochée. facteur_values() étant déjà trié
    # par pourcentage décroissant, garder les premières lignes = garder les 15
    # plus grands pourcentages. Réactive séparée ⟹ testable via testServer et
    # réutilisée par le render plot_facteur.
    facteur_plot_data <- reactive({
      fv <- facteur_values()
      if (is.null(fv) || nrow(fv) == 0) {
        return(NULL)
      }
      n_lignes <- if (isTRUE(input$facteur_afficher_tous)) nrow(fv) else 30
      fv[seq_len(min(n_lignes, nrow(fv))), ]
    })

    # --- Message / état de l'import des données --------------------------------
    output$etat <- renderUI({
      data <- lib_code_atc()
      if (is.null(data)) {
        return(
          div(class = "alert alert-danger",
              icon("exclamation-triangle"),
              strong("Fichier LIB_CODE_ATC_OXOMEMAZINE introuvable ou illisible dans data/."))
        )
      }
      NULL
    })

    # --- Tableau des codes ATC --------------------------------------------------
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
                     "Effectif total" = "Total"),
        # Une seule ligne sélectionnable à la fois (mode "single").
        selection = "single",
        options = list(
          pageLength = 15,
          language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json")
        )
      )
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
    output$plot_genre <- plotly::renderPlotly({
      code <- selected_code()
      gv <- genre_values()
      if (is.null(code) || is.null(gv) || nrow(gv) == 0) {
        # Pas encore de camembert : on renvoie un graphique quasi vide.
        return(plotly::plotly_empty())
      }

      # Couleurs officielles du genre : reprises depuis R/colors.R (reflets des
      # variables CSS de custom.css), mappées sur les libellés (labels).
      pal_genre <- genres_colors()
      col_segments <- unname(pal_genre[gv$Label])

      # Étiquette sur chaque secteur : "Libellé\nEffectif (Pourcentage)"
      txt <- paste0(gv$Label, "<br>", gv$Effectif, " (", gv$Pct_fr, "%)")

      plotly::plot_ly(
        labels = gv$Label,
        values = gv$Effectif,
        type = "pie",
        # Pourcentage (sur Homme + Femme + Autre), transmis pour l'infobulle,
        # déjà formaté en français (virgule décimale).
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
      ) %>%
        plotly::layout(
          title = atc_titre("Répartition par genre —", selected_lib(), code),
          margin = list(l = 20, r = 20, t = 50, b = 20)
        )
    })

    # --- Camembert 2 : données "enceinte" (partie basse) ----------------------
    # Camembert simple : répartition Oui / Non / Non renseigné en nuances de
    # vert. Le pourcentage est recalculé sur l'effectif TOTAL des données
    # "enceinte" (Non + Oui + Non renseigné), conformément aux directives.
    # Chaque secteur affiche l'effectif et le pourcentage (entre parenthèses).
    output$plot_enceinte <- plotly::renderPlotly({
      code <- selected_code()
      ev <- enceinte_values()
      if (is.null(code) || is.null(ev) || nrow(ev) == 0) {
        # Pas encore de camembert : on renvoie un graphique quasi vide.
        return(plotly::plotly_empty())
      }

      # Nuances de vert (Oui / Non / Non renseigné) : reprises depuis R/colors.R
      # (helper enceinte_colors(), reflets des variables CSS de custom.css),
      # mappées sur les libellés (labels).
      pal_enceinte <- enceinte_colors()
      col_segments <- unname(pal_enceinte[ev$Label])

      # Étiquette sur chaque secteur : "Libellé\nEffectif (Pourcentage)"
      txt <- paste0(ev$Label, "<br>", ev$Effectif, " (", ev$Pct_fr, "%)")

      plotly::plot_ly(
        labels = ev$Label,
        values = ev$Effectif,
        type = "pie",
        # Pourcentage (sur Non + Oui + Non renseigné), transmis pour
        # l'infobulle, déjà formaté en français (virgule décimale).
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
      ) %>%
        plotly::layout(
          title = atc_titre("Répartition \"enceinte\" —", selected_lib(), code),
          margin = list(l = 20, r = 20, t = 50, b = 20)
        )
    })


    # --- Barres horizontales des âges (2 niveaux hiérarchiques) ---------------
    # Chaque libellé d'âge est représenté par DEUX barres superposées :
    #   * la première (bordeaux) exprime le pourcentage « par rapport à la
    #     molécule concernée » (dénominateur = total du code ATC sélectionné) ;
    #   * la seconde (orange), juste en dessous, exprime le pourcentage « par
    #     rapport à l'ensemble des cas » (dénominateur = total de l'enquête).
    # Le dénominateur est géré dans age_values() ; ici on ne fait qu'afficher.
    # Les quatre combinaisons (Type : groupe/detail) × (Mode : molécule/ensemble
    # des cas) forment quatre traces distinctes afin d'obtenir une légende
    # lisible.
    output$plot_age <- plotly::renderPlotly({
      code <- selected_code()
      av <- age_values()
      if (is.null(code) || is.null(av) || nrow(av) == 0) {
        return(plotly::plotly_empty())
      }

      # Couleurs officielles : bordeaux pour le mode "molécule", orange pour le
      # mode "ensemble des cas" (helpers de R/colors.R).
      pal_mol <- age_colors()          # c(groupe, detail) — bordeaux
      pal_ens <- age_colors_ensemble() # c(groupe, detail) — orange

      # Couleur attribuée à chaque barre selon (Type, Mode).
      col_vec <- ifelse(
        av$Mode == "molécule",
        ifelse(av$Type == "groupe", unname(pal_mol["groupe"]), unname(pal_mol["detail"])),
        ifelse(av$Type == "groupe", unname(pal_ens["groupe"]), unname(pal_ens["detail"]))
      )
      av$Col <- col_vec

      # Pour une orientation "h", plotly place la PREMIÈRE catégorie du
      # categoryarray en bas : on fournit donc l'ordre inverse de l'affichage
      # voulu pour que la hiérarchie se lise de haut en bas.
      categoryarray <- rev(av$Libelle)

      # Libellé d'axe Y UNIQUE par paire de barres : pour chaque âge, la barre
      # « molécule » (rangée du haut) porte le libellé, tandis que la barre
      # « ensemble des cas » (juste en dessous) n'en affiche pas, afin d'éviter
      # la redondance. tickmode="array" / ticktext permettent de choisir le
      # texte affiché sur chaque rangée d'axe Y.
      av$TickLabel <- av$Libelle
      av$TickLabel[av$Mode == "ensemble des cas"] <- ""

      # Les quatre combinaisons (Type × Mode), servies comme quatre traces
      # pour alimenter la légende.
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
            # Le customdata transporte, pour CHAQUE barre, trois informations :
            # (libellé propre sans gras, effectif, pourcentage arrondi) afin que
            # l'infobulle affiche les bonnes valeurs de la barre survolée, sans
            # réafficher la balise HTML <b> présente dans l'axe Y (Libelle).
            # NB : on passe une LISTE de vecteurs (une par point) — c'est la forme
            # que plotly.js attend pour un customdata à plusieurs champs ; un
            # data.frame provoquerait une erreur de rendu javascript.
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

      p %>%
        plotly::layout(
          title = atc_titre("Répartition par âge —", selected_lib(), code),
          # Chaque catégorie d'axe Y n'appartient qu'à une seule trace, donc
          # "overlay" (pas de regroupement ni d'empilement intempestif).
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
            # Affichage du libellé une seule fois par paire de barres : le texte
            # de chaque rangée d'axe Y est contrôlé ici (vide sur les rangées
            # « ensemble des cas »).
            tickmode = "array",
            tickvals = categoryarray,
            ticktext = rev(av$TickLabel)
          ),
          margin = list(l = 20, r = 20, t = 50, b = 20),
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.12,
            font = list(size = 11)
          )
        )
    })


    # --- Barres horizontales de l'origine du mésusage --------------------------
    # Comme le graphique par âge, DEUX barres par modalité d'origine sont
    # affichées : la première (bordeaux) exprime le pourcentage « par rapport à
    # la molécule concernée » (dénominateur = total du code ATC sélectionné), la
    # seconde (orange), juste en dessous, « par rapport à l'ensemble des cas ».
    # Le dénominateur est géré dans origine_values() ; ici on ne fait
    # qu'afficher. Toutes les modalités étant de niveau "detail", seules les
    # combinaisons "detail" alimentent la légende.
    output$plot_origine <- plotly::renderPlotly({
      code <- selected_code()
      ov <- origine_values()
      if (is.null(code) || is.null(ov) || nrow(ov) == 0) {
        return(plotly::plotly_empty())
      }

      # Couleurs officielles : bordeaux pour le mode "molécule", orange pour le
      # mode "ensemble des cas" (helpers de R/colors.R).
      pal_mol <- age_colors()
      pal_ens <- age_colors_ensemble()

      # Couleur attribuée à chaque barre selon (Type, Mode).
      col_vec <- ifelse(
        ov$Mode == "molécule",
        ifelse(ov$Type == "groupe", unname(pal_mol["groupe"]), unname(pal_mol["detail"])),
        ifelse(ov$Type == "groupe", unname(pal_ens["groupe"]), unname(pal_ens["detail"]))
      )
      ov$Col <- col_vec

      # Pour une orientation "h", plotly place la PREMIÈRE catégorie du
      # categoryarray en bas : on fournit donc l'ordre inverse de l'affichage
      # voulu pour que les libellés se lisent de haut en bas.
      categoryarray <- rev(ov$Libelle)

      # Libellé d'axe Y UNIQUE par paire de barres : la barre « molécule »
      # (rangée du haut) porte le libellé, celle « ensemble des cas » (juste en
      # dessous) n'en affiche pas.
      ov$TickLabel <- ov$Libelle
      ov$TickLabel[ov$Mode == "ensemble des cas"] <- ""

      # Les combinaisons (Type × Mode), servies comme traces pour la légende.
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

      p %>%
        plotly::layout(
          title = atc_titre("Répartition par origine —", selected_lib(), code),
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
          margin = list(l = 20, r = 20, t = 50, b = 20),
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.12,
            font = list(size = 11)
          )
        )
    })

    # --- Barres horizontales du type de mésusage (2 niveaux hiérarchiques) -----
    # Comme le graphique par âge, DEUX barres par libellé de type sont
    # affichées : la première (bordeaux) exprime le pourcentage « par rapport à
    # la molécule concernée » (dénominateur = total du code ATC sélectionné), la
    # seconde (orange), juste en dessous, « par rapport à l'ensemble des cas ».
    # Le dénominateur est géré dans type_values() ; ici on ne fait qu'afficher.
    # Les quatre combinaisons (Type : groupe/detail) × (Mode : molécule/ensemble
    # des cas) forment quatre traces distinctes pour une légende lisible.
    output$plot_type <- plotly::renderPlotly({
      code <- selected_code()
      tv <- type_values()
      if (is.null(code) || is.null(tv) || nrow(tv) == 0) {
        return(plotly::plotly_empty())
      }

      # Couleurs officielles : bordeaux pour le mode "molécule", orange pour le
      # mode "ensemble des cas" (helpers de R/colors.R).
      pal_mol <- age_colors()
      pal_ens <- age_colors_ensemble()

      # Couleur attribuée à chaque barre selon (Type, Mode).
      col_vec <- ifelse(
        tv$Mode == "molécule",
        ifelse(tv$Type == "groupe", unname(pal_mol["groupe"]), unname(pal_mol["detail"])),
        ifelse(tv$Type == "groupe", unname(pal_ens["groupe"]), unname(pal_ens["detail"]))
      )
      tv$Col <- col_vec

      # Pour une orientation "h", plotly place la PREMIÈRE catégorie du
      # categoryarray en bas : on fournit donc l'ordre inverse de l'affichage
      # voulu pour que la hiérarchie se lise de haut en bas.
      categoryarray <- rev(tv$Libelle)

      # Libellé d'axe Y UNIQUE par paire de barres : la barre « molécule »
      # (rangée du haut) porte le libellé, celle « ensemble des cas » (juste en
      # dessous) n'en affiche pas.
      tv$TickLabel <- tv$Libelle
      tv$TickLabel[tv$Mode == "ensemble des cas"] <- ""

      # Les combinaisons (Type × Mode), servies comme traces pour la légende.
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
            # Le customdata transporte, pour CHAQUE barre, trois informations :
            # (libellé propre sans gras, effectif, pourcentage arrondi) afin que
            # l'infobulle affiche les bonnes valeurs de la barre survolée, sans
            # réafficher la balise HTML <b> présente dans l'axe Y (Libelle).
            # NB : on passe une LISTE de vecteurs (une par point) — c'est la forme
            # que plotly.js attend pour un customdata à plusieurs champs ; un
            # data.frame provoquerait une erreur de rendu javascript.
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

      p %>%
        plotly::layout(
          title = atc_titre("Répartition par type —", selected_lib(), code),
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
          margin = list(l = 20, r = 20, t = 50, b = 20),
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.12,
            font = list(size = 11)
          )
        )
    })

    # --- Barres horizontales du type de prise (2 niveaux hiérarchiques) ----------
    # Comme le graphique du type de mésusage, DEUX barres par libellé : la
    # première (bordeaux) exprime le pourcentage « par rapport au code ATC
    # sélectionné », la seconde (orange), juste en dessous, « par rapport à
    # l'ensemble des cas ». Les dénominateurs sont gérés dans type_prise_values() ;
    # ici on ne fait qu'afficher. Les quatre combinaisons (Type : groupe/detail) ×
    # (Mode : molécule/ensemble des cas) forment quatre traces distinctes pour
    # une légende lisible.
    output$plot_type_prise <- plotly::renderPlotly({
      code <- selected_code()
      tp <- type_prise_values()
      if (is.null(code) || is.null(tp) || nrow(tp) == 0) {
        return(plotly::plotly_empty())
      }

      # Couleurs officielles : bordeaux (molécule) / orange (ensemble des cas).
      pal_mol <- age_colors()
      pal_ens <- age_colors_ensemble()

      col_vec <- ifelse(
        tp$Mode == "molécule",
        ifelse(tp$Type == "groupe", unname(pal_mol["groupe"]), unname(pal_mol["detail"])),
        ifelse(tp$Type == "groupe", unname(pal_ens["groupe"]), unname(pal_ens["detail"]))
      )
      tp$Col <- col_vec

      # Pour une orientation "h", plotly place la PREMIÈRE catégorie du
      # categoryarray en bas : on fournit donc l'ordre inverse de l'affichage
      # voulu pour que la hiérarchie se lise de haut en bas.
      categoryarray <- rev(tp$Libelle)

      # Libellé d'axe Y UNIQUE par paire de barres : la barre « molécule »
      # (rangée du haut) porte le libellé, celle « ensemble des cas » (juste en
      # dessous) n'en affiche pas.
      tp$TickLabel <- tp$Libelle
      tp$TickLabel[tp$Mode == "ensemble des cas"] <- ""

      # Les combinaisons (Type × Mode), servies comme traces pour la légende.
      groups <- list(
        list(Mode = "molécule",         Type = "groupe", Nom = "Molécule — groupe"),
        list(Mode = "molécule",         Type = "detail", Nom = "Molécule — détail"),
        list(Mode = "ensemble des cas", Type = "groupe", Nom = "Ensemble des cas — groupe"),
        list(Mode = "ensemble des cas", Type = "detail", Nom = "Ensemble des cas — détail")
      )

      p <- plotly::plot_ly()
      for (g in groups) {
        sub <- tp[tp$Mode == g$Mode & tp$Type == g$Type, ]
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

      p %>%
        plotly::layout(
          title = atc_titre("Répartition par type de prise —", selected_lib(), code),
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
            ticktext = rev(tp$TickLabel)
          ),
          margin = list(l = 20, r = 20, t = 50, b = 20),
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.12,
            font = list(size = 11)
          )
        )
    })

    # --- Barres horizontales des facteurs de mésusage --------------------------
    # Comme dans l'onglet MEDOC_REG, DEUX barres par facteur : bordeaux = « par
    # rapport au code ATC », orange = « par rapport à l'ensemble des cas » (voir
    # facteur_values()). Tous les facteurs sont de niveau "detail".
    # Conteneur du graphe facteur : renderUI injecte le plotlyOutput avec la
    # bonne hauteur (facteur_height()), qui change selon l'état du dépliage.
    output$plot_facteur_ui <- shiny::renderUI({
      plotly::plotlyOutput(ns("plot_facteur"), height = facteur_height())
    })

    output$plot_facteur <- plotly::renderPlotly(
      {
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

        # Orientation "h" : plotly place la 1re catégorie du categoryarray en bas.
        categoryarray <- rev(fv$Libelle)

        # Libellé d'axe Y UNIQUE par paire : la barre « molécule » le porte, celle
        # « ensemble des cas » (juste en dessous) n'en affiche pas.
        fv$TickLabel <- fv$Libelle
        fv$TickLabel[fv$Mode == "ensemble des cas"] <- ""

        groups <- list(
          list(Mode = "molécule",         Type = "groupe", Nom = "Molécule — groupe"),
          list(Mode = "molécule",         Type = "detail", Nom = "Molécule — détail"),
          list(Mode = "ensemble des cas", Type = "groupe", Nom = "Ensemble des cas — groupe"),
          list(Mode = "ensemble des cas", Type = "detail", Nom = "Ensemble des cas — détail")
        )

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

        p %>%
          plotly::layout(
            title = atc_titre("Répartition par facteur —", selected_lib(), code),
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
            margin = list(l = 20, r = 45, t = 50, b = 20),
            legend = list(
              orientation = "h",
              x = 0,
              y = -0.12,
              font = list(size = 11)
            )
          )
      }
    )

    # --- Retour exposé (inoffensif en production) -----------------------------
    # Permet à shiny::testServer de tester les réactives internes du module sans
    # rien changer au comportement de l'application (le retour est ignoré par
    # shiny::runApp).
    list(
      atc_df = atc_df,
      lib_code_atc = lib_code_atc,
      principaux_facteurs = principaux_facteurs,
      selected_code = selected_code,
      facteur_values = facteur_values,
      facteur_plot_data = facteur_plot_data,
      facteur_height = facteur_height
    )
  })
}

