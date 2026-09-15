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
            plotly::plotlyOutput(ns("plot_genre"), height = "320px")
          )
        ),
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_enceinte"), height = "320px")
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
            plotly::plotlyOutput(ns("plot_origine"), height = "320px")
          )
        ),
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_type"), height = "420px")
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
        dplyr::filter(.data$Type_donnee == "effectif") %>%
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
                      .data$Type_donnee == "effectif")

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
                      .data$Type_donnee == "effectif")
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
                      .data$Type_donnee == "effectif")
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
    # Le pourcentage est RECALCULÉ sur l'effectif TOTAL des données "origine",
    # c'est-à-dire la somme des colonnes 22 + 23 + 24 (conformément aux directives).
    origine_values <- reactive({
      code <- selected_code()
      req(code)
      data <- lib_code_atc()
      req(data)

      row <- data %>%
        dplyr::filter(.data[["Code ATC"]] == code,
                      .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      eff <- vapply(22:24, function(i) suppressWarnings(as.numeric(row[[i]])), numeric(1))
      eff[is.na(eff)] <- 0

      total <- sum(eff)
      if (is.na(total) || total <= 0) {
        return(NULL)
      }

      data.frame(
        Libelle  = c(
          "Au moment de la prise du médicament",
          "Au moment de la prescription médicale",
          "Au moment de la dispensation en pharmacie"
        ),
        Effectif = eff,
        Pct      = eff / total * 100,
        stringsAsFactors = FALSE
      )
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
    # Comme pour le graphique des âges, le pourcentage est RECALCULÉ sur le
    # Total (colonne E, index 5) de la ligne "effectif" du code ATC sélectionné.
    type_values <- reactive({
      code <- selected_code()
      req(code)
      data <- lib_code_atc()
      req(data)

      row <- data %>%
        dplyr::filter(.data[["Code ATC"]] == code,
                      .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      total <- suppressWarnings(as.numeric(row[[5]]))
      if (is.na(total) || total <= 0) {
        return(NULL)
      }

      # Définition hiérarchique : (libellé affiché, index colonne, niveau)
      # Ordre d'affichage de haut en bas.
      # Conformément aux directives, les libellés affichés sont ceux précisés
      # entre parenthèses "(Afficher : ...)" : les groupes de niveau 1 sont
      # affichés "Total Posologie, Fréquence, durée" / "Total Indication,
      # population, CI" (en gras), les libellés de niveau 2 ne sont pas en gras
      # (l'arrêt prématuré et injustifié du traitement est volontairement sans
      # libellé affiché).
      defs <- data.frame(
        Libelle_brut = c(
          "Total Posologie, Fréquence, durée",        # groupe (niveau 1)
          "Schéma posologique non conforme",
          "",                                          # arrêt prématuré (sans libellé)
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

      pct <- eff / total * 100
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

      data.frame(
        Libelle      = Libelle_aff,
        Libelle_brut = defs$Libelle_brut,
        Type         = Type,
        Effectif     = eff,
        Pct          = pct,
        stringsAsFactors = FALSE
      )
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
    output$plot_origine <- plotly::renderPlotly({
      code <- selected_code()
      ov <- origine_values()
      if (is.null(code) || is.null(ov) || nrow(ov) == 0) {
        return(plotly::plotly_empty())
      }

      # Une couleur unique pour l'origine (bordeaux "groupe" de R/colors.R).
      col_bar <- rep(unname(age_colors()["groupe"]), nrow(ov))

      # Étiquettes : "effectif (pourcentage)" arrondi à 1 décimale.
      txt <- paste0(ov$Effectif, " (", round(ov$Pct, 1), " %)")

      # Pour une orientation "h", plotly place la PREMIÈRE catégorie du
      # categoryarray en bas : on fournit donc l'ordre inverse de l'affichage
      # voulu pour que les libellés se lisent de haut en bas.
      categoryarray <- rev(ov$Libelle)

      plotly::plot_ly(
        type = "bar",
        orientation = "h",
        x = ov$Pct,
        y = ov$Libelle,
        text = txt,
        textposition = "auto",
        cliponaxis = FALSE,
        marker = list(color = col_bar),
        customdata = ov$Effectif,
        insidetextfont = list(color = "#ffffff"),
        hovertemplate = paste0(
          "%{y}<br>Effectif : %{customdata}<br>Pourcentage : ",
          round(ov$Pct, 1), " %<extra></extra>"
        ),
        showlegend = FALSE
      ) %>%
        plotly::layout(
          title = atc_titre("Répartition par origine —", selected_lib(), code),
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
            tickfont = list(size = 11)
          ),
          margin = list(l = 20, r = 20, t = 50, b = 20)
        )
    })

    # --- Barres horizontales du type de mésusage (2 niveaux hiérarchiques) -----
    output$plot_type <- plotly::renderPlotly({
      code <- selected_code()
      tv <- type_values()
      if (is.null(code) || is.null(tv) || nrow(tv) == 0) {
        return(plotly::plotly_empty())
      }

      # Couleurs officielles des barres de type : mêmes couleurs que le
      # graphique des âges (niveau 1 = groupe, niveau 2 = detail), reprises
      # depuis R/colors.R.
      pal <- age_colors()
      col_bar <- unname(pal[tv$Type])

      # Étiquettes : "effectif (pourcentage)" arrondi à 1 décimale.
      txt <- paste0(tv$Effectif, " (", round(tv$Pct, 1), " %)")

      # Pour une orientation "h", plotly place la PREMIÈRE catégorie du
      # categoryarray en bas : on fournit donc l'ordre inverse de l'affichage
      # voulu pour que la hiérarchie se lise de haut en bas.
      categoryarray <- rev(tv$Libelle)

      plotly::plot_ly(
        type = "bar",
        orientation = "h",
        x = tv$Pct,
        y = tv$Libelle,
        text = txt,
        textposition = "auto",
        cliponaxis = FALSE,
        marker = list(color = col_bar),
        # Le customdata transporte, pour CHAQUE barre, trois informations :
        # (libellé propre sans gras, effectif, pourcentage arrondi) afin que
        # l'infobulle affiche les bonnes valeurs de la barre survolée, sans
        # réafficher la balise HTML <b> présente dans l'axe Y (Libelle).
        # NB : on passe une LISTE de vecteurs (une par point) — c'est la forme
        # que plotly.js attend pour un customdata à plusieurs champs ; un
        # data.frame provoquerait une erreur de rendu javascript.
        customdata = lapply(seq_len(nrow(tv)), function(i) {
          c(tv$Libelle_brut[i], tv$Effectif[i], round(tv$Pct[i], 1))
        }),
        insidetextfont = list(color = "#ffffff"),
        hovertemplate = paste0(
          "%{customdata[0]}<br>Effectif : %{customdata[1]}",
          "<br>Pourcentage : %{customdata[2]} %<extra></extra>"
        ),
        showlegend = FALSE
      ) %>%
        plotly::layout(
          title = atc_titre("Répartition par type —", selected_lib(), code),
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
            tickfont = list(size = 11)
          ),
          margin = list(l = 20, r = 20, t = 50, b = 20)
        )
    })
  })
}

