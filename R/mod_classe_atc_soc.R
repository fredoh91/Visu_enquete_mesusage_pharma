# ============================================================================
# mod_classe_atc_soc.R — Module Shiny « CLASSE ATC SOC »
# Affiche un tableau des différents libellés SOC (colonne B) issus de
# CLASSE_ATC_SOC.xlsx, puis, au clic sur une ligne, plusieurs graphiques
# interactifs (plotly) pour le SOC sélectionné. Cet écran est largement
# analogue aux modules MEDOC_REG, LIB_CODE_ATC et FOCUS_IPP_LAXA_CORTICO
# (même organisation de l'écran et mêmes graphiques), à ceci près qu'on
# n'affiche PAS le 6e graphique des « facteurs » (principaux_facteurs.xlsx
# n'est donc pas chargé).
# ============================================================================
# Les données proviennent de data/CLASSE_ATC_SOC.xlsx via le helper
# read_classe_atc_soc() de R/read_data.R.
#
# NB IMPORTANT : la structure de ce fichier est, pour les colonnes de données,
# IDENTIQUE à celle de MEDOC_REG.xlsx / FOCUS_IPP_LAXA_CORTICO.xlsx (mêmes
# indices de colonnes) :
#   - genre      : colonnes 5, 6, 7
#   - enceinte   : colonnes 8, 9, 10
#   - âge        : colonnes 11, 13-20
#   - origine    : colonnes 21, 22, 23
#   - type       : colonnes 40 à 50
#   - type de prise : colonnes X -> AD (24 à 30)
# La colonne B (index 2) porte le libellé du SOC, tandis que la colonne A
# (index 1) porte le périmètre.
#
# Les 6 graphiques affichés (1 camembert genre, 2 camembert « enceinte »,
# 3 barres âges, 4 barres origine, 5 barres type, 6 barres type de prise)
# sont construits exactement comme dans les onglets précédents. Seul le
# graphique 6 « facteurs » de FOCUS_IPP_LAXA_CORTICO est volontairement
# absent.

library(shiny)
library(dplyr)
library(DT)

# --- UI du module -------------------------------------------------------------
#' UI du module CLASSE ATC SOC.
#'
#' L'écran est organisé en deux grandes zones (comme MEDOC_REG / LIB_CODE_ATC
#' / FOCUS_IPP_LAXA_CORTICO) :
#'   * Zone supérieure (.zone-tableau)  : le tableau des libellés SOC.
#'   * Zone inférieure (.zone-graphiques) : les graphiques relatifs au SOC
#'     sélectionné, chacun dans sa propre carte (.graph-card), disposée de
#'     façon responsive via la grille Bootstrap 5 (col-12 / col-md-6 /
#'     col-xl-4).
#'
#' @param id Identifiant unique du module.
mod_classe_atc_soc_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # --- Zone supérieure : tableau des SOC -----------------------------------
    tags$div(
      class = "zone-tableau",
      uiOutput(ns("etat")),
      DTOutput(ns("table_soc"))
    ),

    # --- Zone inférieure : graphiques (responsive) ----------------------------
    tags$div(
      class = "zone-graphiques",
      tags$h4("Détail du SOC sélectionné"),
      uiOutput(ns("plot_msg")),

      fluidRow(
        # Carte 1 : camembert du genre des patients (Homme / Femme / Autre)
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_genre"), height = "640px")
          )
        ),

        # Carte 2 : camembert des données "enceinte" (Oui / Non / Non renseigné)
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_enceinte"), height = "640px")
          )
        ),

        # Carte 3 : barres horizontales des âges (2 niveaux hiérarchiques)
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_age"), height = "640px")
          )
        ),

        # Carte 4 : barres horizontales de l'origine du mésusage
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_origine"), height = "640px")
          )
        ),

        # Carte 5 : barres horizontales du type de mésusage (2 niveaux)
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_type"), height = "640px")
          )
        ),

        # Carte 6 : barres horizontales du type de prise (2 niveaux hiérarchiques).
        # Les données proviennent des colonnes X -> AD de
        # CLASSE_ATC_SOC.xlsx (24 à 30), comme dans MEDOC_REG / FOCUS.
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
#' Server du module CLASSE ATC SOC.
#' @param id Identifiant unique du module (doit correspondre à l'UI).
mod_classe_atc_soc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Données brutes importées depuis le fichier Excel -------------------
    # La dataframe CLASSE_ATC_SOC est chargée une seule fois au démarrage du
    # module. Une colonne "SOC" (libellé du sous-périmètre, présent en colonne
    # B / index 2 du fichier) est ajoutée pour identifier chaque ligne de façon
    # lisible, au même titre que la colonne "DCI" pour MEDOC_REG.
    soc_data <- reactive({
      data <- tryCatch(
        read_classe_atc_soc(),
        error = function(e) NULL
      )
      if (is.null(data)) {
        return(NULL)
      }
      # La colonne B (index 2) porte le libellé du SOC ; on la nomme
      # explicitement "SOC" pour faciliter les filtres.
      data[["SOC"]] <- as.character(data[[2]])
      data
    })

    # --- Dataframe des SOC (affichage basique) -------------------------------
    # Ne conserve que les lignes du type "effectif" et exclut les lignes
    # "Total" (périmètre global) ainsi que les lignes agrégées "Autre" /
    # "Autres" qui ne correspondent pas à un SOC précis. Ne restent donc que
    # les libellés SOC réels, présentés avec leur effectif total (colonne D
    # "Total"), triés par effectif décroissant.
    soc_df <- reactive({
      data <- soc_data()
      req(data)
      data %>%
        dplyr::filter(.data$Type_donnee == "effectif") %>%
        dplyr::filter(!.data$SOC %in% c("Total", "Autre", "Autres")) %>%
        dplyr::select(SOC, Total) %>%
        dplyr::arrange(dplyr::desc(.data$Total))
    })

    # --- SOC sélectionné dans le tableau ------------------------------------
    # input$table_soc_rows_selected : index de l'unique ligne sélectionnée
    # (NULL tant qu'aucune ligne n'est cliquée, car DT est en mode "single").
    selected_soc <- reactive({
      idx <- input$table_soc_rows_selected
      if (is.null(idx) || length(idx) == 0 || idx < 1 || idx > nrow(soc_df())) {
        return(NULL)
      }
      soc_df()$SOC[idx]
    })

    # --- Ligne "effectif" du SOC sélectionné --------------------------------
    # Permet de récupérer les effectifs du genre (colonnes Homme / Femme / Autre).
    selected_genre <- reactive({
      soc <- selected_soc()
      data <- soc_data()
      req(soc, data)

      row <- data %>%
        dplyr::filter(.data$SOC == soc, .data$Type_donnee == "effectif")

      if (nrow(row) != 1) {
        return(NULL)
      }
      row[1, ]
    })

    # --- Données du camembert du genre des patients --------------------------
    # Construit la dataframe nécessaire au camembert 1 : uniquement la
    # répartition du genre (Homme / Femme / Autre). Les colonnes sont lues par
    # index : genre = 5 (Homme), 6 (Femme), 7 (Autre). Les éventuelles NA sont
    # traitées comme 0 et les segments d'effectif nul sont écartés.
    #
    # Le pourcentage est recalculé sur le total Homme + Femme + Autre.
    genre_values <- reactive({
      row <- selected_genre()
      req(row)

      h <- suppressWarnings(as.numeric(row[[5]]))
      f <- suppressWarnings(as.numeric(row[[6]]))
      a <- suppressWarnings(as.numeric(row[[7]]))
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
      df$Pct <- df$Effectif / total * 100
      df$Pct_fr <- formatC(df$Pct, format = "f", digits = 1,
                           big.mark = " ", decimal.mark = ",")

      df[df$Effectif > 0, ]
    })

    # --- Données du camembert des données "enceinte" --------------------------
    # Construit la dataframe nécessaire au camembert 2 : uniquement la
    # répartition "enceinte" (Oui / Non / Non renseigné), en nuances de vert.
    # Les colonnes sont lues par index : enceinte = 8 (Non), 9 (Oui),
    # 10 (Non renseigné).
    #
    # Le pourcentage est recalculé sur le TOTAL des données "enceinte"
    # (Non + Oui + Non renseigné), conformément aux directives.
    enceinte_values <- reactive({
      row <- selected_genre()
      req(row)

      non <- suppressWarnings(as.numeric(row[[8]]))
      oui <- suppressWarnings(as.numeric(row[[9]]))
      nr  <- suppressWarnings(as.numeric(row[[10]]))
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
      df$Pct <- df$Effectif / total * 100
      df$Pct_fr <- formatC(df$Pct, format = "f", digits = 1,
                           big.mark = " ", decimal.mark = ",")

      df[df$Effectif > 0, ]
    })


    # --- Données des barres d'âge (données S2 — âge du patient) --------------
    # Lecture depuis la ligne "effectif" du SOC sélectionné. Les colonnes
    # concernées vont de K à T (S2. Âge) ; on accède par index numérique car
    # certains libellés contiennent des espaces insécables. La colonne
    # "ST JEUNES, ENFANTS" (qui chevauche enfants et adolescents) est
    # volontairement ignorée.
    #
    # Chaque libellé est représenté par DEUX barres, correspondant à deux modes
    # de calcul des pourcentages :
    #   * mode "molécule"        : pourcentage = effectif / Total SOC * 100,
    #     où le Total (colonne D, index 4) provient de la ligne "effectif" du
    #     SOC sélectionné ;
    #   * mode "ensemble des cas" : pourcentage = effectif / Total enquête * 100,
    #     où le Total (colonne D) provient de la ligne agrégée "Total".
    age_values <- reactive({
      soc <- selected_soc()
      req(soc)
      data <- soc_data()
      req(data)

      row <- data %>%
        dplyr::filter(.data$SOC == soc, .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      total_mol <- suppressWarnings(as.numeric(row[[4]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      row_total <- data %>%
        dplyr::filter(.data$SOC == "Total", .data$Type_donnee == "effectif")
      total_ens <- if (nrow(row_total) >= 1) {
        suppressWarnings(as.numeric(row_total[[4]][1]))
      } else {
        NA
      }
      if (is.na(total_ens) || total_ens <= 0) {
        total_ens <- total_mol
      }

      # Définition hiérarchique : (libellé affiché, index colonne, niveau)
      # Ordre d'affichage de haut en bas.
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
        Index = c(11L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L),
        Niveau = c(1L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L),
        stringsAsFactors = FALSE
      )

      eff <- vapply(defs$Index, function(i) suppressWarnings(as.numeric(row[[i]])), numeric(1))
      eff[is.na(eff)] <- 0

      Type  <- ifelse(defs$Niveau == 1L, "groupe", "detail")
      Libelle_aff <- ifelse(
        defs$Niveau == 1L,
        paste0("<b>", defs$Libelle_brut, "</b>"),
        paste0("    ", defs$Libelle_brut)
      )

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


    # --- Données des barres horizontales de l'origine du mésusage -------------
    # Lecture depuis la ligne "effectif" du SOC sélectionné. Les colonnes
    # concernées vont de U à W (index 21, 22, 23) :
    #   - Au moment de la prise du médicament
    #   - Au moment de la prescription médicale
    #   - Au moment de la dispensation en pharmacie
    #
    # DEUX modes de calcul des pourcentages : mode "molécule" (colonne D de la
    # ligne "effectif" du SOC) et mode "ensemble des cas" (colonne D de la
    # ligne agrégée "Total").
    origine_values <- reactive({
      soc <- selected_soc()
      req(soc)
      data <- soc_data()
      req(data)

      row <- data %>%
        dplyr::filter(.data$SOC == soc, .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]



      total_mol <- suppressWarnings(as.numeric(row[[4]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      row_total <- data %>%
        dplyr::filter(.data$SOC == "Total", .data$Type_donnee == "effectif")
      total_ens <- if (nrow(row_total) >= 1) {
        suppressWarnings(as.numeric(row_total[[4]][1]))
      } else {
        NA
      }
      if (is.na(total_ens) || total_ens <= 0) {
        total_ens <- total_mol
      }

      # Définition hiérarchique : trois modalités d'origine, toutes en niveau 2
      # (détail). La colonne "Total" (index 4 du fichier) n'est pas concernée.
      defs <- data.frame(
        Libelle_brut = c(
          "Au moment de la prise du médicament",
          "Au moment de la prescription médicale",
          "Au moment de la dispensation en pharmacie"
        ),
        Index = c(21L, 22L, 23L),
        Niveau = c(2L, 2L, 2L),
        stringsAsFactors = FALSE
      )

      eff <- vapply(defs$Index, function(i) suppressWarnings(as.numeric(row[[i]])), numeric(1))
      eff[is.na(eff)] <- 0

      Type  <- ifelse(defs$Niveau == 1L, "groupe", "detail")
      Libelle_aff <- ifelse(
        defs$Niveau == 1L,
        paste0("<b>", defs$Libelle_brut, "</b>"),
        paste0("    ", defs$Libelle_brut)
      )

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


    # --- Données des barres horizontales du type de mésusage ------------------
    # Lecture depuis la ligne "effectif" du SOC sélectionné. Les colonnes
    # concernées vont de AN à AX (index 40 à 50) et se répartissent sur 2
    # niveaux hiérarchiques (comme le graphique des âges).
    #
    # DEUX modes de calcul des pourcentages : mode "molécule" (colonne D de la
    # ligne "effectif" du SOC) et mode "ensemble des cas" (colonne D de la
    # ligne agrégée "Total").
    type_values <- reactive({
      soc <- selected_soc()
      req(soc)
      data <- soc_data()
      req(data)

      row <- data %>%
        dplyr::filter(.data$SOC == soc, .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      total_mol <- suppressWarnings(as.numeric(row[[4]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      row_total <- data %>%
        dplyr::filter(.data$SOC == "Total", .data$Type_donnee == "effectif")
      total_ens <- if (nrow(row_total) >= 1) {
        suppressWarnings(as.numeric(row_total[[4]][1]))
      } else {
        NA
      }
      if (is.na(total_ens) || total_ens <= 0) {
        total_ens <- total_mol
      }

      # Définition hiérarchique : (libellé affiché, index colonne, niveau).
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
        Index = c(40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L),
        Niveau = c(1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L),
        stringsAsFactors = FALSE
      )

      eff <- vapply(defs$Index, function(i) suppressWarnings(as.numeric(row[[i]])), numeric(1))
      eff[is.na(eff)] <- 0

      Type  <- ifelse(defs$Niveau == 1L, "groupe", "detail")
      Libelle_aff <- ifelse(
        defs$Niveau == 1L,
        paste0("<b>", defs$Libelle_brut, "</b>"),
        paste0("    ", defs$Libelle_brut)
      )

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
    # Lecture depuis la ligne "effectif" du SOC sélectionné. Les colonnes
    # concernées vont de X à AD (index 24 à 30) et se répartissent sur 2 niveaux
    # hiérarchiques (comme les graphiques des âges et du type) :
    #   Niveau 1 « Médicament avec ordonnance » (colonne X=24) :
    #     - D'une primo-prescription                    (Y=25)
    #     - D'un renouvellement d'ordonnance            (Z=26)
    #   Niveau 1 « Médicament sans ordonnance » (colonne AA=27) :
    #     - D'un médicament sans ordonnance en automédication         (AB=28)
    #     - D'un médicament sans ordonnance sur conseil du pharmacien (AC=29)
    #     - Je ne sais pas                                             (AD=30)
    #
    # DEUX modes de calcul des pourcentages : mode "molécule" (colonne D de la
    # ligne "effectif" du SOC) et mode "ensemble des cas" (colonne D de la
    # ligne agrégée "Total").
    type_prise_values <- reactive({
      soc <- selected_soc()
      req(soc)
      data <- soc_data()
      req(data)

      row <- data %>%
        dplyr::filter(.data$SOC == soc, .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      total_mol <- suppressWarnings(as.numeric(row[[4]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      row_total <- data %>%
        dplyr::filter(.data$SOC == "Total", .data$Type_donnee == "effectif")
      total_ens <- if (nrow(row_total) >= 1) {
        suppressWarnings(as.numeric(row_total[[4]][1]))
      } else {
        NA
      }
      if (is.na(total_ens) || total_ens <= 0) {
        total_ens <- total_mol
      }

      defs <- data.frame(
        Libelle_brut = c(
          "Total Médicament avec ordonnance",         # groupe (niveau 1)
          "D'une primo-prescription",
          "D'un renouvellement d'ordonnance",
          "Total Médicament sans ordonnance",         # groupe (niveau 1)
          "D'un médicament sans ordonnance en automédication",
          "D'un médicament sans ordonnance sur conseil du pharmacien",
          "Je ne sais pas"
        ),
        Index = c(24L, 25L, 26L, 27L, 28L, 29L, 30L),
        Niveau = c(1L, 2L, 2L, 1L, 2L, 2L, 2L),
        stringsAsFactors = FALSE
      )

      eff <- vapply(defs$Index, function(i) suppressWarnings(as.numeric(row[[i]])), numeric(1))
      eff[is.na(eff)] <- 0

      Type  <- ifelse(defs$Niveau == 1L, "groupe", "detail")
      Libelle_aff <- ifelse(
        defs$Niveau == 1L,
        paste0("<b>", defs$Libelle_brut, "</b>"),
        paste0("    ", defs$Libelle_brut)
      )

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



    # --- Message / état de l'import des données --------------------------------
    output$etat <- renderUI({
      data <- soc_data()
      if (is.null(data)) {
        return(
          div(class = "alert alert-danger",
              icon("exclamation-triangle"),
              strong("Fichier CLASSE_ATC_SOC introuvable ou illisible dans data/."))
        )
      }
      NULL
    })

    # --- Tableau des SOC --------------------------------------------------------
    output$table_soc <- renderDT({
      df <- soc_df()
      if (is.null(df) || nrow(df) == 0) {
        return(
          DT::datatable(
            data.frame(Avertissement = "Aucune donnée SOC disponible."),
            options = list(dom = "t"), rownames = FALSE
          )
        )
      }
      DT::datatable(
        df,
        rownames = FALSE,
        colnames = c("SOC" = "SOC",
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
      soc <- selected_soc()
      if (is.null(soc)) {
        return(
          tags$p("Cliquez sur un SOC du tableau ci-dessus pour afficher le détail.")
        )
      }
      tags$p(
        class = "dci-title",
        icon("file-medical"),
        strong(paste("Caractéristiques du SOC —", soc))
      )
    })

    # --- Camembert 1 : genre des patients (partie basse) ----------------------
    output$plot_genre <- plotly::renderPlotly({
      soc <- selected_soc()
      titre <- wrap_titre(paste("Répartition par genre —", soc))
      gv <- genre_values()
      if (is.null(soc) || is.null(gv) || nrow(gv) == 0) {
        return(plotly::plotly_empty())
      }

      pal_genre <- genres_colors()
      col_segments <- unname(pal_genre[gv$Label])

      txt <- paste0(gv$Label, "<br>", gv$Effectif, " (", gv$Pct_fr, "%)")

      plotly::plot_ly(
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
      ) %>%
        plotly::layout(
          title = titre,
          margin = list(l = 20, r = 20, t = titre_margin_top(titre), b = 20)
        )
    })

    # --- Camembert 2 : données "enceinte" (partie basse) ----------------------
    output$plot_enceinte <- plotly::renderPlotly({
      soc <- selected_soc()
      titre <- wrap_titre(paste("Répartition des données \"enceinte\" —", soc))
      ev <- enceinte_values()
      if (is.null(soc) || is.null(ev) || nrow(ev) == 0) {
        return(plotly::plotly_empty())
      }

      pal_enceinte <- enceinte_colors()
      col_segments <- unname(pal_enceinte[ev$Label])

      txt <- paste0(ev$Label, "<br>", ev$Effectif, " (", ev$Pct_fr, "%)")

      plotly::plot_ly(
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
      ) %>%
        plotly::layout(
          title = titre,
          margin = list(l = 20, r = 20, t = titre_margin_top(titre), b = 20)
        )
    })


    # --- Barres horizontales des âges (partie basse) ---------------------------
    # DEUX barres par tranche d'âge : bordeaux = « par rapport au SOC »,
    # orange = « par rapport à l'ensemble des cas » (voir age_values()).
    output$plot_age <- plotly::renderPlotly({
      soc <- selected_soc()
      titre <- wrap_titre(paste("Répartition par âge —", soc))
      av <- age_values()
      if (is.null(soc) || is.null(av) || nrow(av) == 0) {
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



    # --- Barres horizontales de l'origine du mésusage --------------------------
    # DEUX barres par modalité d'origine (bordeaux = SOC, orange = ensemble).
    output$plot_origine <- plotly::renderPlotly({
      soc <- selected_soc()
      titre <- wrap_titre(paste("Répartition par origine —", soc))
      ov <- origine_values()
      if (is.null(soc) || is.null(ov) || nrow(ov) == 0) {
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



    # --- Barres horizontales du type de mésusage -------------------------------
    # DEUX barres par libellé de type (bordeaux = SOC, orange = ensemble).
    output$plot_type <- plotly::renderPlotly({
      soc <- selected_soc()
      titre <- wrap_titre(paste("Répartition par type —", soc))
      tv <- type_values()
      if (is.null(soc) || is.null(tv) || nrow(tv) == 0) {
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



    # --- Barres horizontales du type de prise -----------------------------------
    # DEUX barres par libellé de type de prise (bordeaux = SOC, orange = ensemble).
    output$plot_type_prise <- plotly::renderPlotly({
      soc <- selected_soc()
      titre <- wrap_titre(paste("Répartition par type de prise —", soc))
      tpv <- type_prise_values()
      if (is.null(soc) || is.null(tpv) || nrow(tpv) == 0) {
        return(plotly::plotly_empty())
      }

      pal_mol <- age_colors()
      pal_ens <- age_colors_ensemble()

      col_vec <- ifelse(
        tpv$Mode == "molécule",
        ifelse(tpv$Type == "groupe", unname(pal_mol["groupe"]), unname(pal_mol["detail"])),
        ifelse(tpv$Type == "groupe", unname(pal_ens["groupe"]), unname(pal_ens["detail"]))
      )
      tpv$Col <- col_vec

      tpv$Libelle <- wrap_libelle(tpv$Libelle)
      categoryarray <- rev(tpv$Libelle)

      tpv$TickLabel <- tpv$Libelle
      tpv$TickLabel[tpv$Mode == "ensemble des cas"] <- ""

      groups <- list(
        list(Mode = "molécule",         Type = "groupe", Nom = "Molécule — groupe"),
        list(Mode = "molécule",         Type = "detail", Nom = "Molécule — détail"),
        list(Mode = "ensemble des cas", Type = "groupe", Nom = "Ensemble des cas — groupe"),
        list(Mode = "ensemble des cas", Type = "detail", Nom = "Ensemble des cas — détail")
      )

      p <- plotly::plot_ly()
      for (g in groups) {
        sub <- tpv[tpv$Mode == g$Mode & tpv$Type == g$Type, ]
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
            ticktext = rev(tpv$TickLabel)
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


    # --- Retour exposé (inoffensif en production) -----------------------------
    # Permet à shiny::testServer de tester les réactives internes du module sans
    # rien changer au comportement de l'application (le retour est ignoré par
    # shiny::runApp).
    list(
      soc_df = soc_df,
      soc_data = soc_data,
      selected_soc = selected_soc,
      genre_values = genre_values,
      enceinte_values = enceinte_values,
      age_values = age_values,
      origine_values = origine_values,
      type_values = type_values,
      type_prise_values = type_prise_values
    )

  })
}

