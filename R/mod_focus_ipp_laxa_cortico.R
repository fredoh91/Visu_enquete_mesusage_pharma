# ============================================================================
# mod_focus_ipp_laxa_cortico.R — Module Shiny « Focus IPP / Laxatif / Corticoïdes »
# Affiche un tableau des différents « focus » (Focus IPP (A02BC), Focus Laxatif
# (A06A), Focus Corticoïdes) issus de FOCUS_IPP_LAXA_CORTICO.xlsx, puis, au clic
# sur une ligne, plusieurs graphiques interactifs (plotly) pour le focus
# sélectionné. Cet écran est largement analogue aux modules MEDOC_REG et
# LIB_CODE_ATC (même organisation de l'écran, mêmes sept graphiques).
# ============================================================================
# Les données proviennent de data/FOCUS_IPP_LAXA_CORTICO.xlsx via le helper
# read_focus_ipp_laxa_cortico() de R/read_data.R.
#
# NB IMPORTANT : la structure de ce fichier est, pour les colonnes de données,
# IDENTIQUE à celle de MEDOC_REG.xlsx (mêmes indices de colonnes) :
#   - genre     : colonnes 5, 6, 7
#   - enceinte  : colonnes 8, 9, 10
#   - âge       : colonnes 11, 13-20
#   - origine   : colonnes 21, 22, 23
#   - type      : colonnes 40 à 50
#   - type de prise : colonnes X -> AD (24 à 30)
# La colonne B (index 2) porte le libellé du sous-périmètre (le « focus »),
# tandis que la colonne A (index 1) porte le périmètre (FOCUS_IPP, FOCUS_LAXA,
# FOCUS_CORTI).
#
# Pour le 6e graphique (facteurs), les données proviennent de
# principaux_facteurs.xlsx, aux colonnes ER -> EW (indices 148 à 153) du fichier
# transposé : la liaison se fait par l'en-tête de ligne 2, qui vaut exactement le
# libellé du focus (ex. "Focus IPP (A02BC)").

library(shiny)
library(dplyr)
library(DT)

# --- UI du module -------------------------------------------------------------
#' UI du module Focus IPP / Laxatif / Corticoïdes.
#'
#' L'écran est organisé en deux grandes zones (comme MEDOC_REG / LIB_CODE_ATC) :
#'   * Zone supérieure (.zone-tableau)  : le tableau des focus.
#'   * Zone inférieure (.zone-graphiques) : les graphiques relatifs au focus
#'     sélectionné, chacun dans sa propre carte (.graph-card), disposée de façon
#'     responsive via la grille Bootstrap 5 (col-12 / col-md-6 / col-xl-4).
#'
#' @param id Identifiant unique du module.
mod_focus_ipp_laxa_cortico_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # --- Zone supérieure : tableau des focus ----------------------------------
    tags$div(
      class = "zone-tableau",
      uiOutput(ns("etat")),
      DTOutput(ns("table_focus"))
    ),

    # --- Zone inférieure : graphiques (responsive) ----------------------------
    tags$div(
      class = "zone-graphiques",
      tags$h4("Détail du focus sélectionné"),
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

        # Carte 6 : barres horizontales des facteurs de mésusage.
        # Ce graphique exploite un fichier Excel distinct (principaux_facteurs.xlsx),
        # chargé en parallèle de FOCUS_IPP_LAXA_CORTICO.xlsx. La liaison se fait
        # par le libellé du focus (en-têtes de ligne 2 des colonnes ER -> EW =
        # indices 148:153).
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            # Contrôle de dépliage : par défaut seuls les 15 premiers facteurs
            # (plus grands pourcentages) sont affichés ; on coche pour déplier.
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
        # Les données proviennent des colonnes X -> AD de
        # FOCUS_IPP_LAXA_CORTICO.xlsx (24 à 30), comme dans MEDOC_REG.
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
#' Server du module Focus IPP / Laxatif / Corticoïdes.
#' @param id Identifiant unique du module (doit correspondre à l'UI).
mod_focus_ipp_laxa_cortico_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Données brutes importées depuis le fichier Excel -------------------
    # La dataframe FOCUS_IPP_LAXA_CORTICO est chargée une seule fois au
    # démarrage du module. Une colonne "Focus" (libellé du sous-périmètre,
    # présent en colonne B / index 2 du fichier) est ajoutée pour identifier
    # chaque ligne de façon lisible, au même titre que la colonne "DCI" pour
    # MEDOC_REG.
    focus_data <- reactive({
      data <- tryCatch(
        read_focus_ipp_laxa_cortico(),
        error = function(e) NULL
      )
      if (is.null(data)) {
        return(NULL)
      }
      # La colonne B (index 2) porte le libellé du focus ; on la nomme
      # explicitement "Focus" pour faciliter les filtres.
      data[["Focus"]] <- as.character(data[[2]])
      data
    })

    # --- Données brutes des facteurs (principaux_facteurs.xlsx) -------------
    # Chargé au même moment que FOCUS_IPP_LAXA_CORTICO.xlsx pour alimenter le
    # 6e graphique de l'onglet (barres horizontales des facteurs de mésusage).
    # Fichier transposé (col_names = FALSE) : chaque ligne = un facteur ; chaque
    # focus occupe une colonne dédiée (ER..EW = indices 148:153) dont l'en-tête
    # de ligne 2 porte exactement le libellé du focus (ex. "Focus IPP (A02BC)").
    principaux_facteurs <- reactive({
      tryCatch(
        read_principaux_facteurs(),
        error = function(e) NULL
      )
    })

    # --- Dataframe des focus (affichage basique) ---------------------------
    # Ne conserve que les lignes du type "effectif" et exclut les lignes
    # "Total" (périmètre global) ainsi que les lignes agrégées "Autre" /
    # "Autres" qui ne correspondent pas à un focus précis. Ne restent donc que
    # les trois focus (Focus IPP, Focus Laxatif, Focus Corticoïdes), présentés
    # avec leur effectif total (colonne D "Total"), triés par effectif
    # décroissant.
    focus_df <- reactive({
      data <- focus_data()
      req(data)
      data %>%
        dplyr::filter(.data$Type_donnee == "effectif") %>%
        dplyr::filter(!.data$Focus %in% c("Total", "Autre", "Autres")) %>%
        dplyr::select(Focus, Total) %>%
        dplyr::arrange(dplyr::desc(.data$Total))
    })

    # --- Focus sélectionné dans le tableau ---------------------------------
    # input$table_focus_rows_selected : index de l'unique ligne sélectionnée
    # (NULL tant qu'aucune ligne n'est cliquée, car DT est en mode "single").
    selected_focus <- reactive({
      idx <- input$table_focus_rows_selected
      if (is.null(idx) || length(idx) == 0 || idx < 1 || idx > nrow(focus_df())) {
        return(NULL)
      }
      focus_df()$Focus[idx]
    })

    # --- Ligne "effectif" du focus sélectionné ------------------------------
    # Permet de récupérer les effectifs du genre (colonnes Homme / Femme / Autre).
    selected_genre <- reactive({
      focus <- selected_focus()
      data <- focus_data()
      req(focus, data)

      row <- data %>%
        dplyr::filter(.data$Focus == focus, .data$Type_donnee == "effectif")

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
    # Lecture depuis la ligne "effectif" du focus sélectionné. Les colonnes
    # concernées vont de K à T (S2. Âge) ; on accède par index numérique car
    # certains libellés contiennent des espaces insécables. La colonne
    # "ST JEUNES, ENFANTS" (qui chevauche enfants et adolescents) est
    # volontairement ignorée.
    #
    # Chaque libellé est représenté par DEUX barres, correspondant à deux modes
    # de calcul des pourcentages :
    #   * mode "molécule"        : pourcentage = effectif / Total focus * 100,
    #     où le Total (colonne D, index 4) provient de la ligne "effectif" du
    #     focus sélectionné ;
    #   * mode "ensemble des cas" : pourcentage = effectif / Total enquête * 100,
    #     où le Total (colonne D) provient de la ligne agrégée "Total".
    age_values <- reactive({
      focus <- selected_focus()
      req(focus)
      data <- focus_data()
      req(data)

      row <- data %>%
        dplyr::filter(.data$Focus == focus, .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      total_mol <- suppressWarnings(as.numeric(row[[4]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      row_total <- data %>%
        dplyr::filter(.data$Focus == "Total", .data$Type_donnee == "effectif")
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
    # Lecture depuis la ligne "effectif" du focus sélectionné. Les colonnes
    # concernées vont de U à W (index 21, 22, 23) :
    #   - Au moment de la prise du médicament
    #   - Au moment de la prescription médicale
    #   - Au moment de la dispensation en pharmacie
    #
    # DEUX modes de calcul des pourcentages : mode "molécule" (colonne D de la
    # ligne "effectif" du focus) et mode "ensemble des cas" (colonne D de la
    # ligne agrégée "Total").
    origine_values <- reactive({
      focus <- selected_focus()
      req(focus)
      data <- focus_data()
      req(data)

      row <- data %>%
        dplyr::filter(.data$Focus == focus, .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      total_mol <- suppressWarnings(as.numeric(row[[4]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      row_total <- data %>%
        dplyr::filter(.data$Focus == "Total", .data$Type_donnee == "effectif")
      total_ens <- if (nrow(row_total) >= 1) {
        suppressWarnings(as.numeric(row_total[[4]][1]))
      } else {
        NA
      }
      if (is.na(total_ens) || total_ens <= 0) {
        total_ens <- total_mol
      }

      eff <- vapply(21:23, function(i) suppressWarnings(as.numeric(row[[i]])), numeric(1))
      eff[is.na(eff)] <- 0

      Libelle_brut <- c(
        "Au moment de la prise du médicament",
        "Au moment de la prescription médicale",
        "Au moment de la dispensation en pharmacie"
      )
      Type <- rep("detail", length(Libelle_brut))

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


    # --- Données des barres horizontales du type de mésusage ------------------
    # Lecture depuis la ligne "effectif" du focus sélectionné. Les colonnes
    # concernées vont de AN à AX (index 40 à 50) et se répartissent sur 2
    # niveaux hiérarchiques (comme le graphique des âges).
    #
    # DEUX modes de calcul des pourcentages : mode "molécule" (colonne D de la
    # ligne "effectif" du focus) et mode "ensemble des cas" (colonne D de la
    # ligne agrégée "Total").
    type_values <- reactive({
      focus <- selected_focus()
      req(focus)
      data <- focus_data()
      req(data)

      row <- data %>%
        dplyr::filter(.data$Focus == focus, .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      total_mol <- suppressWarnings(as.numeric(row[[4]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      row_total <- data %>%
        dplyr::filter(.data$Focus == "Total", .data$Type_donnee == "effectif")
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


    # --- Données des barres horizontales du type de prise ----------------------
    # Lecture depuis la ligne "effectif" du focus sélectionné. Les colonnes
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
    # ligne "effectif" du focus) et mode "ensemble des cas" (colonne D de la
    # ligne agrégée "Total").
    type_prise_values <- reactive({
      focus <- selected_focus()
      req(focus)
      data <- focus_data()
      req(data)

      row <- data %>%
        dplyr::filter(.data$Focus == focus, .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      total_mol <- suppressWarnings(as.numeric(row[[4]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      row_total <- data %>%
        dplyr::filter(.data$Focus == "Total", .data$Type_donnee == "effectif")
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
          "Prise avec ordonnance",       # groupe (niveau 1)
          "Primo prescription",
          "Renouvellement",
          "Prise sans ordonnance",       # groupe (niveau 1)
          "Sans ordo en automédication",
          "Sur conseil du pharmacien",
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


    # --- Données des barres horizontales des facteurs de mésusage --------------
    # Les données proviennent du fichier principaux_facteurs.xlsx (réactive
    # principaux_facteurs()), lu en brut (col_names = FALSE) : ligne 2 = en-têtes,
    # colonne B (2) = libellé facteur, colonne C (3) = type ("effectif"),
    # colonne D (4) = "Total" (effectif global), colonnes ER..EW (148 à 153) = une
    # colonne par focus (l'en-tête de ligne 2 donne le libellé du focus, ex.
    # "Focus IPP (A02BC)").
    # DEUX barres par facteur, comme âge/origine/type :
    #   * mode "molécule"        : Pct = effectif_facteur_focus / Total_focus * 100 ;
    #   * mode "ensemble des cas": Pct = Total_facteur / Total_enquête * 100.
    facteur_values <- reactive({
      focus <- selected_focus()
      data <- focus_data()
      pf <- principaux_facteurs()
      req(focus, data, pf)

      # Total du focus (colonne D de la ligne "effectif" du focus).
      row <- data %>%
        dplyr::filter(.data$Focus == focus, .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]
      total_mol <- suppressWarnings(as.numeric(row[[4]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      # Total ensemble des cas (colonne D de la ligne agrégée "Total").
      row_total <- data %>%
        dplyr::filter(.data$Focus == "Total", .data$Type_donnee == "effectif")
      total_ens <- if (nrow(row_total) >= 1) {
        suppressWarnings(as.numeric(row_total[[4]][1]))
      } else {
        NA
      }
      if (is.na(total_ens) || total_ens <= 0) {
        total_ens <- total_mol
      }

      # Colonnes ER..EW (148:153) : une par focus ; l'en-tête (ligne 2) porte le
      # libellé exact du focus (ex. "Focus IPP (A02BC)").
      dci_cols <- 148:153
      en_tetes <- vapply(dci_cols, function(i) {
        v <- pf[[i]][2]
        ifelse(is.na(v), "", as.character(v))
      }, character(1))
      hit <- which(trimws(en_tetes) == trimws(as.character(focus)))
      if (length(hit) == 0) {
        return(NULL)   # pas de colonne dédiée à ce focus
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

      # Tri par pourcentage DÉCROISSANT (critère = barre "molécule", le
      # pourcentage principal affiché pour le focus sélectionné).
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
    facteur_height <- reactive({
      if (isTRUE(input$facteur_afficher_tous)) {
        "1000px"
      } else {
        "820px"
      }
    })

    # --- Jeu de données affiché (filtrage 15 premiers / tous) -----------------
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
      data <- focus_data()
      if (is.null(data)) {
        return(
          div(class = "alert alert-danger",
              icon("exclamation-triangle"),
              strong("Fichier FOCUS_IPP_LAXA_CORTICO introuvable ou illisible dans data/."))
        )
      }
      NULL
    })

    # --- Tableau des focus ------------------------------------------------------
    output$table_focus <- renderDT({
      df <- focus_df()
      if (is.null(df) || nrow(df) == 0) {
        return(
          DT::datatable(
            data.frame(Avertissement = "Aucune donnée focus disponible."),
            options = list(dom = "t"), rownames = FALSE
          )
        )
      }
      DT::datatable(
        df,
        rownames = FALSE,
        colnames = c("Focus" = "Focus",
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
      focus <- selected_focus()
      if (is.null(focus)) {
        return(
          tags$p("Cliquez sur un focus du tableau ci-dessus pour afficher le détail.")
        )
      }
      tags$p(
        class = "dci-title",
        icon("file-medical"),
        strong(paste("Caractéristiques du focus —", focus))
      )
    })

    # --- Camembert 1 : genre des patients (partie basse) ----------------------
    output$plot_genre <- plotly::renderPlotly({
      focus <- selected_focus()
      gv <- genre_values()
      if (is.null(focus) || is.null(gv) || nrow(gv) == 0) {
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
          title = paste("Répartition par genre —", focus),
          margin = list(l = 20, r = 20, t = 50, b = 20)
        )
    })

    # --- Camembert 2 : données "enceinte" (partie basse) ----------------------
    output$plot_enceinte <- plotly::renderPlotly({
      focus <- selected_focus()
      ev <- enceinte_values()
      if (is.null(focus) || is.null(ev) || nrow(ev) == 0) {
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
          title = paste("Répartition des données \"enceinte\" —", focus),
          margin = list(l = 20, r = 20, t = 50, b = 20)
        )
    })


    # --- Barres horizontales des âges (partie basse) ---------------------------
    # DEUX barres par tranche d'âge : bordeaux = « par rapport au focus »,
    # orange = « par rapport à l'ensemble des cas » (voir age_values()).
    output$plot_age <- plotly::renderPlotly({
      focus <- selected_focus()
      av <- age_values()
      if (is.null(focus) || is.null(av) || nrow(av) == 0) {
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
          title = paste("Répartition par âge —", focus),
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
    # DEUX barres par modalité d'origine (bordeaux = focus, orange = ensemble).
    output$plot_origine <- plotly::renderPlotly({
      focus <- selected_focus()
      ov <- origine_values()
      if (is.null(focus) || is.null(ov) || nrow(ov) == 0) {
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
          title = paste("Répartition par origine —", focus),
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


    # --- Barres horizontales du type de mésusage -------------------------------
    # DEUX barres par libellé de type (bordeaux = focus, orange = ensemble).
    output$plot_type <- plotly::renderPlotly({
      focus <- selected_focus()
      tv <- type_values()
      if (is.null(focus) || is.null(tv) || nrow(tv) == 0) {
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
          title = paste("Répartition par type —", focus),
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


    # --- Barres horizontales du type de prise ----------------------------------
    # DEUX barres par libellé (bordeaux = focus, orange = ensemble des cas).
    output$plot_type_prise <- plotly::renderPlotly({
      focus <- selected_focus()
      tp <- type_prise_values()
      if (is.null(focus) || is.null(tp) || nrow(tp) == 0) {
        return(plotly::plotly_empty())
      }

      pal_mol <- age_colors()
      pal_ens <- age_colors_ensemble()

      col_vec <- ifelse(
        tp$Mode == "molécule",
        ifelse(tp$Type == "groupe", unname(pal_mol["groupe"]), unname(pal_mol["detail"])),
        ifelse(tp$Type == "groupe", unname(pal_ens["groupe"]), unname(pal_ens["detail"]))
      )
      tp$Col <- col_vec

      categoryarray <- rev(tp$Libelle)

      tp$TickLabel <- tp$Libelle
      tp$TickLabel[tp$Mode == "ensemble des cas"] <- ""

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
          title = paste("Répartition par type de prise —", focus),
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
    # Comme pour l'origine/le type, DEUX barres par facteur : bordeaux = « par
    # rapport au focus », orange = « par rapport à l'ensemble des cas » (voir
    # facteur_values()). Tous les facteurs sont de niveau "detail" (pas de
    # hiérarchie).
    output$plot_facteur_ui <- shiny::renderUI({
      plotly::plotlyOutput(ns("plot_facteur"), height = facteur_height())
    })

    output$plot_facteur <- plotly::renderPlotly(
      {
        focus <- selected_focus()
        fv <- facteur_plot_data()
        if (is.null(focus) || is.null(fv) || nrow(fv) == 0) {
          return(plotly::plotly_empty())
        }

        pal_mol <- age_colors()
        pal_ens <- age_colors_ensemble()

        col_vec <- ifelse(
          fv$Mode == "molécule",
          ifelse(fv$Type == "groupe", unname(pal_mol["groupe"]), unname(pal_mol["detail"])),
          ifelse(fv$Type == "groupe", unname(pal_ens["groupe"]), unname(pal_ens["detail"]))
        )
        fv$Col <- col_vec

        categoryarray <- rev(fv$Libelle)

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
            title = paste("Répartition par facteur —", focus),
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
      focus_df = focus_df,
      focus_data = focus_data,
      principaux_facteurs = principaux_facteurs,
      selected_focus = selected_focus,
      facteur_values = facteur_values,
      facteur_plot_data = facteur_plot_data,
      facteur_height = facteur_height
    )

  })
}

