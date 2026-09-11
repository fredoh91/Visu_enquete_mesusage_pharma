# ============================================================================
# mod_medoc_reg.R — Module Shiny « Registre des médicaments »
# Affiche un tableau des substances (DCI) issues de MEDOC_REG.xlsx, puis, au
# clic sur une ligne, plusieurs graphiques interactifs (plotly) pour la DCI
# sélectionnée : deux camemberts (genre des patients : Homme / Femme / Autre,
# et données "enceinte" : Oui / Non / Non renseigné) ainsi que des séries de
# barres horizontales (âges, origine du mésusage, type de mésusage).
# ============================================================================
# Conformément aux directives en vigueur, les données proviennent de fichiers
# Excel placés dans le répertoire data/. Ce module importe le contenu de
# MEDOC_REG.xlsx via le helper read_medoc_reg() de R/read_data.R.
#
# NB : le package {plotly} est utilisé pour le camembert interactif (survol des
# valeurs et pourcentages). Il s'agit d'un ajout de package, justifié par le
# besoin d'un graphique interactif moderne, conformément aux directives.

library(shiny)
library(dplyr)
library(DT)

# --- UI du module -------------------------------------------------------------
#' UI du module Registre des médicaments.
#'
#' L'écran est organisé en deux grandes zones :
#'   * Zone supérieure (.zone-tableau)  : le tableau des DCI.
#'   * Zone inférieure (.zone-graphiques) : les graphiques relatifs à la DCI
#'     sélectionnée. Chaque graphique vit dans sa propre carte (.graph-card),
#'     disposée de façon responsive via la grille Bootstrap 5 : selon la
#'     largeur d'écran, on affiche 1 (col-12), 2 (col-md-6) ou 3 (col-xl-4)
#'     graphiques par ligne.
#'
#' @param id Identifiant unique du module.
mod_medoc_reg_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # --- Zone supérieure : tableau des DCI ------------------------------------
    tags$div(
      class = "zone-tableau",
      uiOutput(ns("etat")),
      DTOutput(ns("table_dci"))
    ),

    # --- Zone inférieure : graphiques (responsive) ----------------------------
    tags$div(
      class = "zone-graphiques",
      tags$h4("Détail de la DCI sélectionnée"),
      uiOutput(ns("plot_msg")),

      fluidRow(
        # Carte 1 : camembert du genre des patients (Homme / Femme / Autre)
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_genre"), height = "320px")
          )
        ),

        # Carte 2 : camembert des données "enceinte" (Oui / Non / Non renseigné)
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_enceinte"), height = "320px")
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
            plotly::plotlyOutput(ns("plot_origine"), height = "320px")
          )
        ),

        # Carte 5 : barres horizontales du type de mésusage (2 niveaux hiérarchiques)
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_type"), height = "420px")
          )
        )
        # Les prochains graphiques seront ajoutés ici, chacun dans son propre
        # bloc "col-12 col-md-6 col-xl-4" + "graph-card".
      )
    )
  )
}



# --- Server du module ---------------------------------------------------------
#' Server du module Registre des médicaments.
#' @param id Identifiant unique du module (doit correspondre à l'UI).
mod_medoc_reg_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Données brutes importées depuis le fichier Excel -------------------
    # La dataframe MEDOC_REG est chargée une seule fois au démarrage du module.
    medoc_reg <- reactive({
      tryCatch(
        read_medoc_reg(),
        error = function(e) NULL
      )
    })

    # --- Dataframe des DCI (affichage basique) ------------------------------
    # Ne conserve que les lignes du type "effectif" et exclut la ligne "Total"
    # qui correspond au périmètre global (pas à une substance). Chaque DCI est
    # ainsi présenté avec son effectif total (colonne "Total" du fichier),
    # trié par effectif décroissant.
    dci_df <- reactive({
      data <- medoc_reg()
      req(data)
      data %>%
        dplyr::filter(.data$Type_donnee == "effectif") %>%
        dplyr::filter(.data$DCI != "Total") %>%
        dplyr::select(DCI, Total) %>%
        dplyr::arrange(dplyr::desc(.data$Total))
    })

    # --- DCI sélectionnée dans le tableau -----------------------------------
    # input$table_dci_rows_selected : index de l'unique ligne sélectionnée
    # (NULL tant qu'aucune ligne n'est cliquée, car DT est en mode "single").
    selected_dci <- reactive({
      idx <- input$table_dci_rows_selected
      if (is.null(idx) || length(idx) == 0 || idx < 1 || idx > nrow(dci_df())) {
        return(NULL)
      }
      dci_df()$DCI[idx]
    })

    # --- Ligne "effectif" de la DCI sélectionnée ----------------------------
    # Permet de récupérer les effectifs du genre (colonnes Homme / Femme / Autre).
    selected_genre <- reactive({
      dci <- selected_dci()
      data <- medoc_reg()
      req(dci, data)

      row <- data %>%
        dplyr::filter(.data$DCI == dci, .data$Type_donnee == "effectif")

      if (nrow(row) != 1) {
        return(NULL)
      }
      row[1, ]
    })

    # --- Données du camembert du genre des patients ----------------------------
    # Construit la dataframe nécessaire au camembert 1 : uniquement la
    # répartition du genre (Homme / Femme / Autre). Les colonnes sont lues par
    # index (robustesse face aux libellés avec espaces insécables) : genre =
    # 5 (Homme), 6 (Femme), 7 (Autre). Les éventuelles NA sont traitées comme
    # 0 et les segments d'effectif nul sont écartés.
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
      # Pourcentage sur le total "enceinte" (Non + Oui + Non renseigné), formaté
      # avec la virgule décimale française.
      df$Pct <- df$Effectif / total * 100
      df$Pct_fr <- formatC(df$Pct, format = "f", digits = 1,
                           big.mark = " ", decimal.mark = ",")

      # On écarte les segments d'effectif nul (pas de secteur vide).
      df[df$Effectif > 0, ]
    })

    # --- Données des barres d'âge (données S2 — âge du patient) --------------
    # Lecture depuis la ligne "effectif" de la DCI sélectionnée. Les colonnes
    # concernées vont de K à T (S2. Âge) ; on accède par index numérique car
    # certains libellés contiennent des espaces insécables (ex. "nourrisson
    # (0-23 mois)"). La colonne "ST JEUNES, ENFANTS" (qui chevauche enfants et
    # adolescents) est volontairement ignorée.
    #
    # Chaque libellé est représenté par DEUX barres, correspondant à deux modes
    # de calcul des pourcentages :
    #   * mode "molécule"        : pourcentage = effectif / Total molécule * 100,
    #     où le Total (colonne D, index 4) provient de la ligne "effectif" de la
    #     DCI sélectionnée ;
    #   * mode "ensemble des cas" : pourcentage = effectif / Total enquête * 100,
    #     où le Total (colonne D) provient de la ligne "effectif" de la ligne
    #     agrégée "Total" du périmètre (l'ensemble des cas de l'enquête).
    #
    # Les deux barres d'un même libellé sont affichées l'une juste en dessous de
    # l'autre, avec des couleurs différentes (voir plot_age ci-dessous).
    age_values <- reactive({
      dci <- selected_dci()
      req(dci)
      data <- medoc_reg()
      req(data)

      row <- data %>%
        dplyr::filter(.data$DCI == dci, .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      # Total de la molécule concernée (colonne D de la ligne "effectif" de la
      # DCI sélectionnée).
      total_mol <- suppressWarnings(as.numeric(row[[4]]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      # Total de l'ensemble des cas (colonne D de la ligne "effectif" de la
      # ligne agrégée "Total" du périmètre). En cas d'absence, on retombe sur
      # le total de la molécule (les deux pourcentages seraient alors égaux).
      row_total <- data %>%
        dplyr::filter(.data$DCI == "Total", .data$Type_donnee == "effectif")
      total_ens <- if (nrow(row_total) >= 1) {
        suppressWarnings(as.numeric(row_total[[4]][1]))
      } else {
        NA
      }
      if (is.na(total_ens) || total_ens <= 0) {
        total_ens <- total_mol
      }

      # Définition hiérarchique : (libellé, index colonne, niveau)
      # Ordre d'affichage de haut en bas.
      defs <- data.frame(
        Libelle_brut = c(
          "ENFANTS, ADOLESCENTS",                 # groupe (niveau 1)
          "Nouveau né ou nourrisson (0-23 mois)",
          "Entre 2 et 11 ans (enfant)",
          "Entre 12 et 17 ans (adolescent)",
          "ADULTES",                              # groupe (niveau 1)
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
      # Indentation des barres de 2e niveau pour matérialiser le
      # "léger décalage vers la droite" de la hiérarchie.
      Libelle_base <- ifelse(
        defs$Niveau == 1L,
        defs$Libelle_brut,
        paste0("    ", defs$Libelle_brut)
      )

      # Deux rangées par libellé, dans l'ordre d'affichage (haut → bas) :
      # d'abord la barre "molécule", puis juste en dessous celle "ensemble des
      # cas". Le caractère invisible (espace de largeur nulle \u200B) ajouté au
      # libellé de la seconde barre permet à Plotly de distinguer deux
      # catégories d'axe Y visuellement identiques.
      out <- do.call(rbind, lapply(seq_along(Libelle_base), function(k) {
        rbind(
          data.frame(
            Libelle     = Libelle_base[k],
            Libelle_brut = Libelle_base[k],
            Mode        = "molécule",
            Type        = Type[k],
            Effectif    = eff[k],
            Pct         = if (eff[k] > 0) eff[k] / total_mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle     = paste0(Libelle_base[k], "\u200B"),
            Libelle_brut = Libelle_base[k],
            Mode        = "ensemble des cas",
            Type        = Type[k],
            Effectif    = eff[k],
            Pct         = if (eff[k] > 0) eff[k] / total_ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Données des barres horizontales de l'origine du mésusage --------------
    # Lecture depuis la ligne "effectif" de la DCI sélectionnée. Les colonnes
    # concernées vont de U à W (index 21, 22, 23) :
    #   - Au moment de la prise du médicament
    #   - Au moment de la prescription médicale
    #   - Au moment de la dispensation en pharmacie
    # On accède par index numérique car certains libellés contiennent des
    # espaces insécables.
    #
    # Le pourcentage est RECALCULÉ sur l'effectif TOTAL des données "origine",
    # c'est-à-dire la somme des colonnes U + V + W (conformément aux directives).
    origine_values <- reactive({
      dci <- selected_dci()
      req(dci)
      data <- medoc_reg()
      req(data)

      row <- data %>%
        dplyr::filter(.data$DCI == dci, .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      eff <- vapply(21:23, function(i) suppressWarnings(as.numeric(row[[i]])), numeric(1))
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
    # Lecture depuis la ligne "effectif" de la DCI sélectionnée. Les colonnes
    # concernées vont de AN à AX (index 40 à 50) et se répartissent sur 2
    # niveaux hiérarchiques (comme le graphique des âges) :
    #   Niveau 1 « Posologie, Fréquence, Durée de traitement » (colonne AN=40) :
    #     - Schéma posologique non conforme        (AO=41)
    #     - Arrêt prématuré et injustifié du traitement (AP=42)
    #     - Prolongation de la durée du traitement (AQ=43)
    #     - Voie d'administration non conforme     (AR=44)
    #   Niveau 1 « Indication, population, contre-indications » (colonne AS=45) :
    #     - Utilisation pour une indication hors AMM          (AT=46)
    #     - Utilisation par une population non prévue         (AU=47)
    #     - Utilisation en présence de contre-indications     (AV=48)
    #     - Utilisation en présence d'une interaction interdite (AW=49)
    #     - Autre                                             (AX=50)
    #
    # Comme pour le graphique des âges, le pourcentage est RECALCULÉ sur le
    # Total (colonne D, index 4) de la ligne "effectif" de la DCI sélectionnée.
    type_values <- reactive({
      dci <- selected_dci()
      req(dci)
      data <- medoc_reg()
      req(data)

      row <- data %>%
        dplyr::filter(.data$DCI == dci, .data$Type_donnee == "effectif")
      if (nrow(row) != 1) {
        return(NULL)
      }
      row <- row[1, ]

      total <- suppressWarnings(as.numeric(row[[4]]))
      if (is.na(total) || total <= 0) {
        return(NULL)
      }

      # Définition hiérarchique : (libellé affiché, index colonne, niveau)
      # Ordre d'affichage de haut en bas.
      defs <- data.frame(
        Libelle_brut = c(
          "Posologie, Fréquence, Durée de traitement",  # groupe (niveau 1)
          "Schéma posologique non conforme",
          "Arrêt prématuré et injustifié du traitement",
          "Prolongation de la durée du traitement",
          "Voie d'administration non conforme à l'AMM",
          "Indication, population, contre-indications",  # groupe (niveau 1)
          "Utilisation pour une indication hors AMM",
          "Utilisation par une population non prévue par l'AMM",
          "Utilisation en présence de contre-indications connues",
          "Utilisation en présence d'une interaction médicamenteuse contre-indiquée",
          "Autre"
        ),
        Index = c(40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L),
        Niveau = c(1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L),
        stringsAsFactors = FALSE
      )

      eff <- vapply(defs$Index, function(i) suppressWarnings(as.numeric(row[[i]])), numeric(1))
      eff[is.na(eff)] <- 0

      pct <- eff / total * 100
      Type  <- ifelse(defs$Niveau == 1L, "groupe", "detail")
      # Indentation des barres de 2e niveau pour matérialiser le
      # "léger décalage vers la droite" de la hiérarchie.
      Libelle <- ifelse(
        defs$Niveau == 1L,
        defs$Libelle_brut,
        paste0("    ", defs$Libelle_brut)
      )

      data.frame(
        Libelle     = Libelle,
        Libelle_brut = defs$Libelle_brut,
        Niveau      = defs$Niveau,
        Type        = Type,
        Effectif    = eff,
        Pct         = pct,
        stringsAsFactors = FALSE
      )
    })

    # --- Message d'état (présence du fichier / données importées) ------------
    output$etat <- renderUI({
      data <- medoc_reg()
      if (is.null(data)) {
        return(
          div(class = "alert alert-warning",
              icon("triangle-exclamation"),
              "Fichier Excel MEDOC_REG introuvable ou illisible dans data/.")
        )
      }
      div(class = "alert alert-success",
          icon("file-excel"),
          strong(paste(
            "/!\\ /!\\ DEBUG /!\\ /!\\ : MEDOC_REG importé :",
            nrow(data), "lignes et", ncol(data), "colonnes."
          )))
    })

    # --- Tableau des DCI -----------------------------------------------------
    output$table_dci <- renderDT({
      df <- dci_df()
      if (is.null(df) || nrow(df) == 0) {
        return(
          DT::datatable(
            data.frame(Avertissement = "Aucune donnée DCI disponible."),
            options = list(dom = "t"), rownames = FALSE
          )
        )
      }
      DT::datatable(
        df,
        rownames = FALSE,
        colnames = c("Substance (DCI)" = "DCI", "Effectif total" = "Total"),
        # Une seule ligne sélectionnable à la fois (mode "single").
        selection = "single",
        options = list(
          pageLength = 15,
          language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json")
        )
      )
    })

    # --- Message / titre de la partie détail ---------------------------------
    output$plot_msg <- renderUI({
      dci <- selected_dci()
      if (is.null(dci)) {
        return(
          tags$p("Cliquez sur une DCI du tableau ci-dessus pour afficher le détail.")
        )
      }
      tags$p(
        class = "dci-title",
        icon("file-medical"),
        strong(paste("Caractéristiques de la DCI —", dci))
      )
    })

    # --- Camembert 1 : genre des patients (partie basse) ----------------------
    # Camembert simple : répartition Homme / Femme / Autre. Chaque secteur
    # affiche l'effectif et le pourcentage (entre parenthèses). L'infobulle
    # mentionne le libellé de la donnée, l'effectif et le pourcentage.
    output$plot_genre <- plotly::renderPlotly({
      dci <- selected_dci()
      gv <- genre_values()
      if (is.null(dci) || is.null(gv) || nrow(gv) == 0) {
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
          title = paste("Répartition par genre —", dci),
          margin = list(l = 20, r = 20, t = 50, b = 20)
        )
    })

    # --- Camembert 2 : données "enceinte" (partie basse) ----------------------
    # Camembert simple : répartition Oui / Non / Non renseigné en nuances de
    # vert. Le pourcentage est recalculé sur l'effectif TOTAL des données
    # "enceinte" (Non + Oui + Non renseigné), conformément aux directives.
    # Chaque secteur affiche l'effectif et le pourcentage (entre parenthèses).
    output$plot_enceinte <- plotly::renderPlotly({
      dci <- selected_dci()
      ev <- enceinte_values()
      if (is.null(dci) || is.null(ev) || nrow(ev) == 0) {
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
          title = paste("Répartition \"enceinte\" —", dci),
          margin = list(l = 20, r = 20, t = 50, b = 20)
        )
    })

    # --- Barres horizontales des âges (2 niveaux hiérarchiques) ---------------
    # Chaque libellé d'âge est représenté par DEUX barres superposées :
    #   * la première (bordeaux) exprime le pourcentage « par rapport à la
    #     molécule concernée » (dénominateur = total de la DCI sélectionnée) ;
    #   * la seconde (orange), juste en dessous, exprime le pourcentage « par
    #     rapport à l'ensemble des cas » (dénominateur = total de l'enquête).
    # Le dénominateur est géré dans age_values() ; ici on ne fait qu'afficher.
    # Les quatre combinaisons (Type : groupe/detail) × (Mode : molécule/ensemble
    # des cas) forment quatre traces distinctes afin d'obtenir une légende
    # lisible.
    output$plot_age <- plotly::renderPlotly({
      dci <- selected_dci()
      av <- age_values()
      if (is.null(dci) || is.null(av) || nrow(av) == 0) {
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
            # L'effectif est transmis séparément (customdata) pour être affiché
            # seul dans l'infobulle, sans le pourcentage.
            customdata = sub$Effectif,
            insidetextfont = list(color = "#ffffff"),
            hovertemplate = paste0(
              sub$Libelle_brut, "<br>Effectif : %{customdata}<br>Pourcentage : ",
              round(sub$Pct, 1), " %<extra></extra>"
            ),
            name = g$Nom,
            showlegend = TRUE
          )
      }

      p %>%
        plotly::layout(
          title = paste("Répartition par âge —", dci),
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
      dci <- selected_dci()
      ov <- origine_values()
      if (is.null(dci) || is.null(ov) || nrow(ov) == 0) {
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
          title = paste("Répartition par origine —", dci),
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
      dci <- selected_dci()
      tv <- type_values()
      if (is.null(dci) || is.null(tv) || nrow(tv) == 0) {
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
        customdata = tv$Effectif,
        insidetextfont = list(color = "#ffffff"),
        hovertemplate = paste0(
          "%{y}<br>Effectif : %{customdata}<br>Pourcentage : ",
          round(tv$Pct, 1), " %<extra></extra>"
        ),
        showlegend = FALSE
      ) %>%
        plotly::layout(
          title = paste("Répartition par type —", dci),
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

