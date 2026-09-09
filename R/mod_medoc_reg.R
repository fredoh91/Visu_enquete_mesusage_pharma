# ============================================================================
# mod_medoc_reg.R — Module Shiny « Registre des médicaments »
# Affiche un tableau des substances (DCI) issues de MEDOC_REG.xlsx, puis, au
# clic sur une ligne, un camembert interactif (plotly) du genre des patients
# (Homme / Femme / Autre) pour la DCI sélectionnée.
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
        # Carte 1 : camembert du genre des patients
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_genre"), height = "320px")
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

    # --- Données du camembert (genre des patients) --------------------------
    genre_values <- reactive({
      row <- selected_genre()
      req(row)

      # Les colonnes du genre sont lues comme caractères (avec d'éventuelles
      # valeurs NA). On les convertit en nombres : NA et 0 sont écartés afin de
      # ne pas afficher de segments vides dans le camembert.
      labels <- c("Homme", "Femme", "Autre")
      raw <- c(as.character(row$Homme), as.character(row$Femme),
               as.character(row$`Autre...7`))
      values <- suppressWarnings(as.numeric(raw))
      values[is.na(values)] <- 0

      keep <- values > 0
      data.frame(Genre = labels[keep], Effectif = values[keep])
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
            "MEDOC_REG importé :",
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
        strong(paste("Genre des patients —", dci))
      )
    })

    # --- Camembert du genre des patients (partie basse) ----------------------
    output$plot_genre <- plotly::renderPlotly({
      dci <- selected_dci()
      gv <- genre_values()
      if (is.null(dci) || is.null(gv) || nrow(gv) == 0) {
        # Pas encore de camembert : on renvoie un graphique quasi vide.
        return(plotly::plotly_empty(type = "pie"))
      }

      plotly::plot_ly(
        data = gv,
        labels = ~Genre,
        values = ~Effectif,
        type = "pie",
        hole = 0.4,
        textinfo = "label+percent",
        textposition = "outside",
        insidetextorientation = "horizontal",
        hovertemplate = "%{label}: %{value}<br>%{percent}<extra></extra>",
        marker = list(colors = c("#18bc9c", "#f39c12", "#7f8c8d"))
      ) %>%
        plotly::layout(
          title = paste("Répartition par genre —", dci),
          showlegend = TRUE,
          margin = list(l = 20, r = 20, t = 50, b = 20)
        )
    })
  })
}

