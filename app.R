# ============================================================================
# app.R — Application Shiny « Visualisation enquête mésusage pharmaceutique »
# ============================================================================
# Cette application présente le registre des médicaments (module MEDOC_REG).
# Les données sont lues depuis les fichiers Excel du répertoire data/ (via le
# helper read_medoc_reg() de R/read_data.R), conformément aux directives en vigueur.
#
# Les fichiers du répertoire R/ sont automatiquement chargés par Shiny (≥ 1.5.0),
# c'est pourquoi aucune instruction source() n'est nécessaire ici.
#
# Lancement (depuis la racine du projet) :
#   shiny::runApp()
# ============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinycssloaders)
})

# --- Interface utilisateur ----------------------------------------------------
ui <- page_navbar(
  # Pas de titre global : les onglets sont ainsi plus visibles et détachés.
  # (un éventuel en-tête de marque peut être réintroduit si besoin)
  title = NULL,
  id = "main_navbar",
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#18bc9c"
  ),
  # Feuille de style personnalisée
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  nav_panel(
    "Accueil",
    value = "accueil",
    card(
      card_header("Visualisation des données de l'enquête mésusage pharmaciens"),
      p("Sélectionnez un écran ci-dessous pour accéder aux données de l'enquête."),
      # --- RACCOURCIS VERS LES ÉCRANS ----------------------------------------
      # Chaque écran du registre correspond à une carte (colonne) cliquable.
      # Pour l'instant une seule source existe (MEDOC_REG) ; les futurs fichiers
      # Excel seront ajoutés ici, chacun dans sa propre colonne fluidRow/column.
      fluidRow(
        column(
          width = 6,
          tags$div(
            class = "shortcut-card",
            icon("file-medical"),
            h4("Caractéristiques des mésusages par molécule "),
            p("Tableau des substances (DCI) et statistiques par genre des patients."),
            actionButton("go_medoc", "Accéder aux caractéristiques des mésusages par molécule",
                         class = "btn btn-primary btn-block")
          )
        ),
        column(
          width = 6,
          tags$div(
            class = "shortcut-card",
            icon("table-list"),
            h4("Caractéristiques des mésusages par code ATC "),
            p("Tableau des libellés ATC et codes ATC, et statistiques par genre des patients."),
            actionButton("go_lib_code_atc", "Accéder aux caractéristiques des mésusages par code ATC",
                         class = "btn btn-primary btn-block")
          )
        ),
        column(
          width = 6,
          tags$div(
            class = "shortcut-card",
            icon("bullseye"),
            h4("Focus IPP, Laxatif et Corticoïdes "),
            p("Tableau des focus (Focus IPP, Focus Laxatif, Focus Corticoïdes) et statistiques associées."),
            actionButton("go_focus", "Accéder aux focus IPP / Laxatif / Corticoïdes",
                         class = "btn btn-primary btn-block")
          )
        )
      )
    )
  ),

  nav_panel(
    "MEDOC_REG",
    value = "medoc",
    mod_medoc_reg_ui("medoc")
  ),

  nav_panel(
    "LIB_CODE_ATC",
    value = "lib_code_atc",
    mod_lib_code_atc_ui("lib_code_atc")
  ),

  nav_panel(
    "FOCUS_IPP_LAXA_CORTICO",
    value = "focus_ipp_laxa_cortico",
    mod_focus_ipp_laxa_cortico_ui("focus_ipp_laxa_cortico")
  )
)

# --- Serveur ------------------------------------------------------------------
server <- function(input, output, session) {
  # Raccourci de la page d'accueil : ouvre l'écran MEDOC_REG.
  observeEvent(input$go_medoc, {
    bslib::nav_select("main_navbar", selected = "medoc")
  })

  # Raccourci de la page d'accueil : ouvre l'écran LIB_CODE_ATC.
  observeEvent(input$go_lib_code_atc, {
    bslib::nav_select("main_navbar", selected = "lib_code_atc")
  })

  # Raccourci de la page d'accueil : ouvre l'écran FOCUS_IPP_LAXA_CORTICO.
  observeEvent(input$go_focus, {
    bslib::nav_select("main_navbar", selected = "focus_ipp_laxa_cortico")
  })

  # Le module MEDOC_REG charge les données depuis data/MEDOC_REG.xlsx.
  mod_medoc_reg_server("medoc")

  # Le module LIB_CODE_ATC charge les données depuis data/LIB_CODE_ATC_OXOMEMAZINE.xlsx.
  mod_lib_code_atc_server("lib_code_atc")

  # Le module FOCUS charge les données depuis data/FOCUS_IPP_LAXA_CORTICO.xlsx.
  mod_focus_ipp_laxa_cortico_server("focus_ipp_laxa_cortico")
}

shiny::shinyApp(ui = ui, server = server)
