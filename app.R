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
            h4("Registre des médicaments"),
            p("Tableau des substances (DCI) et statistiques par genre des patients."),
            actionButton("go_medoc", "Accéder au registre MEDOC_REG",
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
  )
)

# --- Serveur ------------------------------------------------------------------
server <- function(input, output, session) {
  # Raccourci de la page d'accueil : ouvre l'écran MEDOC_REG.
  observeEvent(input$go_medoc, {
    bslib::nav_select("main_navbar", selected = "medoc")
  })

  # Le module MEDOC_REG charge les données depuis data/MEDOC_REG.xlsx.
  mod_medoc_reg_server("medoc")
}

shiny::shinyApp(ui = ui, server = server)
