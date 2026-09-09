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
  title = "Enquête mésusage pharmaciens",
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
    card(
      card_header("Bienvenue"),
      # --- MESSAGE DE TEST (validation de l'étape 1) --------------------------
      # Ce paragraphe doit s'afficher dans le navigateur : il atteste que la
      # structure du projet et les packages sont correctement en place.
      div(
        class = "message-test",
        h3("Test OK : la structure de l'application est en place !"),
        p("Félicitations, si vous lisez ce message dans votre navigateur,
          l'architecture modulaire fonctionne. Prochaine étape :
          raccorder le module MEDOC_REG.")
      )
    )
  ),

  nav_panel(
    "Registre des médicaments",
    mod_medoc_reg_ui("medoc")
  )
)

# --- Serveur ------------------------------------------------------------------
server <- function(input, output, session) {
  # Le module MEDOC_REG charge les données depuis data/MEDOC_REG.xlsx.
  mod_medoc_reg_server("medoc")
}

shiny::shinyApp(ui = ui, server = server)
