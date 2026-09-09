# ============================================================================
# mod_medoc_reg.R — Module Shiny « Registre des médicaments »
# Charge les données de l'enquête depuis le fichier Excel data/MEDOC_REG.xlsx.
# ============================================================================
# Conformément aux directives en vigueur, les données proviennent de fichiers
# Excel placés dans le répertoire data/. Ce module importe le contenu de
# MEDOC_REG.xlsx dans une dataframe via le helper read_medoc_reg() de R/read_data.R.
#
# Pour le moment, l'import dans une dataframe est en place mais l'affichage
# détaillé (tableau, graphiques) sera développé dans une étape ultérieure.

library(shiny)
library(dplyr)

# --- UI du module -------------------------------------------------------------
#' UI du module Registre des médicaments.
#' @param id Identifiant unique du module.
mod_medoc_reg_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Le contenu (tableau, filtres, graphiques) sera ajouté dans une étape
    # ultérieure du développement.
    uiOutput(ns("etat"))
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
  })
}
