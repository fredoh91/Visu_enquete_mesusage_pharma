# ============================================================================
# mod_age_patient.R — Module Shiny « AGE PATIENT(E) »
# Affiche un tableau des différentes tranches d'âge (colonne B de
# Age_patient.xlsx), puis, au clic sur une ligne, plusieurs graphiques
# interactifs (plotly) pour la tranche sélectionnée. Cet écran est largement
# analogue aux modules MEDOC_REG, LIB_CODE_ATC, FOCUS_IPP_LAXA_CORTICO,
# ORIGINE MÉSUSAGE et CLASSE ATC SOC (même organisation de l'écran et mêmes
# graphiques), à la différence près :
#   * la tranche d'âge étant le critère de sélection, le 3e graphique (âge)
#     est volontairement absent ;
#   * le tableau supérieur présente les tranches d'âge sur 2 niveaux, les
#     libellés de 1er niveau (« Total ENFANTS et ADOLESCENTS »,
#     « Total ADULTES ») étant affichés en gras.
# ============================================================================
# Les données proviennent de data/Age_patient.xlsx via le helper
# read_age_patient() de R/read_data.R.
#
# NB IMPORTANT : la structure de ce fichier est, pour les colonnes de données,
# IDENTIQUE à celle de MEDOC_REG.xlsx / FOCUS_IPP_LAXA_CORTICO.xlsx (mêmes
# indices de colonnes) :
#   - genre      : colonnes 5, 6, 7
#   - enceinte   : colonnes 8, 9, 10
#   - âge        : colonnes 11, 13-20 (critère de sélection de cet onglet)
#   - origine    : colonnes 21, 22, 23
#   - type de prise : colonnes X -> AD (24 à 30)
#   - type       : colonnes 40 à 50
# La colonne B (index 2) porte la tranche d'âge.
#
# Pour le 6e graphique (facteurs), les données proviennent de
# principaux_facteurs.xlsx, aux colonnes K -> T (indices 11 à 20) du fichier
# transposé : la liaison se fait par l'en-tête de ligne 2, qui vaut exactement
# le libellé de la tranche d'âge (comparaison « normalisée » pour être robuste
# aux espaces/ponctuations).
#
# Pour le 8e graphique (répartition par classe ATC), les données proviennent de
# CLASSE_ATC_SOC.xlsx, lu avec read_classe_atc_soc() (skip = 1) au même moment
# que Age_patient.xlsx : les tranches d'âge y sont les NOMS des colonnes 11 à 20
# (K -> T). La liaison se fait donc par comparaison normalisée entre le libellé
# brut de la tranche sélectionnée et ces noms de colonnes. Chaque classe ATC est
# une ligne identifiée par la colonne B (« Libellé SOC »), avec 3 lignes
# successives selon la colonne « Type_donnee » (effectif / pourcentage /
# significativite). Le camembert affiche les valeurs de la ligne « pourcentage »
# (proportions multipliées par 100) dans la colonne de la tranche choisie.

library(shiny)
library(dplyr)
library(DT)

# --- Correspondances entre libellés affichés et libellés bruts (colonne B) -----
# Chaque choix de la partie supérieure correspond à une tranche d'âge du
# fichier Age_patient.xlsx (colonne B). La table ci-dessous associe :
#   * le libellé À AFFICHER dans l'application (court, sans « ST ») ;
#   * le libellé BRUT (colonne B de Age_patient.xlsx, utilisé pour filtrer) ;
#   * le niveau hiérarchique (1 = groupe affiché en gras, 2 = détail).
# L'ordre des lignes définit l'ordre d'affichage de haut en bas.
age_choices <- data.frame(
  Libelle_affichage = c(
    "Total ENFANTS et ADOLESCENTS",   # niveau 1 (gras)
    "Nouveau né ou nourrisson (0-23 mois)",
    "Entre 2 et 11 ans (enfant)",
    "Entre 12 et 17 ans (adolescent)",
    "Total JEUNES ENFANTS",           # niveau 1 (gras)
    "Total ADULTES",                  # niveau 1 (gras)
    "Entre 18 et 34 ans",
    "Entre 35 et 49 ans",
    "Entre 50 et 64 ans",
    "65 ans et plus"
  ),
  Libelle_brut = c(
    "ST ENFANTS, ADOLESCENTS",
    "Nouveau né ou nourrisson (0-23 mois)",
    "Entre 2 et 11 ans (enfant)",
    "Entre 12 et 17 ans (adolescent)",
    "ST JEUNES, ENFANTS",
    "ST ADULTES",
    "Entre 18 et 34 ans",
    "Entre 35 et 49 ans",
    "Entre 50 et 64 ans",
    "65 ans et plus"
  ),
  Niveau = c(1L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L),
  stringsAsFactors = FALSE
)

#' Libellé d'affichage d'une tranche d'âge à partir du libellé BRUT (colonne B).
#' @param libelle_brut Libellé brut (colonne B d'Age_patient.xlsx).
#' @return Le libellé court à afficher dans l'application.
age_libelle_affichage <- function(libelle_brut) {
  brut <- trimws(as.character(libelle_brut))
  hit <- which(age_choices$Libelle_brut == brut)
  if (length(hit) >= 1) {
    return(age_choices$Libelle_affichage[hit[1]])
  }
  brut
}

# --- Normalisation d'une chaîne pour les comparaisons robustes -----------------
#' Normalise une chaîne pour comparer des libellés malgré les variantes
#' d'espaces (insécables) et de ponctuation (ex. « ST JEUNES, ENFANTS » vs
#' « ST JEUNES ENFANTS »).
#' @param x Chaîne à normaliser.
#' @return La chaîne en minuscules, sans espaces ni ponctuation (accents gardés).
age_normalize_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  gsub("[^a-z0-9àâäéèêëîïôöùûüçœ]", "", x)
}
# --- UI du module -------------------------------------------------------------
#' UI du module AGE PATIENT(E).
#'
#' L'écran est organisé en deux grandes zones (comme MEDOC_REG / LIB_CODE_ATC
#' / FOCUS_IPP_LAXA_CORTICO / ORIGINE MÉSUSAGE) :
#'   * Zone supérieure (.zone-tableau)    : le tableau des tranches d'âge.
#'   * Zone inférieure (.zone-graphiques) : les graphiques relatifs à la tranche
#'     sélectionnée, chacun dans sa propre carte (.graph-card), disposée de
#'     façon responsive via la grille Bootstrap 5 (col-12 / col-md-6 /
#'     col-xl-4). Contrairement aux onglets précédents, le 3e graphique (âge)
#'     est volontairement absent, la tranche d'âge étant le critère de
#'     sélection du tableau supérieur.
#'
#' @param id Identifiant unique du module.
mod_age_patient_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # --- Zone supérieure : tableau des tranches d'âge --------------------------
    tags$div(
      class = "zone-tableau",
      uiOutput(ns("etat")),
      DTOutput(ns("table_age"))
    ),

    # --- Zone inférieure : graphiques (responsive) ----------------------------
    tags$div(
      class = "zone-graphiques",
      tags$h4("Détail de la tranche d'âge sélectionnée"),
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

        # Carte 4 : barres horizontales des données "origine"
        # (Au moment de la prise / de la prescription / de la dispensation)
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
        # chargé en parallèle de Age_patient.xlsx. La liaison se fait par le
        # libellé de la tranche d'âge (en-têtes de ligne 2 des colonnes K -> T =
        # indices 11:20).
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            # Contrôle de dépliage : par défaut seuls le nombre paramétré de
            # facteurs (n_facteurs, par défaut 10) aux plus grands pourcentages
            # sont affichés ; on coche pour déplier.
            shiny::checkboxInput(
              ns("facteur_afficher_tous"),
              label = "Afficher les 29 facteurs (déplier)",
              value = FALSE
            ),
            # La hauteur est pilotée côté serveur (renderUI + facteur_height()) :
            # compacte pour la vue "n_facteurs premiers", agrandie après dépliage.
            shiny::uiOutput(ns("plot_facteur_ui"))
          )
        ),

        # Carte 7 : barres horizontales du type de prise (2 niveaux hiérarchiques).
        # Les données proviennent des colonnes X -> AD de
        # Age_patient.xlsx (24 à 30), comme dans les onglets précédents.
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_type_prise"), height = "640px")
          )
        ),

        # Carte 8 : camembert de la répartition des classes ATC.
        # Les données proviennent de CLASSE_ATC_SOC.xlsx (chargé au même moment
        # que Age_patient.xlsx) : chaque classe ATC est une ligne (colonne B),
        # la liaison avec la tranche d'âge sélectionnée se fait par la colonne
        # correspondante (en-têtes de ligne 2 des colonnes K -> T = indices 11 à
        # 20). Le camembert affiche les pourcentages (ligne "pourcentage") et
        # les libellés des classes ATC ; une infobulle indique libellé, effectif
        # et pourcentage. La hauteur est volontairement supérieure aux autres
        # camemberts pour caser les 14 libellés ATC dans les secteurs.
        tags$div(
          class = "col-12 col-md-6 col-xl-4",
          tags$div(
            class = "graph-card",
            plotly::plotlyOutput(ns("plot_atc"), height = "720px")
          )
        )
      )
    )
  )
}

# --- Serveur du module ---------------------------------------------------------
#' Serveur du module AGE PATIENT(E).
#' @param id Identifiant unique du module (doit correspondre à l'UI).
#' @param n_facteurs Nombre de facteurs de mésusage affichés par défaut dans le
#'   6e graphique (barres horizontales des facteurs) avant dépliage. Chaque
#'   facteur génère 2 barres (molécule + ensemble des cas), donc le nombre de
#'   lignes tracées par défaut vaut \code{n_facteurs * 2}. Paramétrable :
#'   \code{mod_age_patient_server(\"age\", theme, n_facteurs = 10)}.
mod_age_patient_server <- function(id, theme, n_facteurs = 10) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Données brutes importées depuis le fichier Excel -------------------
    # La dataframe Age_patient est chargée une seule fois au démarrage du
    # module. Une colonne "Age" (libellé BRUT de la tranche d'âge, présent en
    # colonne B / index 2 du fichier) est ajoutée pour identifier chaque ligne
    # de façon fiable.
    age_data <- reactive({
      data <- tryCatch(
        read_age_patient(),
        error = function(e) NULL
      )
      if (is.null(data)) {
        return(NULL)
      }
      # La colonne B (index 2) porte le libellé brut de la tranche d'âge.
      data[["Age"]] <- trimws(as.character(data[[2]]))
      data
    })

    # --- Données brutes des facteurs (principaux_facteurs.xlsx) -------------
    # Chargé au même moment que Age_patient.xlsx pour alimenter le 6e graphique
    # de l'onglet (barres horizontales des facteurs de mésusage). Fichier
    # transposé (col_names = FALSE) : chaque ligne = un facteur ; chaque tranche
    # d'âge occupe une colonne dédiée (K..T = indices 11:20) dont l'en-tête de
    # ligne 2 porte exactement le libellé de la tranche d'âge.
    principaux_facteurs <- reactive({
      tryCatch(
        read_principaux_facteurs(),
        error = function(e) NULL
      )
    })

    # --- Données brutes des classes ATC (CLASSE_ATC_SOC.xlsx) ----------------
    # Chargé au même moment que Age_patient.xlsx pour alimenter le 8e graphique
    # de l'onglet (camembert des classes ATC). Fichier transposé traité par
    # read_classe_atc_soc() (skip = 1) : la ligne 2 du fichier devient l'en-tête
    # de colonnes, de sorte que les tranches d'âge (K..T = indices 11:20) sont
    # directement des NOMS de colonnes portant exactement le libellé de la
    # tranche d'âge — la liaison se fait donc par comparaison normalisée des noms
    # de colonnes 11:20 avec la tranche sélectionnée. Chaque classe ATC occupe
    # une ligne identifiée par la colonne B ("Libellé SOC") avec 3 lignes
    # successives (effectif / pourcentage / significativite).
    classe_atc_soc <- reactive({
      tryCatch(
        read_classe_atc_soc(),
        error = function(e) NULL
      )
    })

    # --- Dataframe des tranches d'âge (partie supérieure) -------------------
    # Construit le tableau des choix disponibles (colonne B). Chaque choix est
    # établi à partir de la table age_choices (correspondances + hiérarchie) ;
    # l'effectif total (colonne D) est récupéré de la ligne "effectif" de la
    # tranche correspondante dans le fichier.
    age_df <- reactive({
      data <- age_data()
      req(data)
      req(nrow(age_choices) > 0)

      eff <- vapply(age_choices$Libelle_brut, function(brut) {
        # Comparaison normalisée (robuste aux espaces insécables et aux
        # variantes de ponctuation) : le libellé brut du cahier des charges
        # peut différer légèrement de celui du fichier (ex. « ST JEUNES,
        # ENFANTS » vs « ST JEUNES ENFANTS », espaces insécables).
        key <- age_normalize_key(brut)
        idx <- which(age_normalize_key(data$Age) == key &
                       data$Type_donnee == "effectif")
        if (length(idx) >= 1) {
          v <- suppressWarnings(as.numeric(data[[4]][idx[1]]))
          ifelse(is.na(v), 0, v)
        } else {
          0
        }
      }, numeric(1))

      data.frame(
        Libelle_affichage = age_choices$Libelle_affichage,
        Libelle_brut      = age_choices$Libelle_brut,
        Niveau            = age_choices$Niveau,
        Total             = eff,
        stringsAsFactors  = FALSE
      )
    })

    # --- Tranche d'âge sélectionnée dans le tableau -------------------------
    # input$table_age_rows_selected : index de l'unique ligne sélectionnée
    # (NULL tant qu'aucune ligne n'est cliquée, car DT est en mode "single").
    # Renvoie le libellé BRUT de la tranche (utilisé pour tous les filtres).
    selected_age <- reactive({
      idx <- input$table_age_rows_selected
      if (is.null(idx) || length(idx) == 0 || idx < 1 || idx > nrow(age_df())) {
        return(NULL)
      }
      age_df()$Libelle_brut[idx]
    })

    # Libellé COURT (affichage) de la tranche sélectionnée (pour les titres).
    selected_age_court <- reactive({
      age <- selected_age()
      if (is.null(age)) {
        return(NULL)
      }
      age_libelle_affichage(age)
    })

    # --- Ligne "effectif" de la tranche d'âge sélectionnée ------------------
    # Permet de récupérer les effectifs du genre (col 5-7), de "enceinte"
    # (col 8-10), de l'origine (col 21-23), du type de prise (col 24-30) et du
    # type (col 40-50) pour la tranche sélectionnée.
    selected_age_row <- reactive({
      age <- selected_age()
      data <- age_data()
      req(age, data)

      row <- data[age_normalize_key(data$Age) == age_normalize_key(age) &
                    data$Type_donnee == "effectif", , drop = FALSE]

      if (nrow(row) != 1) {
        return(NULL)
      }
      row[1, ]
    })

    # --- Effectif total de la tranche (colonne D) et de l'ensemble des cas ----
    # total_mol : colonne D de la ligne "effectif" de la tranche sélectionnée.
    # total_ens : colonne D de la ligne agrégée "Total" (périmètre global).
    age_totals <- reactive({
      age <- selected_age()
      data <- age_data()
      req(age, data)

      row <- data[age_normalize_key(data$Age) == age_normalize_key(age) &
                    data$Type_donnee == "effectif", , drop = FALSE]
      if (nrow(row) != 1) {
        return(NULL)
      }
      total_mol <- suppressWarnings(as.numeric(row[[4]][1]))
      if (is.na(total_mol) || total_mol <= 0) {
        return(NULL)
      }

      row_total <- data[age_normalize_key(data$Age) == "total" &
                          data$Type_donnee == "effectif", , drop = FALSE]
      total_ens <- if (nrow(row_total) >= 1) {
        suppressWarnings(as.numeric(row_total[[4]][1]))
      } else {
        NA
      }
      if (is.na(total_ens) || total_ens <= 0) {
        total_ens <- total_mol
      }

      list(total_mol = total_mol, total_ens = total_ens)
    })

    # --- Données du camembert du genre des patients --------------------------
    # Construit la dataframe nécessaire au camembert 1 : uniquement la
    # répartition du genre (Homme / Femme / Autre). Les colonnes sont lues par
    # index : genre = 5 (Homme), 6 (Femme), 7 (Autre). Les éventuelles NA sont
    # traitées comme 0 et les segments d'effectif nul sont écartés.
    #
    # Le pourcentage est recalculé sur le total Homme + Femme + Autre.
    genre_values <- reactive({
      row <- selected_age_row()
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
      row <- selected_age_row()
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

    # --- Données des barres horizontales de l'origine du mésusage --------------
    # Lecture depuis la ligne "effectif" de la tranche d'âge sélectionnée. Les
    # colonnes concernées vont de U à W (index 21, 22, 23) :
    #   - Au moment de la prise du médicament
    #   - Au moment de la prescription médicale
    #   - Au moment de la dispensation en pharmacie
    # On accède par index numérique car certains libellés contiennent des
    # espaces insécables.
    #
    # Comme pour les onglets précédents, DEUX modes de calcul des pourcentages
    # sont produits pour chaque libellé :
    #   * mode "molécule"         : dénominateur = effectif total de la tranche
    #     d'âge sélectionnée (colonne D de la ligne "effectif") ;
    #   * mode "ensemble des cas" : dénominateur = effectif total de l'ensemble
    #     de l'enquête (colonne D de la ligne agrégée "Total").
    # Cela génère donc 2 rangées par modalité d'origine (toutes de niveau
    # "detail", sans hiérarchie).
    origine_values <- reactive({
      age <- selected_age()
      req(age)
      row <- selected_age_row()
      req(row)
      tot <- age_totals()
      req(tot)

      eff <- vapply(21:23, function(i) suppressWarnings(as.numeric(row[[i]])), numeric(1))
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
            Pct          = if (eff[k] > 0) eff[k] / tot$total_mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(Libelle_brut[k], "\u200B"),
            Libelle_brut = Libelle_brut[k],
            Mode         = "ensemble des cas",
            Type         = Type[k],
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / tot$total_ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Données des barres horizontales du type de mésusage -------------------
    # Lecture depuis la ligne "effectif" de la tranche d'âge sélectionnée. Les
    # colonnes concernées vont de AN à AX (index 40 à 50) et se répartissent
    # sur 2 niveaux hiérarchiques (comme les graphiques du type précédents) :
    #   Niveau 1 « Posologie, Fréquence, Durée » (colonne AN=40) :
    #     - Schéma posologique non conforme        (AO=41)
    #     - Arrêt prématuré et injustifié          (AP=42)
    #     - Prolongation de la durée du traitement (AQ=43)
    #     - Voie d'administration non conforme     (AR=44)
    #   Niveau 1 « Indication, population, CI » (colonne AS=45) :
    #     - Utilisation pour une indication hors AMM          (AT=46)
    #     - Utilisation par une population non prévue         (AU=47)
    #     - Utilisation en présence de contre-indications     (AV=48)
    #     - Utilisation en présence d'une interaction interdite (AW=49)
    #     - Autre                                             (AX=50)
    #
    # DEUX modes de calcul des pourcentages : mode "molécule" (colonne D de la
    # ligne "effectif" de la tranche d'âge) et mode "ensemble des cas" (colonne
    # D de la ligne agrégée "Total").
    type_values <- reactive({
      age <- selected_age()
      req(age)
      row <- selected_age_row()
      req(row)
      tot <- age_totals()
      req(tot)

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

      Type <- ifelse(defs$Niveau == 1L, "groupe", "detail")
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
            Pct          = if (eff[k] > 0) eff[k] / tot$total_mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(Libelle_aff[k], "\u200B"),
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "ensemble des cas",
            Type         = Type[k],
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / tot$total_ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Données des barres horizontales du type de prise ----------------------
    # Lecture depuis la ligne "effectif" de la tranche d'âge sélectionnée. Les
    # colonnes concernées vont de X à AD (index 24 à 30) et se répartissent sur
    # 2 niveaux hiérarchiques (comme les graphiques du type précédents) :
    #   Niveau 1 « Prise avec ordonnance » (colonne X=24) :
    #     - Primo prescription                    (Y=25)
    #     - Renouvellement                        (Z=26)
    #   Niveau 1 « Prise sans ordonnance » (colonne AA=27) :
    #     - Sans ordo en automédication         (AB=28)
    #     - Sur conseil du pharmacien           (AC=29)
    #     - Je ne sais pas                       (AD=30)
    #
    # DEUX modes de calcul des pourcentages : mode "molécule" (colonne D de la
    # ligne "effectif" de la tranche d'âge) et mode "ensemble des cas" (colonne
    # D de la ligne agrégée "Total").
    type_prise_values <- reactive({
      age <- selected_age()
      req(age)
      row <- selected_age_row()
      req(row)
      tot <- age_totals()
      req(tot)

      # Définition hiérarchique : (libellé affiché, index colonne, niveau).
      defs <- data.frame(
        Libelle_brut = c(
          "Prise avec ordonnance",        # groupe (niveau 1)
          "Primo prescription",
          "Renouvellement",
          "Prise sans ordonnance",        # groupe (niveau 1)
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

      Type <- ifelse(defs$Niveau == 1L, "groupe", "detail")
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
            Pct          = if (eff[k] > 0) eff[k] / tot$total_mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle      = paste0(Libelle_aff[k], "\u200B"),
            Libelle_brut = defs$Libelle_brut[k],
            Mode         = "ensemble des cas",
            Type         = Type[k],
            Effectif     = eff[k],
            Pct          = if (eff[k] > 0) eff[k] / tot$total_ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Données du camembert des classes ATC --------------------------------
    # Construit la dataframe nécessaire au camembert 8 : pour chaque classe ATC
    # (lignes identifiées dans la colonne B / "Libellé SOC" du fichier
    # CLASSE_ATC_SOC.xlsx), on lit dans la colonne correspondant à la tranche
    # d'âge sélectionnée :
    #   * la ligne "pourcentage" -> Pct = valeur × 100 (les valeurs du fichier
    #     sont des proportions comprises entre 0 et 1) ;
    #   * la ligne "effectif"    -> Effectif = valeur.
    # La liaison tranche <> colonne se fait par comparaison normalisée du
    # libellé brut de la tranche (selected_age) avec l'en-tête (nom) des colonnes
    # 11:20 du fichier. Comme pour la liste SOC de l'onglet « CLASSE ATC SOC »,
    # les lignes agrégées "Total" / "Autre" / "Autres" sont exclues.
    atc_values <- reactive({
      age <- selected_age()
      data <- classe_atc_soc()
      req(age, data)

      colnames_age <- names(data)[11:20]
      key <- age_normalize_key(age)
      cidx <- which(vapply(
        colnames_age,
        function(nm) age_normalize_key(nm) == key,
        logical(1)
      ))
      if (length(cidx) == 0) {
        return(NULL)
      }
      col_name <- colnames_age[cidx[1]]

      # Classes ATC réelles (colonne B), hors lignes agrégées.
      socs <- unique(data[[2]])
      socs <- socs[!socs %in% c("Total", "Autre", "Autres")]

      rows <- lapply(socs, function(s) {
        rp <- data[data[[2]] == s & data$Type_donnee == "pourcentage", ]
        re <- data[data[[2]] == s & data$Type_donnee == "effectif", ]
        p <- if (nrow(rp) >= 1) suppressWarnings(as.numeric(rp[[col_name]][1])) else NA
        e <- if (nrow(re) >= 1) suppressWarnings(as.numeric(re[[col_name]][1])) else NA
        if (is.na(p)) p <- 0
        if (is.na(e)) e <- 0
        data.frame(Label = s, Effectif = e, Pct = p * 100, stringsAsFactors = FALSE)
      })
      df <- do.call(rbind, rows)
      df <- df[df$Effectif > 0 | df$Pct > 0, , drop = FALSE]
      if (nrow(df) == 0) {
        return(NULL)
      }
      df <- df[order(-df$Pct), , drop = FALSE]
      df$Pct_fr <- format(round(df$Pct, 1), nsmall = 1, decimal.mark = ",")
      df
    })



    # --- Données des barres horizontales des facteurs de mésusage ---------------
    # Ce graphique n'exploite PAS Age_patient.xlsx mais principaux_facteurs.xlsx
    # (chargé au même moment que Age_patient.xlsx). On y lit, pour chaque
    # facteur (ligne), la valeur de la tranche d'âge sélectionnée.
    #
    # Les tranches d'âge sont stockées dans les colonnes K -> T (indices 11 à 20)
    # du fichier transposé, dont l'en-tête (ligne 2) vaut exactement le libellé
    # de la tranche d'âge. On identifie la colonne dédiée par une comparaison
    # « normalisée » (voir age_normalize_key), robuste aux espaces insécables et
    # aux variantes de ponctuation.
    #
    # Les libellés des barres sont ceux de la colonne B ; on ne retient que les
    # lignes dont la colonne C indique "effectif" (colonne "Total" = ensemble
    # des cas globale, en regard pour le calcul « ensemble des cas »).
    facteur_values <- reactive({
      age <- selected_age()
      req(age)
      pf <- principaux_facteurs()
      req(pf)
      tot <- age_totals()
      req(tot)

      # Colonnes K -> T : les tranches d'âge (indices 11 à 20).
      age_cols <- 11:20

      # En-têtes de ligne 2 de ces colonnes (identification de la tranche d'âge).
      en_tetes <- vapply(age_cols, function(i) {
        v <- pf[[i]][2]
        ifelse(is.na(v), "", as.character(v))
      }, character(1))

      # Colonne dédiée à la tranche sélectionnée (comparaison normalisée).
      hit <- which(age_normalize_key(en_tetes) == age_normalize_key(age))
      if (length(hit) == 0) {
        return(NULL)   # pas de colonne dédiée à cette tranche d'âge
      }
      age_col <- age_cols[hit[1]]

      # Lignes "effectif" ; on écarte les 2 lignes d'en-tête puis la ligne
      # agrégée "Total" (périmètre global).
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

      # Valeurs de la tranche d'âge sélectionnée (colonne dédiée) et de
      # l'ensemble des cas (colonne 4 = Total global).
      eff_dci <- vapply(eff_lines, function(i) {
        suppressWarnings(as.numeric(pf[[age_col]][i]))
      }, numeric(1))
      eff_dci[is.na(eff_dci)] <- 0
      eff_global <- vapply(eff_lines, function(i) {
        suppressWarnings(as.numeric(pf[[4]][i]))
      }, numeric(1))
      eff_global[is.na(eff_global)] <- 0

      # Tri par pourcentage DÉCROISSANT (critère = barre "molécule", le
      # pourcentage principal affiché pour la tranche d'âge sélectionnée).
      pct_mol <- ifelse(eff_dci > 0, eff_dci / tot$total_mol * 100, 0)
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
            Pct = if (eff_dci[k] > 0) eff_dci[k] / tot$total_mol * 100 else 0,
            stringsAsFactors = FALSE
          ),
          data.frame(
            Libelle = paste0(lib_aff[k], "\u200B"), Libelle_brut = lib_aff[k],
            Mode = "ensemble des cas", Type = Type[k], Effectif = eff_global[k],
            Pct = if (eff_global[k] > 0) eff_global[k] / tot$total_ens * 100 else 0,
            stringsAsFactors = FALSE
          )
        )
      }))
      out
    })

    # --- Hauteur du graphique facteur (responsive au dépliage) ----------------
    # Vue "n_facteurs premiers" (par défaut 10) : hauteur proportionnelle au nombre
    # de facteurs paramétré (n_facteurs * 55 px) ; vue "tous" (29) : 1000 px.
    facteur_height <- reactive({
      if (isTRUE(input$facteur_afficher_tous)) {
        "1000px"
      } else {
        paste0(n_facteurs * 55, "px")
      }
    })

    # --- Jeu de données affiché (filtrage n_facteurs premiers / tous) ---------
    # Extrait de facteur_values() le sous-ensemble tracé : n_facteurs*2 lignes
    # (n_facteurs facteurs × 2 barres) par défaut, ou la totalité si la case
    # "Afficher les 29 facteurs" est cochée.
    facteur_plot_data <- reactive({
      fv <- facteur_values()
      if (is.null(fv) || nrow(fv) == 0) {
        return(NULL)
      }
      n_lignes <- if (isTRUE(input$facteur_afficher_tous)) nrow(fv) else n_facteurs * 2
      fv[seq_len(min(n_lignes, nrow(fv))), ]
    })

    # --- Message / état de l'import des données --------------------------------
    output$etat <- renderUI({
      data <- age_data()
      if (is.null(data)) {
        return(
          div(class = "alert alert-danger",
              icon("exclamation-triangle"),
              strong("Fichier Age_patient introuvable ou illisible dans data/."))
        )
      }
      NULL
    })

    # --- Tableau des tranches d'âge (partie supérieure) ------------------------
    # Les libellés de 1er niveau (groupes) sont affichés en gras, les libellés
    # de 2e niveau (détails) sont décalés vers la droite. On utilise du HTML
    # dans la colonne des libellés (escape = FALSE).
    output$table_age <- renderDT({
      df <- age_df()
      if (is.null(df) || nrow(df) == 0) {
        return(
          DT::datatable(
            data.frame(Avertissement = "Aucune donnée de tranche d'âge disponible."),
            options = list(dom = "t"), rownames = FALSE
          )
        )
      }

      lib_html <- vapply(seq_len(nrow(df)), function(k) {
        lbl <- htmltools::htmlEscape(df$Libelle_affichage[k])
        if (df$Niveau[k] == 1L) {
          paste0("<b>", lbl, "</b>")
        } else {
          # Décalage léger des libellés de 2e niveau (hiérarchie).
          paste0("<span style='display:inline-block;padding-left:1.6em;'>",
                 lbl, "</span>")
        }
      }, character(1))

      DT::datatable(
        data.frame(Tranche = lib_html, Total = df$Total),
        rownames = FALSE,
        escape = FALSE,
        colnames = c("Tranche d'âge" = "Tranche", "Effectif total" = "Total"),
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
      age <- selected_age_court()
      if (is.null(age)) {
        return(
          tags$p("Cliquez sur une tranche d'âge du tableau ci-dessus pour afficher le détail.")
        )
      }
      tags$p(
        class = "dci-title",
        icon("exclamation-circle"),
        strong(paste("Caractéristiques de la tranche d'âge —", age))
      )
    })

    # --- Camembert 1 : genre des patients (partie basse) ----------------------
    output$plot_genre <- render_plotly_theme(theme, {
      age <- selected_age_court()
      titre <- wrap_titre(paste("Répartition par genre —", age))
      gv <- genre_values()
      if (is.null(age) || is.null(gv) || nrow(gv) == 0) {
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
    output$plot_enceinte <- render_plotly_theme(theme, {
      age <- selected_age_court()
      titre <- wrap_titre(paste("Répartition des données \"enceinte\" —", age))
      ev <- enceinte_values()
      if (is.null(age) || is.null(ev) || nrow(ev) == 0) {
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

    # --- Barres horizontales de l'origine du mésusage --------------------------
    # On affiche les 3 modalités d'origine (prise / prescription / dispensation)
    # avec, pour chacune, DEUX barres (bordeaux = tranche d'âge, orange =
    # ensemble des cas), comme dans les onglets précédents.
    output$plot_origine <- render_plotly_theme(theme, {
      age <- selected_age_court()
      titre <- wrap_titre(paste("Répartition par origine —", age))
      ov <- origine_values()
      if (is.null(age) || is.null(ov) || nrow(ov) == 0) {
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
        list(Mode = "molécule",         Type = "detail", Nom = "Molécule — détail"),
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
    # DEUX barres par libellé de type (bordeaux = tranche d'âge, orange =
    # ensemble des cas), avec hiérarchie sur 2 niveaux (groupes en gras).
    output$plot_type <- render_plotly_theme(theme, {
      age <- selected_age_court()
      titre <- wrap_titre(paste("Répartition par type —", age))
      tv <- type_values()
      if (is.null(age) || is.null(tv) || nrow(tv) == 0) {
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

    # --- Barres horizontales du type de prise ----------------------------------
    # DEUX barres par libellé de type de prise (bordeaux = tranche d'âge,
    # orange = ensemble des cas), avec hiérarchie sur 2 niveaux.
    output$plot_type_prise <- render_plotly_theme(theme, {
      age <- selected_age_court()
      titre <- wrap_titre(paste("Répartition par type de prise —", age))
      tp <- type_prise_values()
      if (is.null(age) || is.null(tp) || nrow(tp) == 0) {
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

      tp$Libelle <- wrap_libelle(tp$Libelle)
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
            ticktext = rev(tp$TickLabel)
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

    # --- Camembert 8 : répartition par classe ATC ----------------------------
    output$plot_atc <- render_plotly_theme(theme, {
      age <- selected_age_court()
      titre <- wrap_titre(paste("Répartition par classe ATC —", age))
      av <- atc_values()
      if (is.null(age) || is.null(av) || nrow(av) == 0) {
        return(plotly::plotly_empty())
      }

      pal_atc <- atc_colors()
      col_segments <- pal_atc[seq_len(nrow(av))]

      # Libellé + pourcentage dans chaque secteur.
      txt <- paste0(av$Label, "<br>", av$Pct_fr, "%")

      plotly::plot_ly(
        labels = av$Label,
        values = av$Effectif,
        type = "pie",
        customdata = av$Pct_fr,
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



    # --- Conteneur du graphique des facteurs (hauteur dynamique) ---------------
    # Le 6e graphique est rendu dans une carte dont la hauteur dépend du
    # dépliage (15 premiers facteurs vs tous) : 820px par défaut, 1000px une
    # fois le dépliage activé.
    output$plot_facteur_ui <- renderUI({
      plotly::plotlyOutput(ns("plot_facteur"), height = facteur_height())
    })

    # --- Barres horizontales des facteurs de mésusage --------------------------
    # Deux barres par facteur (bordeaux = tranche d'âge, orange = ensemble des
    # cas), triées par pourcentage décroissant. La liaison avec la tranche d'âge
    # sélectionnée s'effectue via les colonnes K -> T (11:20) de
    # principaux_facteurs.xlsx.
    output$plot_facteur <- render_plotly_theme(theme, {
      age <- selected_age_court()
      titre <- wrap_titre(paste("Répartition par facteurs —", age))
      fv <- facteur_plot_data()
      if (is.null(age) || is.null(fv) || nrow(fv) == 0) {
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

      fv$Libelle <- wrap_libelle(fv$Libelle)
      categoryarray <- rev(fv$Libelle)

      fv$TickLabel <- fv$Libelle
      fv$TickLabel[fv$Mode == "ensemble des cas"] <- ""

      groups <- list(
        list(Mode = "molécule",         Type = "detail", Nom = "Molécule — détail"),
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
            ticktext = rev(fv$TickLabel)
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
  })
}

