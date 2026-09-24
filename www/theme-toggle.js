/* ============================================================================
   theme-toggle.js — Application et persistance du thème (clair / sombre)
   ============================================================================
   Ce script complète app.R côté client :
     1. Il met à jour l'attribut `data-bs-theme` de <html> quand le serveur lui
        demande (message "apply-theme"), afin que Bootstrap 5 et les sélecteurs
        CSS `[data-bs-theme="..."]` de custom.css réagissent.
     2. Il persiste la préférence dans localStorage (clé "app-theme") quand le
        serveur lui envoie le message "write-theme".
   La lecture initiale de la préférence est gérée par le script inline déclaré
   dans app.R (handler "ask-initial-theme").
   ============================================================================ */

(function () {
  "use strict";

  var STORAGE_KEY = "app-theme";

  if (!window.Shiny) {
    return;
  }

  function writeStoredTheme(theme) {
    try {
      window.localStorage.setItem(STORAGE_KEY, theme);
    } catch (e) {
      /* stockage indisponible : on ignore */
    }
  }

  function applyThemeAttribute(theme) {
    var root = document.documentElement;
    // La valeur métier reçue du serveur est "sombre"/"clair" ; on la convertit
    // en valeur Bootstrap standard "dark"/"light" pour l'attribut data-bs-theme,
    // attendue par Bootstrap 5 et les sélecteurs CSS `[data-bs-theme="dark"]`.
    root.setAttribute("data-bs-theme", theme === "sombre" ? "dark" : "light");
  }

  Shiny.addCustomMessageHandler("apply-theme", function (theme) {
    applyThemeAttribute(theme);
  });

  Shiny.addCustomMessageHandler("write-theme", function (theme) {
    writeStoredTheme(theme);
  });
})();
