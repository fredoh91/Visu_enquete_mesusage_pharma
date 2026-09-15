suppressPackageStartupMessages({library(readxl); library(dplyr)})
pf <- readxl::read_excel("data/principaux_facteurs.xlsx", sheet = 1L, col_names = FALSE, .name_repair = "minimal")
mr <- readxl::read_excel("data/MEDOC_REG.xlsx", sheet = 1L, skip = 1L, .name_repair = "unique")

dci <- "PARACETAMOL"
# total molecule
row <- mr %>% dplyr::filter(.data$DCI == dci, .data$Type_donnee == "effectif")
total_mol <- suppressWarnings(as.numeric(row[[4]][1]))
row_total <- mr %>% dplyr::filter(.data$DCI == "Total", .data$Type_donnee == "effectif")
total_ens <- suppressWarnings(as.numeric(row_total[[4]][1]))
cat(sprintf("total_mol=%g total_ens=%g\n", total_mol, total_ens))

dci_cols <- 135:147
en_tetes <- vapply(dci_cols, function(i){v<-pf[[i]][2]; ifelse(is.na(v),"",as.character(v))}, character(1))
hit <- which(en_tetes == dci)
dci_col <- dci_cols[hit[1]]
cat(sprintf("DCI colonne=%d en-tete=%s\n", dci_col, en_tetes[hit[1]]))

types <- pf[[3]]
eff_lines <- which(types == "effectif")
eff_lines <- eff_lines[eff_lines > 2][-1]
cat(sprintf("nb facteurs=%d\n", length(eff_lines)))
# valeurs pour le 1er facteur et le total
for (i in eff_lines[1:3]) {
  cat(sprintf("ligne %d | lib=%s | eff_dci=%s | total(%s)=%s\n", i,
              as.character(pf[[2]][i]), as.character(pf[[dci_col]][i]),
              "glob", as.character(pf[[4]][i])))
}
# verification coherence : somme des X
all_dci_col <- vapply(eff_lines, function(i) suppressWarnings(as.numeric(pf[[dci_col]][i])), numeric(1))
all_dci_col[is.na(all_dci_col)] <- 0
cat("somme eff_dci (PARACETAMOL) sur tous facteurs =", sum(all_dci_col), "vs total_mol =", total_mol, "\n")




