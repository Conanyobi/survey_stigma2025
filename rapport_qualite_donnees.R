# RAPPORT DE QUALITÉ DES DONNÉES
# Enquête Stigmatisation VIH/SIDA 2025
# Généré automatiquement par le script de préparation

library(dplyr)

# Charger les données
load("donnees_avec_labels.RData")

cat("=========================================================\n")
cat("       RAPPORT DE QUALITÉ DES DONNÉES\n")
cat("       Enquête Stigmatisation VIH/SIDA 2025\n")
cat("=========================================================\n")
cat("Date de génération:", format(Sys.Date(), "%d/%m/%Y"), "\n")
cat("Heure de génération:", format(Sys.time(), "%H:%M:%S"), "\n\n")

# =========================
# 1. APERÇU GÉNÉRAL
# =========================
cat("1. APERÇU GÉNÉRAL DU DATASET\n")
cat("============================\n")
cat("• Nombre total d'observations :", nrow(data_complete), "\n")
cat("• Nombre total de variables   :", ncol(data_complete), "\n")
cat("• Variables avec labels       :", sum(sapply(data_complete, is.factor)), "\n")
cat("• Variables numériques        :", sum(sapply(data_complete, is.numeric)), "\n")
cat("• Variables caractères        :", sum(sapply(data_complete, is.character)), "\n")
cat("• Taille estimée en mémoire   :", round(object.size(data_complete)/1024/1024, 1), "MB\n\n")

# =========================
# 2. COMPLÉTUDE DES DONNÉES
# =========================
cat("2. COMPLÉTUDE DES DONNÉES\n")
cat("=========================\n")

# Calculer le taux de complétude par variable
completude <- data.frame(
  Variable = names(data_complete),
  Type = sapply(data_complete, function(x) class(x)[1]),
  Total = nrow(data_complete),
  Valeurs_manquantes = sapply(data_complete, function(x) sum(is.na(x))),
  Taux_completude = round(sapply(data_complete, function(x) (1 - sum(is.na(x))/length(x)) * 100), 1)
)

# Variables avec le plus de données manquantes
variables_problematiques <- completude[completude$Taux_completude < 95, ]
if(nrow(variables_problematiques) > 0) {
  cat("Variables avec >5% de données manquantes :\n")
  print(variables_problematiques[order(variables_problematiques$Taux_completude), ])
} else {
  cat("✓ Aucune variable avec >5% de données manquantes\n")
}

cat("\nStatistiques de complétude :\n")
cat("• Complétude moyenne          :", round(mean(completude$Taux_completude), 1), "%\n")
cat("• Variables 100% complètes    :", sum(completude$Taux_completude == 100), "\n")
cat("• Variables >95% complètes    :", sum(completude$Taux_completude >= 95), "\n")
cat("• Variables <90% complètes    :", sum(completude$Taux_completude < 90), "\n\n")

# =========================
# 3. VARIABLES DÉMOGRAPHIQUES
# =========================
cat("3. QUALITÉ DES VARIABLES DÉMOGRAPHIQUES\n")
cat("========================================\n")

# Sexe
if("SEXE" %in% names(data_complete)) {
  cat("SEXE :\n")
  sexe_table <- table(data_complete$SEXE, useNA = "ifany")
  print(sexe_table)
  cat("• Taux de complétude :", round((1 - sum(is.na(data_complete$SEXE))/nrow(data_complete))*100, 1), "%\n")
  cat("\n")
}

# Âge
if("AGE" %in% names(data_complete)) {
  cat("ÂGE :\n")
  ages_valides <- data_complete$AGE[!is.na(data_complete$AGE)]
  cat("• Nombre de valeurs valides  :", length(ages_valides), "\n")
  cat("• Âge minimum                :", min(ages_valides), "ans\n")
  cat("• Âge maximum                :", max(ages_valides), "ans\n")
  cat("• Âge moyen                  :", round(mean(ages_valides), 1), "ans\n")
  cat("• Âge médian                 :", round(median(ages_valides), 1), "ans\n")
  
  # Problèmes d'âge
  ages_problematiques <- sum(ages_valides < 18)
  if(ages_problematiques > 0) {
    cat("⚠ ATTENTION : ", ages_problematiques, " participant(s) <18 ans\n")
  }
  cat("\n")
}

# Classes d'âge
if("CLASSES_AGE" %in% names(data_complete)) {
  cat("CLASSES D'ÂGE :\n")
  classes_table <- table(data_complete$CLASSES_AGE, useNA = "ifany")
  print(classes_table)
  cat("• Pourcentages :\n")
  print(round(prop.table(classes_table)*100, 1))
  cat("\n")
}

# Région militaire
if("REGION_MILITAIRE" %in% names(data_complete)) {
  cat("RÉGION MILITAIRE :\n")
  region_table <- table(data_complete$REGION_MILITAIRE, useNA = "ifany")
  print(region_table)
  cat("• Taux de renseignement :", round((1 - sum(is.na(data_complete$REGION_MILITAIRE))/nrow(data_complete))*100, 1), "%\n")
  cat("\n")
}

# =========================
# 4. VARIABLES LIÉES AU TRAITEMENT
# =========================
cat("4. QUALITÉ DES VARIABLES LIÉES AU TRAITEMENT\n")
cat("=============================================\n")

# Date de début de traitement
if("DEBUT_TTT" %in% names(data_complete)) {
  debut_valides <- !is.na(data_complete$DEBUT_TTT) & data_complete$DEBUT_TTT != ""
  cat("DEBUT_TTT :\n")
  cat("• Dates valides              :", sum(debut_valides), "/", nrow(data_complete), "\n")
  cat("• Taux de complétude         :", round(sum(debut_valides)/nrow(data_complete)*100, 1), "%\n")
  
  if(sum(debut_valides) > 0) {
    dates_debut <- data_complete$DEBUT_TTT[debut_valides]
    # Extraire les années
    annees <- as.numeric(substr(dates_debut, 7, 10))
    cat("• Année la plus ancienne     :", min(annees, na.rm = TRUE), "\n")
    cat("• Année la plus récente      :", max(annees, na.rm = TRUE), "\n")
  }
  cat("\n")
}

# Durée de traitement
if("DUREE_TTT" %in% names(data_complete)) {
  durees_valides <- data_complete$DUREE_TTT[!is.na(data_complete$DUREE_TTT)]
  cat("DUREE_TTT :\n")
  cat("• Durées calculées           :", length(durees_valides), "/", nrow(data_complete), "\n")
  cat("• Taux de complétude         :", round(length(durees_valides)/nrow(data_complete)*100, 1), "%\n")
  
  if(length(durees_valides) > 0) {
    cat("• Durée moyenne              :", round(mean(durees_valides), 1), "mois\n")
    cat("• Durée médiane              :", round(median(durees_valides), 1), "mois\n")
    cat("• Durée minimum              :", round(min(durees_valides), 1), "mois\n")
    cat("• Durée maximum              :", round(max(durees_valides), 1), "mois\n")
    
    # Répartition par durée
    cat("• Répartition par durée :\n")
    cat("  - < 12 mois                :", sum(durees_valides < 12), "personnes\n")
    cat("  - 12-60 mois               :", sum(durees_valides >= 12 & durees_valides < 60), "personnes\n")
    cat("  - 60-120 mois              :", sum(durees_valides >= 60 & durees_valides < 120), "personnes\n")
    cat("  - ≥ 120 mois               :", sum(durees_valides >= 120), "personnes\n")
  }
  cat("\n")
}

# =========================
# 5. ENREGISTREMENTS PROBLÉMATIQUES
# =========================
cat("5. ENREGISTREMENTS PROBLÉMATIQUES\n")
cat("==================================\n")

# Compter les différents types de problèmes
nb_total <- nrow(data_complete)

# 1. Âge < 18 ans
age_mineur <- sum(data_complete$AGE < 18 & !is.na(data_complete$AGE))
cat("• Participants <18 ans        :", age_mineur, "(", round(age_mineur/nb_total*100, 1), "%)\n")

# 2. Durée traitement < 12 mois
if("DUREE_TTT" %in% names(data_complete)) {
  duree_courte <- sum(data_complete$DUREE_TTT < 12 & !is.na(data_complete$DUREE_TTT))
  cat("• Traitement <12 mois         :", duree_courte, "(", round(duree_courte/nb_total*100, 1), "%)\n")
}

# 3. DEBUT_TTT manquant
if("DEBUT_TTT" %in% names(data_complete)) {
  debut_manquant <- sum(is.na(data_complete$DEBUT_TTT) | data_complete$DEBUT_TTT == "")
  cat("• Date début traitement vide :", debut_manquant, "(", round(debut_manquant/nb_total*100, 1), "%)\n")
}

# 4. Enregistrements quasi-vides
variables_base <- c("OPERATOR_ID", "CODE", "NUM_QUESTIONNAIRE", "DATE_ENQUETE", "HEURE_ENQUETE")
variables_autres <- setdiff(names(data_complete), variables_base)
nb_valeurs_renseignees <- apply(data_complete[, variables_autres], 1, function(x) sum(!is.na(x)))
quasi_vides <- sum(nb_valeurs_renseignees <= 3)
cat("• Enregistrements quasi-vides :", quasi_vides, "(", round(quasi_vides/nb_total*100, 1), "%)\n")

# Total problématiques
problemes_total <- age_mineur + duree_courte + debut_manquant + quasi_vides
# Ajuster pour éviter le double-comptage
if(exists("data_complete")) {
  age_pb <- data_complete$AGE < 18 & !is.na(data_complete$AGE)
  duree_pb <- data_complete$DUREE_TTT < 12 & !is.na(data_complete$DUREE_TTT)
  debut_pb <- is.na(data_complete$DEBUT_TTT) | data_complete$DEBUT_TTT == ""
  quasi_pb <- nb_valeurs_renseignees <= 3
  
  problemes_uniques <- sum(age_pb | duree_pb | debut_pb | quasi_pb)
  cat("• TOTAL UNIQUE problématique :", problemes_uniques, "(", round(problemes_uniques/nb_total*100, 1), "%)\n")
}

cat("\n")

# =========================
# 6. RECOMMANDATIONS
# =========================
cat("6. RECOMMANDATIONS POUR L'ANALYSE\n")
cat("==================================\n")

if(age_mineur > 0) {
  cat("⚠ Vérifier les critères d'inclusion (âge ≥18 ans)\n")
}
if(debut_manquant > 20) {
  cat("⚠ Nombre élevé de dates de début manquantes - impact sur analyses temporelles\n")
}
if(quasi_vides > 0) {
  cat("⚠ Exclure les enregistrements quasi-vides des analyses\n")
}
if(duree_courte > 0) {
  cat("• Considérer une analyse séparée pour les traitements récents (<12 mois)\n")
}

# Calcul du dataset "propre"
dataset_propre <- nb_total - quasi_vides - age_mineur
cat("• Dataset recommandé pour analyse principale :", dataset_propre, "observations\n")
cat("• Taux d'utilisation des données            :", round(dataset_propre/nb_total*100, 1), "%\n")

cat("\n")

# =========================
# 7. VARIABLES SPÉCIFIQUES À L'ÉTUDE
# =========================
cat("7. VARIABLES SPÉCIFIQUES À L'ÉTUDE VIH/SIDA\n")
cat("============================================\n")

# Variables de stigmatisation
variables_stigma <- names(data_complete)[grepl("STIG_|EXCL_|VECU_STIGMA", names(data_complete))]
if(length(variables_stigma) > 0) {
  cat("Variables de stigmatisation   :", length(variables_stigma), "\n")
  
  # Complétude moyenne des variables de stigma
  completude_stigma <- sapply(data_complete[variables_stigma], function(x) (1 - sum(is.na(x))/length(x)) * 100)
  cat("• Complétude moyenne          :", round(mean(completude_stigma), 1), "%\n")
  cat("• Variables <90% complètes    :", sum(completude_stigma < 90), "\n")
}

# Variables de santé
variables_sante <- names(data_complete)[grepl("SANTE_|DIAG_|SERV_", names(data_complete))]
if(length(variables_sante) > 0) {
  cat("Variables de santé            :", length(variables_sante), "\n")
  
  # Complétude moyenne des variables de santé
  completude_sante <- sapply(data_complete[variables_sante], function(x) (1 - sum(is.na(x))/length(x)) * 100)
  cat("• Complétude moyenne          :", round(mean(completude_sante), 1), "%\n")
}

# Variables d'observance
variables_observance <- names(data_complete)[grepl("ADHERENCE_|OBSERVANCE_|FREQUENCE_", names(data_complete))]
if(length(variables_observance) > 0) {
  cat("Variables d'observance        :", length(variables_observance), "\n")
  
  # Complétude moyenne des variables d'observance
  completude_observance <- sapply(data_complete[variables_observance], function(x) (1 - sum(is.na(x))/length(x)) * 100)
  cat("• Complétude moyenne          :", round(mean(completude_observance), 1), "%\n")
}

cat("\n")

# =========================
# 8. CONCLUSION
# =========================
cat("8. CONCLUSION\n")
cat("=============\n")

qualite_globale <- round(mean(completude$Taux_completude), 1)
if(qualite_globale >= 95) {
  cat("✓ EXCELLENTE qualité des données (", qualite_globale, "% de complétude)\n")
} else if(qualite_globale >= 90) {
  cat("✓ BONNE qualité des données (", qualite_globale, "% de complétude)\n")
} else if(qualite_globale >= 80) {
  cat("⚠ QUALITÉ MOYENNE des données (", qualite_globale, "% de complétude)\n")
} else {
  cat("⚠ QUALITÉ PRÉOCCUPANTE des données (", qualite_globale, "% de complétude)\n")
}

cat("\nLe dataset est prêt pour l'analyse avec :\n")
cat("• ", dataset_propre, " observations analysables\n")
cat("• ", sum(sapply(data_complete, is.factor)), " variables avec labels appliqués\n")
cat("• Variables créées : CLASSES_AGE, DUREE_TTT, REGION_MILITAIRE (renseignée)\n")
cat("• Format des dates uniformisé (jj-mm-aaaa)\n")

cat("\n=========================================================\n")
cat("Fin du rapport - ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n")
cat("=========================================================\n")