# Script pour extraire le contenu du plan d'analyse Word
# et identifier les titres de tableaux corrects

cat("=== EXTRACTION DU PLAN D'ANALYSE ===\n")

# Vérifier si le fichier existe
plan_file <- "2025-08-12_Stigma_Discrimination_Data_Analysis_Plan_Draft.docx"

if(file.exists(plan_file)) {
  cat("✓ Fichier trouvé :", plan_file, "\n")
  cat("Taille :", round(file.size(plan_file)/1024, 2), "KB\n")
  
  # Essayer différentes méthodes d'extraction
  
  # Méthode 1: Utiliser unzip pour extraire le XML
  cat("\n=== TENTATIVE D'EXTRACTION XML ===\n")
  
  # Créer un dossier temporaire
  temp_dir <- "temp_docx_extract"
  if(dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }
  dir.create(temp_dir)
  
  # Extraire le contenu
  result <- tryCatch({
    system(paste("cd", temp_dir, "&& unzip -q ../", plan_file), intern = TRUE)
    
    # Lire le document principal
    if(file.exists(file.path(temp_dir, "word/document.xml"))) {
      xml_content <- readLines(file.path(temp_dir, "word/document.xml"), warn = FALSE)
      cat("✓ Contenu XML extrait\n")
      
      # Sauvegarder pour inspection
      writeLines(xml_content, "plan_analyse_xml.txt")
      cat("✓ Contenu sauvegardé dans plan_analyse_xml.txt\n")
      
      TRUE
    } else {
      FALSE
    }
  }, error = function(e) {
    cat("✗ Erreur d'extraction XML :", e$message, "\n")
    FALSE
  })
  
  # Nettoyer
  if(dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }
  
} else {
  cat("✗ Fichier non trouvé :", plan_file, "\n")
}

cat("\n=== DÉFINITION DES TITRES DE TABLEAUX CORRECTS ===\n")

# Définir les titres standard pour l'analyse de stigmatisation
titres_tableaux <- list(
  
  # Section 1: Caractéristiques socio-démographiques
  "SEXE" = "Tableau 1.1 : Répartition des participants selon le sexe",
  "AGE" = "Tableau 1.2 : Statistiques descriptives de l'âge des participants",
  "CLASSES_AGE" = "Tableau 1.3 : Répartition des participants par classes d'âge",
  "NIV_EDUC" = "Tableau 1.4 : Répartition des participants selon le niveau d'éducation",
  "CATEGORIE" = "Tableau 1.5 : Répartition des participants selon la catégorie professionnelle",
  "SITUATION_PROF" = "Tableau 1.6 : Répartition des participants selon la situation professionnelle",
  
  # Section 2: Caractéristiques géographiques
  "REGION_MILITAIRE" = "Tableau 2.1 : Répartition des participants par région militaire",
  "SITE" = "Tableau 2.2 : Répartition des participants par site de collecte",
  
  # Section 3: Relations et soutien social
  "REL_INTIME_ACTU" = "Tableau 3.1 : Situation de relation intime/sexuelle actuelle",
  "PARTENAIRE_PVVIH" = "Tableau 3.2 : Statut VIH du/des partenaire(s)",
  "GRP_SOUTIEN_VIH" = "Tableau 3.3 : Participation à un groupe de soutien VIH",
  
  # Section 4: Dépistage et diagnostic
  "DECISION_DEPIST" = "Tableau 4.1 : Circonstances de la décision de dépistage VIH",
  "RAISON_PRINC" = "Tableau 4.2 : Raison principale du dépistage VIH",
  "HESITATION_DEPIST" = "Tableau 4.3 : Hésitation au dépistage par peur des réactions",
  "DELAI_DEBUT_TTT" = "Tableau 4.4 : Délai entre diagnostic et début du traitement ARV",
  
  # Section 5: État de santé et comorbidités
  "IMPR_ETAT_SANTE" = "Tableau 5.1 : Perception de l'état de santé général",
  "DIAG_TB" = "Tableau 5.2 : Diagnostic de tuberculose",
  "DIAG_HV" = "Tableau 5.3 : Diagnostic d'hépatite virale",
  "DIAG_IST" = "Tableau 5.4 : Diagnostic d'infections sexuellement transmissibles",
  "DIAG_TB_MENT" = "Tableau 5.5 : Diagnostic de troubles de santé mentale",
  
  # Section 6: Services de santé
  "SITE_PEC_VIH" = "Tableau 6.1 : Lieu habituel de prise en charge VIH",
  "SERV_TTT" = "Tableau 6.2 : Accès aux services de traitement VIH",
  "SERV_INFO" = "Tableau 6.3 : Accès aux informations sur le VIH",
  "SERV_DEP" = "Tableau 6.4 : Accès aux services de dépistage VIH",
  
  # Section 7: Expériences de stigmatisation - Exclusion sociale
  "EXCL_ACT_SOCIAL" = "Tableau 7.1 : Exclusion d'activités ou rencontres sociales",
  "EXCL_ACT_RELIG" = "Tableau 7.2 : Exclusion d'activités religieuses",
  "EXCL_ACT_FAML" = "Tableau 7.3 : Exclusion d'activités familiales",
  "EXCL_CRIT_FAM" = "Tableau 7.4 : Remarques discriminatoires de la famille",
  "EXCL_CRIT_AUTRES" = "Tableau 7.5 : Remarques discriminatoires d'autres personnes",
  "EXCL_HARCEL_VERB" = "Tableau 7.6 : Expériences de harcèlement verbal",
  "EXCL_CHANTAGE" = "Tableau 7.7 : Expériences de chantage",
  "EXCL_HARC_PHYS" = "Tableau 7.8 : Expériences de harcèlement physique",
  "EXCL_REFUS_EMPLOI" = "Tableau 7.9 : Refus d'emploi ou perte de travail",
  "EXCL_TRAVAIL_POSTE" = "Tableau 7.10 : Changements de poste ou refus de promotion",
  "EXCL_DISCR_CONJ_ENFTS" = "Tableau 7.11 : Discrimination subie par le conjoint ou les enfants",
  
  # Section 8: Stigmatisation dans les services de santé
  "STIG_SERV_SAN_REFUS" = "Tableau 8.1 : Refus de services de santé en raison du statut VIH",
  "STIG_PAS_RAPPORT" = "Tableau 8.2 : Conseil d'éviter les relations sexuelles",
  "STIG_CRIT" = "Tableau 8.3 : Commérages liés au statut VIH",
  "STIG_VIOLEN_VERB" = "Tableau 8.4 : Violence verbale dans les services de santé",
  "STIG_VIOLEN_PHYS" = "Tableau 8.5 : Violence physique dans les services de santé",
  "STIG_CONTACT_PHYS" = "Tableau 8.6 : Évitement de contacts physiques par le personnel",
  "STIG_INFO_SANS_CONS" = "Tableau 8.7 : Divulgation du statut sans consentement",
  
  # Section 9: Révélation du statut
  "REVEL_STATUT" = "Tableau 9.1 : Pratiques de révélation du statut VIH",
  
  # Section 10: Traitement ARV
  "DUREE_TTT" = "Tableau 10.1 : Durée du traitement antirétroviral",
  "DEBUT_TTT" = "Tableau 10.2 : Date de début du traitement ARV",
  "AUTRES_TTT" = "Tableau 10.3 : Autres traitements en cours",
  
  # Section 11: Adhérence et observance
  "OBSERVANCE_GLOBALE" = "Tableau 11.1 : Niveau d'observance thérapeutique globale",
  "ADHERENCE_SUBJECTIVE" = "Tableau 11.2 : Adhérence subjective au traitement",
  "FREQUENCE_OUBLI" = "Tableau 11.3 : Fréquence d'oubli des prises médicamenteuses",
  
  # Section 12: Tableaux de synthèse
  "SYNTHESE_EXCLUSION" = "Tableau 12.1 : Synthèse des expériences d'exclusion sociale",
  "SYNTHESE_STIGMA_SANTE" = "Tableau 12.2 : Synthèse de la stigmatisation dans les services de santé",
  "SYNTHESE_GENERALE" = "Tableau 12.3 : Synthèse générale des caractéristiques de l'échantillon"
)

cat("✓ Définition de", length(titres_tableaux), "titres de tableaux\n")

# Sauvegarder la liste des titres
save(titres_tableaux, file = "titres_tableaux_corrects.RData")
cat("✓ Titres sauvegardés dans titres_tableaux_corrects.RData\n")

# Afficher quelques exemples
cat("\n=== EXEMPLES DE TITRES DÉFINIS ===\n")
examples <- c("SEXE", "EXCL_ACT_SOCIAL", "STIG_SERV_SAN_REFUS", "DUREE_TTT")
for(var in examples) {
  if(var %in% names(titres_tableaux)) {
    cat(var, ":", titres_tableaux[[var]], "\n")
  }
}

cat("\n=== EXTRACTION TERMINÉE ===\n")