# Installer si nécessaire
# install.packages(c("dplyr", "stringr", "jsonlite"))

library(dplyr)
library(stringr)
library(jsonlite)

# -------------------------------
# 1️⃣ Charger le fichier RData
# -------------------------------
# Supposons que le fichier contient un objet `data` (data.frame)
load("stigma2025data.RData")  # charge l'objet(s) dans l'environnement
# Vérifier quel objet a été chargé
ls()  # par exemple 'data'

# Fusion des différentes sections par les identifiants communs
data_complete <- SECTION0 %>%
  left_join(SECTION1, by = c("OPERATOR_ID", "CODE", "NUM_QUESTIONNAIRE", "DATE_ID", "HEURE_ID")) %>%
  left_join(SECTION2, by = c("OPERATOR_ID", "CODE", "NUM_QUESTIONNAIRE", "DATE_ID", "HEURE_ID")) %>%
  left_join(SECTION3, by = c("OPERATOR_ID", "CODE", "NUM_QUESTIONNAIRE", "DATE_ID", "HEURE_ID")) %>%
  left_join(SECTION4, by = c("OPERATOR_ID", "CODE", "NUM_QUESTIONNAIRE", "DATE_ID", "HEURE_ID"))

# Aperçu des données fusionnées
cat("Dimensions du dataset complet:", dim(data_complete), "\n")
cat("Nombre total de variables:", ncol(data_complete), "\n")

# Création d'un identifiant unique
data_complete$ID_UNIQUE <- 1:nrow(data_complete)

# -------------------------------
# 1.5️⃣ Créer la variable CLASSES_AGE
# -------------------------------
# Créer les classes d'âge à partir de la variable AGE
if("AGE" %in% names(data_complete)) {
  # Créer les classes d'âge selon les spécifications
  data_complete$CLASSES_AGE <- cut(data_complete$AGE, 
                                   breaks = c(17, 24, 59, Inf),
                                   labels = c("18-24", "25-59", "60+"),
                                   right = TRUE,
                                   include.lowest = TRUE)
  
  # Afficher la distribution des classes d'âge
  cat("Distribution des classes d'âge:\n")
  print(table(data_complete$CLASSES_AGE, useNA = "ifany"))
  
  # Vérifier s'il y a des âges en dehors de la plage normale (< 18 ans)
  ages_hors_plage <- data_complete$AGE < 18 & !is.na(data_complete$AGE)
  if(any(ages_hors_plage)) {
    cat("ATTENTION: Il y a", sum(ages_hors_plage), "personne(s) de moins de 18 ans\n")
    cat("Ages concernés:", data_complete$AGE[ages_hors_plage], "\n")
  }
  cat("\n")
} else {
  cat("Variable AGE non trouvée - CLASSES_AGE non créée\n")
}

# -------------------------------
# 1.6️⃣ Uniformiser le format de DEBUT_TTT
# -------------------------------
if("DEBUT_TTT" %in% names(data_complete)) {
  cat("Uniformisation du format DEBUT_TTT vers jj-mm-aaaa\n")
  
  # Sauvegarder les valeurs originales pour debug
  original_debut_ttt <- data_complete$DEBUT_TTT
  
  # Fonction pour nettoyer et uniformiser les dates
  uniformiser_date <- function(date_str) {
    if(is.na(date_str) || date_str == "") return(NA)
    
    # Supprimer les espaces
    date_str <- trimws(date_str)
    
    # Cas 1: Format "jj/mm/aaaa" -> "jj-mm-aaaa" (avec formatage à 2 chiffres)
    if(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date_str)) {
      parts <- strsplit(date_str, "/")[[1]]
      return(paste0(sprintf("%02d", as.numeric(parts[1])), "-", 
                   sprintf("%02d", as.numeric(parts[2])), "-", 
                   parts[3]))
    }
    
    # Cas 2: Format "jj/mm/aa" -> "jj-mm-20aa"
    if(grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", date_str)) {
      parts <- strsplit(date_str, "/")[[1]]
      year <- as.numeric(parts[3])
      # Assuming years 00-30 are 2000s, 31-99 are 1900s
      full_year <- ifelse(year <= 30, 2000 + year, 1900 + year)
      return(paste0(sprintf("%02d", as.numeric(parts[1])), "-", 
                   sprintf("%02d", as.numeric(parts[2])), "-", full_year))
    }
    
    # Cas 3: Format "jjmmaaaa" -> "jj-mm-aaaa"
    if(grepl("^\\d{8}$", date_str)) {
      jour <- sprintf("%02d", as.numeric(substr(date_str, 1, 2)))
      mois <- sprintf("%02d", as.numeric(substr(date_str, 3, 4)))
      annee <- substr(date_str, 5, 8)
      return(paste0(jour, "-", mois, "-", annee))
    }
    
    # Cas 4: Format "jj/mmaaaa" -> "jj-mm-aaaa"
    if(grepl("^\\d{1,2}/\\d{6}$", date_str)) {
      parts <- strsplit(date_str, "/")[[1]]
      mm_yyyy <- parts[2]
      jour <- sprintf("%02d", as.numeric(parts[1]))
      mois <- sprintf("%02d", as.numeric(substr(mm_yyyy, 1, 2)))
      annee <- substr(mm_yyyy, 3, 6)
      return(paste0(jour, "-", mois, "-", annee))
    }
    
    # Cas 5: Année seule "aaaa" -> "01-01-aaaa"
    if(grepl("^\\d{4}$", date_str)) {
      return(paste0("01-01-", date_str))
    }
    
    # Cas 6: Format "j/m/aaaa" ou "jj/m/aaaa" ou "j/mm/aaaa" -> "jj-mm-aaaa"
    if(grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}$", date_str)) {
      parts <- strsplit(date_str, "/")[[1]]
      jour <- sprintf("%02d", as.numeric(parts[1]))
      mois <- sprintf("%02d", as.numeric(parts[2]))
      annee <- parts[3]
      # Si année sur 2 chiffres, convertir en 4 chiffres
      if(nchar(annee) == 2) {
        year_num <- as.numeric(annee)
        annee <- ifelse(year_num <= 30, paste0("20", annee), paste0("19", annee))
      }
      return(paste0(jour, "-", mois, "-", annee))
    }
    
    # Cas 7: Caractères spéciaux ou formats non reconnus
    if(grepl("^[^0-9/\\-]*$", date_str)) {
      return(NA)  # Non-numeric strings like "TLD"
    }
    
    # Format non reconnu - retourner NA avec avertissement
    return(NA)
  }
  
  # Appliquer la fonction de normalisation
  data_complete$DEBUT_TTT <- sapply(data_complete$DEBUT_TTT, uniformiser_date)
  
  # Statistiques de transformation
  nb_transforme <- sum(!is.na(data_complete$DEBUT_TTT))
  nb_original <- sum(!is.na(original_debut_ttt))
  nb_perdu <- nb_original - nb_transforme
  
  cat("Résultats de l'uniformisation:\n")
  cat("- Valeurs originales non-NA:", nb_original, "\n")
  cat("- Valeurs transformées:", nb_transforme, "\n")
  cat("- Valeurs perdues (non reconnues):", nb_perdu, "\n")
  
  # Exemples de transformation
  cat("Exemples de dates uniformisées:\n")
  exemples <- head(data_complete$DEBUT_TTT[!is.na(data_complete$DEBUT_TTT)], 10)
  print(exemples)
  cat("\n")
  
} else {
  cat("Variable DEBUT_TTT non trouvée\n")
}

# -------------------------------
# 1.7️⃣ Créer la variable DUREE_TTT en mois
# -------------------------------
if("DEBUT_TTT" %in% names(data_complete) && "DATE_ENQUETE" %in% names(data_complete)) {
  cat("Création de la variable DUREE_TTT (durée en mois)\n")
  
  # Fonction pour calculer la différence en mois
  calculer_duree_mois <- function(date_debut, date_enquete) {
    if(is.na(date_debut) || is.na(date_enquete) || date_debut == "" || date_enquete == "") {
      return(NA)
    }
    
    tryCatch({
      # Convertir DEBUT_TTT (format jj-mm-aaaa) en date
      if(grepl("^\\d{2}-\\d{2}-\\d{4}$", date_debut)) {
        parts_debut <- strsplit(date_debut, "-")[[1]]
        date_debut_obj <- as.Date(paste(parts_debut[3], parts_debut[2], parts_debut[1], sep="-"))
      } else {
        return(NA)
      }
      
      # Convertir DATE_ENQUETE (format jj-mm-aaaa) en date
      date_enquete_clean <- gsub("/", "-", date_enquete)
      if(grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", date_enquete_clean)) {
        parts_enquete <- strsplit(date_enquete_clean, "-")[[1]]
        # Assurer le formatage à 2 chiffres pour jour et mois
        jour <- sprintf("%02d", as.numeric(parts_enquete[1]))
        mois <- sprintf("%02d", as.numeric(parts_enquete[2]))
        annee <- parts_enquete[3]
        date_enquete_obj <- as.Date(paste(annee, mois, jour, sep="-"))
      } else {
        return(NA)
      }
      
      # Calculer la différence en mois
      # Utiliser la fonction difftime puis convertir en mois approximatifs
      diff_jours <- as.numeric(date_enquete_obj - date_debut_obj)
      duree_mois <- round(diff_jours / 30.44, 1)  # 30.44 = moyenne de jours par mois
      
      return(duree_mois)
      
    }, error = function(e) {
      return(NA)
    })
  }
  
  # Appliquer le calcul
  data_complete$DUREE_TTT <- mapply(calculer_duree_mois, 
                                    data_complete$DEBUT_TTT, 
                                    data_complete$DATE_ENQUETE)
  
  # Statistiques
  durees_valides <- data_complete$DUREE_TTT[!is.na(data_complete$DUREE_TTT)]
  nb_valides <- length(durees_valides)
  nb_total <- nrow(data_complete)
  
  cat("Résultats du calcul de DUREE_TTT:\n")
  cat("- Durées calculées:", nb_valides, "sur", nb_total, "\n")
  cat("- Durée moyenne:", round(mean(durees_valides), 1), "mois\n")
  cat("- Durée médiane:", round(median(durees_valides), 1), "mois\n")
  cat("- Min-Max:", round(min(durees_valides), 1), "-", round(max(durees_valides), 1), "mois\n")
  
  # Distribution par quartiles
  cat("- Quartiles:\n")
  print(round(quantile(durees_valides, probs = c(0.25, 0.5, 0.75)), 1))
  
  # Exemples
  cat("Exemples de calculs:\n")
  exemples <- head(data_complete[!is.na(data_complete$DUREE_TTT), 
                                c("DEBUT_TTT", "DATE_ENQUETE", "DUREE_TTT")], 8)
  print(exemples)
  cat("\n")
  
} else {
  variables_manquantes <- c()
  if(!"DEBUT_TTT" %in% names(data_complete)) variables_manquantes <- c(variables_manquantes, "DEBUT_TTT")
  if(!"DATE_ENQUETE" %in% names(data_complete)) variables_manquantes <- c(variables_manquantes, "DATE_ENQUETE")
  cat("Variable(s) manquante(s) pour calculer DUREE_TTT:", paste(variables_manquantes, collapse=", "), "\n")
}

# -------------------------------
# 1.8️⃣ Supprimer les variables DATE_ID et HEURE_ID
# -------------------------------
variables_a_supprimer <- c("DATE_ID", "HEURE_ID")
variables_presentes <- intersect(variables_a_supprimer, names(data_complete))

if(length(variables_presentes) > 0) {
  cat("Suppression des variables:", paste(variables_presentes, collapse=", "), "\n")
  
  # Compter les variables avant suppression
  nb_variables_avant <- ncol(data_complete)
  
  # Supprimer les variables
  data_complete <- data_complete[, !names(data_complete) %in% variables_a_supprimer]
  
  # Compter les variables après suppression
  nb_variables_apres <- ncol(data_complete)
  nb_supprimees <- nb_variables_avant - nb_variables_apres
  
  cat("- Variables supprimées:", nb_supprimees, "\n")
  cat("- Variables restantes:", nb_variables_apres, "\n")
  cat("\n")
} else {
  cat("Variables DATE_ID et HEURE_ID non trouvées dans le dataset\n")
}



# -------------------------------
# 2️⃣ Lire le fichier JSON (format CSPro)
# -------------------------------
labels_list <- list()

tryCatch({
  # Lire le fichier JSON
  dict_data <- fromJSON("dico_stigma.dcf", simplifyVector = FALSE)
  cat("Dictionnaire chargé avec succès\n")
  
  # -------------------------------
  # 3️⃣ Extraire les labels depuis le JSON
  # -------------------------------
  
  # Fonction récursive pour extraire les valueSets
  extract_valuesets <- function(items) {
    if(is.null(items)) return(NULL)
    
    for(item in items) {
      if(!is.null(item$name) && !is.null(item$valueSets)) {
        var_name <- item$name
        
        for(vs in item$valueSets) {
          if(!is.null(vs$values)) {
            labels_list[[var_name]] <<- list()
            
            for(value in vs$values) {
              if(!is.null(value$pairs) && !is.null(value$labels)) {
                code <- value$pairs[[1]]$value
                if(!is.null(value$labels[[1]]$text)) {
                  label <- value$labels[[1]]$text
                  labels_list[[var_name]][[code]] <<- label
                }
              }
            }
          }
        }
      }
      
      # Traitement récursif pour les niveaux imbriqués
      if(!is.null(item$records)) {
        for(record in item$records) {
          if(!is.null(record$items)) {
            extract_valuesets(record$items)
          }
        }
      }
      if(!is.null(item$items)) {
        extract_valuesets(item$items)
      }
    }
  }
  
  # Extraire depuis le niveau principal
  if(!is.null(dict_data$levels)) {
    for(level in dict_data$levels) {
      if(!is.null(level$ids$items)) {
        extract_valuesets(level$ids$items)
      }
      if(!is.null(level$records)) {
        for(record in level$records) {
          if(!is.null(record$items)) {
            extract_valuesets(record$items)
          }
        }
      }
    }
  }
  
  cat("Nombre de variables avec labels:", length(labels_list), "\n")
  
}, error = function(e) {
  cat("Erreur lors du chargement du dictionnaire JSON:", e$message, "\n")
  cat("Le script continuera sans appliquer les labels\n")
})

# -------------------------------
# 4️⃣ Appliquer les labels aux variables existantes (méthode R standard)
# -------------------------------
variables_avec_labels <- 0

for(var in names(labels_list)){
  if(var %in% names(data_complete)){
    tryCatch({
      # Convertir en facteur avec labels
      current_data <- data_complete[[var]]
      
      # Créer le vecteur des niveaux et labels
      codes <- names(labels_list[[var]])
      labels <- unlist(labels_list[[var]])
      
      # Créer le facteur
      data_complete[[var]] <- factor(current_data, 
                                   levels = codes, 
                                   labels = labels)
      
      variables_avec_labels <- variables_avec_labels + 1
      
    }, error = function(e) {
      cat("Erreur pour la variable", var, ":", e$message, "\n")
    })
  }
}

cat("Nombre de variables auxquelles les labels ont été appliqués:", variables_avec_labels, "\n")

# -------------------------------
# 4.9️⃣ Renseigner REGION_MILITAIRE à partir du SITE
# -------------------------------
if("SITE" %in% names(data_complete) && "REGION_MILITAIRE" %in% names(data_complete)) {
  cat("Renseignement de REGION_MILITAIRE à partir du SITE\n")
  
  # Fonction pour déterminer la région militaire basée sur la ville dans le site
  determiner_region <- function(site_name) {
    if(is.na(site_name) || site_name == "") return(NA)
    
    site_upper <- toupper(as.character(site_name))
    
    # 1ère Région Militaire (Sud-Est) - Abidjan et environs
    if(grepl("ABIDJAN|ABOBO|YOPOUGON|AKOUEDO|PLATEAU|TREICHVILLE|AGBAN|ADIAKÉ|ABOISSO|INDÉNIÉ|INDE|HMA|CPS", site_upper)) {
      return("1ère Région Militaire")
    }
    
    # 2e Région Militaire (Sud-Ouest) - San-Pedro et environs
    if(grepl("SAN-PEDRO|PEDRO|SOUBRE|GUIGLO|MAN|DALOA", site_upper)) {
      return("2e Région Militaire")
    }
    
    # 3e Région Militaire (Centre) - Yamoussoukro, Bouaké et environs
    if(grepl("YAMOUSSOUKRO|BOUAKÉ|BOUAKE|DIMBOKRO|DAOUKRO|BONDOUKOU", site_upper)) {
      return("3e Région Militaire")
    }
    
    # 4e Région Militaire (Nord) - Korhogo, Odienné et environs
    if(grepl("KORHOGO|ODIENNÉ|ODIENN|SEGUELA", site_upper)) {
      return("4e Région Militaire")
    }
    
    # Si aucune correspondance trouvée
    return(NA)
  }
  
  # Sauvegarder les valeurs originales
  region_originale <- data_complete$REGION_MILITAIRE
  nb_na_avant <- sum(is.na(region_originale))
  
  # Appliquer la fonction de détermination
  data_complete$REGION_MILITAIRE <- sapply(data_complete$SITE, determiner_region)
  
  # Convertir en facteur avec les bons niveaux
  data_complete$REGION_MILITAIRE <- factor(data_complete$REGION_MILITAIRE,
                                          levels = c("1ère Région Militaire", 
                                                   "2e Région Militaire", 
                                                   "3e Région Militaire", 
                                                   "4e Région Militaire"))
  
  # Statistiques de renseignement
  nb_na_apres <- sum(is.na(data_complete$REGION_MILITAIRE))
  nb_renseignes <- nb_na_avant - nb_na_apres
  
  cat("Résultats du renseignement:\n")
  cat("- Valeurs NA avant:", nb_na_avant, "\n")
  cat("- Valeurs NA après:", nb_na_apres, "\n")
  cat("- Valeurs renseignées:", nb_renseignes, "\n")
  
  # Distribution par région
  cat("Distribution par région militaire:\n")
  print(table(data_complete$REGION_MILITAIRE, useNA = "ifany"))
  
  # Exemples de correspondances
  cat("\nExemples de correspondances SITE -> REGION_MILITAIRE:\n")
  exemples <- unique(data_complete[!is.na(data_complete$REGION_MILITAIRE), 
                                  c("SITE", "REGION_MILITAIRE")])[1:10,]
  print(exemples)
  cat("\n")
  
} else {
  variables_manquantes <- c()
  if(!"SITE" %in% names(data_complete)) variables_manquantes <- c(variables_manquantes, "SITE")
  if(!"REGION_MILITAIRE" %in% names(data_complete)) variables_manquantes <- c(variables_manquantes, "REGION_MILITAIRE")
  cat("Variable(s) manquante(s):", paste(variables_manquantes, collapse=", "), "\n")
}

# -------------------------------
# 5️⃣ Vérification (échantillon)
# -------------------------------
cat("\n=== VÉRIFICATION DES LABELS ===\n")
vars_to_check <- intersect(names(labels_list), names(data_complete))
if(length(vars_to_check) > 0) {
  vars_to_check <- vars_to_check[seq_len(min(5, length(vars_to_check)))]
  
  for(var in vars_to_check){
    cat("\nVariable:", var, "\n")
    tryCatch({
      print(table(data_complete[[var]], useNA = "ifany"))
    }, error = function(e) {
      cat("Erreur lors de l'affichage de", var, ":", e$message, "\n")
    })
  }
}

# -------------------------------
# 6️⃣ Résumé final et sauvegarde
# -------------------------------
cat("\n=== RÉSUMÉ FINAL ===\n")
cat("Dimensions finales:", dim(data_complete), "\n")
cat("Variables avec labels appliqués:", variables_avec_labels, "\n")
cat("Nombre total de variables:", ncol(data_complete), "\n")

# -------------------------------
# 7️⃣ Recherche d'enregistrements problématiques
# -------------------------------
cat("\n=== RECHERCHE D'ENREGISTREMENTS PROBLÉMATIQUES ===\n")

# 1. AGE inférieur à 18 ans
cat("\n1. Enregistrements avec AGE < 18 ans:\n")
age_mineur <- data_complete$AGE < 18 & !is.na(data_complete$AGE)
if(any(age_mineur)) {
  cat("Nombre d'enregistrements:", sum(age_mineur), "\n")
  enreg_mineurs <- data_complete[age_mineur, 
                                c("ID_UNIQUE", "NUM_QUESTIONNAIRE", "AGE", "SEXE", "SITE")]
  print(enreg_mineurs)
} else {
  cat("Aucun enregistrement avec AGE < 18 ans\n")
}

# 2. Durée de traitement inférieure à 12 mois
cat("\n2. Enregistrements avec DUREE_TTT < 12 mois:\n")
duree_courte <- data_complete$DUREE_TTT < 12 & !is.na(data_complete$DUREE_TTT)
if(any(duree_courte)) {
  cat("Nombre d'enregistrements:", sum(duree_courte), "\n")
  enreg_duree_courte <- data_complete[duree_courte, 
                                     c("ID_UNIQUE", "NUM_QUESTIONNAIRE", "DUREE_TTT", 
                                       "DEBUT_TTT", "DATE_ENQUETE")]
  print(enreg_duree_courte)
} else {
  cat("Aucun enregistrement avec DUREE_TTT < 12 mois\n")
}

# 3. Enregistrements avec toutes les variables vides sauf les variables de base
cat("\n3. Enregistrements avec données quasi-vides:\n")
variables_base <- c("OPERATOR_ID", "CODE", "NUM_QUESTIONNAIRE", "DATE_ENQUETE", "HEURE_ENQUETE")
variables_autres <- setdiff(names(data_complete), variables_base)

# Compter les valeurs non-NA pour chaque enregistrement (hors variables de base)
nb_valeurs_renseignees <- apply(data_complete[, variables_autres], 1, function(x) sum(!is.na(x)))

# Identifier les enregistrements avec très peu de données (seuil: <= 3 variables renseignées)
quasi_vides <- nb_valeurs_renseignees <= 3
if(any(quasi_vides)) {
  cat("Nombre d'enregistrements quasi-vides (≤3 variables renseignées):", sum(quasi_vides), "\n")
  enreg_quasi_vides <- data_complete[quasi_vides, 
                                    c("ID_UNIQUE", "NUM_QUESTIONNAIRE", "AGE", "SEXE", 
                                      "DEBUT_TTT", "DUREE_TTT")]
  enreg_quasi_vides$nb_variables_renseignees <- nb_valeurs_renseignees[quasi_vides]
  print(enreg_quasi_vides)
} else {
  cat("Aucun enregistrement quasi-vide détecté\n")
}

# 4. DEBUT_TTT vide
cat("\n4. Enregistrements avec DEBUT_TTT vide:\n")
debut_ttt_vide <- is.na(data_complete$DEBUT_TTT) | data_complete$DEBUT_TTT == ""
if(any(debut_ttt_vide)) {
  cat("Nombre d'enregistrements:", sum(debut_ttt_vide), "\n")
  enreg_debut_vide <- data_complete[debut_ttt_vide, 
                                   c("ID_UNIQUE", "NUM_QUESTIONNAIRE", "DEBUT_TTT", 
                                     "DUREE_TTT", "AGE", "SEXE")]
  print(head(enreg_debut_vide, 15))  # Limiter l'affichage si trop nombreux
  if(sum(debut_ttt_vide) > 15) {
    cat("... et", sum(debut_ttt_vide) - 15, "autres enregistrements\n")
  }
} else {
  cat("Aucun enregistrement avec DEBUT_TTT vide\n")
}

# 5. Résumé des problèmes
cat("\n=== RÉSUMÉ DES PROBLÈMES DÉTECTÉS ===\n")
cat("- AGE < 18 ans:", sum(age_mineur), "enregistrements\n")
cat("- DUREE_TTT < 12 mois:", sum(duree_courte), "enregistrements\n")
cat("- Quasi-vides (≤3 variables):", sum(quasi_vides), "enregistrements\n")
cat("- DEBUT_TTT vide:", sum(debut_ttt_vide), "enregistrements\n")

# Enregistrements avec plusieurs problèmes
problemes_multiples <- age_mineur | duree_courte | quasi_vides | debut_ttt_vide
cat("- Total unique avec au moins 1 problème:", sum(problemes_multiples), "enregistrements\n")

if(sum(problemes_multiples) > 0) {
  cat("\nPourcentage d'enregistrements problématiques:", 
      round(sum(problemes_multiples)/nrow(data_complete)*100, 1), "%\n")
}

cat("\n")

# Sauvegarder le résultat
save(data_complete, file = "donnees_avec_labels.RData")
cat("Données sauvegardées dans 'donnees_avec_labels.RData'\n")

# -------------------------------
# 8️⃣ Génération du rapport de qualité
# -------------------------------
cat("\n=== GÉNÉRATION DU RAPPORT DE QUALITÉ ===\n")
cat("Génération du rapport détaillé de qualité des données...\n")

# Rediriger la sortie vers un fichier
rapport_file <- "RAPPORT_QUALITE_DONNEES.txt"
sink(rapport_file)
tryCatch({
  source("rapport_qualite_donnees.R")
}, error = function(e) {
  cat("Erreur lors de la génération du rapport:", e$message, "\n")
})
sink()

cat("Rapport de qualité sauvegardé dans '", rapport_file, "'\n", sep = "")

# -------------------------------
# 9️⃣ Nettoyage des enregistrements problématiques
# -------------------------------

cat("\n=== NETTOYAGE DES ENREGISTREMENTS PROBLÉMATIQUES ===\n\n")

# Variables de base à conserver pour l'identification
variables_base <- c("OPERATOR_ID", "CODE", "NUM_QUESTIONNAIRE", "DATE_ENQUETE", "HEURE_ENQUETE")

# Dataset original
n_original <- nrow(data_complete)
cat("Nombre d'enregistrements original :", n_original, "\n\n")

# 1. IDENTIFIER LES ENREGISTREMENTS PROBLÉMATIQUES
cat("1. IDENTIFICATION DES ENREGISTREMENTS PROBLÉMATIQUES\n")
cat("====================================================\n")

# AGE < 18 ans
age_mineur <- data_complete$AGE < 18 & !is.na(data_complete$AGE)
ids_age_mineur <- data_complete[age_mineur, c("ID_UNIQUE", "CODE", "NUM_QUESTIONNAIRE", "AGE", "SEXE")]

# Durée de traitement < 12 mois
duree_courte <- data_complete$DUREE_TTT < 12 & !is.na(data_complete$DUREE_TTT)
ids_duree_courte <- data_complete[duree_courte, c("ID_UNIQUE", "CODE", "NUM_QUESTIONNAIRE", "DUREE_TTT", "DEBUT_TTT")]

# DEBUT_TTT vide
debut_vide <- is.na(data_complete$DEBUT_TTT) | data_complete$DEBUT_TTT == ""
ids_debut_vide <- data_complete[debut_vide, c("ID_UNIQUE", "CODE", "NUM_QUESTIONNAIRE", "DEBUT_TTT", "AGE")]

# Enregistrements quasi-vides (≤3 variables renseignées hors variables de base)
variables_autres <- setdiff(names(data_complete), variables_base)
nb_valeurs_renseignees <- apply(data_complete[, variables_autres], 1, function(x) sum(!is.na(x)))
quasi_vides <- nb_valeurs_renseignees <= 3
ids_quasi_vides <- data_complete[quasi_vides, c("ID_UNIQUE", "CODE", "NUM_QUESTIONNAIRE", "AGE", "SEXE")]
ids_quasi_vides$nb_variables_renseignees <- nb_valeurs_renseignees[quasi_vides]

# Affichage des listes
cat("\nA. PARTICIPANTS <18 ANS :", sum(age_mineur), "enregistrements\n")
if(sum(age_mineur) > 0) {
  print(ids_age_mineur)
  cat("Codes concernés:", paste(ids_age_mineur$CODE, collapse=", "), "\n")
}

cat("\nB. DURÉE DE TRAITEMENT <12 MOIS :", sum(duree_courte), "enregistrements\n")
if(sum(duree_courte) > 0) {
  print(ids_duree_courte)
  cat("Codes concernés:", paste(ids_duree_courte$CODE, collapse=", "), "\n")
}

cat("\nC. DATE DÉBUT TRAITEMENT VIDE :", sum(debut_vide), "enregistrements\n")
if(sum(debut_vide) > 0) {
  if(sum(debut_vide) <= 10) {
    print(ids_debut_vide)
  } else {
    print(head(ids_debut_vide, 10))
    cat("... et", sum(debut_vide) - 10, "autres\n")
  }
  codes_debut_vide <- ids_debut_vide$CODE
  if(length(codes_debut_vide) <= 15) {
    cat("Codes concernés:", paste(codes_debut_vide, collapse=", "), "\n")
  } else {
    cat("Codes concernés:", paste(head(codes_debut_vide, 15), collapse=", "), ", ...\n")
  }
}

cat("\nD. ENREGISTREMENTS QUASI-VIDES :", sum(quasi_vides), "enregistrements\n")
if(sum(quasi_vides) > 0) {
  print(ids_quasi_vides)
  cat("Codes concernés:", paste(ids_quasi_vides$CODE, collapse=", "), "\n")
}

# 2. STRATÉGIE DE NETTOYAGE
cat("\n\n2. STRATÉGIE DE NETTOYAGE\n")
cat("=========================\n")

# Définir les enregistrements à supprimer (TOUS les critères problématiques)
a_supprimer <- age_mineur | quasi_vides | duree_courte | debut_vide

cat("ENREGISTREMENTS À SUPPRIMER (TOUS les critères problématiques) :\n")
cat("• AGE < 18 ans :", sum(age_mineur), "\n")
cat("• Quasi-vides :", sum(quasi_vides), "\n")
cat("• Durée traitement < 12 mois :", sum(duree_courte), "\n")
cat("• Date début manquante :", sum(debut_vide), "\n")
cat("• TOTAL À SUPPRIMER :", sum(a_supprimer), "\n")

# Calculer les chevauchements
chevauchements <- sum(age_mineur & quasi_vides) + sum(age_mineur & duree_courte) + 
                  sum(age_mineur & debut_vide) + sum(quasi_vides & duree_courte) + 
                  sum(quasi_vides & debut_vide) + sum(duree_courte & debut_vide)
cat("• Chevauchements entre critères :", chevauchements, "\n")

# 3. LISTE COMPLÈTE DES CODES À SUPPRIMER
cat("\n\n3. LISTE DES CODES À SUPPRIMER\n")
cat("==============================\n")

if(sum(a_supprimer) > 0) {
  enreg_a_supprimer <- data_complete[a_supprimer, c("CODE", "SITE", "OPERATOR_ID", "NUM_QUESTIONNAIRE", "AGE", "SEXE", "DEBUT_TTT")]
  # Déterminer les raisons (peut y en avoir plusieurs)
  enreg_a_supprimer$RAISON <- apply(cbind(age_mineur[a_supprimer], 
                                          quasi_vides[a_supprimer], 
                                          duree_courte[a_supprimer], 
                                          debut_vide[a_supprimer]), 1, function(x) {
    raisons <- c()
    if(x[1]) raisons <- c(raisons, "Patient de moins de 18 ans")
    if(x[2]) raisons <- c(raisons, "Enregistrement vide")
    if(x[3]) raisons <- c(raisons, "Durée de traitement < 12 mois")
    if(x[4]) raisons <- c(raisons, "Date de début de traitement manquante")
    paste(raisons, collapse="; ")
  })
  
  cat("Enregistrements à supprimer :\n")
  print(enreg_a_supprimer)
  
  codes_supprimes <- enreg_a_supprimer$CODE
  cat("\nLISTE DES CODES SUPPRIMÉS :\n")
  cat(paste(codes_supprimes, collapse=", "), "\n")
  
  # Sauvegarder la liste des codes supprimés
  # Forcer le format texte avec apostrophe pour éviter la notation scientifique dans OnlyOffice
  enreg_a_supprimer$CODE <- paste0("'", enreg_a_supprimer$CODE)
  write.csv(enreg_a_supprimer, "enregistrements_supprimes.csv", row.names = FALSE)
  
  # Créer un fichier alternatif avec point-virgule comme séparateur pour OnlyOffice
  write.csv2(enreg_a_supprimer, "enregistrements_supprimes_onlyoffice.csv", row.names = FALSE)
  cat("Liste sauvegardée dans : enregistrements_supprimes.csv et enregistrements_supprimes_onlyoffice.csv\n")
} else {
  cat("Aucun enregistrement à supprimer selon les critères stricts.\n")
}

# 4. CRÉATION DU DATASET NETTOYÉ
cat("\n\n4. CRÉATION DU DATASET NETTOYÉ\n")
cat("===============================\n")

# Créer le dataset nettoyé
if(sum(a_supprimer) > 0) {
  data <- data_complete[!a_supprimer, ]
} else {
  data <- data_complete
}

n_clean <- nrow(data)
n_supprimes <- n_original - n_clean

cat("• Enregistrements originaux :", n_original, "\n")
cat("• Enregistrements supprimés :", n_supprimes, "\n")
cat("• Enregistrements conservés :", n_clean, "\n")
cat("• Taux de conservation :", round(100 * n_clean / n_original, 1), "%\n")

# Recalculer l'ID_UNIQUE pour le dataset nettoyé
data$ID_UNIQUE <- seq_len(nrow(data))

# 5. VÉRIFICATION DE LA QUALITÉ APRÈS NETTOYAGE
cat("\n\n5. QUALITÉ APRÈS NETTOYAGE\n")
cat("==========================\n")

# Vérifications
age_valides <- sum(data$AGE >= 18, na.rm = TRUE)
age_manquants <- sum(is.na(data$AGE))

cat("• Participants ≥18 ans :", age_valides, "\n")
cat("• Âges manquants :", age_manquants, "\n")

if("DUREE_TTT" %in% names(data)) {
  durees_valides <- sum(!is.na(data$DUREE_TTT))
  durees_courtes_restantes <- sum(data$DUREE_TTT < 12, na.rm = TRUE)
  cat("• Durées de traitement calculées :", durees_valides, "\n")
  cat("• Durées < 12 mois restantes :", durees_courtes_restantes, "\n")
}

debut_valides <- sum(!is.na(data$DEBUT_TTT) & data$DEBUT_TTT != "")
cat("• Dates de début renseignées :", debut_valides, "\n")

# Complétude générale
completude_moyenne <- round(mean(sapply(data, function(x) 100 * sum(!is.na(x)) / length(x))), 1)
cat("• Complétude moyenne :", completude_moyenne, "%\n")

# 6. SAUVEGARDE DU DATASET NETTOYÉ
cat("\n\n6. SAUVEGARDE\n")
cat("=============\n")

# Sauvegarder le dataset nettoyé
save(data, file = "donnees_nettoyees.RData")
cat("Dataset nettoyé sauvegardé : donnees_nettoyees.RData\n")

# Sauvegarder aussi en CSV pour inspection
# Forcer le format texte avec apostrophe pour éviter la notation scientifique dans OnlyOffice
data$CODE <- paste0("'", as.character(data$CODE))
write.csv(data, "donnees_nettoyees.csv", row.names = FALSE)

# Créer un fichier alternatif avec point-virgule comme séparateur pour OnlyOffice
write.csv2(data, "donnees_nettoyees_onlyoffice.csv", row.names = FALSE)
cat("Dataset nettoyé sauvegardé : donnees_nettoyees.csv et donnees_nettoyees_onlyoffice.csv\n")

# 7. RÉSUMÉ FINAL
cat("\n\n7. RÉSUMÉ DU NETTOYAGE\n")
cat("======================\n")

cat("CRITÈRES DE SUPPRESSION APPLIQUÉS :\n")
cat("✓ Participants <18 ans : SUPPRIMÉS\n")
cat("✓ Enregistrements quasi-vides : SUPPRIMÉS\n")
cat("✓ Durées <12 mois : SUPPRIMÉS\n")
cat("✓ Dates début manquantes : SUPPRIMÉS\n")

cat("\nRÉSULTAT FINAL :\n")
cat("• Dataset original :", n_original, "observations\n")
cat("• Dataset nettoyé :", n_clean, "observations\n")
cat("• Perte :", n_supprimes, "observations (", round(100 * n_supprimes / n_original, 1), "%)\n")
cat("• Qualité :", completude_moyenne, "% de complétude\n")

cat("\nLe dataset est maintenant prêt pour l'analyse !\n")

cat("\n=== FIN DU NETTOYAGE ===\n")

