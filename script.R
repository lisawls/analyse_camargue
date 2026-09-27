# ANALYSE TERRITORIALE — COMMUNES CIBLES

# CONFIG ----
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(forcats)
library(readxl)
library(janitor)
library(DT)
library(ggplot2)
library(sf)
library(writexl)
library(ggrepel)
library(biscale)
library(cowplot)
library(patchwork)


# Chemins relatifs à la racine du projet (ouvrir analyse_camargue.Rproj)
DATA_DIR <- "data/raw"
DATA_DIR_processed <- "data/processed"
OUTPUT <- "output"

communes_cibles <- c("30059", #Le Cailar
                     "30341", #Vauvert
                     "30258", #Saint Gilles
                     "30091", #Congénies
                     "30347", #Vestric-et-Candiac,
                     "30276", #Saint Laurent d'Aigouze
                     "30006", #Aimargues
                     "30003", #Aigues-Mortes,
                     "30321", #Sommières
                     "30123", #Gallargues le Montueux
                     "30032", #Beaucaire
                     "30004", #Aigues-Vives
                     "30083", #Codognan
                     "30344", #Vergèze
                     "30333", #Uchaud
                     "30133", #Grau du roi
                     "30189", #Nimes
                     "13004" #Arles,
                     # "30033", #Beauvoisin
) 

nuances_gauche_2026 <- c("LEXG", "LUG", "LECO", "LCOM", "LFI", "LSOC", "LDVG", "LVEC")
nuances_gauche_2020 <- c("LEXG", "LRDG","LUG", "LECO", "LCOM", "LFI", "LSOC", "LDVG", "LVEC")
nuances_gauche_2024 <- c("EXG", "UG", "ECO", #dans la base
                         "COM", "FI", "SOC", "RDG", "DVG", "VEC") # pas dans la base
nuances_gauche_2017 <- c("EXG", "COM", "FI", "SOC", "ECO", "DVG", # dans la base
                         "RDG") # pas dans la base
candidats_gauche_2022 <- c("MÉLENCHON", "HIDALGO", "JADOT", "ROUSSEL", "POUTOU", "ARTHAUD") # candidats gauche/NFP au T1

candidats_gauche_2002 <- c("JOSPIN", "LAGUILLER", "CHEVENEMENT", "MAMERE", "BESANCENOT", "HUE", "TAUBIRA", "LEPAGE", "GLUCKSTEIN") # candidats gauche/NFP au T1

annees <- c("02", "17", "22", "24")

type_election <- c(
  "02" = "présidentielles",
  "17" = "législatives",
  "22" = "présidentielles",
  "24" = "législatives"
)
# FONCTIONS----
path <- function(...) file.path(DATA_DIR, ...)

# Pivote un fichier électoral du format wide (une ligne par commune, un bloc de colonnes
# par candidat) vers le format long (une ligne par commune × candidat).
# Passer le fichier brut AVANT tout right_join — les colonnes jointes faussent le compte.
pivot_election <- function(df, n_cols_fixes, vars_candidat) {
  
  n_vars     <- length(vars_candidat)
  n_candidats <- (ncol(df) - n_cols_fixes) / n_vars
  if (n_candidats %% 1 != 0) stop("Le nombre de colonnes candidats n'est pas divisible par n_vars — vérifie n_cols_fixes ou vars_candidat")
  
  names(df)[(n_cols_fixes + 1):ncol(df)] <- paste0(
    rep(vars_candidat, times = n_candidats),
    "_",
    rep(1:n_candidats, each = n_vars)
  )
  
  df %>%
    pivot_longer(
      cols          = (n_cols_fixes + 1):ncol(.),
      names_to      = c(".value", "num_candidat"),
      names_pattern = "^(.+)_(\\d+)$"
    ) %>%
    filter(!is.na(nb_voix) & nb_voix != "")
}

normalize <- function(x) {
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) return(rep(0, length(x)))
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# RÉFÉRENTIEL COMMUNES ----
selection_communes <- read_csv(path("v_commune_2025.csv")) %>%
  select(COM, 
         #NCC, 
         #NCCENR, 
         LIBELLE) %>%
  filter(COM %in% communes_cibles) %>% clean_names()

# CHARGEMENT DES DONNÉES BRUTES----
## Recensement INSEE 2022 ----
insee_recensement_raw <- list(
  read_delim(path("insee_recensement/base-cc-coupl-fam-men-2022.CSV"),          delim = ";", trim_ws = TRUE),
  read_delim(path("insee_recensement/base-cc-evol-struct-pop-2022.CSV"),        delim = ";", trim_ws = TRUE),
  read_delim(path("insee_recensement/base-cc-logement-2022.CSV"),               delim = ";", trim_ws = TRUE),
  read_delim(path("insee_recensement/base-cc-diplomes-formation-2022.CSV"),     delim = ";", trim_ws = TRUE),
  read_delim(path("insee_recensement/base-cc-emploi-pop-active-2022.CSV"),      delim = ";", trim_ws = TRUE),
  read_delim(path("insee_recensement/base-cc-caract_emp-2022.CSV"),             delim = ";", trim_ws = TRUE)
) %>%
  reduce(left_join, by = "CODGEO") %>%
  left_join(
    read_delim(path("insee_recensement/base-cc-serie-historique-2022.CSV"), delim = ";", trim_ws = TRUE) %>%
      select(CODGEO, SUPERF),
    by = "CODGEO"
  ) %>%
  filter(CODGEO %in% communes_cibles) %>%
  left_join(selection_communes, by = c("CODGEO" = "com")) %>%
  select(com = CODGEO, libelle, SUPERF, starts_with("P22_"), starts_with("C22_"))

## Pauvreté ----
precarite <- read_excel(path("precarite/20260306-indicateurs-precarite.xlsx")) %>%
  mutate(across(-c(ID, `Niveau géographique`, NOM), as.numeric)) %>% 
  filter(`ID` %in% communes_cibles) %>% 
  select(
    com = ID,
    revenu_median_2021      = `Revenu médian (Insee FiLoSoFi 2021)`,
    tx_pauvrete_2021      = `Taux de pauvreté au seuil de 60% (Insee FiLoSoFi 2021)`,
    part_rsa_2022           = `Part des allocataires du RSA (CAF 2024) parmi les ménages (Insee 2022)`,
    part_chomage_15_24_2022 = `Part des 15-24 ans actifs au chômage (Insee 2022)`,
  )

precarite_filosofi <- read_delim(
  path("precarite/base-cc-filosofi-2020_CSV/cc_filosofi_2020_COM.csv"),
  delim = ";", trim_ws = TRUE
) %>% 
  filter(`CODGEO` %in% communes_cibles) %>% 
  select(com =CODGEO,
         tx_menage_fiscaux_imposables_2020 = PIMP20,
         part_prestations_sociales_2020 = PPSOC20,
         decile1_2020 = D120,
         decile9_2020 = D920,
         rapport_interdecile_2020 = RD20) %>% 
  mutate(across(-com, ~ as.numeric(gsub(",", ".", na_if(., "s")))))

  
observatoire_territoire_raw <- read_excel(
  path("observatoire_territoires.xlsx"),
  skip = 3) %>% filter(`Code` %in% communes_cibles)

## Élections ----
election_municipale_2026_raw <- read_delim(
  path("resultat_election/election_municipale_2026_T1.csv"),
  delim = ";", trim_ws = TRUE
) %>% filter(`Code commune` %in% communes_cibles)

election_legislative_2024_raw <- read_delim(
  path("resultat_election/election_legislative_2024_T1.csv"),
  delim = ";", trim_ws = TRUE
) %>% filter(`Code commune` %in% communes_cibles)
election_presidentielle_2022_raw  <- read_excel(path("resultat_election/election_presidentielle_2022_T1.xlsx"))
election_municipale_2020_raw <- read_excel(
  path("resultat_election/election_municipale_2020_T1_commune_1000plus.xlsx"),
  col_types = "text")
election_legislative_2017_raw <- read_excel(path("resultat_election/election_legislative_2017_T1.xlsx"), skip = 3)
election_presidentielle_2002_raw  <- read_excel(path("resultat_election/election_presidentielle_2002_T1.xls"))

## Équipements sportifs ----
installation <- read_delim(
  path("data-es-installation.csv"),
  delim = ";", trim_ws = TRUE
) %>%
  filter(`insee` %in% communes_cibles) %>% 
  select(com = insee, nom_installation = nom, type_installation = install_particuliere)

## Associations ----
associations <- bind_rows(
  read_delim(path("association/rna_import_20260601_dpt_30.csv"), delim = ";", trim_ws = TRUE),
  read_delim(path("association/rna_import_20260601_dpt_13.csv"), delim = ";", trim_ws = TRUE)
) %>%
  select(libelle = libcom, titre_asso = titre, objet_asso = objet) %>%
  right_join(selection_communes %>%
               mutate(libelle = toupper(iconv(libelle, to = "ASCII//TRANSLIT")),
                      libelle = gsub("^LE |^LA |^LES ", "", libelle)),
             by ="libelle") %>%
  select(com, titre_asso, objet_asso)

asso_corrida_course_raw <- bind_rows(
  read_csv(path("association/Les Associations de l'Union des Clubs Taurins de France/CORRIDA.csv"))          %>% mutate(Type = "corrida"),
  read_csv(path("association/Les Associations de l'Union des Clubs Taurins de France/COURSE CAMARGUAISE.csv")) %>% mutate(Type = "course_camarguaise"),
  read_csv(path("association/Les Associations de l'Union des Clubs Taurins de France/COURSE LANDAISE.csv"))    %>% mutate(Type = "course_landaise"))

culture_taurine <- read_excel(
  path("culture_taurine.xlsx")) %>% 
  right_join(selection_communes, by = c("Commune" = "libelle")) %>% 
  select(-Commune)

parc_naturel_regional <- read_excel(
  path("parc_naturels_regionaux.xlsx"),
  skip = 3) %>% 
  filter(`Code` %in% communes_cibles) %>% 
  select(com = Code,
         pnr = "PNR - Parcs naturels régionaux")


# TRANSFORMATIONS----
## Recensement INSEE 2022 ----
insee_recensement <- insee_recensement_raw %>% select("com", "libelle",
                                  "SUPERF",
                                  "P22_POP",
                                  "P22_POP0014", "P22_POP1529", "P22_POP3044", "P22_POP4559", "P22_POP6074", "P22_POP7589", "P22_POP90P",
                                  "P22_POP1519",
                                  "P22_POP2024", "P22_POP2539", "P22_POP4054", "P22_POP5564.x", "P22_POP6579", "P22_POP80P", "C22_POP15P",
                                  "C22_POP15P_STAT_GSEC11_21", "C22_POP15P_STAT_GSEC12_22", "C22_POP15P_STAT_GSEC13_23", "C22_POP15P_STAT_GSEC14_24", "C22_POP15P_STAT_GSEC15_25", "C22_POP15P_STAT_GSEC16_26", "C22_POP15P_STAT_GSEC32", "C22_POP15P_STAT_GSEC40",
                                  "C22_MEN", "C22_MENPSEUL", "C22_MENHSEUL", "C22_MENFSEUL", 
                                  "C22_MENCOUPSENF", "C22_MENCOUPAENF", "C22_MENFAMMONO",
                                  "C22_MENFAM", "C22_MENSFAM", 
                                  "C22_MEN1FCOUPSENF", "C22_MEN1FCOUPUNIQENFCOUP", "C22_MEN1FMONOH", "C22_MEN1FMONOF",
                                  "P22_LOG", "P22_RP", "P22_RSECOCC", "P22_LOGVAC", "P22_MAISON", "P22_APPART",
                                  "P22_RP_PROP", "P22_RP_LOC", "P22_RP_LOCHLMV",
                                  "P22_MEN", "P22_MEN_ANEM0002", "P22_MEN_ANEM0204", "P22_MEN_ANEM0509", "P22_MEN_ANEM10P", "P22_MEN_ANEM1019", "P22_MEN_ANEM2029", "P22_MEN_ANEM30P",
                                  "P22_RP_ELEC", "P22_RP_CGAZV", "P22_RP_CFIOUL", "P22_RP_CELEC", "P22_RP_CGAZB", "P22_RP_CAUT",
                                  "P22_RP_VOIT1P",
                                  "P22_POP0205", "P22_POP0610", "P22_POP1114", "P22_POP1517", "P22_POP1824", "P22_POP2529", "P22_POP30P", "P22_SCOL0205", "P22_SCOL0610", "P22_SCOL1114", "P22_SCOL1517", "P22_SCOL1824", "P22_SCOL2529", "P22_SCOL30P",
                                  "P22_NSCOL15P", "P22_NSCOL15P_DIPLMIN", "P22_NSCOL15P_BEPC", "P22_NSCOL15P_CAPBEP", "P22_NSCOL15P_BAC", "P22_NSCOL15P_SUP2", "P22_NSCOL15P_SUP34", "P22_NSCOL15P_SUP5", 
                                  "P22_POP1564", "P22_ACT1564", "P22_ACTOCC1564", "P22_CHOM1564", "P22_CHOM_DIPLMIN", "P22_CHOM_BEPC", "P22_CHOM_CAPBEP", "P22_CHOM_BAC", "P22_CHOM_SUP2", "P22_CHOM_SUP34", "P22_CHOM_SUP5", "P22_ACT_DIPLMIN", "P22_ACT_BEPC", "P22_ACT_CAPBEP", "P22_ACT_BAC", "P22_ACT_SUP2", "P22_ACT_SUP34", "P22_ACT_SUP5", 
                                  "P22_INACT1564", "P22_ETUD1564", "P22_RETR1564", "P22_AINACT1564",
                                  "P22_ACTOCC15P","P22_ACTOCC15P_TP","P22_ACTOCC15P_ILT1", "P22_ACTOCC15P_ILT2P", "P22_ACTOCC15P_ILT2", "P22_ACTOCC15P_ILT3", "P22_ACTOCC15P_ILT4", "P22_ACTOCC15P_ILT5", 
                                  "P22_ACTOCC15P_PASTRANS", "P22_ACTOCC15P_MARCHE", "P22_ACTOCC15P_VELO", "P22_ACTOCC15P_2ROUESMOT", "P22_ACTOCC15P_VOITURE", "P22_ACTOCC15P_COMMUN", 
                                  "P22_SAL15P", "P22_NSAL15P",  "P22_SAL15P_TP",
                                  "P22_HSAL15P", "P22_HSAL15P_TP", "P22_HSAL15P_CDI", "P22_HSAL15P_CDD", "P22_HSAL15P_INTERIM", "P22_HSAL15P_EMPAID", "P22_HSAL15P_APPR",
                                  "P22_FSAL15P", "P22_FSAL15P_TP", "P22_FSAL15P_CDI", "P22_FSAL15P_CDD", "P22_FSAL15P_INTERIM", "P22_FSAL15P_EMPAID", "P22_FSAL15P_APPR", "P22_FNSAL15P",
                                  "P22_HNSAL15P", "P22_HNSAL15P_INDEP", "P22_HNSAL15P_EMPLOY", "P22_HNSAL15P_AIDFAM", "P22_FACTOCC15P",  "P22_FNSAL15P_INDEP", "P22_FNSAL15P_EMPLOY", "P22_FNSAL15P_AIDFAM",
                                  "P22_POP15P.x",
                                  "P22_ACTOCC", "P22_ACT15P") %>% mutate(
  
  # DÉMOGRAPHIE
  densite_pop_22      = P22_POP / SUPERF,
  tx_jeunes_22        = (P22_POP0014 + P22_POP1529) / P22_POP,
  tx_seniors_22       = (P22_POP6074 + P22_POP7589 + P22_POP90P) / P22_POP,
  idx_jeunesse_22     = (P22_POP0014 + P22_POP1529) / (P22_POP6074 + P22_POP7589 + P22_POP90P),
  
  # CSP
  tx_agri_22      = C22_POP15P_STAT_GSEC11_21 / C22_POP15P,
  tx_artisan_22   = C22_POP15P_STAT_GSEC12_22 / C22_POP15P,
  tx_cadre_22     = C22_POP15P_STAT_GSEC13_23 / C22_POP15P,
  tx_intermed_22  = C22_POP15P_STAT_GSEC14_24 / C22_POP15P,
  tx_employe_22   = C22_POP15P_STAT_GSEC15_25 / C22_POP15P,
  tx_ouvrier_22   = C22_POP15P_STAT_GSEC16_26 / C22_POP15P,
  tx_retraite_csp_22 = C22_POP15P_STAT_GSEC32 / C22_POP15P,
  tx_inactif_csp_22  = C22_POP15P_STAT_GSEC40 / C22_POP15P,
  
  # MÉNAGES & FAMILLES
  tx_men_seule_22    = C22_MENPSEUL / C22_MEN,
  tx_couple_senf_22   = C22_MEN1FCOUPSENF        / C22_MEN,
  tx_couple_aenf_22   = C22_MEN1FCOUPUNIQENFCOUP / C22_MEN,
  tx_mono_total_22    = (C22_MEN1FMONOH + C22_MEN1FMONOF) / C22_MENFAM,
  tx_mono_feminise_22 = C22_MEN1FMONOF / (C22_MEN1FMONOH + C22_MEN1FMONOF),
  
  # Logement — types
  tx_res_princ_22     = P22_RP       / P22_LOG,
  tx_res_secocc_22    = P22_RSECOCC  / P22_LOG,
  tx_vacance_22       = P22_LOGVAC   / P22_LOG,
  tx_maison_22        = P22_MAISON   / P22_LOG,
  tx_appart_22        = P22_APPART   / P22_LOG,
  # Logement — statut d'occupation
  tx_proprio_22       = P22_RP_PROP    / P22_RP,
  tx_locataire_22     = P22_RP_LOC     / P22_RP,
  tx_hlm_loc_22       = P22_RP_LOCHLMV / P22_RP_LOC,
  tx_hlm_22           = P22_RP_LOCHLMV / P22_RP,
  # Logement — ancienneté d'emménagement
  tx_enracinement_22 = (P22_MEN_ANEM1019 + P22_MEN_ANEM2029 + P22_MEN_ANEM30P )/ P22_MEN,
  
  # SCOLARISATION
  tx_scol1824_22      = P22_SCOL1824 / P22_POP1824,
  tx_scol2529_22      = P22_SCOL2529 / P22_POP2529,
  
  # DIPLÔMES
  tx_dipl_sup_tot_22  = (P22_NSCOL15P_SUP2 + P22_NSCOL15P_SUP34 + P22_NSCOL15P_SUP5) / P22_NSCOL15P,
  
  # Emploi & chômage — global
  tx_activite_22      = P22_ACT1564    / P22_POP1564,
  tx_emploi_22        = P22_ACTOCC1564 / P22_POP1564,
  tx_chomage_22       = P22_CHOM1564   / P22_ACT1564,
  tx_inactivite_22    = P22_INACT1564  / P22_POP1564,
  tx_retraite_22      = P22_RETR1564   / P22_POP1564,
  tx_etud_1564_22     = P22_ETUD1564   / P22_POP1564,
  
  # Emploi & chômage — par diplôme
  tx_chom_bac_max_22 = (P22_CHOM_DIPLMIN + P22_CHOM_BEPC + P22_CHOM_CAPBEP + P22_CHOM_BAC) / (P22_ACT_DIPLMIN + P22_ACT_BEPC + P22_ACT_CAPBEP + P22_ACT_BAC),
  tx_chom_sup_22 = (P22_CHOM_SUP2 + P22_CHOM_SUP34 + P22_CHOM_SUP5) / (P22_ACT_SUP2 + P22_ACT_SUP34 + P22_ACT_SUP5),
  
  # STATUT D'EMPLOI & PRÉCARITÉ
  tx_cdi_22 = (P22_HSAL15P_CDI + P22_FSAL15P_CDI) / (P22_SAL15P),
  tx_cdd_22 = (P22_HSAL15P_CDD + P22_FSAL15P_CDD) / (P22_SAL15P),
  tx_indep_22       = (P22_HNSAL15P_INDEP + P22_FNSAL15P_INDEP)  / P22_NSAL15P,
  
  # Mobilité domicile-travail — modes
  tx_mob_voiture_22   = P22_ACTOCC15P_VOITURE  / P22_ACTOCC15P,

  # Mobilité — distance
  tx_travail_mcommune_22  = P22_ACTOCC15P_ILT1 / P22_ACTOCC15P,  # même commune
  tx_trajet_adépartement_22   = P22_ACTOCC15P_ILT3 / P22_ACTOCC15P,  # autre département
  tx_trajet_arégion_22   = P22_ACTOCC15P_ILT4 / P22_ACTOCC15P,  # autre région
) %>%
  select(-starts_with("P22_"), -starts_with("C22_"),
         P22_POP, C22_MEN, P22_POP1564) %>%
  relocate(com, libelle, SUPERF,
           P22_POP, P22_POP1564, C22_MEN) %>% clean_names() %>% rename(superficie = superf, pop_22 = p22_pop, pop1564_22 = p22_pop1564, men_22 = c22_men)

## OBSERVATOIRE TERRITOIRE----
observatoire_territoire <- observatoire_territoire_raw %>% 
  select(
    com = Code,
    nb_licencies_sportifs_percent_2022          = `Nombre de licenciés sportifs pour 100 habitants 2022`,
    densite_7niveaux                    = `Grille communale de densité en 7 niveaux`,
    part_immigres_2022                  = `Part des immigrés dans la population 2022`,
    part_emploi_agri_2022               = `Part des emplois dans l'agriculture 2022`,
    part_emploi_industrie_2022          = `Part des emplois dans l'industrie 2022`,
    part_emploi_tertiaire_2022          = `Part des emplois dans le tertiaire 2022`,
    etab_1_4sal_2023             = `Part d'établissements par classe d'effectifs salariés 2023_x000d_\n1 à 4 salariés`,
    etab_5_9sal_2023             = `Part d'établissements par classe d'effectifs salariés 2023_x000d_\n5 à 9 salariés`,
    etab_10_19sal_2023           = `Part d'établissements par classe d'effectifs salariés 2023_x000d_\n10 à 19 salariés`,
    etab_20_49sal_2023           = `Part d'établissements par classe d'effectifs salariés 2023_x000d_\n20 à 49 salariés`,
    etab_50_99sal_2023           = `Part d'établissements par classe d'effectifs salariés 2023_x000d_\n50 à 99 salariés`,
    etab_100_199sal_2023         = `Part d'établissements par classe d'effectifs salariés 2023_x000d_\n100 à 199 salariés`,
    etab_200_499sal_2023         = `Part d'établissements par classe d'effectifs salariés 2023_x000d_\n200 à 499 salariés`,
    etab_500psal_2023            = `Part d'établissements par classe d'effectifs salariés 2023_x000d_\n500 salariés et plus`,
    ratio_cadres_ouvriers_2022   = `Ratio entre les "cadres" et les "ouvriers" 2022_x000d_\nEnsemble`,
    part_dipl_sup_25_34_2022            = `Part des 25-34 ans titulaires d'un diplôme de l'enseignement supérieur 2022`,
    part_neet_2022                      = `Part des jeunes non insérés (ni en emploi, ni scolarisés - NEET) 2022`,
    part_20_24_sans_dipl_2022           = `Part des 20-24 ans sans diplôme 2022`,
    nb_stations_tc_2025                 = `Nombre de stations de transports en commun pour 1 000 habitants 2025`,
    part_eloigne_sante_2023             = `Part de la population éloignée de plus de 20 minutes d'au moins un des services de santé de proximité 2023`,) %>%
  mutate(across(-c(com, densite_7niveaux),
                ~ as.numeric(gsub(",", ".", na_if(as.character(.), "s"))))) %>% 
  mutate(part_TPE_23 = etab_1_4sal_2023 + etab_5_9sal_2023,
         part_PME_23 = etab_10_19sal_2023 + etab_20_49sal_2023 + etab_50_99sal_2023,
         part_GE_23 = etab_100_199sal_2023 + etab_200_499sal_2023 + etab_500psal_2023) %>% 
  select(-starts_with("etab_"))


## ÉLECTIONS ----
### --- Législatives 2024 ----
election_legislative_2024<- election_legislative_2024_raw %>%
  pivot_election(
    n_cols_fixes   = 18,
    vars_candidat  = c("numero_panneau", "nuance_candidat", "nom_candidat", 
                       "prenom_candidat", "sexe_candidat", "nb_voix", 
                       "percent_voix_inscrits", "percent_voix_exprimees", "elu")
  ) %>%   clean_names() %>% 
  select(com = code_commune, percent_abstentions, nuance_candidat, percent_voix_exprimees) %>% 
  mutate(across(
    c(`percent_abstentions`, `percent_voix_exprimees`),
    ~ as.numeric(gsub(",", ".", gsub("%", "", .)))
  )) %>%
  group_by(com) %>%
  # Les % sont calculés sur les exprimés de toute la commune : pour une commune
  # répartie sur plusieurs circonscriptions (Nîmes), les scores d'une même nuance s'additionnent.
  summarise(
    pct_rn_24         = sum(percent_voix_exprimees[nuance_candidat == "RN"], na.rm = TRUE),
    pct_gauche_24     = sum(percent_voix_exprimees[nuance_candidat %in% nuances_gauche_2024], na.rm = TRUE),
    pct_abstention_24 = first(percent_abstentions),
    .groups = "drop"
  )

### --- Législatives 2017 ----
election_legislative_2017<- election_legislative_2017_raw %>%
  mutate(across(everything(), as.character)) %>%
  pivot_election(
    n_cols_fixes  = 20,
    vars_candidat = c("numero_panneau", "sexe_candidat", "nom_candidat",
                      "prenom_candidat", "nuance_candidat", "nb_voix",
                      "percent_voix_inscrits", "percent_voix_exprimees")
  ) %>%
  filter(`Code du département` %in% c("30", "13")) %>%
  right_join(selection_communes, by = c("Libellé de la commune" = "libelle")) %>%
  clean_names() %>%
  mutate(
    percent_voix_exprimees = as.numeric(gsub(",", ".", percent_voix_exprimees)),
    percent_abs_ins        = as.numeric(gsub(",", ".", percent_abs_ins))
  ) %>%
  select(com, code_de_la_circonscription, percent_abs_ins, nuance_candidat, percent_voix_exprimees) %>%
  group_by(com, code_de_la_circonscription, nuance_candidat) %>%
  summarise(
    pct_moyen      = mean(percent_voix_exprimees, na.rm = TRUE),
    pct_abstention = first(percent_abs_ins),
    .groups = "drop"
  ) %>%
  # Ajout lignes manquantes FN — candidat absent du fichier source
  add_row(com = "30189", 
          code_de_la_circonscription = "1",
          nuance_candidat = "FN",
          pct_moyen = 18.97,
          pct_abstention = 59.10) %>%
  add_row(com = "30032", 
          code_de_la_circonscription = "1",
          nuance_candidat = "FN",
          pct_moyen = 47.79,
          pct_abstention = 53.98) %>%
  group_by(com) %>%
  summarise(
    pct_rn_17         = mean(pct_moyen[nuance_candidat == "FN"], na.rm = TRUE),
    pct_gauche_17     = sum(pct_moyen[nuance_candidat %in% nuances_gauche_2017], na.rm = TRUE),
    pct_abstention_17 = mean(pct_abstention),
    .groups = "drop"
  )

### --- Présidentielle 2022 ----
election_presidentielle_2022<- election_presidentielle_2022_raw %>%
  pivot_election(
    n_cols_fixes  = 19,
    vars_candidat = c("numero_panneau", "sexe_candidat", "nom_candidat",
                      "prenom_candidat", "nb_voix",
                      "percent_voix_inscrits", "percent_voix_exprimees")
  ) %>%
  filter(`Code du département` %in% c("30", "13")) %>%
  right_join(selection_communes, by = c("Libellé de la commune" = "libelle")) %>% clean_names() %>% 
  mutate(
    percent_voix_exprimees = as.numeric(gsub(",", ".", percent_voix_exprimees)),
    percent_abs_ins        = as.numeric(gsub(",", ".", percent_abs_ins))
  ) %>%
  select(com, percent_abs_ins, nom_candidat, percent_voix_exprimees) %>%
  group_by(com) %>%
  summarise(
    pct_rn_22         = mean(percent_voix_exprimees[nom_candidat == "LE PEN"], na.rm = TRUE),
    pct_gauche_22     = sum(percent_voix_exprimees[nom_candidat %in% candidats_gauche_2022], na.rm = TRUE),
    pct_abstention_22 = first(percent_abs_ins),
    .groups = "drop"
  )

### --- Présidentielle 2002 ----
election_presidentielle_2002<- election_presidentielle_2002_raw %>%
  pivot_election(
    n_cols_fixes  = 15,
    vars_candidat = c("sexe_candidat", "nom_candidat",
                      "prenom_candidat", "nb_voix",
                      "percent_voix_inscrits", "percent_voix_exprimees")
  ) %>%
  filter(`Code du département` %in% c("30", "13")) %>%
  right_join(selection_communes, by = c("Libellé de la commune" = "libelle")) %>% clean_names() %>% 
  mutate(
    percent_voix_exprimees = as.numeric(gsub(",", ".", percent_voix_exprimees)),
    percent_abs_ins        = as.numeric(gsub(",", ".", percent_abs_ins))
  ) %>%
  select(com, percent_abs_ins, nom_candidat, percent_voix_exprimees) %>%
  group_by(com) %>%
  summarise(
    pct_rn_02         = mean(percent_voix_exprimees[nom_candidat == "LE PEN"], na.rm = TRUE),
    pct_gauche_02     = sum(percent_voix_exprimees[nom_candidat %in% candidats_gauche_2002], na.rm = TRUE),
    pct_abstention_02 = first(percent_abs_ins),
    .groups = "drop"
  )


### --- Municipales 2026 ----
election_municipale_2026<- election_municipale_2026_raw %>%
  pivot_election(
    n_cols_fixes   = 18,
    vars_candidat  = c("numero_panneau", "nom_candidat", "prenom_candidat",
                       "sexe_candidat", "nuance_candidat","libelle_abreg_liste",
                       "libelle_liste", "nb_voix", "percent_voix_inscrits",
                       "percent_voix_exprimees", "elu", "siege_cm", "siege_cc")
  ) %>%   clean_names() %>% 
  select(com = code_commune, percent_abstentions, nuance_candidat, percent_voix_exprimees) %>% 
  mutate(across(
    c(`percent_abstentions`, `percent_voix_exprimees`),
    ~ as.numeric(gsub(",", ".", gsub("%", "", .)))
  )) %>%
  group_by(com) %>%
  summarise(
    candidat_rn_26     = as.integer(any(nuance_candidat == "LRN")),
    candidat_gauche_26 = if (all(is.na(nuance_candidat))) NA_integer_ else
      as.integer(any(nuance_candidat %in% nuances_gauche_2026, na.rm = TRUE)),
    candidat_se_26 = as.integer(any(nuance_candidat == "LDIV")),
    pct_abstention_26     = first(percent_abstentions),
    .groups = "drop"
  )

### --- Municipales 2020 ----
election_municipale_2020 <- election_municipale_2020_raw %>%
  pivot_election(
    n_cols_fixes   = 18,
    vars_candidat  = c("numero_panneau", "nuance_candidat", "sexe_candidat",
                       "nom_candidat", "prenom_candidat", "liste", "siege_elu",
                       "siege_secteur", "siege_cc", "nb_voix", "percent_voix_inscrits",
                       "percent_voix_exprimees")) %>% 
  filter(`Code du département` %in% c("30", "13")) %>%
  right_join(selection_communes, by = c("Libellé de la commune" = "libelle")) %>% clean_names() %>% 
  mutate(
    percent_voix_exprimees = as.numeric(gsub(",", ".", percent_voix_exprimees)),
    percent_abs_ins        = as.numeric(gsub(",", ".", percent_abs_ins))
  ) %>%
  select(com, percent_abs_ins, nuance_candidat, percent_voix_exprimees) %>%
  mutate(across(
    c(`percent_abs_ins`, `percent_voix_exprimees`),
    ~ as.numeric(gsub(",", ".", gsub("%", "", .)))
  )) %>%
  group_by(com) %>%
  summarise(
    candidat_rn_20 = if (all(is.na(nuance_candidat) | nuance_candidat == "LNC")) NA_integer_ else
      as.integer(any(nuance_candidat == "LRN")),
    candidat_gauche_20 = if (all(is.na(nuance_candidat) | nuance_candidat == "LNC")) NA_integer_ else
      as.integer(any(nuance_candidat %in% nuances_gauche_2020)),
    candidat_se_20 = if (all(is.na(nuance_candidat) | nuance_candidat == "LNC")) NA_integer_ else
      as.integer(any(nuance_candidat == "LDIV")),
    pct_abstention_20     = first(percent_abs_ins),
    .groups = "drop"
  )

## ASSOCIATIONS ----
densite_associative <- associations %>%
  count(com, name = "nb_asso_total") %>%
  left_join(insee_recensement %>% select(com, pop_22), by = "com") %>%
  mutate(
    nb_asso_total = replace_na(nb_asso_total, 0),
    asso_pour_1000hab = (nb_asso_total / pop_22) * 1000
  ) %>%
  select(com, asso_pour_1000hab)


## CULTURE TAURINE ----
arene <- installation %>%
  group_by(com) %>%
  summarise(
    arene      = as.integer(any(grepl("arene|arène", nom_installation, ignore.case = TRUE))),
    .groups = "drop"
  ) %>%
  mutate(arene = ifelse(com == "30003", 1L, arene))
# Correctif manuel : l'arène d'Aigues-Mortes (30003) est absente du fichier d'équipements sportifs mais la commune dispose bien d'une arène.

mots_cles_taurin <- paste0(
  "taurin|bouvine|cocarde|manade|gardian|abrivado|bandido|raseteur|razeteur|biou|bouvino|aficion|ferrade|manadier|tauromachi"
)

asso_taurines <- associations %>% 
  filter(
    grepl(mots_cles_taurin, titre_asso, ignore.case = TRUE) |
      grepl(mots_cles_taurin, objet_asso, ignore.case = TRUE) |
      grepl("camarguais|camarguaise", objet_asso, ignore.case = TRUE)
  ) %>%
  filter(!titre_asso %in%  c(
    "EX-TETARD",
    "FERRADENTAIRE",
    "ASSOCIATION DES LOCATAIRES DU CLOS DES GARDIANS",
    "'REFLET NATUREL DE CAMARGUE'",
    "DEFENSE DES TRADITIONS TEXTILES DANS LE GARD",
    "ASSOCIATION DES CLASSES D'INFORMATIQUE INDUSTRIELLE DE DHUODA (ACIID)",
    "MIEUX VIVRE ENSEMBLE",
    "PHOTOS TRADITIONS",
    "PENA L'AFICION"
  )) %>% 
  count(com, name = "nb_asso_taurines_rna") %>%
  right_join(selection_communes, by ="com") %>% 
  select(com, nb_asso_taurines_rna) %>% 
  mutate(nb_asso_taurines_rna = replace_na(nb_asso_taurines_rna, 0))


asso_corrida_course <- asso_corrida_course_raw %>%
  right_join(selection_communes %>% 
               mutate(libelle = gsub("-", " ", toupper(iconv(libelle, to = "ASCII//TRANSLIT")))), 
             by = c("VILLE" = "libelle")) %>%
  clean_names() %>%
  select(com, nom_asso = name, type) %>%
  group_by(com, type) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = type, values_from = n, values_fill = 0) %>% 
  select(-`NA`) %>% 
  rename(asso_corrida = corrida,
         asso_course_camarguaise = course_camarguaise)


# BASE COMPLÈTE----
final <-  insee_recensement %>% 
  left_join(precarite, by = "com") %>%
  left_join(precarite_filosofi, by = "com") %>%
  left_join(observatoire_territoire, by = "com") %>%
  left_join(election_presidentielle_2002, by = "com") %>% 
  left_join(election_presidentielle_2022, by = "com") %>%
  left_join(election_legislative_2017, by = "com") %>%
  left_join(election_legislative_2024, by = "com") %>%
  left_join(election_municipale_2020, by = "com") %>%
  left_join(election_municipale_2026, by = "com") %>%
  mutate(
    evol_rn_02_22     = pct_rn_22     - pct_rn_02,
    evol_rn_17_24     = pct_rn_24     - pct_rn_17,
    evol_gauche_17_24 = pct_gauche_24 - pct_gauche_17
  ) %>% 
  left_join(densite_associative, by = "com") %>% 
  left_join(arene, by = "com") %>%
  left_join(asso_taurines, by = "com") %>%
  left_join(asso_corrida_course, by = "com") %>% 
  left_join(culture_taurine, by = "com") %>%
  left_join(parc_naturel_regional, by = "com") %>% 
  mutate(
    nb_asso_taurines_p1000        = (nb_asso_taurines_rna      / pop_22) * 1000,
    nb_asso_corrida_p1000         = (asso_corrida            / pop_22) * 1000,
    nb_asso_course_cam_p1000      = (asso_course_camarguaise / pop_22) * 1000,
    nb_j_fetes_votives_p1000      = (nb_j_fetes_votives_an      / pop_22) * 1000,
    nb_j_abrivados_p1000          = (nb_abrivados_bandidos_an   / pop_22) * 1000,
    nb_j_courses_arenes_p1000     = (nb_j_courses_arenes_an     / pop_22) * 1000,
    nb_manade_p1000               = (nb_manade                  / pop_22) * 1000,
  ) %>% 
  relocate(libelle, com) 

final_analyse <- final %>%
  mutate(
    # Normalisation + IAPC v1
    across(
      c(nb_asso_taurines_p1000,
        arene, nb_j_courses_arenes_p1000, nb_j_abrivados_p1000, nb_j_fetes_votives_p1000,
        nb_manade_p1000, manade_profil_agro, manade_profil_sport_culture,
        tx_enracinement_22),
      list(norm = normalize)
    ),
    dim_institutionnel = nb_asso_taurines_p1000_norm,
    dim_pratique       = 0.4  * arene_norm +
      0.3  * nb_j_abrivados_p1000_norm +
      0.2  * nb_j_courses_arenes_p1000_norm +
      0.1  * nb_j_fetes_votives_p1000_norm,
    dim_patrimonial    = 0.4  * nb_manade_p1000_norm +
      0.25 * manade_profil_agro_norm +
      0.25 * manade_profil_sport_culture_norm +
      0.1  * tx_enracinement_22_norm,
    IAPC               = (dim_institutionnel + dim_pratique + dim_patrimonial) / 3
  ) %>%
  mutate(
    # Normalisation + IAPC v2
    across(
      c(nb_manade_p1000,
        nb_asso_taurines_p1000, nb_asso_corrida_p1000, nb_asso_course_cam_p1000,
        nb_j_abrivados_p1000, nb_j_courses_arenes_p1000, nb_j_fetes_votives_p1000,
        arene, manade_profil_agro, manade_profil_sport_culture, tx_enracinement_22),
      list(norm = normalize)
    ),
    dim_productive     = nb_manade_p1000_norm,
    dim_associative    = 0.8 * nb_asso_taurines_p1000_norm +
      0.1 * nb_asso_corrida_p1000_norm +
      0.1 * nb_asso_course_cam_p1000_norm,
    dim_evenementielle = 0.45 * nb_j_abrivados_p1000_norm +
      0.4 * nb_j_courses_arenes_p1000_norm +
      0.15 * nb_j_fetes_votives_p1000_norm,
    dim_patrimoniale   = 0.4 * arene_norm +
      0.25 * manade_profil_agro_norm +
      0.25 * manade_profil_sport_culture_norm +
      0.1 * tx_enracinement_22_norm,
    IAPC_v2            = (dim_productive + dim_associative + dim_evenementielle + dim_patrimoniale) / 4
  ) %>%  mutate(
    # dim_constance
    above_med_02 = as.integer(pct_rn_02 > median(pct_rn_02, na.rm = TRUE)),
    above_med_17 = as.integer(pct_rn_17 > median(pct_rn_17, na.rm = TRUE)),
    above_med_22 = as.integer(pct_rn_22 > median(pct_rn_22, na.rm = TRUE)),
    above_med_24 = as.integer(pct_rn_24 > median(pct_rn_24, na.rm = TRUE)),
    dim_constance = (above_med_02 + above_med_17 + above_med_22 + above_med_24) / 4,
    
    # dim_niveau
    across(c(pct_rn_02, pct_rn_17, pct_rn_22, pct_rn_24), 
           list(norm = normalize)),
    dim_niveau = (pct_rn_02_norm + pct_rn_17_norm + 
                    pct_rn_22_norm + pct_rn_24_norm) / 4,
    
    # IARN
    IARN = 0.5 * dim_niveau + 0.5 * dim_constance 
    # + 0.2 * dim_dynamique
  ) %>%
select(-ends_with("_norm"), -starts_with("above_med_"),
       -dim_institutionnel, -dim_pratique, -dim_patrimonial,
       -dim_productive, -dim_associative, -dim_evenementielle, -dim_patrimoniale)

write_xlsx(final_analyse,
           path = file.path(DATA_DIR_processed, "final_analyse.xlsx"))

datatable(final_analyse, 
          options = list(
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 2)
          ),
          extensions = "FixedColumns") %>%
  formatRound(columns = names(final_analyse)[sapply(final_analyse, is.numeric)], digits = 2)

# GRAPHIQUES ----
final_analyse <- read_excel(file.path(DATA_DIR_processed, "final_analyse.xlsx"))
fond_carte <- st_read(path("fond_carte/communes-20220101.shp"))

## Fonctions globales ----
make_map <- function(data_bi, coords, titre_version) {
  ggplot() +
    geom_sf(data = data_bi, aes(fill = bi_class),
            color = "white", linewidth = 0.3, show.legend = FALSE) +
    bi_scale_fill(pal = "DkCyan", dim = 2) +
    geom_label_repel(
      data = coords, aes(x = x, y = y, label = libelle),
      size = 2.5, fontface = "bold",
      fill = alpha("white", 0.85), color = "black",
      label.size = 0.2, label.r = unit(0.15, "lines"),
      box.padding = 0.5, point.padding = 0.3,
      segment.color = "grey40", segment.size = 0.4,
      max.overlaps = Inf, min.segment.length = 0
    ) +
    labs(title = titre_version) +
    theme_void() +
    theme(
      plot.title  = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    )
}

make_scatter <- function(data, x_var, y_var, x_label, y_label) {
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(
      shape = 21, fill = "steelblue", color = "white",
      stroke = 0.6, alpha = 0.75, size = 3
    ) +
    geom_smooth(
      method = "lm", se = TRUE,
      color = "firebrick", linewidth = 0.8,
      fill = "firebrick", alpha = 0.1
    ) +
    geom_label_repel(
      aes(label = libelle),
      size = 2.8, fontface = "bold",
      color = "black", fill = alpha("white", 0.75),
      label.size = 0.2, box.padding = 0.4,
      point.padding = 0.3, segment.color = "grey40",
      segment.size = 0.4, max.overlaps = Inf,
      min.segment.length = 0
    ) +
    labs(x = x_label, y = y_label) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

## Précalculs communs----

carte_base <- fond_carte %>%
  inner_join(final_analyse, by = c("insee" = "com"))

coords <- carte_base %>%
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

seuil_iapc1 <- round((min(carte_base$IAPC,    na.rm = TRUE) + max(carte_base$IAPC,    na.rm = TRUE)) / 2, 2)
seuil_iapc2 <- round((min(carte_base$IAPC_v2, na.rm = TRUE) + max(carte_base$IAPC_v2, na.rm = TRUE)) / 2, 2)
seuil_iarn  <- round((min(carte_base$IARN,    na.rm = TRUE) + max(carte_base$IARN,    na.rm = TRUE)) / 2, 2)

## IAPC × vote RN----
### Pression festive----

walk(annees, function(an) {
  col_rn         <- paste0("pct_rn_", an)
  election       <- type_election[an]
  annee_complete <- paste0("20", an)
  
  pression_festive <- final_analyse %>%
    arrange(desc(nb_j_abrivados_p1000)) %>%
    mutate(
      libelle    = fct_reorder(libelle, nb_j_abrivados_p1000),
      pct_rn_an  = .data[[col_rn]]
    )
  
  p <- ggplot(pression_festive, aes(x = nb_j_abrivados_p1000, y = libelle)) +
    geom_col(aes(fill = pct_rn_an), width = 0.7) +
    geom_text(
      aes(label = paste0(round(pct_rn_an, 1), "% RN")),
      hjust = -0.1, size = 3, color = "grey30"
    ) +
    scale_fill_gradient(
      low  = "#f0f4ff", high = "#001E96",
      name = paste0("Score RN au premier tour des élections en ", annee_complete, " (%)")
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(
      title    = paste0("Pression festive taurine et vote RN (élections ", election, " ", annee_complete, ")"),
      subtitle = "Classement des communes par jours d'abrivados pour 1 000 hab.",
      x        = "Jours d'abrivados pour 1 000 hab.",
      y        = NULL,
      caption  = paste0(
        "Source : nombre de jours d'abrivados (FFCC) ; population (INSEE 2022) ; ",
        "résultats des élections ", election, " ", annee_complete, " au 1er tour (Ministère de l'Intérieur)"
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(color = "grey40", size = 10),
      legend.position  = "bottom",
      legend.key.width = unit(2, "cm"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.y        = element_text(size = 9),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA)
    )
  
  ggsave(
    filename = file.path(OUTPUT, paste0("pression_festive_rn_", annee_complete, ".png")),
    plot = p, width = 10, height = 8, dpi = 300
  )
})

### Cartes biscales IAPC × vote RN----

walk(annees, function(an) {
  col_rn         <- paste0("pct_rn_", an)
  election       <- type_election[an]
  annee_complete <- paste0("20", an)
  
  carte  <- carte_base %>% rename(pct_rn_an = all_of(col_rn))
  seuil_rn <- round((min(carte$pct_rn_an, na.rm = TRUE) + max(carte$pct_rn_an, na.rm = TRUE)) / 2, 1)
  
  data_bi1 <- bi_class(carte, x = pct_rn_an, y = IAPC,    style = "equal", dim = 2)
  data_bi2 <- bi_class(carte, x = pct_rn_an, y = IAPC_v2, style = "equal", dim = 2)
  
  map1 <- make_map(data_bi1, coords, "IAPC v1")
  map2 <- make_map(data_bi2, coords, "IAPC v2")
  
  legend1 <- bi_legend(pal = "DkCyan", dim = 2,
                       xlab = paste0("% vote RN (seuil : ", seuil_rn,    "%)"),
                       ylab = paste0("IAPC v1 (seuil : ",  seuil_iapc1, ")"),
                       size = 8)
  legend2 <- bi_legend(pal = "DkCyan", dim = 2,
                       xlab = paste0("% vote RN (seuil : ", seuil_rn,    "%)"),
                       ylab = paste0("IAPC v2 (seuil : ",  seuil_iapc2, ")"),
                       size = 8)
  
  panel1 <- ggdraw() + draw_plot(map1, 0, 0, 1, 1) + draw_plot(legend1, 0.68, 0.03, 0.28, 0.28)
  panel2 <- ggdraw() + draw_plot(map2, 0, 0, 1, 1) + draw_plot(legend2, 0.68, 0.03, 0.28, 0.28)
  
  p_final <- ggdraw() +
    draw_label(
      paste0("Indice d'Ancrage Paysager et Culturel (IAPC) et vote RN au premier tour des élections ", election, " ", annee_complete),
      fontface = "bold", size = 13, y = 0.97, vjust = 1
    ) +
    draw_label(
      paste0("Sources : IAPC (RNA, FFCC)"),
      size = 8, color = "grey40", y = 0.01, vjust = 0
    ) +
    draw_plot(plot_grid(panel1, panel2, ncol = 2), 0, 0.04, 1, 0.92)
  
  ggsave(
    filename = file.path(OUTPUT, paste0("carte_IAPC_v1v2_rn_", annee_complete, ".png")),
    plot = p_final, width = 18, height = 8, dpi = 300, bg = "white"
  )
})

### Nuages de points IAPC × vote RN ----

walk(annees, function(an) {
  col_rn         <- paste0("pct_rn_", an)
  election       <- type_election[an]
  annee_complete <- paste0("20", an)
  
  data <- final_analyse %>% rename(pct_rn_an = all_of(col_rn))
  
  p1 <- make_scatter(data, x_var = "IAPC",    y_var = "pct_rn_an",
                     x_label = "IAPC v1", y_label = "% vote RN")
  p2 <- make_scatter(data, x_var = "IAPC_v2", y_var = "pct_rn_an",
                     x_label = "IAPC v2", y_label = "% vote RN")
  
  p_final <- p1 + p2 +
    plot_annotation(
      title = paste0("Lien entre l'Indice d'Ancrage Paysager et Culturel (IAPC) et le vote RN au premier tour des élections ", election, " ", annee_complete),
      theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
    )
  
  ggsave(
    filename = file.path(OUTPUT, paste0("scatter_IAPC_v1v2_rn_", annee_complete, ".png")),
    plot = p_final, width = 16, height = 7, dpi = 300, bg = "white"
  )
})

### Corrélations IAPC × vote RN----

walk(annees, function(an) {
  data <- final_analyse %>% rename(pct_rn_an = all_of(paste0("pct_rn_", an)))
  
  cat("\n=== ", paste0("20", an), "-", type_election[an], "===\n")
  cat("-- IAPC v1 --\n") ; print(cor.test(data$IAPC,    data$pct_rn_an))
  cat("-- IAPC v2 --\n") ; print(cor.test(data$IAPC_v2, data$pct_rn_an))
})

## IAPC × IARN ----
### Carte biscale IAPC × IARN ----

data_bi1 <- bi_class(carte_base, x = IARN, y = IAPC,    style = "equal", dim = 2)
data_bi2 <- bi_class(carte_base, x = IARN, y = IAPC_v2, style = "equal", dim = 2)

map1 <- make_map(data_bi1, coords, "IAPC v1")
map2 <- make_map(data_bi2, coords, "IAPC v2")

legend1 <- bi_legend(pal = "DkCyan", dim = 2,
                     xlab = paste0("IARN (seuil : ",    seuil_iarn,  ")"),
                     ylab = paste0("IAPC v1 (seuil : ", seuil_iapc1, ")"),
                     size = 8)
legend2 <- bi_legend(pal = "DkCyan", dim = 2,
                     xlab = paste0("IARN (seuil : ",    seuil_iarn,  ")"),
                     ylab = paste0("IAPC v2 (seuil : ", seuil_iapc2, ")"),
                     size = 8)

panel1 <- ggdraw() + draw_plot(map1, 0, 0, 1, 1) + draw_plot(legend1, 0.68, 0.03, 0.28, 0.28)
panel2 <- ggdraw() + draw_plot(map2, 0, 0, 1, 1) + draw_plot(legend2, 0.68, 0.03, 0.28, 0.28)

p_carte <- ggdraw() +
  draw_label(
    "Indice d'Ancrage Paysager et Culturel (IAPC) et Indice d'Ancrage RN (IARN)",
    fontface = "bold", size = 13, y = 0.97, vjust = 1
  ) +
  draw_label(
    "Sources : RNA, FFCC, ministère de l'Intérieur",
    size = 8, color = "grey40", y = 0.01, vjust = 0
  ) +
  draw_plot(plot_grid(panel1, panel2, ncol = 2), 0, 0.04, 1, 0.92)

ggsave(
  filename = file.path(OUTPUT, "carte_IAPC_v1v2_IARN.png"),
  plot = p_carte, width = 18, height = 8, dpi = 300, bg = "white"
)

### Nuages de points IAPC × IARN ----

p1 <- make_scatter(final_analyse, x_var = "IAPC",    y_var = "IARN",
                   x_label = "IAPC v1 (intensité de la pratique taurine)",
                   y_label = "IARN (ancrage RN)")
p2 <- make_scatter(final_analyse, x_var = "IAPC_v2", y_var = "IARN",
                   x_label = "IAPC v2 (intensité de la pratique taurine)",
                   y_label = "IARN (ancrage RN)")

p_scatter <- p1 + p2 +
  plot_annotation(
    title   = "Lien entre l'Indice d'Ancrage Paysager et Culturel (IAPC) et l'Indice d'Ancrage RN (IARN)",
    caption = "Sources : RNA, FFCC, ministère de l'Intérieur",
    theme   = theme(
      plot.title   = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.caption = element_text(size = 8, color = "grey40")
    )
  )

ggsave(
  filename = file.path(OUTPUT, "scatter_IAPC_v1v2_IARN.png"),
  plot = p_scatter, width = 16, height = 7, dpi = 300, bg = "white"
)

### Corrélations IAPC × IARN ----

cat("-- IAPC v1 × IARN --\n") ; print(cor.test(final_analyse$IAPC,    final_analyse$IARN))
cat("-- IAPC v2 × IARN --\n") ; print(cor.test(final_analyse$IAPC_v2, final_analyse$IARN))