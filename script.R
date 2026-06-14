# ANALYSE TERRITORIALE — COMMUNES CIBLES

# CONFIG ----
library(readr)
library(dplyr)
library(readxl)
library(tidyverse)
library(janitor)
library(DT)

DATA_DIR <- "C:/Users/lisaw/Documents/TRAVAIL/analyse_camargue/data"
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

# RÉFÉRENTIEL COMMUNES ----
selection_communes <- read_csv(path("v_commune_2025.csv")) %>%
  select(COM, 
         #NCC, 
         #NCCENR, 
         LIBELLE) %>%
  filter(COM %in% communes_cibles) %>% clean_names()

# CHARGEMENT DES DONNÉES BRUTES----

## --- Recensement INSEE 2022 ----
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

## --- Pauvreté ----
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
         # revenu_median_2020=MED20,
         tx_menage_fiscaux_imposables_2020 = PIMP20,
         # tx_pauvrete_2020 = TP6020,
         part_prestations_sociales_2020 = PPSOC20,
         decile1_2020 = D120,
         decile9_2020 = D920,
         rapport_interdecile_2020 = RD20) %>% 
  mutate(across(-com, ~ as.numeric(gsub(",", ".", na_if(., "s")))))

  
observatoire_territoire_raw <- read_excel(
  path("observatoire_territoires.xlsx"),
  skip = 3) %>% filter(`Code` %in% communes_cibles)

## --- Élections ----
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

## --- Équipements sportifs ----
installation <- read_delim(
  path("data-es-installation.csv"),
  delim = ";", trim_ws = TRUE
) %>%
  filter(`insee` %in% communes_cibles) %>% 
  select(com = insee, nom_installation = nom, type_installation = install_particuliere)

## --- Associations ----
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
  # tx_motorisation  = P22_RP_VOIT1P  / P22_MEN,
  # Logement — ancienneté d'emménagement
  # tx_anem_recent_22   = P22_MEN_ANEM0002 / P22_MEN,
  # tx_anem_2_9ans_22   = (P22_MEN_ANEM0204 +P22_MEN_ANEM0509) / P22_MEN,
  tx_enracinement_22 = (P22_MEN_ANEM1019 + P22_MEN_ANEM2029 + P22_MEN_ANEM30P )/ P22_MEN,

  # Logement — chauffage
  # tx_chauf_elec_22    = P22_RP_CELEC  / P22_RP,
  # tx_chauf_gazv_22    = P22_RP_CGAZV  / P22_RP,
  # tx_chauf_fioul_22   = P22_RP_CFIOUL / P22_RP,
  # tx_chauf_gazb_22    = P22_RP_CGAZB  / P22_RP,
  
  # SCOLARISATION
  # tx_scol0205_22      = P22_SCOL0205 / P22_POP0205,
  # tx_scol0610_22      = P22_SCOL0610 / P22_POP0610,
  # tx_scol1114_22      = P22_SCOL1114 / P22_POP1114,
  # tx_scol1517_22      = P22_SCOL1517 / P22_POP1517,
  tx_scol1824_22      = P22_SCOL1824 / P22_POP1824,
  tx_scol2529_22      = P22_SCOL2529 / P22_POP2529,
  
  # DIPLÔMES
  # tx_dipl_min_22      = P22_NSCOL15P_DIPLMIN / P22_NSCOL15P,
  # tx_dipl_bepc_22     = P22_NSCOL15P_BEPC    / P22_NSCOL15P,
  # tx_dipl_capbep_22   = P22_NSCOL15P_CAPBEP  / P22_NSCOL15P,
  # tx_dipl_bac_22      = P22_NSCOL15P_BAC     / P22_NSCOL15P,
  # tx_dipl_sup2_22     = P22_NSCOL15P_SUP2    / P22_NSCOL15P,
  # tx_dipl_sup34_22    = P22_NSCOL15P_SUP34   / P22_NSCOL15P,
  # tx_dipl_sup5_22     = P22_NSCOL15P_SUP5    / P22_NSCOL15P,
  tx_dipl_sup_tot_22  = (P22_NSCOL15P_SUP2 + P22_NSCOL15P_SUP34 + P22_NSCOL15P_SUP5) / P22_NSCOL15P,
  
  # Emploi & chômage — global
  tx_activite_22      = P22_ACT1564    / P22_POP1564,
  tx_emploi_22        = P22_ACTOCC1564 / P22_POP1564,
  tx_chomage_22       = P22_CHOM1564   / P22_ACT1564,
  tx_inactivite_22    = P22_INACT1564  / P22_POP1564,
  tx_retraite_22      = P22_RETR1564   / P22_POP1564,
  tx_etud_1564_22     = P22_ETUD1564   / P22_POP1564,
  
  # Emploi & chômage — par diplôme
  # tx_chom_diplmin_22  = P22_CHOM_DIPLMIN / P22_ACT_DIPLMIN,
  # tx_chom_bepc_22     = P22_CHOM_BEPC    / P22_ACT_BEPC,
  # tx_chom_capbep_22   = P22_CHOM_CAPBEP  / P22_ACT_CAPBEP,
  # tx_chom_bac_22      = P22_CHOM_BAC     / P22_ACT_BAC,
  tx_chom_bac_max_22 = (P22_CHOM_DIPLMIN + P22_CHOM_BEPC + P22_CHOM_CAPBEP + P22_CHOM_BAC) / (P22_ACT_DIPLMIN + P22_ACT_BEPC + P22_ACT_CAPBEP + P22_ACT_BAC),
  # tx_chom_sup2_22     = P22_CHOM_SUP2    / P22_ACT_SUP2,
  # tx_chom_sup34_22    = P22_CHOM_SUP34   / P22_ACT_SUP34,
  # tx_chom_sup5_22     = P22_CHOM_SUP5    / P22_ACT_SUP5,
  tx_chom_sup_22 = (P22_CHOM_SUP2 + P22_CHOM_SUP34 + P22_CHOM_SUP5) / (P22_ACT_SUP2 + P22_ACT_SUP34 + P22_ACT_SUP5),
  
  # STATUT D'EMPLOI & PRÉCARITÉ
  # tx_salaries_22      = P22_SAL15P   / P22_ACTOCC15P,
  # tx_nonsalaries_22   = P22_NSAL15P  / P22_ACTOCC15P,
  # tx_salaries_tp_22     = P22_SAL15P_TP / P22_SAL15P,
  # tx_tp_22     = P22_ACTOCC15P_TP   / P22_ACTOCC15P,
  tx_cdi_22 = (P22_HSAL15P_CDI + P22_FSAL15P_CDI) / (P22_SAL15P),
  tx_cdd_22 = (P22_HSAL15P_CDD + P22_FSAL15P_CDD) / (P22_SAL15P),
  # tx_interim_22 = (P22_HSAL15P_INTERIM + P22_FSAL15P_INTERIM) / (P22_SAL15P),
  tx_indep_22       = (P22_HNSAL15P_INDEP + P22_FNSAL15P_INDEP)  / P22_NSAL15P,
  
  # Mobilité domicile-travail — modes
  tx_mob_voiture_22   = P22_ACTOCC15P_VOITURE  / P22_ACTOCC15P,
  # tx_mob_commun_22    = P22_ACTOCC15P_COMMUN   / P22_ACTOCC15P,
  # tx_mob_velo_22      = P22_ACTOCC15P_VELO     / P22_ACTOCC15P,
  # tx_mob_marche_22    = P22_ACTOCC15P_MARCHE   / P22_ACTOCC15P,
  # tx_mob_pastrans_22  = P22_ACTOCC15P_PASTRANS  / P22_ACTOCC15P,
  # tx_mob_douce_22     = (P22_ACTOCC15P_VELO + P22_ACTOCC15P_MARCHE) / P22_ACTOCC15P,
  
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
    # typo_ruralite                       = `Typologie diversité des ruralités (Commune)`,
    part_immigres_2022                  = `Part des immigrés dans la population 2022`,
    # part_etrangers_2022                 = `Part des étrangers dans la population 2022`,
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
  summarise(
    pct_rn_24         = mean(`percent_voix_exprimees`[`nuance_candidat` == "RN"], na.rm = TRUE),
    pct_gauche_24     = {
      by_nuance <- tapply(
        percent_voix_exprimees[nuance_candidat %in% nuances_gauche_2024],
        nuance_candidat[nuance_candidat %in% nuances_gauche_2024],
        mean, na.rm = TRUE
      )
      sum(by_nuance, na.rm = TRUE)
    },pct_abstention_24 = first(`percent_abstentions`),
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
  # Source : [à préciser]
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
    pct_rn_26 = ifelse(
      any(nuance_candidat == "LRN"),
      mean(percent_voix_exprimees[nuance_candidat == "LRN"], na.rm = TRUE),
      NA_real_
    ),    pct_gauche_26 = ifelse(
      any(nuance_candidat %in% nuances_gauche_2026),
      sum(percent_voix_exprimees[nuance_candidat %in% nuances_gauche_2026], na.rm = TRUE),
      NA_real_
    ),
    pct_abstention_26 = first(percent_abstentions),
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
    pct_rn_20 = ifelse(
      any(nuance_candidat == "LRN"),
      mean(percent_voix_exprimees[nuance_candidat == "LRN"], na.rm = TRUE),
      NA_real_
    ),    pct_gauche_20 = ifelse(
      any(nuance_candidat %in% nuances_gauche_2020),
      sum(percent_voix_exprimees[nuance_candidat %in% nuances_gauche_2020], na.rm = TRUE),
      NA_real_
    ),
    pct_abstention_20 = first(percent_abs_ins),
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
    # boulodrome = as.integer(any(grepl("boul|petanque|pétanque", nom_installation, ignore.case = TRUE))),
    .groups = "drop"
  ) %>%
  mutate(arene = ifelse(com == "30003", 1L, arene))
# Correctif manuel : l'arène d'Aigues-Mortes (30003) est absente du fichier d'équipements sportifs mais la commune dispose bien d'une arène.
# Source : [à préciser — site de la commune / vérification terrain]

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


normalize <- function(x) {
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) return(rep(0, length(x)))
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}


final_IAPC <- final %>%
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
  ) %>%
  select(-ends_with("_norm"),
         -dim_institutionnel, -dim_pratique, -dim_patrimonial,
         -dim_productive, -dim_associative, -dim_evenementielle, -dim_patrimoniale)

datatable(final_IAPC, 
          options = list(
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 2)
          ),
          extensions = "FixedColumns") %>%
  formatRound(columns = names(final_IAPC)[sapply(final_IAPC, is.numeric)], digits = 2)