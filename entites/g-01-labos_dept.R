load(file = rdata_last_version(paste0(data_result, "data/"),"part_fr_f01_"))


# quallification entreprises et detection organismes -> REMPLACER PAR LE CJ 
# part_fr <- part_fr %>% select(-organisme)
temp <- part_fr %>% select(lid, entite, name_2) %>% 
  dplyr::mutate(org1=qualif_organisation(entite), 
                org2=qualif_organisation(name_2)) %>% 
  mutate_all(na_if,"") %>%
  separate_rows(org1, org2, sep=",") %>% 
  unique() %>% 
  pivot_longer(cols=c(org1, org2), values_to = "organisme", names_repair="check_unique", values_drop_na = TRUE) %>% 
  select(-name, -entite, -name_2) %>% unique() %>% 
  group_by(lid) %>% 
  dplyr::mutate(across(everything(), ~str_c(., collapse=","))) %>% 
  unique() %>% 
  ungroup()
  

part_fr <- 
  left_join(part_fr, temp, by="lid") %>% 
  dplyr::mutate_if(is.character, list(~na_if(.,""))) %>% 
  relocate(organisme, .after=org_id)
#modif 18/2/2021
  #        t1 = as.numeric(map(entite,qualif_societe)),
  #        t2= as.numeric(map(name_2, qualif_societe)),
  #        entreprise= if_else(t1+t2 > 0, 1, 0),) %>%
  # select(-t1, -t2)



if (nrow(part_fr %>% dplyr::count(lid) %>% dplyr::filter(n>1))>0){
  x <- part_fr %>% dplyr::count(lid) %>% dplyr::filter(n>1)
  print("vérifier x problèmes de doublons")
}else{print("ok")}

##################################################################################################
##################################################################################################

# EXTRACTION des label labo des libellés sources -> liste_sigles
temp1 <- part_fr %>% 
  dplyr::mutate(liste_sigles1 = if_else(!is.na(name_2),labo_extraction(name_2), NA_character_),
         liste_sigles1 = if_else(str_detect(liste_sigles1, "\\bur1\\b"), str_replace(liste_sigles1, "\\bur1\\b", ""), liste_sigles1)) %>% 
  unique() %>% 
  select(lid, liste_sigles1) %>% 
  dplyr::filter(liste_sigles1!="")

temp2 <- part_fr %>% 
  dplyr::mutate(liste_sigles2 = if_else(!is.na(entite),labo_extraction(entite), NA_character_),
         liste_sigles2 = if_else(str_detect(liste_sigles2, "\\bur1\\b"), str_replace(liste_sigles2, "\\bur1\\b", ""), liste_sigles2)) %>% 
  unique() %>% 
  select(lid, liste_sigles2) %>% 
  dplyr::filter(liste_sigles2!="")

temp3 <- full_join(temp1, temp2) %>% 
  dplyr::mutate(liste_sigles=if_else(is.na(liste_sigles1), liste_sigles2, liste_sigles1),
         liste_sigles = str_remove_all(str_squish(liste_sigles), "(,$+)|(^,+)"),
         liste_sigles = str_squish(liste_sigles)) %>% 
  select(-liste_sigles2, -liste_sigles1) %>% unique() %>% 
  dplyr::mutate_if(is.character, str_squish)

nrow(temp %>% dplyr::count(lid) %>% dplyr::filter(n>1))

part_fr <- full_join(part_fr, temp3) %>% 
  relocate(liste_sigles, .before=organisme)

nrow(part_fr %>% dplyr::count(lid) %>% dplyr::filter(n>1))

save("part_fr", file=paste0(data_result, "data/part_lab_g01_", date_sauve, ".Rdata"))

####################################################################################
load(file = rdata_last_version(paste0(data_result, "data/"), "part_lab_g01_"))

part_fr <- part_fr %>% 
  dplyr::mutate_if(is.character, list(~na_if(.,""))) 


#####################################################################################
# fichier issu des annuaires de l'INSERM ou des listes electorales ; nom contact + affil structures
inserm <- readxl::read_xlsx(paste0(data_result, "org/retour_org/INSERM_2018_06.xlsx")) %>% 
  dplyr::mutate(across(where(is.character), tolower),
         across(where(is.character), str_squish))
colnames(inserm) <- stringr::str_to_lower(colnames(inserm))
perso_contact <- load_one_obj(rdata=paste0(data_result, "data/perso_contact.RData"), "perso_contact")

# jointure des contacts avec données ecorda ; extraction des obs sans liste_sigles
extract <- part_fr %>% 
  # dplyr::filter(organizations_id=="180036048") %>% 
  dplyr::filter(organizations_id=="180036048", is.na(org_id), is.na(liste_sigles)) %>%
  left_join(perso_contact, by = c("project_id", "participant_pic", "applicant_pic")) %>% 
  select(id, participates_as, participant_order, contact, liste_sigles) %>% 
  dplyr::filter(!is.na(contact)) %>% 
  unique()


# contact_sigles <- extract %>% select(contact, liste_sigles) %>% dplyr::filter(!is.na(liste_sigles))

extract <- extract %>% left_join(inserm, by = c("contact"="nom"))

extract <- extract %>% 
  dplyr::rename(labo=structures) %>% 
  dplyr::filter(!is.na(labo)) %>% 
  mutate(labo=str_replace_all(labo, "[[:space:]]", ""),
         labo=str_remove_all(labo, "^[[:alpha:]]+$")) %>% 
  group_by(id, participates_as, participant_order) %>% 
  dplyr::summarise(labo=str_c(labo, collapse=",")) %>% 
  dplyr::filter(labo!="") %>% 
  ungroup() %>% unique()

part_fr <- left_join(part_fr, extract, by=c("id", "participates_as", "participant_order")) %>% 
  dplyr::mutate(liste_sigles = if_else(is.na(liste_sigles), labo, liste_sigles)) %>% 
  select(-labo) %>% 
  unique()
##############################################################################################
# recuperation des liens déjà existants entre org_id et liste_sigles pour mettre à jour les nouveaux liste_sigles

lien_nns_sigle <- part_fr %>% 
  select(liste_sigles, org_id_temp=org_id) %>%
  dplyr::filter(!is.na(liste_sigles)) %>% 
  unique() %>% 
  group_by(liste_sigles) %>% 
  mutate(org_id_temp=str_c(unique(org_id_temp), collapse = ",")) %>% 
  dplyr::filter(str_detect(org_id_temp, ",", negate=T))


part_fr <- part_fr %>% 
  left_join(lien_nns_sigle, by = "liste_sigles") %>% 
  mutate(org_id=ifelse(is.na(org_id), org_id_temp, org_id)) %>% 
  select(-org_id_temp) %>% 
  unique()


###################################################################################################
# extraction des liste_sigles non identifiés par organismes pour rnsr
labo <- part_fr %>% 
  dplyr::filter(is.na(org_id) & !is.na(liste_sigles)) %>% 
  select(id, participant_order, participates_as, year, entite, nom_labo=name_dept, ville_labo=city_tag, labo=liste_sigles) %>% 
  unique() %>% 
  separate_rows(labo, sep=",") %>% 
  unique() %>% 
  arrange(desc(year))

write.csv2(labo, file=paste0(identification, "labo_", date_sauve, ".csv"), row.names = FALSE, fileEncoding = "UTF-8")

#######################################################################################
# RETOUR LABO
filename="_traitement_labo.xlsx"

# labo_retour <- 
#   readxl::read_xlsx(paste0(identification, "corrected/", filename), sheet="_traitement_lab", na ="", col_types = "text") %>% 
#   select(liste_sigles, labo, name_2=nom_labo, city_tag=ville_labo, year=annee, match=match_rnsr) %>% 
#   mutate(liste_sigles=if_else(is.na(liste_sigles), labo, liste_sigles)) %>% 
#   mutate_if(is.character, str_squish) %>% 
#   filter(!is.na(match)) %>% 
#   select(-labo) %>% 
#   unique()

# provisire avec id prochain code du dessus
labo_retour <- 
  readxl::read_xlsx(paste0(identification, "corrected/", filename), sheet="_init", na ="", col_types = "text") %>% 
  select(id, participates_as, participant_order, match) %>% 
  separate_rows(match, sep = ",") %>% 
  dplyr::mutate(match=str_squish(gsub("[][']", "", match))) %>% 
  dplyr::filter(match!="", str_detect(match, "^[A-Z_]+$", negate=TRUE)) %>% 
  unique() %>% 
  group_by(id, participates_as, participant_order) %>% 
  dplyr::mutate(match=str_c(unique(match), collapse=","), 
                participant_order=as.numeric(participant_order)) %>% 
  ungroup()

part_fr <- 
  left_join(part_fr, labo_retour, by = c("id", "participates_as", "participant_order")) %>% 
  unique() %>% 
  dplyr::rename(match_rnsr=match) %>% 
  relocate(match_rnsr, .before=liste_sigles) %>% 
  unique()

########

# liste des labos non identifiés après alignement avec rnsr
labo_non_traite <- function(){
  lab <- part_fr %>%
    dplyr::filter(is.na(org_id) & !is.na(liste_sigles) & is.na(match_rnsr)) %>% 
    select(lid, year, nom_labo=name_2, ville_labo=city_tag, labo=liste_sigles) %>% unique()
  if (nrow(lab)>0){
    write.csv2(lab, file=paste0(identification, "labo_rnsr_", date_sauve,".csv"), row.names = FALSE)
    message(nrow(lab), " nouvelles lignes à recoder dans le rnsr")
  }else{
    print("ok")
  }
}

labo_non_traite()

############################################
# Traitement INSERM restants àla main

inserm_retour <- 
  readxl::read_xlsx(paste0(identification, "corrected/", filename), sheet="_traitement_inserm", na ="") %>% 
  dplyr::rename(match_inserm=match_rnsr) %>% 
  dplyr::mutate_if(is.character, str_squish) %>% 
  dplyr::filter(!is.na(match_inserm)) %>% 
  unique()

inserm_traite <- inserm_retour %>% 
  select(-labo) %>% unique()

# x <- inserm_traite %>% count(project_id, participant_pic, applicant_pic) %>% filter(n>1)
inserm_traite <- inserm_traite %>% 
  group_by(project_id, participant_pic, applicant_pic) %>% 
  dplyr::summarise(match_inserm = str_c(match_inserm, collapse=",")) %>% 
  ungroup()


# part_fr <- part_fr %>% select(-match_inserm)
part_fr <- left_join(part_fr,inserm_traite, by = c("project_id", "participant_pic", "applicant_pic")) %>% 
  relocate(match_inserm, .after=match_rnsr) %>% 
  unique()

extract <- part_fr %>% 
  dplyr::filter(organizations_id=="180036048", is.na(org_id), is.na(liste_sigles), is.na(match_rnsr), is.na(match_inserm))
if (nrow(extract)>0){
  extract %>% select(id, participant_order, participates_as, organisme, year, name_dept, city_dept) %>% unique() %>% 
  write.csv2(file=paste0(identification, "labo_rnsr_", date_sauve,".csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
  message(nrow(extract), " nouvelles lignes à recoder dans le rnsr")
}


inserm_retour <- 
  readxl::read_xlsx(paste0(identification, "corrected/", filename), sheet="_2022_07_06_inserm", na ="") %>% 
  dplyr::filter(!is.na(match)) %>% 
  dplyr::mutate_if(is.character, str_squish) %>% 
  select(participant_order, participates_as, id, match_temp=match) %>% 
  unique()


part_fr <- left_join(part_fr, inserm_retour, by = c( "participant_order", "participates_as", "id")) %>% 
  mutate(match_inserm=ifelse(is.na(match_inserm), match_temp, match_inserm)) %>% 
  select(-match_temp) %>% 
  unique()

##########################################################################################
# création PARTICIPANT_ID

part_fr <- part_fr %>% 
  dplyr::mutate(participant_id = org_id, 
         participant_id = if_else(is.na(participant_id) & str_detect(organizations_id, pattern_rnsr), organizations_id, participant_id),
         participant_id = if_else(is.na(participant_id), match_rnsr, participant_id),
         participant_id = if_else(is.na(participant_id), match_inserm, participant_id),
         participant_id = remove_space(participant_id))

part_fr <- part_fr %>% 
  relocate(participant_id, .after=organizations_id)


# Si ajout d'identifiants trouvés à part ; de préférence les ajouter à la table pic_id_fr
# add_id <- read.csv2(file=paste0(identification, "corrected/id_add_2020_11_13.csv"), colClasses = "character", na.strings = "") %>% 
#   select(-name_source)
# part_fr <- left_join(part_fr, add_id, by = c("num_projet", "pic_id", "pic_id2")) %>% 
#   relocate(part)
# part_fr <- part_fr %>%  mutate(participant_id = if_else(!is.na(part), part, participant_id)) %>% 
#   select(-part)

#################################################################################################"
# Essai juste sur le lib labo dispo 
# lib_labo <- part_fr %>% filter(is.na(participant_id) & cj_lib %in% cj_etat_etab) %>% 
lib_labo <- part_fr %>% 
  dplyr::filter(is.na(participant_id) & !is.na(organisme)) %>%   
  dplyr::mutate(labo=paste(name_2, entite), ville_labo=city_tag) %>%
  # select(year, labo, ville_labo) %>% unique() %>% 
  select(year, organizations_id, supervisor= name_source, labo, ville_labo) %>% 
  unique()
write.csv2(lib_labo, file=paste0(identification, "labo_lib_", date_sauve,".csv"), 
           row.names = FALSE, fileEncoding = "UTF-8")

########################################################################################################
# retour identification affiliation sur le libellé DEPT ; partie du traitement manuel à concevoir pour le robot

temp <- 
  readxl::read_xlsx(path = paste0(identification, "corrected/_id_labo_nns.xlsx"), sheet = "_2022_07",
    col_types = "text", na=c("", "#N/A", "0")) %>% 
  dplyr::filter(!is.na(match)) %>% 
  dplyr::mutate(match=str_squish(gsub("[][']| ", "", match))) %>%
  dplyr::mutate(id_temp=str_replace_all(match, ";", ",")) %>% 
  select(-supervisor, -q, -match) %>% unique()
# filter(is.na(participant_id) & cj_lib %in% cj_etat_etab) %>% 

part_fr <- part_fr %>% 
  dplyr::mutate(labo=paste(name_2, entite)) %>%
  left_join(temp, by = c("organizations_id", "labo", 'year', 'city_tag'='ville_labo')) %>% 
  unique() %>%
  dplyr::mutate(participant_id=if_else(is.na(participant_id), id_temp, participant_id),
                id_temp=if_else(id_temp==participant_id, NA_character_, id_temp)) %>% 
  select(-labo) %>% 
  relocate(id_temp) %>% 
  unique()


nrow(part_fr %>% dplyr::count(lid) %>% dplyr::filter(n>1))
  
# remplacement des participant_id de l'inserm trouvés automatiquement par la correction manuelle
# 200311835F remplacé par -> 201722408M / 201420846C -> 201521757N
part_fr <- part_fr %>% 
  dplyr::mutate(participant_id=if_else(organisme=="inserm" & !is.na(id_temp), str_c(participant_id, id_temp, sep = ","), participant_id),
         participant_id=if_else(!is.na(org_id) & is.na(participant_id), org_id, participant_id),
         participant_id=if_else(is.na(participant_id) & !is.na(id_temp), id_temp, participant_id))
         # participant_id=if_else(participant_id=="200311835F", "201722408M", participant_id),
         # participant_id=if_else(participant_id=="201420846C", "201521757N", participant_id))


save("part_fr", file=paste0(data_result, "data/part_fr_g01_", date_sauve, ".Rdata"))

rm(list=c("inserm_traite","extract","inserm","inserm_retour","lib_labo", "lien_nns_sigle",
          "labo","labo_retour","temp","temp1", "temp2","temp3"))
gc()
