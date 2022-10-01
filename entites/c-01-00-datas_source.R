# livraison = "2021_04"
# chemin_pc = "C:/Users/zfriant/OneDrive/PCRI/"
# data_source = paste0(chemin_pc, "traitement/", livraison, "/donnees/")
# data_result = paste0(participants, livraison, "/")
#################################################################################################################
#PARTICIPANTS
#chargement des datas nettoyées

proj <- 
  load_one_obj(paste0(data_source,"donnees/datas.RData"), "project") %>% 
  select(NUM_PROJET, ACRONYM_PROJET, programme_abbr, ACRONYM_PROJET, STADE_PROJET, ANNEE) %>% 
  dplyr::mutate(NUM_PROJET=as.character(NUM_PROJET), stage="project")
prop <- 
  load_one_obj(paste0(data_source,"donnees/datas.RData"), "proposal") %>% 
  select(NUM_PROJET, ACRONYM_PROJET, programme_abbr, ACRONYM_PROJET, STATUT_EVAL, ANNEE) %>% 
  dplyr::mutate(NUM_PROJET=as.character(NUM_PROJET), 
         STADE_PROJET=if_else(STATUT_EVAL=="MAIN", "MAIN", "ELIGIBLE"),
         stage="proposal") %>% 
  select(-STATUT_EVAL)

temp <- inner_join(prop,proj, by = "NUM_PROJET")
if (nrow(temp)==nrow(proj)){print("prop/proj -> ok")} else {print("probleme")}

part <- 
  load_one_obj(paste0(data_source,"donnees/datas.RData"), "participant") %>% 
  dplyr::rename(PIC_ID = CODE_ORG, name_source=NOM_LONG, acronym_source=NOM_COURT) %>%
  dplyr::mutate(id = paste0(NUM_PROJET,"-",PIC_ID2,"-",PIC_ID), 
                NUM_PROJET=as.character(NUM_PROJET),
                ORDRE_PART=as.numeric(ORDRE_PART), proj=1, 
                ROLE_PART=if_else(ROLE_PART=="participant", "partner", ROLE_PART)) %>%
  select(-org_tri, -PME) %>% 
  inner_join(proj, by = "NUM_PROJET") %>% 
  filter(!is.na(STADE_PROJET)) %>% unique()


# traitement des doublons
doublons <- part %>% 
  dplyr::count(STADE_PROJET, id, ORDRE_PART, TYPE_PART) %>% 
  filter(n>1)
if (nrow(doublons)> 0){
  
  supp <- doublons %>% left_join(part)
  print("verifier quelle ligne supprimer")
  supp$id
}else{print("ok")}

# PROVISOIRE - MAJ A VERIFIER A CHAQUE ACTU
# erc ch ordre_all = 1-0
# part <- part %>%  filter(!(id=="884664-951853920-951853920"& ORDRE_ALL=="1-0"))

# correction ordre_all
# ordre_cor <- read.csv2(file="C:/Users/zfriant/OneDrive/PCRI/participants/2019_11/part_ordre_corrige.csv") %>%
#   dplyr::mutate(NUM_PROJET = as.character(NUM_PROJET), ORDRE_ALL=as.character(ORDRE_ALL))

# a revoir pb ardre_all
# part <- part %>%
#   left_join(ordre_cor, by=c("NUM_PROJET", "PIC_ID", "PIC_ID2", "ORDRE_PART", "ORDRE_ALL", "TYPE_PART")) %>%
#   select(-ORDRE_PART, -ORDRE_ALL) %>%
#   dplyr::rename(ORDRE_PART=ordre_origine) %>%
#   unique()

# # doublons <- participant %>% count(id) %>% filter(n>1)
# doublons <- participant[which(duplicated(participant$id)),]


applicant <- 
  load_one_obj(paste0(data_source,"donnees/datas.RData"), "applicant") %>% 
  dplyr::rename(PIC_ID=CODE_ORG, name_source=NOM_LONG, acronym_source=NOM_COURT, SUBV_PROP=SUBV) %>%
  dplyr::mutate(TYPE_PART = ifelse(TYPE_PART=="partnerorganisation", "partnerorganization",TYPE_PART),
         id = paste0(NUM_PROJET,"-",PIC_ID2,"-",PIC_ID), NUM_PROJET=as.character(NUM_PROJET),
         prop=1) %>%
  inner_join(prop, by = "NUM_PROJET") %>% filter(!is.na(STADE_PROJET)) %>% 
  dplyr::mutate(stage="proposal", ROLE_PART=if_else(ROLE_PART=="participant", "partner", ROLE_PART)) %>% 
  select(-ORDRE_ALL)

########################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### ATTENTION !
# prog c-01-01 table finale SUBV_PROP_TAB

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
########################################################################

part <- part %>% 
  group_by(id, ORDRE_PART, TYPE_PART) %>% 
  dplyr::mutate(nb_id=n()) %>% 
  ungroup() %>% 
  select(-proj)

#detection de doublons dans subv_prop_tab
temp3 <- 
  inner_join(part, subv_prop_tab, by = c("id", "ORDRE_PART", "TYPE_PART")) %>% 
  unique() %>% 
  group_by(id, ORDRE_PART, TYPE_PART) %>% 
  dplyr::mutate(nb_id2=n()) %>% 
  ungroup() %>% 
  filter(nb_id!=nb_id2)

# apres verification
#somme des subv_prop pour les doublons si pas besoin de corriger une mauvaise jointure 
temp <- subv_prop_tab %>% unique() %>% 
  group_by(id, ORDRE_PART, TYPE_PART) %>% 
  dplyr::mutate(subv_prop = sum(SUBV_PROP, na.rm=T)) %>% 
  ungroup() %>% 
  select(id, ORDRE_PART, TYPE_PART, proposal, subv_prop, prop, proj) %>% 
  unique()

prettyNum(sum(temp$subv_prop), big.mark = " ") # 557 190 773 866
# [2019_11] -> 395 076 369 511
# [2021_04] -> 548 630 074 863

#jointure entre temp sans doublons et part
temp3 <- inner_join(part, temp, by = c("id", "ORDRE_PART", "TYPE_PART")) %>% 
  unique()

temp2 <- temp %>% filter(is.na(proj)) %>% 
  select(id, ORDRE_PART, TYPE_PART, subv_prop, prop) %>% 
  inner_join(select(applicant, c(-prop, -SUBV_PROP)), 
             by = c("id", "ORDRE_PART", "TYPE_PART")) %>% 
  unique()


# participations project + reliquat proposal
participant <- bind_rows(temp3, temp2) %>% 
  dplyr::mutate(PIC_ID=as.character(PIC_ID), PIC_ID2=as.character(PIC_ID2)) %>% 
  group_by(id, ORDRE_PART) %>% 
  dplyr::mutate(nb_id=n(), proposal=if_else(is.na(proposal) & stage == "proposal", "YES", proposal)) %>% 
  ungroup() # 1 121 836
# [2021_04] -> 1 085 969


colnames(participant) <- tolower(colnames(participant))

participant <- participant %>% 
  dplyr::rename(project_id=num_projet, applicant_pic=pic_id2, participant_order=ordre_part,
                participant_pic=pic_id, role=role_part, participant_type_code=org_abbr, 
                participant_type_name=org_typ, participates_as=type_part,
                country_code=code_pays, country_name=pays_lib, country_level_1=pays_statut_1,
                country_level_2=pays_statut_2, post_code_source=cp,
                address_source=adress, city_source=city, lat_source=lat, lng_source=lng,
                year=annee, status=stade_projet, website=url_part) %>% 
  dplyr::mutate(lat_source=str_squish(as.character(lat_source)), lng_source=str_squish(as.character(lng_source)))


# participant <- participant %>%
#   dplyr::mutate(participant_order= as.character(ordre_part)) %>% 
#   dplyr::rename(applicant_pic=pic_id2,
#                 participant_pic=pic_id,  participant_type_code=org_abbr, 
#                 participant_type_name=org_typ, participates_as=type_part,
#                  country_name=pays_lib, country_level_1=pays_statut_1,
#                 country_level_2=pays_statut_2) %>%  select(-ordre_part)

# participant <- participant %>% relocate(project_id, id, participant_order, source_id)

unique(participant$role)
# participant <- participant %>% dplyr::mutate(participant_order=as.character(participant_order))

verif <- participant %>% filter(country_code=="FR")  
test <- verif %>% 
  select(status, project_id, participant_pic, applicant_pic, participant_order, participates_as) %>% 
  dplyr::count(status, project_id, participant_pic, applicant_pic, participant_order, participates_as) %>% 
  filter(n>1)
  
doublons <- participant %>% 
  dplyr::count(status, id, participant_order) %>% 
  filter(n>1)

####################################################################################################################
######################################################################################################


x <- participant %>% filter(country_code=="FR") %>% 
  dplyr::count(id, participant_order, stage, participates_as) %>% 
  filter(n >1)

save("participant", "subv_prop_tab" , file=paste0(data_result, "data/participants_source.Rdata"))

rm(list=c("applicant", "doublons", "ordre_cor", "part", "proj", "prop", "test", "x", "subv_prop_tab",
          "verif", "np_1"))
rm(list=ls(pattern="^(temp)"))
gc()
