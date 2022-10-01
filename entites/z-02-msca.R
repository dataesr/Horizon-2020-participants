# load(file=paste0(participants, "referentiels/grid_pic.Rdata"))
load(file=rdata_last_version(paste0(data_result, "data/"), "participant_ID_d02_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "ref_part_q01_"))
load(file=paste0(data_result, "data/subvent.Rdata"))
load(file=paste0(data_result, "data/lien_id_proj.Rdata"))
load(file=rdata_last_version(paste0(data_result, "data/"), "prop_proj_"))
etr <- load_one_obj(rdata=paste0(data_result, "data/participants_etrangers.Rdata"), "etr")
load(file=rdata_last_version(paste0(data_result, "data/"), "geoloc_key_s01_"))
load(file=paste0(data_result, "data/perso_msca.RData"))
pays <- load_one_obj(rdata=paste0(chemin, "traitement/exe/nomenclature/pays.rdata"), "pays_fr")
cat_name <- readxl::read_xlsx(paste0(participants, "nomenclatures/_category.xlsx"), 
                              sheet="category_name", na="") %>% mutate_if(is.character,str_squish) 

#  données France traitées au niveau du participant
temp <- lien_id_proj %>% 
  dplyr::filter(programme_abbr=="MSCA") %>% 
  select(project_id, stage, programme_abbr, proposal, 
         id, participant_order, participates_as, organizations_id, status, status_orig) %>% 
  group_by(stage, id, participant_order, participates_as, organizations_id, status, status_orig) %>% 
  ungroup() %>% 
  unique()

subv <- subvent %>% 
  select(project_id, id, participant_order, role, participates_as, subv, subv_net, subv_prop) %>% 
  unique()

#extraction des projets signés avec subv
proj <- temp %>% 
  dplyr::filter(status %in% "SIGNED") %>% 
  left_join(subv, by=c("project_id", "id", "participant_order","participates_as"))
  # mutate(status="SIGNED")

main <- temp %>% 
  dplyr::filter(status %in% c("SIGNED", "MAIN")) %>% 
  left_join(subv, by=c("project_id", "id", "participant_order","participates_as")) %>% 
  mutate(status="MAIN", stage="proposal")

elig <- temp %>%
  left_join(subv, by=c("project_id", "id", "participant_order","participates_as")) %>% 
  mutate(status="ELIGIBLE", stage="proposal")  

msca <- bind_rows(proj, main, elig) %>% select(-status_orig)
msca <- ref_info %>% 
  select(organizations_id=id_ref, organizations_name=name_ref, 
         organizations_acronym=acronym_ref, sector, category, cat, category_2, category_5) %>% 
  left_join(cat_name, by="category") %>% 
  dplyr::rename(category_1_name=category_name) %>% 
  left_join(cat_name, by=c("category_2"="category")) %>% 
  dplyr::rename(category_2_name=category_name) %>% 
  left_join(cat_name, by=c("category_5"="category")) %>% 
  dplyr::rename(category_5_name=category_name) %>% 
  right_join(msca, by="organizations_id")

# recuperation des noms d'orga manquants
if (nrow(msca %>% dplyr::filter(is.na(organizations_name)))>0){
  print('maj de msca avec l ename_source de participant')
  msca <- 
    left_join(msca, 
              select(participant, c(id, participant_order, participates_as, participant_pic, name_source)), 
              by = c("id","participant_order", "participates_as")) %>% 
    mutate(organizations_name=ifelse(is.na(organizations_name), str_to_sentence(name_source), organizations_name),
    ) %>% 
    select(-name_source, -participant_pic)
  if (nrow(msca %>% dplyr::filter(is.na(organizations_name)))>0){
    print('verifier à nouveau ; problème de name manquant non résolu')
  }
}else{print('ok')}


# selection infos projet
temp1 <- prop_proj %>%
  select(stage, year, call_code, project_id, area_abbr, area_lib, starts_with("action"), starts_with("msca"), year) %>% 
  right_join(msca, by= c("project_id", "stage")) %>% 
  mutate(country_code="FR", country_name="France", country_level_1="Pays membres", country_level_2="Pays membres (UE15)")


verif <- msca %>% 
  select(status, id, participant_order, participates_as, organizations_id) %>%
  dplyr::count(status, id, participant_order, participates_as) %>% 
  dplyr::filter(n>1)
if (nrow(verif)>0){
  print('verif doublon dans verif')
}


# etranger

temp2 <- prop_proj %>% 
  dplyr::filter(area_abbr=="MSCA") %>% 
  select(stage, call_code, project_id, area_abbr, area_abbr, area_lib, starts_with("action"), 
         starts_with("msca")) %>% 
  inner_join(etr, by= c("project_id", "stage")) %>% 
  select(id, year, call_code, stage, status, project_id, year, programme_abbr, area_abbr, 
         area_lib, starts_with("action"), 
         starts_with("msca"), organizations_id, participant_order, participates_as, role,
         organizations_name=name, subv, subv_net, subv_prop, 
         country_code, country_name, country_level_1, country_level_2, proposal)

proj <- temp2 %>% 
  dplyr::filter(stage=="project") %>% 
  mutate(status="SIGNED")
main <- temp2 %>% 
  dplyr::filter(status %in% c("MAIN", "SIGNED")) %>% 
  mutate(status="MAIN", stage="proposal")
elig <- temp2 %>% 
  mutate(status="ELIGIBLE", stage="proposal")

msca_etr <- bind_rows(proj, main, elig)

verif <- msca_etr %>% 
  select(status, id, participant_order, participates_as, organizations_id) %>%
  dplyr::count(status, id, participant_order, participates_as) %>% 
  dplyr::filter(n>1)
# doublon pays par exemple SYNGENTA UK, CH -> deux id_ROR différents

# table finale msca fr+etr ; traitement funding/2 pour id_etr CH/UK
msca <- bind_rows(temp1, msca_etr) %>% 
  group_by(status, id, participant_order, participates_as) %>% 
  dplyr::mutate(n_participation=n(), funding=if_else(stage=="project", subv, subv_prop), 
                funding=funding/n_participation) %>% 
  ungroup()

nrow(msca %>% dplyr::filter(country_code=="FR") %>% unique()) # 27 774
msca %>% dplyr::filter(country_code=="FR") %>% group_by(status) %>% summarize(sum(funding), na.rm=T)# "65 063 175 205"
# 1 ELIGIBLE    4 979 043 226.  
# 2 MAIN         633 843 782.  
# 3 SIGNED       617 225 763  
# msca %>% group_by(status) %>% summarize(sum(funding), na.rm=T)# "65 063 175 205"
# msca %>% group_by(status) %>% summarize(sum(n_participation), na.rm=T)# "315 818"
# msca %>% group_by(status) %>% summarize(n_distinct(project_id), na.rm=T)# "78 611"

msca <- msca %>%   
  separate(., col = id, into=c("id_p", "pic2", "pic1"), sep = "-", remove = F) %>% 
  select(-id_p, -pic2) %>% 
  dplyr::mutate(organizations_id= if_else(is.na(organizations_id), paste0("NonIdent", pic1), organizations_id),
         action4_id=if_else(stage=="proposal" & action3_id=="MSCA-IF-EF", action3_id, action4_id),
         action4_lib=if_else(stage=="proposal" & action3_id=="MSCA-IF-EF", action3_lib, action4_lib),
         action1_id=if_else(str_detect(call_code, "NIGHT"), "MSCA", action1_id),
         action1_lib=if_else(str_detect(call_code, "NIGHT"), "Marie Sklodowska-Curie actions", action1_lib)) %>% 
  mutate_at(vars(matches("^action[2-4]{1}_id$")), funs(ifelse(str_detect(call_code, "NIGHT"), "MSCA-NIGHT", .))) %>% 
  mutate_at(vars(matches("^action[2-4]{1}_lib$")), funs(ifelse(str_detect(call_code, "NIGHT"), "European Researchers' Night", .)))


msca %>% group_by(status) %>% summarize(sum(n_participation), na.rm=T)# "315 818"
msca %>% group_by(status) %>% dplyr::mutate(n=n_distinct(project_id)) %>% 
  select(status, n) %>% unique()# "78 611"

# tot <- msca %>% 
#   select(year, call_code, stage, area_abbr, area_lib, action4_id, action4_lib, 
#          action3_id, action3_lib, action2_id, action2_lib,
#          action1_id, action1_lib, msca_code, msca_name, programme_abbr, status,
#          project_id, funding, n_participation) %>% 
#   group_by(year, call_code, stage, area_abbr, area_lib, action4_id, action4_lib, 
#            action3_id, action3_lib, action2_id, action2_lib, action1_id, action1_lib,
#            msca_code, msca_name, programme_abbr, status) %>% 
#   dplyr::summarize(nb=n_distinct(project_id), n_participation=sum(n_participation, na.rm=T), 
#             funding=sum(funding, na.rm=T)) %>% 
#   unique() %>% 
#   group_by(year, call_code, stage, area_abbr, area_lib, action4_id, action4_lib, 
#            action3_id, action3_lib, action2_id, action2_lib,
#            action1_id, action1_lib, msca_code, msca_name, programme_abbr, status) %>% 
#   dplyr::summarize(n_projet=n(), n_participation=sum(n_participation, na.rm=T), 
#             funding=sum(funding, na.rm=T)) %>% 
#   ungroup() %>% 
#   unique()

tot <- msca %>% 
  group_by(year, call_code, stage, area_abbr, area_lib, action4_id, action4_lib, 
           action3_id, action3_lib, action2_id, action2_lib, action1_id, action1_lib,
           msca_code, msca_name, programme_abbr, status) %>% 
  dplyr::summarize(n_project=n_distinct(project_id), n_participation=sum(n_participation, na.rm=T), 
                   funding=sum(funding, na.rm=T)) %>% 
  select(year, call_code, stage, area_abbr, area_lib, action4_id, action4_lib, 
         action3_id, action3_lib, action2_id, action2_lib, action1_id, action1_lib,
         msca_code, msca_name, programme_abbr, status,
         n_project, funding, n_participation) %>% 
  unique() 


test <- tot %>% group_by(stage, status) %>% 
  unique() %>% dplyr::summarize(n())
test <- tot %>% select(status, msca_name) %>% 
  unique() %>% group_by(status, msca_name) %>% dplyr::summarize(n())

  
tot %>% group_by(status) %>% summarize(sum(funding, na.rm=T))
tot %>% group_by(status) %>% summarize(sum(n_participation, na.rm=T))
tot %>% group_by(status) %>% dplyr::mutate(n=sum(n_project)) %>% select(status, n) %>% unique()

p <- msca %>% select(country_code, country_name, country_level_2, country_level_1) %>% unique()
msca <- merge(p, tot) %>% bind_rows(msca)


# geoloc fr
msca_geo <- geoloc_full %>% 
  select(id, participant_order, participates_as, com_code, com_nom, uucr_id, uucr_nom, 
          country_code, dep_id, dep_nom, dep_num_nom, aca_id, aca_nom, reg_id, reg_nom, latitude, longitude) %>% 
  mutate(loc=if_else(!is.na(com_code), com_code, latitude)) %>% 
  select(-latitude, -longitude) %>%
  unique()

test <- msca_geo %>% select(id, participant_order, participates_as) %>% unique() %>% 
  dplyr::count(id, participant_order, participates_as)

proj <- msca %>% 
  dplyr::filter(country_code=="FR", status=="SIGNED", !is.na(project_id)) %>% 
  left_join(msca_geo, by = c( "id", "participant_order", "participates_as")) %>% 
  unique()

main <- msca %>% 
  dplyr::filter(country_code=="FR", status=="MAIN", !is.na(project_id)) %>% 
  left_join(msca_geo, by = c("id", "participant_order", "participates_as")) %>% 
  unique()

elig <- msca %>% 
  dplyr::filter(country_code=="FR", status=="ELIGIBLE", !is.na(project_id)) %>% 
  left_join(msca_geo, by = c("id", "participant_order", "participates_as")) %>% 
  unique()


# denombrement des acteurs par participation, et des localisations par part + orga
msca_geo <- bind_rows(proj, main, elig) %>% 
  select(-starts_with("subv"), -n_project) %>% 
  group_by(status, id, participant_order, participates_as) %>% 
  dplyr::mutate(n_organization=n_distinct(organizations_id)) %>%
  group_by(status, id, participant_order, participates_as, organizations_id) %>% 
  dplyr::mutate(n_location=n()) %>%
  dplyr::mutate(funding_location=funding/n_organization/n_location) %>%
  ungroup() %>% 
  unique()         

#101007531

msca_geo %>% group_by(status) %>% 
  dplyr::summarize(sum(funding_location), na.rm=T)
####################################################################################
# Quel projet FR est IF
# msca_if <- lien_id_proj %>% select(stage, project_id) %>% unique() %>% 
#   left_join(prop_proj, by = c("project_id", "stage")) %>% 
#   filter(action2_id == "MSCA-IF") %>% 
#   select(project_id) %>% unique()

# pour quand tous les pays
# msca_if <- prop_proj %>%
#   dplyr::filter(programme_abbr=="MSCA", action2_id == "MSCA-IF") %>%
#   select(project_id) %>% unique()

msca_if <- msca %>%
  dplyr::filter(!is.na(project_id), programme_abbr=="MSCA", action2_id == "MSCA-IF", role=='coordinator') %>%
  select(project_id, country_code, organizations_name) %>% 
  group_by(project_id) %>% 
  mutate(country_code=str_c(na.omit(unique(country_code)), collapse = ",")) %>% 
  ungroup() %>% 
  unique()


perso <- perso_msca %>% 
  select(project_id, contact, last_name, first_name, title_name, gender,
         nationality_source=cd_main_nationality, country_origin_source =cd_origin_country,
         country_residence_source = cd_residence_country, country_host=cd_host_country,
         orcid_id, researcher_id, google_scholar_id, scopus_auth_id) %>% 
  unique() %>%
  right_join(msca_if, by="project_id")


# perso <- perso %>% 
#     dplyr::mutate(
#       country_residence=if_else(country_residence %in% pays_fr, "FR", country_residence),
#       nationality=if_else(nationality %in% pays_fr, "FR", nationality),
#       nationality=if_else(is.na(nationality), country_residence, nationality))

perso <- perso %>% 
  left_join(select(pays, c(CODE_PAYS, CODE_PAYS_RGP)), by = c("nationality_source"="CODE_PAYS")) %>% 
  dplyr::rename(nationality = CODE_PAYS_RGP) %>% 
  left_join(select(pays, c(CODE_PAYS, CODE_PAYS_RGP)), by = c("country_origin_source"="CODE_PAYS")) %>% 
  dplyr::rename(country_origin = CODE_PAYS_RGP) %>% 
  left_join(select(pays, c(CODE_PAYS, CODE_PAYS_RGP)), by = c("country_residence_source"="CODE_PAYS")) %>% 
  dplyr::rename(country_residence = CODE_PAYS_RGP) %>% 
  dplyr::mutate(nationality = if_else(is.na(nationality) & !is.na(country_origin), country_origin, nationality)) %>% 
  select(-ends_with("source"))

# verification du gender -> "F" et "M"
if (length(unique(perso$gender))==2){
  print("ok")
}else{message("corriger gender : ", unique(perso$gender))}

if (length(is.na(msca$nationality))>0){
  print("ok")
}else{message("corriger nationality : info manquante")}



#########################################################
# si necessaire chargement des personnes déjà iderefisees
id_ref <- read.csv2(paste0(identification, "corrected/contact_idref_2021-05-14T08.csv"), 
                     na="", encoding = "UTF-8") %>% 
  select(contact, old_idref=idref) %>% 
  dplyr::filter(!is.na(old_idref))


perso %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>% 
  unique() %>% 
  dplyr::filter(!is.na(contact), str_detect(country_code, "FR")) %>%
  select(last_name, first_name, contact, old_gender=gender, nationality, organizations_name, orcid_id) %>% 
  left_join(id_ref, by="contact") %>% 
  unique() %>% 
write.csv2(paste0(identification, "perso_msca_api_", date_sauve, ".csv"), row.names = F, na="")

#retour
id_ref <- read.csv2(paste0(identification, "corrected/contact_idref_2021-05-14T08.csv"), 
                    na="", encoding = "UTF-8") %>% 
  filter(!is.na(idref)) %>% 
  select(contact, idref, lien_idref)

##################################################################################################

perso <- left_join(perso, id_ref, by="contact")

################################################################################################

# projet avec equipe FR
# perso_fr <- left_join(msca_if, perso_msca) %>% 
#   filter(!is.na(contact))

# perso_gender <- perso_api %>% 
#   filter(is.na(gender), !is.na(first_name)) %>% 
#   select(first_name) %>% 
#   write.csv2(paste0(identification, "gender_", date_sauve, ".csv"), row.names = F, fileEncoding = "UTF-8")
# 
# # fichir retarvaillé un peu à la main avant importation
# perso_gender <- readxl::read_xlsx(paste0(identification, "corrected/_gender.xlsx")) %>% 
#   mutate(gender_temp=if_else(gender=="F", "Female", if_else(gender=="M", "Male", NA_character_))) %>% 
#   select(-gender) %>% unique() 


# msca_perso <- perso %>% 
#   left_join(perso_gender, by = "first_name") %>% 
#   mutate(gender=if_else(is.na(gender), gender_temp, gender)) %>% 
#   select(-gender_temp, -stage) %>% 
#   filter(!is.na(contact)) %>% 
#   separate_rows(status, sep=",") %>% 
#   unique()
# 
# perso_gender <- msca_perso %>% 
#   filter(is.na(gender), !is.na(first_name)) %>% 
#   select(first_name) %>% 
#   write.csv2(paste0(identification, "gender_", date_sauve, ".csv"), row.names = F, fileEncoding = "UTF-8")


msca_perso <- perso %>%  
  select(project_id, contact, title_name, gender, nationality) %>% 
  unique() %>% arrange(contact, project_id, nationality) %>% 
  group_by(contact) %>% 
  fill(nationality, .direction = "downup") %>% 
  ungroup() %>% 
  unique()

msca_perso <- msca_perso %>% 
  left_join(prop_proj, by= c("project_id")) %>% 
  select(call_code, call_date, year, acronym_projet, name_project, budget_total, budget_financed, 
         duration, topic_code,  year, topic_name, area_abbr, action4_id, action4_lib, action3_id, 
         action3_lib, action2_id, action2_lib, action1_id, action1_lib, 
         stage, status, project_id, msca_code, msca_name,
         nationality, gender) %>% unique() %>%  
  left_join(select(pays, c(CODE_PAYS, nationality_name=NOM_PAYS)), by=c("nationality"="CODE_PAYS")) %>% 
  unique() %>% 
  group_by(project_id, status) %>% 
  dplyr::mutate(n=n())

msca_perso <- msca_perso %>% 
  filter(!(n == 2 & is.na(nationality)))
  
perso_fr <- lien_id_proj %>% 
  select(project_id, organizations_id, participates_as) %>% 
  unique() %>% 
  inner_join(msca_perso, by=c("project_id"))

perso_fr <- ref_info %>% 
  select(id_ref, id_gref, organizations_name = name, acronym) %>% 
  right_join(perso_fr, by=c("id_ref"="organizations_id")) %>% 
  select(-id_ref) %>% 
  dplyr::mutate(country_code = "FR", country_name = "France", country_level_1 = "Pays membres") %>% 
  unique()

test <- geoloc_fin %>% 
  separate(id, c("project_id", NA, NA), sep="-") %>% 
  separate(gps, c("lat", "lng"), sep=",") %>% 
  select(project_id, lat, lng)


perso_aut <- anti_join(msca_perso, perso_fr, by=c("project_id")) %>% 
  unique() %>% 
  inner_join(select(etr, c(project_id, participates_as, organizations_id, organizations_name=name,
                          country_code, country_name, country_level_1)),
             by="project_id") %>% 
  unique() %>% 
  group_by(project_id, status, organizations_id) %>% 
  dplyr::mutate(n=n())

msca_perso <- bind_rows(perso_fr, perso_aut)

unique(msca_perso$action3_id)

################################################################################################"

#collab

benef <- msca %>%  filter(participates_as=="beneficiary")
parten <- msca %>% filter(participates_as!="beneficiary", !is.na(organizations_id)) %>%
  select(call_code, year, status, project_id, organizations_id, organizations_name, 
         organizations_acronym, sector,
         category, cat, category_2, category_5, participant_order, participates_as,
         country_code, country_name, country_level_1, country_level_2)

colnames(parten) <- paste(colnames(parten), "part", sep = "_")

collab <- inner_join(benef, parten, by = c("status"="status_part", "project_id"="project_id_part"),
                   "participant_order"="participant_order_part")


###############################################################################################
# ref

proj <- participant %>% filter(stage=="project") %>% 
  group_by(stage, project_id, id, participant_order, participates_as) %>% 
  summarize(status="SIGNED", funding=sum(subv, na.rm=T), nb_part2=n()) %>% 
  ungroup() %>% unique()

main <- participant %>% filter(stage=="project" | status=="MAIN") %>% 
  mutate(subv=if_else(proposal=="NO", subv, subv_prop), stage="proposal", status="MAIN") %>% 
  group_by(stage, status, project_id, id, participant_order, participates_as) %>% 
  summarize(funding=sum(subv, na.rm=T), nb_part2=n()) %>% 
  ungroup() %>% unique()

elig <- participant %>% 
  mutate(subv=if_else(proposal=="NO", subv, subv_prop), stage="proposal", status="ELIGIBLE") %>%
  group_by(stage, status, project_id, id, participant_order, participates_as) %>% 
  summarize(funding=sum(subv, na.rm=T), nb_part2=n()) %>% 
  ungroup() %>% unique()
  
 ref <- bind_rows(proj, main, elig) %>% 
  left_join(select(prop_proj, -status), by = c("stage", "project_id")) %>% 
  group_by(stage, status, programme_abbr, action2_id, action3_id) %>% 
  summarize(funding=sum(funding), n_participation = sum(nb_part2), n_projet=n_distinct(project_id)) %>% 
  ungroup()
  
ref %>% group_by(status) %>% summarize(sum(funding, na.rm=T))
ref %>% group_by(status) %>% summarize(sum(n_participation, na.rm=T))
ref %>% group_by(status) %>% summarize(sum(n_projet, na.rm=T))


write.csv2(ref, file=paste0(tableau, "H2020_ref.csv"), na="", fileEncoding = "UTF-8",
           row.names = F)
write.csv2(collab, file=paste0(tableau, "msca_collab.csv"), na="", fileEncoding = "UTF-8",
           row.names = F)
write.csv2(msca_perso, file=paste0(tableau, "msca_perso.csv"), na="", fileEncoding = "UTF-8",
           row.names = F)
write.csv2(msca_geo, file=paste0(tableau, "msca_geo.csv"), na="", fileEncoding = "UTF-8",
           row.names = F)
write.csv2(msca, file=paste0(tableau, "msca.csv"), na="", fileEncoding = "UTF-8",
           row.names = F)
write.csv2(tot, file=paste0(tableau, "msca_tot2.csv"), na="", fileEncoding = "UTF-8",
           row.names = F)
