## DONNEES POUR SCANR

load(file=rdata_last_version(paste0(data_result, "data/"), "geoloc_key_s01_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "prop_proj_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "participant_ID_d02_"))
load(file=paste0(data_result, "data/lien_id_proj.Rdata"))
load(file=paste0(data_result, "data/subvent.Rdata"))

# subventions par participant niveau fin (labos) -> data pour scanR juste project signed
part_id <- lien_id_proj %>% 
  dplyr::filter(stage=="project", status_orig=="SIGNED") %>% 
  select(stage, id, participant_order, participates_as, organizations_id, participant_id) %>% 
  unique() %>% 
  left_join(subvent, by = c("stage", "id", "participant_order", "participates_as")) %>%
  unique() %>%
  # group_by(stage, id, participant_order, participates_as, organizations_id) %>% 
  # mutate(nb_p=n_distinct(participant_id), funding_p=subv_net/nb_p) %>% 
  select(stage, project_id, id, participant_order, participates_as, organizations_id, participant_id, 
         funding=subv_net)


message(
  "subv participant  :\n",
  prettyNum(sum(part_id$funding, na.rm=T), big.mark = " "), "\n",
  "subv orga gestionnaire :\n",
  prettyNum(part_id %>% select(stage, id, participant_order, participates_as, organizations_id, funding) %>% 
          unique() %>% dplyr::summarize(sum(funding, na.rm=T)), big.mark = " "), "\n",
  "nbre participants :\n",
  prettyNum(part_id %>% select(id, participant_order, participates_as) %>% unique() %>% 
              dplyr::summarize(n()), big.mark = " "), "\n",
  "nbre projets :\n",
  prettyNum(part_id %>% select(project_id) %>% unique() %>% 
              dplyr::summarize(n_distinct(project_id)), big.mark = " ")
)

################################################################################################

#########################################################################################
### CREATION DU NIVEAU 0 ET AUTRE -> CO_PARTICIPANT
## identifiant pour scanr niveau participant
part <- part_id %>%
  select(-participant_id) %>% 
  unique() %>% 
  mutate(co_participant = "participant") %>% 
  # ungroup() %>% 
  dplyr::rename(participant_id=organizations_id) 
  

## (idntifiant pour scanr niveau co-participant
temp <- part_id %>% dplyr::filter(organizations_id!=participant_id) %>% 
  mutate(co_participant = 'co-participant',
         funding=0) %>% 
  # ungroup() %>% 
  select(-organizations_id)

# concatenation de structure_part et temp -> création de ligne pour le id_scan
T1<-Sys.time()
part <- bind_rows(part, temp) %>% 
  arrange(project_id, participant_order, id, participates_as, desc(co_participant)) %>% 
  group_split(project_id, participant_order) %>%
  purrr::map_df(~.x %>% group_by(id, participates_as) %>% 
                dplyr::mutate(ligne = cur_group_id())) %>%
  ungroup() %>% 
  unique()
T2<-Sys.time()
T2-T1

structure_part <- part %>% 
  group_by(id, participant_order, participates_as, ligne) %>% 
  dplyr::mutate(
    ligne1 = row_number(),
    ligne2 = if_else(co_participant=="participant", 0, NaN),
    ligne2 = if_else(co_participant=="co-participant", ligne1-1, ligne2),
    id_scanr=paste(id, participant_order, ligne, ligne2, sep="-")) %>% 
  ungroup() %>% 
  select(-starts_with("ligne"))

  if(nrow(structure_part %>% group_by(id_scanr) %>% dplyr::count() %>% dplyr::filter(n>1)) > 1){
    print("doublons de id_scanr à corriger")
  }else{print("ok")}
  
  test <- 
    anti_join(part_id, structure_part, by = c("stage", "project_id", "id", "participant_order"))
  if (nrow(test)>0){print("Attention, il manque des participations dans structure_part")
  }else{print("ok")}

# info du participant ID
load(file=rdata_last_version(paste0(data_result, "data/"), "ref_part_q01_"))
structure_part <- structure_part %>% 
  left_join(select(ref_info, c(id_ref, name_ref, acronym_ref, category, sector, typage_temp)), 
            by = c("participant_id"="id_ref")) 


  prettyNum(sum(structure_part$funding, na.rm=T), big.mark = " ")
  message(
      "subv participant :\n",
      prettyNum(structure_part %>% 
              select(stage, id, participant_order, participates_as, participant_id, funding) %>% 
              unique() %>% summarize(sum(funding, na.rm=T)), big.mark = " "), "\n",
      "subv orga gestionnaire :\n",
      prettyNum(structure_part %>% 
              select(stage, id, participant_order, participates_as, funding) %>% 
              unique() %>% summarize(sum(funding, na.rm=T)), big.mark = " ")
  )

####
# entities <- read.csv('C:/Users/zfriant/Documents/OneDrive/PCRI/traitement/2022_05/bd/project/legal_entities.csv', 
#                      encoding = "UTF-8", colClasses = "character") %>% 
#   select(PIC, GENERAL_PIC, COUNTRY_CODE, GENERAL_STATE, LEGAL_ENTITY_TYPE_ABBR) %>% 
#   mutate(LEGAL_ENTITY_TYPE_ABBR=ifelse(LEGAL_ENTITY_TYPE_ABBR=="HES", "Ens. supérieur",
#                                        ifelse(LEGAL_ENTITY_TYPE_ABBR=="PRC", "Org. privés",
#                                               ifelse(LEGAL_ENTITY_TYPE_ABBR=="OTH", "Autres",
#                                                      ifelse(LEGAL_ENTITY_TYPE_ABBR=="PUB", "Org. publics",
#                                                             ifelse(LEGAL_ENTITY_TYPE_ABBR=="REC", "Recherche",LEGAL_ENTITY_TYPE_ABBR))))))




# récupération variables dans participant
  toString(colnames(structure_part))

  type_org <- participant %>% select(participant_type_code, participant_type_name) %>% unique()

structure_part <- participant %>% 
  select(stage, id, participant_order, participates_as, participant_pic, applicant_pic, 
         role, participant_type_code, cout_tot_part,
         name_source, acronym_source, website, pme, country_code, country_name, 
         country_level_1, country_level_2) %>% 
  right_join(structure_part, by = c("stage", "id", 'participant_order', "participates_as")) %>% 
  mutate(participant_type_code=ifelse(!is.na(typage_temp), typage_temp, participant_type_code),
         participant_type_code=ifelse(co_participant=="co-participant", NA_character_, participant_type_code)) %>% 
  select(-typage_temp) %>% 
  left_join(type_org)


  prettyNum(sum(structure_part$funding, na.rm=T), big.mark = " ")
  message("subv orga gestionnaire :\n",
    prettyNum(structure_part %>% select(stage, id, participant_order, participates_as, participant_id, funding) %>% 
          unique() %>% summarize(sum(funding, na.rm=T)), big.mark = " "), "\n",
    "nbre participants :\n",
    prettyNum(structure_part %>% select(id, participant_order, participates_as) %>% unique() %>% 
                dplyr::summarize(n()), big.mark = " "), "\n",
    "nbre projets :\n",
    prettyNum(structure_part %>% select(project_id) %>% 
                unique() %>% dplyr::summarize(n()), big.mark = " ")
  )

#############################################################################################
# GEOLOC

toString(colnames(geoloc_full))

p <- geoloc_full %>% 
  dplyr::filter(stage=="project") %>% 
  select(-country_name, -country_code, -n) %>% 
  unique() %>% 
  # unite(gps, lat, lng, sep=",", na.rm=T, remove=T) %>% 
  group_by(stage, id, participant_order, participates_as, idr) %>%
  # summarize_at(vars(.), str_c(collapse=";"))
  summarise_all(~str_c(.x, collapse=";"))
  
p2 <- structure_part %>% 
  left_join(p, by = c("stage", "id", "participant_order", "participates_as", "participant_id"='idr')) %>% 
  unique()

nrow(p2 %>% dplyr::filter(!is.na(latitude)))

prettyNum(sum(p2$funding, na.rm=T), big.mark = " ")
message("subv orga gestionnaire :\n",
        prettyNum(p2 %>% select(stage, id, participant_order, participates_as, participant_id, funding) %>% 
                    unique() %>% summarize(sum(funding, na.rm=T)), big.mark = " ")
)
#################################################################################################
#####
### données pour SCANR
##
load(file=paste0(referentiel, "etab.Rdata"))

scanr <- p2 %>% 
  dplyr::rename(id_ref=participant_id, name=name_ref, acronym=acronym_ref, post_code=cp) %>% 
  left_join(select(etab, c("paysage", "siret", "uai", "ed")), by = c("id_ref"="paysage")) %>% 
  unique() %>% 
  dplyr::mutate(
         participant_id=siret,
         participant_id=if_else(is.na(participant_id), uai, participant_id),
         participant_id=if_else(is.na(participant_id), ed, participant_id),
         participant_id=if_else(is.na(participant_id), id_ref, participant_id)) %>% 
  select(-siret, -uai, -ed) %>% 
  relocate(stage, id, id_scanr, participant_order, participates_as, participant_id) %>% 
  unique()
  

prettyNum(sum(scanr$funding, na.rm=T), big.mark = " ")
message(
  "subv orga gestionnaire :\n",
   prettyNum(scanr %>% 
            select(stage, id, participant_order, participates_as, participant_id, funding) %>% 
            unique() %>% 
            summarize(sum(funding, na.rm=T)), big.mark = " "), "\n",
  "nbre participants : \n",
   scanr %>% 
    dplyr::filter(participates_as=="beneficiary") %>% 
    select(id,participant_order,participates_as) %>% 
    unique() %>% dplyr::summarize(n())
)


#################################################################################################################
#PARTICIPANTS
# filename="participant_ID3_2021-03-18-T22-16.Rdata"
# part <- load_one_obj(rdata=paste0(data_result, "data/", filename), "participant") %>%
#   mutate(participant_order=as.character(participant_order))

# part_fr pour adresse 
# p <- part_fr %>%
#   dplyr::filter(stage=="project") %>% 
#   # mutate(cout_tot_part=as.numeric(cout_tot_part)) %>% 
#   select(id, stage, participant_order, participates_as, vat_id=source_id) %>% 
#   unique()


###############################################################################################
# extraction étrangers de part (projects) pour scanr et collab
etr <- load_one_obj(rdata=paste0(data_result, "data/participants_etrangers.RData"), "etr")

etr2 <- etr %>%
  dplyr::filter(stage=="project", status_orig=="SIGNED") %>%
  # inner_join(select(prop_proj, c(stage, project_id)), by=c("stage", "project_id")) %>%
  unique() %>%
  dplyr::mutate(subv_net=if_else(subv_net<0, 0, subv_net)) %>%
  dplyr::rename(participant_id=organizations_id,
                address=address_source, post_code=post_code_source, city=city_source,
                funding=subv_net) 

T1<-Sys.time()
etr2 <- etr2 %>%
  arrange(project_id, participant_order, id, participates_as) %>% 
  group_split(project_id, participant_order) %>%
  purrr::map_df(~.x %>% group_by(id, participates_as) %>% 
                  dplyr::mutate(ligne = cur_group_id())) %>%
  ungroup() %>% unique()
T2<-Sys.time()
T2-T1 

part_etr <- etr2 %>% 
  dplyr::mutate(id_scanr=paste(id, participant_order, ligne, "0", sep="-"),
                co_participant="participant") %>%
  select(-year, -programme_abbr, -statut_part, -status, -status_orig, -prop, -proj,
         -subv_prop, -subv, -proposal, -ligne, -n_part, -adr) %>% 
  rename(adresse=address) %>% 
  relocate(project_id, participant_order, participant_id, funding, country_name)
##################################################################################################

#######################################################################################################

scanr <- bind_rows(scanr, part_etr) %>% 
  dplyr::mutate(sies_id = if_else(!is.na(participant_id), 1, 0), project_type = "H2020",
                name=if_else(is.na(name), str_to_sentence(name_source), name),
                acronym=if_else(is.na(acronym), acronym_source, acronym)) %>% 
  unique() %>% 
  select(-id, id=id_scanr)

  # verification du project_id null
  if(nrow(scanr %>% select(project_id) %>% dplyr::filter(is.na(project_id)))>0){
    print("project_id null -> corriger l'erreur")
    verif <- scanr %>% select(country_code, project_id) %>% dplyr::filter(is.na(project_id))
  }else{print("ok")}
  
  
  # verification unicite du id
  if(nrow(scanr %>% dplyr::count(id) %>% dplyr::filter(n>1))>0){
    x <- scanr %>% dplyr::count(id) %>% dplyr::filter(n>1)
    print("doublon de id")
  }else{print("ok")}
  
  prettyNum(sum(scanr$funding, na.rm=T), big.mark = " ")
  
  message("subv orga gestionnaire :\n",
          prettyNum(scanr %>% 
            select(stage, id, participant_order, participates_as, participant_id, funding) %>% 
            unique() %>% summarize(sum(funding, na.rm=T)), big.mark = " ")
  )

options(scipen = 999)
write.csv2(scanr, file = paste0(data_result, "data/scanr_participant.csv"), row.names = FALSE, 
           na = "", fileEncoding = "UTF-8")
save(scanr, file=paste0(data_result, "data/scanr_base.RData"))
# load(file=paste0(data_result, "data/scanr_base.Rdata"))


#############################################################################################
#########OPEN-DATA
toString(colnames(scanr))

vec <- c("stage, id, participant_order, participates_as, participant_id, participant_pic, 
         applicant_pic, role, participant_type_code, cout_tot_part, name_source, acronym_source, 
         website, pme, country_code, country_name, country_level_1, country_level_2, project_id, 
         id_ref, funding, co_participant, name, acronym, category, sector, participant_type_name, 
         adresse, post_code, city, com_code, latitude, longitude, com_nom, aca_id, aca_nom, 
         uucr_id, uucr_nom, dep_id, dep_nom, dep_num_nom, dep_nom_num, reg_id, reg_nom, 
         geo_admin1_code, geo_admin1_name, geo_admin1_name_ascii, geo_nuts1_code, geo_nuts1_name, 
         sies_id, project_type")


type_id <- function(var){
  if_else(str_detect(var, pattern_ror), "ROR",
  if_else(str_detect(var, pattern_siren), "Siren",
  if_else(str_detect(var, pattern_siret), "Siret",
  if_else(str_detect(var, pattern_paysage), "Paysage",
  if_else(str_detect(var, pattern_rnsr), "RNSR",
  if_else(str_detect(var, pattern_uai), "UAI",
  if_else(str_detect(var, pattern_assoc), "RNA",
  if_else(str_detect(var, pattern_grid), "Grid",
  if_else(str_detect(var, pattern_finess), "Finess",
  if_else(str_detect(var, pattern_pic), "PIC",
  NA_character_))))))))))
}

#ne pas séparer gps en lat et lng car plusieurs coordonnées gps (sep=";") pour une même ligne
ods <- scanr %>% 
   dplyr::mutate(participation_id = str_c(project_id, participant_order, sep="-"),
         name=if_else(is.na(name), str_to_sentence(name_source), name),
         acronym=if_else(is.na(acronym), acronym_source, acronym))%>% 
  select(project_type, project_id, participation_id, role, participates_as, participant_id, 
         co_participant, participant_type_code, participant_type_name, name, acronym, funding,
         country_code, country_name, country_level_1, com_code, com_nom,
         uucr_id, uucr_nom, dep_id, dep_nom, dep_num_nom, aca_id, aca_nom,
         reg_id, reg_nom, latitude, longitude) %>% 
  unique() %>% 
  relocate(project_type, participation_id, participant_id, name, acronym, role, participates_as, 
           co_participant, participant_type_code, participant_type_name, 
           funding, country_code, country_name, country_level_1, com_code, com_nom,
           uucr_id, uucr_nom, dep_id, dep_nom, dep_num_nom, aca_id, aca_nom, reg_id, reg_nom, latitude, longitude, project_id)


ods$participant_id_type <- type_id(ods$participant_id)

unique(ods$participant_id_type)

prettyNum(sum(ods$funding, na.rm=T), big.mark = " ")

ods <- prop_proj %>% dplyr::filter(stage=="project") %>% 
  dplyr::mutate(source_url = paste0("https://cordis.europa.eu/project/id/", project_id)) %>% 
  select(project_id, acronym_projet, name_project, start_date, end_date, budget_financed,
         duration, keywords_en, source_url, call_code, topic_code, programme_abbr, 
         programme_lib) %>% 
  inner_join(ods, by="project_id") %>% 
  relocate(project_id, acronym_projet, name_project, start_date, end_date, budget_financed, 
           duration, keywords_en, source_url, 
           call_code, topic_code, programme_abbr, programme_lib, .after=participant_id_type)

verif_na <- ods[apply(ods, 2, function(x) any(is.na(x)))]

# verification et comparaison des funding et budget_financed (doivent être égaux au niveau du projet)
test <- ods %>% select(project_id, budget_financed, funding) %>% 
  group_by(project_id, budget_financed) %>% 
  dplyr::mutate(funding=sum(funding, na.rm=T), b=unique(budget_financed),
                diff=b-funding) %>% 
  unique()


nrow(ods %>% dplyr::filter(!is.na(latitude)))

write.csv2(ods, file=paste0(data_result, "data/fr-esr-projets-participants-h2020.csv"), 
           na="", fileEncoding = "UTF-8", row.names = F)

rm(list=c("p", "p2", "part_id", "part", "etab", "etab2", "sirene_net", "s_info", "participant",
          "structure_part", "part_etr", "lien_id_scan","subv_scan", "etr"))
gc()
