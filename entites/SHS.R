
#####################################################################
load(file=rdata_last_version(paste0(data_result, "data/"), "prop_proj_"))

entities <- read.csv(file="C:/Users/zfriant/Documents/OneDrive/PCRI/traitement/2021_12/bd/proposal/legal_entities.csv") %>% 
  mutate(applicant_pic=as.character(CD_APPL_PIC), participant_pic=as.character(CD_PART_PIC)) %>% 
  select(applicant_pic, participant_pic, CD_ORG_TYPE, CD_LE_STATUS) %>% 
  unique()

test <- entities %>% group_by(applicant_pic, participant_pic) %>% mutate(n=n()) %>% 
  dplyr::filter(n>1)
test <- entities %>% group_by(applicant_pic) %>% mutate(n=n()) %>% 
  dplyr::filter(n>1)

##################################################################
load(file=paste0(data_result, "data/participants_etrangers.RData"))

prettyNum(sum(etr$subv, na.rm = T), big.mark = " ") 

temp <- prop_proj %>% dplyr::filter(programme_abbr=="SOCIETY") %>%
  select(stage, project_id, call_code, call_date, action4_id, topic_code, acronym_projet, status_orig, status)

benef_etr <- etr %>% 
  select(applicant_pic, participant_pic, stage, project_id, organizations_id, organizations_name=name, 
         role, participates_as,
         country_code, country_name, subv_prop, subv, subv_net) %>% 
  inner_join(temp) %>% 
  unique()
  
type <- left_join(select(benef_etr, c(applicant_pic, participant_pic)), 
                  entities, by=c("applicant_pic", "participant_pic")) %>% 
  unique()
type_no <- type %>% 
  dplyr::filter(is.na(CD_ORG_TYPE)) %>% 
  select(applicant_pic, participant_pic) %>% 
  unique() %>% 
  left_join(select(entities, -participant_pic), by="applicant_pic")
type <- anti_join(type, type_no, by = c("applicant_pic", "participant_pic")) %>% 
  bind_rows(type_no)

benef_etr <- benef_etr %>% 
  left_join(type, by=c("applicant_pic", "participant_pic")) %>% 
  select(-participant_pic, -applicant_pic, -CD_LE_STATUS) %>% 
  rename(organizations_type=CD_ORG_TYPE) %>% 
  unique()


#################################################################
load(file= paste0(data_result, "data/lien_id_proj.RData"))
load(file= paste0(data_result, "data/subvent.RData"))

test <- lien_id_proj %>% 
  dplyr::filter(programme_abbr=="SOCIETY") %>% 
  select(project_id, stage) %>% unique() %>% 
  group_by(project_id) %>% mutate(n=n())


temp <- lien_id_proj %>% 
  dplyr::filter(programme_abbr=="SOCIETY") %>% 
  select(project_id, stage, id, participant_order, participates_as, organizations_id, participant_id)


benef <- inner_join(temp, subvent, by=c("stage", "project_id", "id", "participant_order", "participates_as")) %>% 
  unique() %>% 
  mutate(participant_id=ifelse(organizations_id==participant_id, NA_character_, participant_id),
         country_name="France") %>% 
  group_by(stage, id, participant_order, participates_as) %>% 
  mutate(n=n(),
         participant_id=ifelse(!is.na(participant_id),
           str_c(na.omit(participant_id), collapse = ";"),
           NA_character_)) %>% 
  unique() %>% 
  select(-proposal, -nb, -n)



load(file=rdata_last_version(paste0(data_result, "data/"), "ref_part_q01_"))
cat_name <- readxl::read_xlsx(paste0(participants, "nomenclatures/_category.xlsx"), 
                              sheet="category_name", na="") %>% dplyr::mutate_all(~str_squish(.))


benef <- left_join(benef, 
                   select(ref_info, c(id_ref, organizations_name=name_ref, category_5)), 
                   by=c("organizations_id"="id_ref")) %>% 
          left_join(cat_name, by=c("category_5"="category")) %>% 
          relocate(organizations_name, category_name,  .after=organizations_id) %>% 
          select(-category_5)

benef <- benef %>% separate_rows(participant_id, sep=";") %>% 
  left_join(select(ref_info, c(id_ref, participant_name=name_ref)), 
            by=c("participant_id"="id_ref")) %>% 
  relocate(participant_id, participant_name, .after=last_col())

# load(file=rdata_last_version(paste0(data_result, "data/"), "participant_ID_d02_"))


# benef <- inner_join(benef, 
#                    select(participant, c(project_id, stage, id, participant_order, participates_as, participant_type_name)),
#       by = c("project_id", "stage", "id", "participant_order", "participates_as"))


benef <- prop_proj %>% 
  select(stage, project_id, call_code, call_date, action4_id, topic_code, acronym_projet, status_orig, status) %>% 
  inner_join(benef, by=c("stage", "project_id"))


type <- benef %>% select(id) %>% 
  mutate(applicant_pic=str_extract(id, "(?<=-)[0-9]{9}(?=-)"), 
         participant_pic=str_extract(id, "(?<=-)[0-9]{9}$")) %>% 
  left_join(entities, by=c("applicant_pic", "participant_pic")) %>% 
  unique()
type_no <- type %>% 
  dplyr::filter(is.na(CD_ORG_TYPE)) %>% 
  select(applicant_pic, participant_pic) %>% 
  unique() %>% 
  left_join(select(entities, -participant_pic), by="applicant_pic")
if (nrow(type_no)>0){
type <- anti_join(type, type_no, by = c("applicant_pic", "participant_pic")) %>% 
  bind_rows(type_no)
}


benef <- benef %>% 
  left_join(type, by='id') %>% 
  select(-participant_pic, -applicant_pic, -CD_LE_STATUS, -id, -participant_order) %>% 
  rename(organizations_type=CD_ORG_TYPE) %>% 
  unique()

test <- benef %>% select(stage, project_id) %>% unique() %>% group_by(project_id) %>% 
  mutate(n=n())

# benef <- benef %>%
#   select(-id, -participant_order)

shs <- bind_rows(benef, benef_etr) %>% 
  unique()


shs <- shs %>% 
  rename(subventions_demandées=subv_prop, subventions_obtenues=subv, 
         subventions_obtenues_ventilées=subv_net) %>% 
  relocate(country_name, .after=country_code) %>% 
  relocate(status, status_orig, .after=stage) %>% 
  relocate(role, participates_as, .after=organizations_name) %>% 
  relocate(organizations_type, .before = category_name)



test <- shs %>% dplyr::filter(is.na(organizations_name)) %>% 
  select(stage,organizations_id) %>% unique()

if (nrow(test)>0){
  test <- test %>% mutate(participant_pic=str_replace(organizations_id, "^NonIdent", ""))
  test <- test %>% left_join(select(participant, c(stage, participant_pic, name_source)), 
                                    by=c("participant_pic", "stage")) %>% 
                               unique()
  test %>% dplyr::filter(is.na(name_source))
}


shs <- left_join(shs, test, by=c("stage", "organizations_id")) %>% 
  mutate(organizations_name=ifelse(is.na(organizations_name), name_source, organizations_name)) %>% 
  select(-name_source, -participant_pic)


proj_new <- read.csv(file="C:/Users/zfriant/Documents/OneDrive/PCRI/traitement/2022_05/bd/proposal/proposals.csv") %>% 
  mutate(project_id=as.character(PROPOSAL_NBR))


shs <- left_join(shs, select(proj_new,c(project_id, TOTAL_SCORE, RANK)), by="project_id") %>% 
  relocate(TOTAL_SCORE, RANK, .after=status_orig)

write.csv2(shs, file=paste0(chemin, "bilan H2020/projets_society.csv"), na="", fileEncoding = "UTF-8")

# shs[shs == "" | shs == " "] <- NA
# shs <- shs %>% 
#   arrange(organizations_id) %>% 
#   group_by(organizations_id) %>% 
#   fill(organizations_type_name, .direction="downup")
