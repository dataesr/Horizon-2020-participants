load(file = rdata_last_version(paste0(data_result, "data/"),"participant_ID_d02_"))
load(paste0(data_result, "org/org_identified.RData" ))

x <- org_identified %>% 
  dplyr::count(id, stage) %>% 
  dplyr::filter(n>1)

# provisoire 2022_08 modifier dans le programme organismes
org_identified <- org_identified %>% mutate(org_id=ifelse(org_id=="NC", NA_character_, org_id))

# org_identified <- org_identified %>% 
#   dplyr::mutate(org_id = remove_space(org_id), 
#                 org_id = str_replace_all(org_id, ";", ","))

#provisoire : faire cette modif dans organisme project : remplacet pivot_wider

# temp <- org_identified %>% 
#   separate_rows(org_id, sep=",") %>% 
#   unique()
# 
# org_identified <- temp %>% 
#   group_by(stage, id, org) %>% 
#   dplyr::mutate(org_id = paste(unique(org_id), collapse=',')) %>% 
#   ungroup() %>% 
#   unique()
  

participant <- participant %>% 
  left_join(org_identified) %>% 
  unique()



lien_old <- load_one_obj(rdata = paste0(data_old, "lien_id_proj.Rdata"), "lien_id_proj") %>% 
  select(stage, project_id, id, participant_order, participates_as, participant_nns) %>% 
  dplyr::filter(!is.na(participant_nns)) %>% 
  group_by(stage, project_id, id, participant_order, participates_as) %>% 
  mutate(participant_nns=str_c(participant_nns, collapse=","),
         old="YES") %>% 
  ungroup() %>% 
  unique()


participant <- left_join(participant, lien_old, by = c("project_id", "participant_order", "participates_as", "id", "stage")) %>% 
  mutate(org_id=ifelse(!is.na(old), participant_nns, org_id)) %>% 
  select(-participant_nns)



# vérification que tous les participtions identifiées par les orgnismes ont un lien pic+id
temp <- participant %>% 
  dplyr::filter(is.na(organizations_id), !is.na(org_id), country_code=="FR")
if (nrow(temp)>0){ 
  print("compléter part_pic avec les sources id manquants")
  }else{print("ok")}


save("participant", file=paste0(data_result, "data/participant_org_e01_", date_sauve, ".Rdata"))


rm(list=c("org_identified", "x", "temp", 'lien_old'))
gc()

