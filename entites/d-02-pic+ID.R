participant <- load_one_obj(rdata=paste0(data_result, "data/participants_source.Rdata"), "participant")

prettyNum(sum(participant %>% dplyr::filter(country_code!="FR") %>% select(subv), na.rm = T), big.mark = " ") 
prettyNum(sum(participant %>% dplyr::filter(country_code!="FR") %>% select(subv_net), na.rm = T), big.mark = " ")  

####################################################################################################
# vérifier et compléter dans access les identifiants de la base ecorda
# réimporter la liste des identifiants validés
filename="_id_pic_2022_08.csv"
pic_id <-
  read.csv2(file = paste0(identification, "corrected/", filename), colClasses = "character", na.strings = "", encoding = "UTF-8") %>%
  # unique() %>%
  # dplyr::mutate(organizations_id = ifelse(!is.na(fix_id) & fix_id!=0, fix_id, id)) %>%
  dplyr::filter(!is.na(id)) %>%
  select(participant_pic, country_code, organizations_id=id) %>%
  map_dfc(., remove_space) %>%
  unique()

# 
# pic_id <- pic_id_new %>% 
#   dplyr::filter(!is.na(id)) %>% 
#   select(participant_pic, country_code, organizations_id=id)

participant <- participant %>% 
  left_join(pic_id, by = c("participant_pic", "country_code")) %>% 
  relocate(organizations_id) %>% 
  unique()
# modif 24/01 conserver source_id
  # dplyr::rename(organizations_id=source_id) %>% 
  # dplyr::mutate(organizations_id = remove_space(organizations_id)) %>%
  # relocate(organizations_id)

x <- participant %>% 
  dplyr::count(id, participates_as, stage, participant_order, country_code) %>% 
  dplyr::filter(n >1)

# liste de tous les identifiants Pour extraire les données liés aux identifiants des différents référentiels
# identifiant <- participant %>% 
#   select(organizations_id) %>% 
#   dplyr::filter(!is.na(organizations_id)) %>% 
#   unique()
# write.csv2(identifiant, file = paste0(identification, "identifiant_api.csv"), row.names = FALSE)

save("participant", file=paste0(data_result, "data/participant_ID_d02_", date_sauve, ".Rdata"))

rm(list=c("pic_id", "x"))
gc()
