# load(file = rdata_last_version(paste0(data_result, "data/"), "part_fr_g01_"))
# load(file = rdata_last_version(paste0(data_result, "data/"), "part_key_h01_"))


temp <- key_part %>% dplyr::filter(str_detect(p, pattern_rnsr)) %>% 
  select(lid, stage, id, participant_order, participates_as, participant_id=p) %>% 
  unique() %>% 
  group_by(lid, stage, id, participant_order, participates_as) %>% 
  dplyr::mutate(across(participant_id, ~str_c(., collapse=","))) %>% 
  unique()

org <- part_fr %>% 
  dplyr::mutate(source = ifelse(!is.na(org), "organisme", NA_character_),
                source = ifelse(is.na(org) & !is.na(liste_sigles), "eCorda", source)) %>% 
  relocate(source) %>% 
  select(lid, id, stage, status, project_id, participant_order, participant_pic, applicant_pic,
         participates_as, role, name_source, acronym_source, address_source,
         post_code_source, city_source, country_code, source, organisme, liste_sigles) %>% 
  left_join(temp)

maj_id <- cumul_id(pattern_rnsr) %>% 
  write.csv2(file=paste0(identification, "scanr_existe.csv"), row.names = FALSE)

#lien scanr actif
filename = "id_url_scanr_2021-12-29T19-17"
url <- 
  read.csv2(file = paste0(identification, 'corrected/', filename,".csv"), 
            colClasses = "character", na.strings = "", encoding = 'UTF-8')

#création de chaque fichier en supprimant les infos des autres orgas et création csv
list = c("cnrs", "cea", "inrae", "inria", "onera")

for (i in list) {
    assign(paste0("org_", i), org %>% 
             dplyr::mutate(participant_id = ifelse(str_detect(str_to_lower(organisme), i), participant_id, NA_character_)) %>% 
             left_join(select(rnsr, c(id, sigles_rnsr, acronym, name)), by=c('participant_id'='id')) %>% 
             mutate(code_labo=sigles_rnsr, labo=str_c(acronym, name, sep=" ")) %>% 
             select(-lid, -sigles_rnsr, -acronym, -name) %>% 
             left_join(url,  by=c('participant_id'='id'))) %>%
             unique() %>% 
    write_csv2(file= paste0(data_result, "org/", i,"_2021_11.csv"), na="")
  
  }
  
rm(list = ls(pattern="^org"), "temp", "url")
