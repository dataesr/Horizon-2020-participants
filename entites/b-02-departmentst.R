#################################################################################################################
# récupération d'informations plus précises
PART_DEPT <- read.csv(paste0(chemin_pc, "traitement/", livraison,"/bd/project/participants_dept.csv"), na="",encoding = "UTF-8") %>% 
  dplyr::mutate(project_id=as.character(CD_PROJ_ID), participant_pic=as.character(CD_PART_PIC), applicant_pic=as.character(CD_APPL_PIC),
         stage = "project", participant_order=ORDER_NBR) %>% 
  select(-CD_PROJ_ID, -CD_PART_PIC, -CD_APPL_PIC)

participant_add <- PART_DEPT %>% 
  dplyr::mutate(LB_DEPT_NAME = str_replace(str_trim(LB_DEPT_NAME), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", NA_character_ ),
         LB_DEPT_ADRS = str_replace(str_trim(LB_DEPT_ADRS), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", NA_character_ ),
         CD_DEPT_COUNTRY = str_replace(str_trim(CD_DEPT_COUNTRY), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", NA_character_ ),
         LB_DEPT_CITY = str_replace(str_trim(LB_DEPT_CITY), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", NA_character_ ),
         CD_DEPT_POST = str_replace(str_trim(CD_DEPT_POST), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", NA_character_ ),
         loc = str_length(paste0(str_trim(LB_DEPT_NAME), str_trim(LB_DEPT_ADRS)))
  ) %>% 
  filter(str_trim(CD_DEPT_COUNTRY) != "", loc > 0) %>% 
  select(project_id, participant_pic, applicant_pic, participant_order, name_dept = LB_DEPT_NAME, 
         address_dept = LB_DEPT_ADRS, city_dept = LB_DEPT_CITY, post_code_dept = CD_DEPT_POST, 
         country_dept = CD_DEPT_COUNTRY, stage, lat_dept=CD_LATITUDE, lng_dept=CD_LONGITUDE)

x <- participant_add %>% 
  dplyr::count(project_id, participant_pic, applicant_pic, participant_order) %>% 
  filter(n>1)


APPL_DEPT <- 
  read.csv(paste0(chemin_pc, "traitement/", livraison,"/bd/proposal/proposal_applicants_dept.csv"),
            na="", encoding = "UTF-8") %>% 
  dplyr::mutate(stage = "proposal", project_id=as.character(CD_PROP_ID), participant_pic=as.character(CD_PART_PIC),
         participant_order=ORDER_NBR, applicant_pic=as.character(CD_APPL_PIC))

applicant_add <- APPL_DEPT %>% 
  dplyr::mutate(LB_DEPT_NAME = str_replace(str_trim(LB_DEPT_NAME), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", NA_character_ ),
         LB_DEPT_ADRS = str_replace(str_trim(LB_DEPT_ADRS), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", NA_character_ ),
         CD_DEPT_COUNTRY = str_replace(str_trim(CD_DEPT_COUNTRY), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", NA_character_ ),
         LB_DEPT_CITY = str_replace(str_trim(LB_DEPT_CITY), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", NA_character_  ),
         CD_DEPT_POST = str_replace(str_trim(CD_DEPT_POST), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", NA_character_ ),
         loc = str_length(paste0(str_trim(LB_DEPT_NAME), str_trim(LB_DEPT_ADRS)))) %>%
  filter(CD_DEPT_COUNTRY != "", loc > 0) %>% 
  select(stage, project_id, participant_pic, applicant_pic, participant_order, name_dept = LB_DEPT_NAME, 
         address_dept = LB_DEPT_ADRS, city_dept = LB_DEPT_CITY, post_code_dept = CD_DEPT_POST, 
         country_dept = CD_DEPT_COUNTRY, lat_dept=CD_LATITUDE, lng_dept=CD_LONGITUDE)

# applicant_add <- anti_join(applicant_add, participant_add, by=c("project_id", "participant_pic", "applicant_pic"))

participants_add <- bind_rows(participant_add, applicant_add) %>% unique() %>% 
  dplyr::mutate(name_dept = if_else(name_dept == "", NA_character_, name_dept),
         address_dept = if_else(address_dept == "", NA_character_, address_dept),
         city_dept = if_else(city_dept == "", NA_character_, city_dept),
         lat_dept=as.character(lat_dept), lng_dept=as.character(lng_dept)) %>% 
  dplyr::mutate_if(is.character, str_squish)


x <- participants_add %>% filter(country_dept=="FR") %>% 
  dplyr::count(stage, project_id, participant_pic, applicant_pic, participant_order) %>% 
  filter(n>1)


save("participants_add", file=paste0(data_result, "data/participants_departments.Rdata"))


rm(list=c("APPL_DEPT", "applicant_add", "PART_DEPT", "participant_add", "x", "participants_add"))
gc()

####################################################################################################################




