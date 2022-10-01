load(file=rdata_last_version(paste0(data_result, "data/"), "prop_proj_"))

# liste de prénoms genrés
perso_gender <- gender_datas()

#chargement tables brutes personnes
prop <- read.csv(paste0(data_result, "perso/proposal_contact_pers.csv"), na="",
                   sep=",", header = TRUE, colClasses = "character", encoding = "UTF-8") %>% 
  mutate(stage="proposal", actu="2021_11") %>% 
  dplyr::rename(project_id=CD_PROP_ID, ROLE=CD_ROLE, participant_pic=CD_PART_PIC, applicant_pic=CD_APPL_PIC)

cont <- 
  read.csv(paste0(data_result, "perso/project_pcoco.csv"), header = TRUE, sep=',',
            colClasses = "character", encoding = "UTF-8", na="") %>% 
  mutate(stage="project", actu="2021_11") %>% 
  dplyr::rename(project_id=CD_PROJ_ID, participant_pic=CD_PART_PIC, applicant_pic=CD_APPL_PIC,
                LAST_NAME=FAMILY_NAME)

# création table perso
perso <- bind_rows(cont, prop)

perso_contact <- perso %>% 
  mutate(LAST_NAME = str_to_lower(LAST_NAME),
         last_name2 = replace_special_letters(LAST_NAME), 
         last_name2 = clean_exo_char(last_name2), 
         FIRST_NAME = str_to_lower(FIRST_NAME),
         first_name2 = replace_special_letters(FIRST_NAME), 
         first_name2 = clean_exo_char(first_name2), 
         TITLE = str_to_lower(TITLE),
         title_name2 = replace_special_letters(TITLE),
         title_name2 = clean_exo_char(title_name2),
         role = tolower_noblank(ROLE), 
         email = tolower_noblank(EMAIL),
         dom_email = email_domain(email),
         # tel_clean = tel_clean(CD_PHONE),
         last_name = str_replace_all(last_name2, "[[:punct:]]", " "),
         first_name = str_replace_all(first_name2, "[[:punct:]]", " "),
         title_name = str_replace_all(title_name2, "[[:punct:]]", " "),
         contact = paste(last_name, first_name, sep = " ")) %>%
  mutate_at(c('ORCID_ID', 'GOOGLE_SCHOLAR_ID', 'SCOPUS_AUTH_ID', 'RESEARCHER_ID'), funs(str_replace(., "NA", NA_character_))) %>%
  select(-last_name2, -first_name2, -LAST_NAME, -FIRST_NAME, -TITLE, -title_name2, -ROLE, -EMAIL, -email, -CD_PHONE) %>%
  dplyr::filter(last_name!="", last_name!="deleted", !is.na(last_name)) %>% 
  unique() %>% 
  mutate_all(~str_squish(.)) %>% 
  rename_all(tolower) %>% 
  relocate(contact, title_name, gender, participant_pic, applicant_pic, project_id, stage) %>% 
  left_join(select(prop_proj, c(project_id, stage, status)), by=c("project_id", "stage"))
  
  
###################################################

perso_contact <- left_join(perso_contact, perso_gender, by = "first_name") %>% 
  dplyr::mutate(gender=if_else(is.na(gender)|gender=="Missing", gender_temp, gender)) %>% 
  select(-gender_temp)


perso_contact <- perso_contact %>% 
  dplyr::group_by(project_id, participant_pic, applicant_pic, contact) %>% 
  fill(title_name, .direction = "updown") %>% 
  fill(gender, .direction = "updown") %>%
  fill(orcid_id, .direction = "updown") %>% 
  ungroup()


# pour identification api gender
miss <- perso_contact %>% 
  dplyr::filter(is.na(gender) | gender=="Missing") %>% 
  select(first_name, status) %>% unique()

if (nrow(miss)>0){
  write.csv(miss, paste0(identification, "gender_", date_sauve, ".csv"), 
            row.names = F, fileEncoding = "utf8")
}

# après traitement recharger perso_gender
perso_gender <- gender_datas()

perso_contact <- left_join(perso_contact, perso_gender, by = "first_name") %>% 
  dplyr::mutate(gender=if_else(is.na(gender)|gender=="Missing", gender_temp, gender)) %>% 
  select(-gender_temp)


save("perso_contact", file=paste0(data_result, "data/perso_contact.RData"))

rm(list=c("cont", "prop", "perso", "miss"))
gc()

############################################################################
#'''msca'''

prop_msca <- 
  read.csv(paste0(data_result, "perso/proposal_msca_res.csv"), encoding = "UTF-8",
           colClasses = "character", na.strings="") %>% 
  dplyr::mutate(stage="proposal") %>% 
  dplyr::rename(project_id=CD_PROP_ID) %>% 
  mutate_if(is.character, str_squish)

proj_msca <- 
  read.csv(paste0(data_result, "perso/project_msca_res.csv"), na="", encoding = "UTF-8",
           colClasses = "character") %>% 
  dplyr::mutate(stage="project") %>% 
  dplyr::rename(project_id=CD_PROJ_ID) %>% 
  mutate_if(is.character, str_squish)

perso <- bind_rows(prop_msca, proj_msca)


perso_msca <- perso %>% 
  mutate(LAST_NAME = str_to_lower(LAST_NAME),
         last_name2 = replace_special_letters(LAST_NAME),
         last_name2 = clean_exo_char(last_name2), 
         FIRST_NAME = str_to_lower(FIRST_NAME),
         first_name2 = replace_special_letters(FIRST_NAME), 
         first_name2 = clean_exo_char(first_name2), 
         TITLE = str_to_lower(TITLE),
         title_name2 = replace_special_letters(TITLE),
         title_name2 = clean_exo_char(title_name2),
         title_name = str_replace_all(title_name2, "[[:punct:]]", " "),
         last_name = str_replace_all(last_name2, "[[:punct:]]", " "),
         first_name = str_replace_all(first_name2, "[[:punct:]]", " "),
         contact = paste(last_name, first_name, sep = " ")) %>%
  mutate_at(c('ORCID_ID', 'GOOGLE_SCHOLAR_ID', 'SCOPUS_AUTH_ID', 'RESEARCHER_ID'), funs(str_replace(., "NA", NA_character_))) %>% 
  mutate_if(is.character, str_squish) %>% 
  dplyr::filter(!is.na(last_name), last_name!="deleted") %>% unique() %>% 
  select(-first_name2, -last_name2, -LAST_NAME, -FIRST_NAME, -title_name2, -TITLE,
         -CD_PHONE, -EMAIL, -CD_FAX) %>% 
  mutate_all(~str_squish(.), ~stri_trans_general(.)) %>% 
  rename_all(tolower) %>% 
  relocate(contact, title_name, gender, project_id, stage) %>% 
  left_join(select(prop_proj, c(project_id, stage, status)), by=c("project_id", "stage"))

####################
# traitement genre
perso_msca <- left_join(perso_msca, perso_gender, by = "first_name") %>% 
  dplyr::mutate(gender=if_else(is.na(gender)|gender=="Missing", gender_temp, gender)) %>% 
  select(-gender_temp)

perso_msca <- perso_msca %>% 
  dplyr::group_by(project_id, contact) %>% 
  fill(title_name, gender, orcid_id, google_scholar_id, scopus_auth_id, researcher_id,
       cd_main_nationality, cd_origin_country, cd_residence_country,
       .direction = "updown") %>% 
  ungroup()

###############
#'''traitement gender manquant'''

miss <- perso_msca %>% 
  dplyr::filter((gender == "Missing") | (is.na(gender)), !is.na(status))
if (nrow(miss)>0){
  write.csv(miss, paste0(identification, "gender_", date_sauve, ".csv"), 
            row.names = F, fileEncoding = "utf8")
}

# retour de la maj des genres dans perso_gender
perso_gender <- gender_datas()

perso_msca <- left_join(perso_msca, perso_gender, by = "first_name") %>% 
  dplyr::mutate(gender=if_else(gender=='Missing', gender_temp, gender)) %>% 
  select(-gender_temp)


save(perso_msca, file=paste0(data_result, "data/perso_msca.RData"))

rm(list=c("proj_msca", "prop_msca", "miss"))
gc()
###########################################################################################

#'''ERC'''

prop_erc <- 
  read.csv(paste0(data_result, "perso/proposal_erc_pi.csv"), na="", encoding = "UTF-8",
           colClasses = "character") %>% 
  dplyr::mutate(stage="proposal") %>% 
  dplyr::rename(project_id=CD_PROP_ID, participant_pic=CD_HOST_PART_PIC, applicant_pic=CD_HOST_APPL_PIC) %>% 
  mutate_if(is.character, str_squish)

proj_erc <- 
  read.csv(paste0(data_result, "perso/project_erc_pi.csv"), na="", encoding = "UTF-8",
           colClasses = "character") %>% 
  dplyr::mutate(stage="project") %>% 
  dplyr::rename(project_id=CD_PROJ_ID, participant_pic=CD_HOST_PART_PIC, applicant_pic=CD_HOST_APPL_PIC) %>% 
  mutate_if(is.character, str_squish)

perso <- bind_rows(prop_erc, proj_erc)


perso_erc <- perso %>% 
  mutate(LAST_NAME = str_to_lower(LAST_NAME),
         last_name2 = replace_special_letters(LAST_NAME),
         last_name2 = clean_exo_char(last_name2), 
         FIRST_NAME = str_to_lower(FIRST_NAME),
         first_name2 = replace_special_letters(FIRST_NAME), 
         first_name2 = clean_exo_char(first_name2), 
         TITLE = str_to_lower(TITLE),
         title_name2 = replace_special_letters(TITLE),
         title_name2 = clean_exo_char(title_name2),
         title_name = str_replace_all(title_name2, "[[:punct:]]", " "),
         last_name = str_replace_all(last_name2, "[[:punct:]]", " "),
         first_name = str_replace_all(first_name2, "[[:punct:]]", " "),
         #contact = paste(last_name, first_name, sep = " ")
         ) %>%
  mutate_if(is.character, str_squish) %>% 
  dplyr::filter(!is.na(last_name), last_name!="deleted") %>% 
  unique() %>% 
  select(-first_name2, -last_name2, -LAST_NAME, -FIRST_NAME, -title_name2, -TITLE,
         -CD_PHONE, -EMAIL, -CD_FAX) %>% 
  mutate_all(~str_squish(.), ~stri_trans_general(.)) %>% 
  rename_all(tolower) %>% 
  inner_join(select(prop_proj, c(project_id, stage, status, action2_id)), by=c("project_id", "stage")) %>% 
  relocate(stage, last_name, first_name, title_name, gender, project_id, status)


perso_erc <- perso_erc %>% 
  mutate(across(.cols=c(researcher_id, orcid_id, scopus_auth_id, google_scholar_id),
                .fns=~str_replace(., "^NA$", NA_character_)),
         across(.cols=c(cd_main_nationality, cd_origin_country, cd_residence_country), 
                .fns=~str_replace(., "^GB$", "UK"))) %>% 
  unique() %>% 
  group_by(stage, project_id) %>% 
  mutate(n=n_distinct(last_name, first_name)) %>% 
  ungroup()


tab <- perso_erc %>% select(action2_id,n) %>% unique()

# traitement des noms + prenoms
test <- perso_erc %>% 
  select(last_name, first_name, stage, project_id, participant_pic, applicant_pic, action2_id, orcid_id) %>% 
  unique() %>% 
  group_by(project_id, action2_id) %>% 
  mutate(n_ligne=n(),
    n_stage=n_distinct(stage),
    n_last=n_distinct(last_name)) %>% 
  group_by(action2_id, project_id, last_name) %>% 
  mutate(n_first=n_distinct(first_name)) %>% 
  ungroup()


temp1 <- test %>% 
  dplyr::filter(n_ligne==1)

test <- anti_join(test, temp1)

temp1 <- test %>% 
  dplyr::filter(n_ligne==2, n_stage==2, n_last==1, n_first==1) %>% 
  bind_rows(temp1)

test <- anti_join(test, temp1)

# si un seul nom avec ++ prenoms
temp2 <- test %>% 
  dplyr::filter(action2_id != "ERC-SyG", n_stage==2, n_last<=2) %>%
  group_by(project_id) %>% 
  mutate(last_name_proposal=ifelse(stage=="proposal", last_name, NA_character_),
         first_name_proposal=ifelse(stage=="proposal", first_name, NA_character_),
         contact=ifelse(stage=="proposal", paste(last_name, first_name, sep=" "), NA_character_)) %>% 
  fill(contact, last_name_proposal, first_name_proposal, .direction = "updown") %>% 
  mutate(contact_signe=ifelse(stage=="project", paste(last_name, first_name, sep=" "), NA_character_),
         contact_signe=ifelse(contact_signe==contact, NA_character_, contact_signe)) %>% 
  fill(contact_signe, .direction = "updown") %>% 
  ungroup() %>% 
  select(-last_name, -first_name) %>% 
  rename(last_name=last_name_proposal, first_name=first_name_proposal) %>% 
  unique()

temp1 <- bind_rows(temp1, temp2)

test <- anti_join(test, temp1, by="project_id")

# si 2 stage avec 1 prénom et ++ noms et non SYG
temp2 <- test %>% 
  dplyr::filter(action2_id != "ERC-SyG")


if (nrow(temp2)==0){
  
  temp1 <- bind_rows(temp1, test) %>% 
    select(-n_ligne, -n_stage, -n_last, -n_first) %>% 
    unique()
}


temp1 <- temp1 %>% 
  mutate(contact=ifelse(is.na(contact), paste(last_name, first_name, sep=" "), contact)) %>% 
  group_by(stage, project_id) %>% 
  mutate(n_ligne=n())

test <- temp1 %>% dplyr::filter(action2_id != "ERC-SyG")
  
### traitement nationalité ###
temp2 <- perso_erc %>% 
  select(stage, project_id, last_name, first_name, cd_main_nationality) %>% 
  unique() %>% 
  group_by(project_id, last_name, first_name) %>% 
  fill(cd_main_nationality, .direction = "updown") %>% 
  unique() %>% 
  group_by(project_id, last_name, first_name) %>% 
  mutate(n=n_distinct(cd_main_nationality))

temp2 <- temp2 %>% 
  select(project_id, last_name, first_name, cd_main_nationality) %>% 
  group_by(project_id, last_name, first_name) %>% 
  mutate(nationality = str_c(unique(cd_main_nationality), collapse = ",")) %>% 
  select(-cd_main_nationality) %>% 
  unique()


temp1 <- left_join(temp1, temp2)# %>% 
#   right_join(temp1, by=c("stage", "project_id", "last_name", "first_name"))


### traitement genre ###

temp2 <- left_join(temp1, perso_erc) %>% 
  select(stage, project_id, last_name, first_name, gender) %>% 
  unique() %>% 
  group_by(project_id, last_name, first_name) %>% 
  fill(gender, .direction = "updown") %>% 
  unique()


if (nrow(temp2 %>% group_by(project_id, last_name, first_name) %>% 
         mutate(n=n_distinct(gender)) %>% dplyr::filter(n>1))!=0){
  
  t <- temp2 %>% 
    group_by(project_id, last_name, first_name) %>% 
    mutate(n=n_distinct(gender)) %>%
    dplyr::filter(n>1, stage=="project") %>% 
    select(project_id, last_name, first_name, g=gender)
  
  if (nrow(t)>0){
    temp2 <- temp2 %>% left_join(t, by = c("project_id", "last_name", "first_name")) %>%
      mutate(gender=ifelse(!is.na(g), g, gender)) %>% 
      select(-g)
  }
    
}else{print("ok")}



test <- temp2 %>% dplyr::filter(is.na(gender))
unique(temp2$gender)


perso_erc <- left_join(temp1, temp2, by=c("stage", "project_id", "last_name", "first_name")) %>% 
  ungroup() %>% 
  unique()

###
#orcid

perso_erc <- perso_erc %>% 
  group_by(contact) %>% 
  fill(orcid_id, .direction = "updown") %>% 
  ungroup() %>% 
  unique() 



# perso_erc <- left_join(perso_erc, perso_gender, by = "first_name") %>% 
#   dplyr::mutate(gender=if_else(is.na(gender)|gender=="Missing", gender_temp, gender)) %>% 
#   select(-gender_temp)
# 
# perso_erc <- perso_erc %>% 
#   dplyr::group_by(project_id, contact) %>% 
#   fill(title_name, gender, orcid_id, cd_main_nationality, cd_origin_country, cd_residence_country,
#        .direction = "updown") %>%
#   ungroup()

###################################
# miss <- perso_erc %>% filter((gender == "Missing") | (is.na(gender)), !is.na(status))
# if (nrow(miss)>0){
#   write.csv(miss, paste0(identification, "gender_", date_sauve, ".csv"), 
#             row.names = F, fileEncoding = "utf8")
# }
# 
# # retour de la maj des genres dans perso_gender
# perso_gender <- gender_datas()
# 
# perso_erc <- left_join(perso_erc, perso_gender, by = "first_name") %>% 
#   dplyr::mutate(gender=if_else(gender=='Missing', gender_temp, gender)) %>% 
#   select(-gender_temp)


save(perso_erc, file=paste0(data_result, "data/perso_erc.RData"))

rm(list=c("proj_erc", "prop_erc", "miss", "perso"))
gc()
