load(file = rdata_last_version(paste0(data_result, "data/"),"participant_org_e01_"))
load(file=paste0(data_result, "data/participants_departments.Rdata"))



nrow(participants_add %>% dplyr::filter(country_dept=="FR") %>% 
  dplyr::count(project_id, participant_pic, applicant_pic)) #69 671

# indicateurs base
nrow(participant %>% dplyr::filter(country_code=="FR") %>% 
       select(id, participant_order, participates_as) %>% 
       unique()) #87 901

part <- participant %>% dplyr::filter(country_code=="FR") %>% 
  left_join(participants_add, all.x=TRUE) %>% 
  unique()

nrow(part %>% dplyr::filter(country_code=="FR") %>% 
       select(id, participant_order, participates_as) %>% 
       unique()) #87 901

# part_etr <- part %>% filter(country_code != "FR") %>% 
#   save(file=paste0(data_result, "data/participant_etr5_", date_sauve, ".Rdata"))

rm(list=c("participant","participants_add"))
gc()
# doublons <- 
# participants[duplicated(participants$NUM_PROJET,participants$PIC_ID, participants$PIC_ID2, participants$STAGE, participants$ORDRE_ALL,),]

part_fr <- part %>% 
  select(-statut_part, -website, -cout_tot_part, -dt_last_update, -ordre_all, 
        -country_level_1, -country_level_2, -participant_type_code, -participant_type_name,
        -acronym_projet, -prop, -proj, -pme) %>% 
  relocate(country_code, lat_source, lng_source, name_source, acronym_source, project_id, 
           participant_pic, applicant_pic, subv_prop, subv, subv_net, role, 
           programme_abbr, cd_reg_vat_id, cd_reg_id, nb_id,  .after = last_col())


part_fr <- part_fr %>% 
  dplyr::filter(country_code=="FR") %>% 
  dplyr::mutate(country_dept = if_else(is.na(country_dept), country_code, country_dept),
         # code_postal
         pc_net = tolower_noblank(post_code_dept),
         pcs_net = tolower_noblank(post_code_source),
         pc_net = clean_post_code(pc_net),
         pcs_net = clean_post_code(pcs_net),
         pc_net = if_else(is.na(pc_net)|pc_net =='', pcs_net, pc_net),
         pc_net = if_else(is.na(pc_net) & str_detect(post_code_source, "(\\d{5})")==TRUE, post_code_source, pc_net),
         #  # city
         city_net = tolower_noblank(city_dept), city_source_net = tolower_noblank(city_source),
         city_net = replace_special_letters(city_net), city_source_net = replace_special_letters(city_source_net),
         city_net = clean_exo_char(city_net), city_source_net = clean_exo_char(city_source_net),
         city_net = clean_city(city_net), city_source_net = clean_city(city_source_net),
         city_net = if_else(city_net == "NA"|is.na(city_net)|city_net =='', city_source_net, city_net),
         city_net = if_else(substr(pc_net,1,2)=="75", "paris", city_net),
         city_tag = str_replace_all(str_squish(city_net), " ", "-"))
         # # gps
         # lat = if_else(!is.na(lat_dept), lat_dept, lat_source),
         # lng = if_else(!is.na(lng_dept), lng_dept, lng_source))

part_fr <- part_fr %>% 
  dplyr::mutate(         # #  # address
         address_2 = tolower_noblank(address_dept), address_source_2 = tolower_noblank(address_source),
         address_2 = replace_special_letters(address_2), 
         address_source_2 = replace_special_letters(address_source_2),
         address_2 = clean_exo_char(address_2), address_source_2 = clean_exo_char(address_source_2),
         address_2 = if_else(!is.na(address_2),word_to_replace(address_2), address_2),
         address_source_2 = word_to_replace(address_source_2),
         address_2 = word_stop_word(address_2), address_source_2 = word_stop_word(address_source_2),
         address_net = clean_address(address_2), address_source_net = clean_address(address_source_2),
         address_net = finition(address_net), address_source_net = finition(address_source_net),
         address_net = word_to_delete(address_net, word_del), address_source_net = word_to_delete(address_source_net, word_del),
         address_net = if_else((is.na(address_net)|address_net=="") & pc_net==pcs_net, address_source_net, address_net),
         address_net = if_else((is.na(address_net)|address_net=="") & city_net==city_source_net, address_source_net, address_net),
         address_tag = word_to_delete(address_net, word_del_tag), address_tag = str_squish(address_tag),
         # #  # name
         entite = if_else(is.na(acronym_source)|str_detect(name_source, acronym_source)==TRUE, name_source, str_c(name_source, acronym_source, sep=" ")),
         name_2 = tolower_noblank(name_dept), entite = tolower_noblank(entite),
         name_2 = replace_special_letters(name_2), entite = replace_special_letters(entite),
         name_2 = remove_point(name_2), entite = remove_point(entite),
         name_2 = clean_exo_char(name_2), entite = clean_exo_char(entite),
         name_2 = word_to_replace(name_2), entite = word_to_replace(entite),
         name_2 = word_stop_word(name_2), entite = word_stop_word(entite),
         name_2 = if_else((is.na(name_2)|name_2 == ""), entite, name_2)) %>%
  select(-address_2,-address_source_2)

x <- part_fr %>% dplyr::count(id, stage, participant_order, participates_as) %>% 
  dplyr::filter(n>1)


# numérotation des lignes ; 
part_fr <- part_fr %>% 
  arrange(id, stage, participant_order) %>% 
  dplyr::mutate(lid = row_number(), lid = as.character(lid))


# regroupement par famille de keymaster entite+address_tag+city_tag
temp <- part_fr %>% 
  select(lid,entite,address_tag,city_tag) %>% 
  dplyr::mutate(keymaster = paste(entite,address_tag,city_tag), 
         keymaster=str_replace_all(keymaster, "\\s", "")) %>% 
  group_by(keymaster) %>% 
  dplyr::mutate(famille_nb = n(), famille = cur_group_id()) %>% 
  ungroup() %>% 
  select(-entite,-address_tag,-city_tag)

part_fr <- left_join(part_fr, temp, by = "lid")

part_fr <- part_fr %>% relocate(lid, organizations_id, org, org_id, name_2, entite)

save("part_fr", file=paste0(data_result, "data/part_fr_f01_", date_sauve, ".Rdata"))

rm(list=c("x", "temp", "part"))
gc()
# temp <- part_fr %>% select(address_dept, address_2, address_2) %>% 
#   unique() %>% dplyr::dplyr::mutate(ad=address_2) %>% 
#   unnest_tokens(word, ad) %>% 
#   dplyr::mutate(test=word_to_replace(word), test2=word_stop_word(test))
# x <- temp %>% count(test2, sort = TRUE) 
# 
# temp <- temp %>% dplyr::mutate(test2=word_stop_word(test))