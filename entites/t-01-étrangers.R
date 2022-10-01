load(file=rdata_last_version(paste0(data_result, "data/"), "participant_ID_d02_"))

etr <- participant %>% 
  dplyr::filter(country_code!="FR") %>% 
  select(-nb_id, -dt_last_update, -acronym_projet, -cd_reg_id, -cd_reg_vat_id, -ordre_all) %>% 
  dplyr::mutate(subv_net=if_else(subv_net<0, 0, subv_net),
                status_orig=status, 
                status=if_else(stage=="project", "SIGNED", status)) %>% 
  group_by(stage, project_id, participant_pic, applicant_pic, participant_order, country_code) %>% 
  dplyr::mutate(n_part=n()) %>% 
  relocate(id, project_id, participant_order, organizations_id, subv, subv_net, subv_prop, country_code) %>% 
  ungroup() %>% unique()
  
prettyNum(sum(etr$subv, na.rm = T), big.mark = " ") 
prettyNum(sum(etr$subv_net, na.rm = T), big.mark = " ") 

# gps <- etr %>% 
#   select(country_code, country_name, lat_source, lng_source) %>% 
#   filter(!is.na(lat_source), !is.na(lng_source)) %>% 
#   dplyr::mutate(lt=str_extract(lat_source, "^.+(?=\\.)"), lg=str_extract(lng_source, "^.+(?=\\.)")) %>% 
#   select(-lat_source, -lng_source) %>% 
#   unique()
# 
# gps <- grid %>% 
#   select(country_code, lat, lng) %>% 
#   filter(!is.na(lat), !is.na(lng)) %>% 
#   dplyr::mutate(lt=str_extract(lat, "^.+(?=\\.)"), lg=str_extract(lng, "^.+(?=\\.)")) %>% 
#   select(-lat, -lng) %>% 
#   unique()


# grid <- load_one_obj(rdata=paste0(participants, "referentiels/grid_pic.Rdata"), "grid_ref") %>% 
#   select(gid=id, country_code, name, lat, lng) %>% 
#   filter(!is.na(lat)) %>% 
#   # unite(gps_grid, lat, lng, sep=",", na.rm=T, remove=T) %>% 
#   dplyr::mutate_if(is.character, list(~na_if(.,""))) %>% 
#   dplyr::mutate_all(., str_squish) %>% 
#   unique()

load(file = paste0(referentiel,"ror.RData"))
ror <- ror %>%
  select(id, name, country_code, latitude, longitude, matches("^geo_")) %>% 
  mutate(name=str_replace(name, "\\(.+\\)", ""))

etr2 <- left_join(etr, select(ror, -country_code), by=c("organizations_id"="id")) %>% 
  dplyr::mutate(diff=ifelse(is.na(name), 1,0),
    name=if_else(is.na(name), str_to_sentence(name_source), name)) %>% 
  select(-lat_source, -lng_source, -diff) 

# 1 000 378
proj <- etr2 %>% select(project_id, participant_order, participant_pic) %>% unique()

dept <- load_one_obj(rdata=paste0(data_result, "data/participants_departments.Rdata"), "participants_add") %>% 
  inner_join(proj) %>% 
  select(stage, project_id, participant_pic, applicant_pic, participant_order, country_dept, lat_dept, lng_dept) %>% 
  dplyr::filter(!is.na(lat_dept)) %>% 
  unique() %>% 
  group_by(stage, project_id, participant_pic, applicant_pic, participant_order) %>% 
  dplyr::mutate(n=n()) %>% 
  ungroup() %>% 
  dplyr::filter(n==1) %>% 
  unique()


etr2 <- left_join(etr2, dept) %>% 
  dplyr::mutate(gps = ifelse(is.na(gps), paste(lat_dept, lng_dept, sep=","), gps)) %>% 
  unique()


# dept <- inner_join(dept, temp,  by = c("stage", "project_id", "participant_pic", "applicant_pic", "participant_order")) %>% 
#   filter(!is.na(gps_source)) %>% 
#   select(stage, project_id, participant_pic, applicant_pic, participant_order,organizations_id, name_source, 
#          country_code, lat_dept, lng_dept) 
# 
# etr2 <- left_join(etr2, dept) %>% 
#   dplyr::mutate(gps_source=if_else(is.na(lat) & !is.na(lat_dept), "dept", gps_source),
#                 lat=if_else(gps_source=="dept", lat_dept, lat), 
#                 lng=if_else(gps_source=="dept", lng_dept, lng))



etr <- etr2 %>% 
  dplyr::mutate(organizations_id=if_else(is.na(organizations_id), paste0("NonIdent", participant_pic), organizations_id),
                adr=paste(str_squish(str_to_lower(address_source)), str_squish(str_to_lower(city_source))))
# 1 000 378

#########################################################################################
# Traitement d'une partie des etrangers sans geolocalisation
no_gps <- etr %>% 
  dplyr::filter((is.na(gps) | nchar(gps)<6)) %>% 
  select(stage, id, participant_order, participates_as, organizations_id, city_source, 
         country_code, country_name) %>% 
  dplyr::mutate(city=str_to_lower(city_source)) %>% 
  unique()

# gps_city <- read.csv(file=paste0(referentiel, "worldcities.csv"), sep=",", encoding = "UTF-8") %>% 
#   dplyr::mutate(city=str_to_lower(city_ascii)) %>% 
#   dplyr::mutate_all(., str_squish) %>% 
#   select(-id)
# 
# temp <- inner_join(no_gps, gps_city, by=c("country_code"="iso2", "city")) %>% 
#   select(stage, id, participant_order, participates_as, organizations_id, city_source, country_code, lt=lat, lg=lng) %>% 
#   unique() %>% 
#   group_by(city_source, country_code) %>% 
#   dplyr::mutate(n=n()) %>% 
#   dplyr::filter(n<2) %>% 
#   ungroup() %>% 
#   select(stage, id, participant_order, participates_as, organizations_id, country_code, lt, lg) %>% 
#   unite(gps_, lt, lg, sep=",", na.rm=T, remove=T) %>% 
#   dplyr::mutate_if(is.character, list(~na_if(.,""))) %>% 
#   unique()
####################################################################################

# etr <- left_join(etr, test, by=c("country_code", "city_source")) %>% 
# etr <- left_join(etr, temp, by=c("country_code", "stage", "id", "participant_order", "participates_as", "organizations_id")) %>%   
#   dplyr::mutate(gps=if_else(is.na(gps), gps_, gps),
#                 adr=paste(str_squish(str_to_lower(address_source)), str_squish(str_to_lower(city_source)))) %>% 
#   select(-gps_)
  
# etr <- etr %>% 
  # dplyr::mutate(lat=str_extract(lat, pattern_gps), lng=str_extract(lng, pattern_gps))
# gps <- etr %>% filter(!is.na(gps), !is.na(adr)) %>% 
#   select(adr, gps_=gps) %>% unique() %>% 
#   group_by(adr) %>%  dplyr::mutate(n=n()) %>% 
#   filter(n<2)
# 
# etr <- etr %>% 
#   left_join(gps, by="adr") %>% 
#   dplyr::mutate(gps=if_else(is.na(gps), gps_, gps)) %>% 
#   unique() %>% 
#   select(-n, -gps_, -adr)


save(etr, file=paste0(data_result, "data/participants_etrangers.RData"))

write.csv2(no_gps, file=paste0(identification, "etr_no_gps.csv"), row.names = F, na="", fileEncoding = "UTF-8")

rm(list=c("no_gps", "participant", "gps_city", "test", "etr", "etr2",  "dept", "temp", "ror"))
