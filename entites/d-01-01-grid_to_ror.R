participant <- load_one_obj(rdata=paste0(data_result, "data/participants_source.Rdata"), "participant")

grid <- load_one_obj(rdata=paste0(referentiel, "grid_pic.Rdata"), "grid_ref") %>%
  filter(!is.na(base)) %>% 
  dplyr::rename(id_etr=id, participant_pic=pic_id) %>% 
  group_by(participant_pic, id_etr) %>% 
  dplyr::mutate(nb_id=n()) %>% 
  relocate(nb_id)
# colnames(grid) <- stringr::str_to_lower(colnames(grid))

temp <- participant %>% filter(country_code !="FR") %>% 
  # select(participant_pic, name_source, website, city_source, num_projet, stage) %>% 
  # unique() %>% 
  left_join(grid, by = c("participant_pic", "country_code")) %>% 
  select(participant_pic, id_etr, name_source, website, address_source, post_code_source, city_source, country_name, country_code, project_id, stage) %>% 
  group_by(participant_pic, stage) %>% 
  dplyr::mutate(nb = n_distinct(project_id)) %>% 
  ungroup() %>% 
  select(-project_id) %>% unique() %>% 
  group_by(participant_pic) %>% 
  dplyr::mutate(n_pic=n_distinct(participant_pic)) %>% 
  ungroup() %>% 
  unique()

temp <- temp %>% 
  dplyr::mutate(across(c(name_source, address_source, city_source), str_to_lower)) %>% 
  dplyr::mutate(across(c(name_source, address_source, city_source), clean_exo_char))

##########################################################
pp <- temp %>% select(participant_pic, country_code, stage, nb) %>% unique() #147 381
pid <- temp %>% select(participant_pic, id_etr, country_code) %>% 
  filter(!is.na(id_etr)) %>% unique() %>% 
  group_by(participant_pic, country_code) %>% 
  dplyr::mutate(n_id=n()) %>% ungroup()
pp <- merge(pp, pid, all.x = TRUE)


pivot <- pivot_wider(pp, id_cols=c('participant_pic', 'id_etr', 'country_code'), 
                     names_from=stage, values_from = nb, values_fn = sum) 


# liste des grid ROR

ror <- read.csv2(file = "C:/Users/zfriant/OneDrive/referentiels/GRID_TO_ROR_2021-12-03T16-14.csv")
ror <- ror %>% 
  dplyr::mutate(country_code=if_else(country.country_code=="GR", "EL", country.country_code),
                country_code=if_else(country.country_code=="GB", "UK", country_code),
                id = str_split(id, '(/)+'),
                id = unlist(map(id, dplyr::last(map(id, length))))) %>% 
  select(grid_old, id, country_code) %>% 
  unique()
 

pivot <- pivot %>% left_join(ror, by = c('id_etr'='grid_old', "country_code"))


#utilisation des grid matché
grid_ror <- read.csv(file = "C:/Users/zfriant/OneDrive/referentiels/ORGA_GRID_MATCH_ROR_2021-12-03T16-27.csv", colClasses = 'character') %>% 
  dplyr::mutate(country_code=if_else(country_code=="GR", "EL", country_code),
                country_code=if_else(country_code=="GB", "UK", country_code))

gr <- grid_ror %>% select(participant_pic, country_code, grid_old, id) %>% 
  dplyr::mutate(id = str_split(id, '(/)+'),
                id = unlist(map(id, dplyr::last(map(id, length))))) %>% 
  unique()

piv <- pivot %>% filter(is.na(id)) %>% 
  select(-id) %>% 
  left_join(select(gr, c(participant_pic, id, country_code)), by= c('participant_pic', 'country_code')) %>% 
  filter(!is.na(id)) %>% 
  select(participant_pic, country_code, idt=id) %>% 
  unique() %>% 
  group_by(participant_pic) %>% 
  dplyr::mutate(n=n()) %>% 
  filter(n==1) %>% 
  select(-n)
  
x <- piv %>% group_by(participant_pic) %>% dplyr::count()


pivot <- pivot %>% left_join(piv, by= c('participant_pic', 'country_code')) %>% 
  dplyr::mutate(id=ifelse(is.na(id), idt, id)) %>% 
  select(-idt)


test <- pivot %>% filter(is.na(id))


# liste des grid à roriser
grid_ror <- read.csv2(file = "C:/Users/zfriant/OneDrive/referentiels/GRID_TO_ROR_2021-12-06T22-29.csv", colClasses = 'character') 

gr <- grid_ror %>%  
  dplyr::mutate(country_code=if_else(country.country_code=="GR", "EL", country.country_code),
                country_code=if_else(country.country_code=="GB", "UK", country_code),
                id = str_split(id, '(/)+'),
                idt = unlist(map(id, dplyr::last(map(id, length))))) %>% 
  select(grid_old, country_code, idt) %>%
  unique()

pivot <- pivot %>% left_join(gr, by= c('id_etr'='grid_old', 'country_code')) %>% 
  dplyr::mutate(id=ifelse(is.na(id), idt, id)) %>% 
  select(-idt)

test <- pivot %>% filter(is.na(id))

grid <- test %>%  filter(!is.na(id_etr)) %>% 
  select(id_etr) %>% 
  unique()
write.csv2(grid, file=paste0("C:/Users/zfriant/OneDrive/referentiels/grid_to_ror_", date_sauve, ".csv"), row.names = FALSE)

grid_ror <- read.csv2(file = "C:/Users/zfriant/OneDrive/referentiels/GRID_TO_ROR_2021-12-07T17-21.csv", colClasses = 'character') 
gr <- grid_ror %>%  
  dplyr::mutate(country_code=if_else(country.country_code=="GR", "EL", country.country_code),
                country_code=if_else(country.country_code=="GB", "UK", country_code),
                id = str_split(id, '(/)+'),
                idt = unlist(map(id, dplyr::last(map(id, length))))) %>% 
  select(grid_old, country_code, idt) %>%
  unique()

pivot <- pivot %>% left_join(gr, by= c('id_etr'='grid_old', 'country_code')) %>% 
  dplyr::mutate(id=ifelse(is.na(id), idt, id)) %>% 
  select(-idt)

x <- pivot %>% filter(is.na(id), !is.na(id_etr)) %>% select(participant_pic) %>% unique()

pivot <- pivot %>% mutate(id_etr = ifelse(participant_pic %in% x$participant_pic, NA_character_, id_etr))

# recuperer infos dans table links de ecorda
test <- pivot %>% filter(is.na(id_etr), is.na(id)) %>% 
  select(-id, -id_etr)

link_h <-
  read.csv(file= paste0("C:/Users/zfriant/OneDrive/PCRI/traitement/",livraison,"/bd/project/legal_entities_links.csv")) %>%
  dplyr::mutate(participant_pic = as.character(CD_PART_PIC)) %>% unique() %>%
  # filter(LB_DATASET == "GRID") %>%
  filter(LB_DATASET %in% c("ROR", "GRID")) %>%
  group_by(participant_pic) %>%
  dplyr::mutate(nb=n_distinct(CD_LINK_ID), source=paste(LB_DATASET, collapse = ","), id_multi = paste(CD_LINK_ID, collapse = ",")) %>%
  ungroup() %>%
  select(participant_pic, source, id_multi, nb) %>%
  unique()

piv <- test %>% inner_join(link_h, by = "participant_pic") %>%
  mutate(id_etr = if_else(nb==1, id_multi, NA_character_)) %>%
  unique()

#isoler les participant_pic avec id ror pour les double ID GRID/ROR
piv2 <- piv %>% filter(nb>1) %>% 
  separate_rows(source, id_multi, sep=',', ) %>%
  filter(source=='GRID') %>%
  group_by(participant_pic) %>%
  dplyr::mutate(nb=n()) %>%
  filter(nb==1) %>% 
  dplyr::rename(idt=id_multi) %>% 
  select(participant_pic, country_code, idt)
#
piv <- piv %>% left_join(piv2, by = c("participant_pic", "country_code")) %>%
  mutate(id_etr=ifelse(is.na(id_etr), idt, id_etr)) %>% 
  filter(!is.na(id_etr)) %>% 
  select(-idt) %>% 
  unique()
piv %>% select(id_etr) %>% unique() %>% 
write.csv2(piv, file=paste0("C:/Users/zfriant/OneDrive/referentiels/grid_to_ror_", date_sauve, ".csv"), row.names = FALSE)

grid_ror <- read.csv2(file = "C:/Users/zfriant/OneDrive/referentiels/GRID_TO_ROR_2021-12-07T17-41.csv", colClasses = 'character') 
gr <- grid_ror %>%  
  dplyr::mutate(country_code=if_else(country.country_code=="GR", "EL", country.country_code),
                country_code=if_else(country.country_code=="GB", "UK", country_code),
                id = str_split(id, '(/)+'),
                idt = unlist(map(id, dplyr::last(map(id, length))))) %>% 
  select(grid_old, country_code, idt) %>%
  unique()
piv <- piv %>% left_join(gr, by = c("id_etr"="grid_old", "country_code"))

pivot <- piv %>% select(participant_pic, country_code, idt) %>% unique() %>% 
  right_join(pivot, by = c("participant_pic", 'country_code'))
pivot <- pivot %>% mutate(id=ifelse(is.na(id), idt, id)) %>% 
  select(-idt)

x <- pivot %>% filter(is.na(id)) %>%  select(participant_pic, country_code) %>% 
  unique() %>%
  left_join(select(temp, c(participant_pic, name_source, address_source, city_source, country_name, country_code )), )
write.csv2(x, file=paste0("C:/Users/zfriant/OneDrive/referentiels/match_grid_to_ror_", date_sauve, ".csv"), row.names = FALSE)


#retour match grid




############################################################################

id_etr <- pivot %>% select(participant_pic, country_code, id, proposal, project) %>% 
  mutate_if(is.numeric, replace_na, 0)

ID_etranger <- id_etr %>% 
  left_join(select(temp, c(-stage, -nb, -n_pic, -id_etr, -address_source, -post_code_source)), 
            by = c('participant_pic', 'country_code')) %>% 
  mutate(id = ifelse(!is.na(id), paste0('R',id), id)) %>% 
  unique()



ID_new <- participant %>%
  select(participant_pic, name_source, website, address_source, post_code_source, city_source, country_name, country_code, project_id, stage) %>% 
  dplyr::mutate(across(c(name_source, address_source, post_code_source, city_source), str_to_lower)) %>% 
  dplyr::mutate(across(c(name_source, address_source,  post_code_source, city_source), clean_exo_char)) %>% 
  group_by(participant_pic, stage) %>% 
  dplyr::mutate(nb = n_distinct(project_id)) %>% 
  ungroup() %>% 
  select(-project_id) %>% unique() %>% 
  group_by(participant_pic) %>% 
  dplyr::mutate(n_pic=n_distinct(participant_pic)) %>% 
  ungroup() %>% 
  unique()

ID_new_piv <- pivot_wider(ID_new, id_cols=c('participant_pic', 'country_code'), 
                       names_from=stage, values_from = nb, values_fn = sum) 
  
ID_new_piv <- ID_new %>% select(-stage, -n_pic, -nb) %>% 
    left_join(ID_new_piv, by = c('participant_pic', 'country_code')) %>% 
  unique()

filename="id_pic_2021_05_27.txt"
pic_id_fr <-
  read.csv2(file = paste0(chemin_pc, "participants/identification/corrected/", filename), colClasses = "character", na.strings = "") %>% 
  unique() %>% 
  filter(!is.na(source_id)) %>% 
  select(participant_pic, source_id) %>% 
  map_dfc(., remove_space)


pic_id_fr <- pic_id_fr %>% mutate(country_code="FR") %>% 
  group_by(participant_pic) %>% 
  dplyr::mutate(n_id=n(), 
                source_id = ifelse(str_detect(source_id, pattern = pattern_ror), paste0('R',source_id), source_id)) %>% 
  ungroup()

ID_new_piv <- pic_id_fr %>% select(-n_id) %>% 
  right_join(ID_new_piv, by = c('participant_pic', 'country_code'))
  
ID_new_piv <- ID_etranger %>% select(participant_pic, country_code, id) %>% 
  unique() %>% 
  right_join(ID_new_piv, by = c('participant_pic', 'country_code')) %>% 
  mutate(id=ifelse(country_code=="FR", source_id, id)) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  select(-source_id) %>% 
  unique()
write.csv2(ID_new_piv, file=paste0(identification, "id_complete_2021_12_08.csv"), na="",
             row.names = FALSE, fileEncoding = 'utf-8')
