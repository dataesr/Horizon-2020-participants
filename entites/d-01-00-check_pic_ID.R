participant <- load_one_obj(rdata=paste0(data_result, "data/participants_source.Rdata"), "participant")
################################
#pour ETAPE de VERIFICATION et NETTOYAGE -> aller plus bas si c'est déjà fait

#import de la dernière table id_pour_api dont les correspondances pic -> id ont été vérifiés
filename="_id_pic_2022_06.csv"
pic_id_orig <-
  read.csv2(file = paste0(chemin, "participants/identification/corrected/", filename), 
            colClasses = "character", na.strings = "", encoding = "UTF-8") %>% 
  mutate(id=ifelse(!is.na(fix_id), fix_id, id), source=NA_character_) %>% 
  map_dfc(., str_squish) 

n_distinct(pic_id_orig$participant_pic)

pic_id <- pic_id_orig %>% 
  select(participant_pic, country_name, country_code, id, error=error_2021_11, comments) %>% 
  group_by(participant_pic, country_name, country_code) %>% 
  mutate(across(.cols = c(id, error, comments), .fns = ~str_c(na.omit(unique(.)), collapse="#"))) %>% 
  ungroup() %>% 
  unique()

pic_id[pic_id == "" | pic_id == " "] <- NA

supp_id_from_verif <- pic_id %>% dplyr::filter(nchar(error)==3) %>% select(id, error) %>% 
  bind_rows(pic_id %>% dplyr::filter(nchar(error)>3) %>% select(id=error)) %>% unique()

to_clear <- supp_id_from_verif$id

##########################################################################################
#1-2 ## 
# nettoyage REG_ID pour récupérer les pic-siren -> TRAITEMENT FRANCAIS

id_verif <- participant %>% 
  dplyr::filter(country_code=="FR") %>% 
  group_by(participant_pic) %>% 
  dplyr::mutate(nb=n(), s=paste(unique(stage), collapse=","), name_source = str_squish(name_source)) %>% 
  dplyr::filter(!(s=="project,proposal"&stage=="proposal")) %>% 
  ungroup() %>% 
  select(participant_pic, name_source, country_code, cd_reg_id, cd_reg_vat_id, nb) %>%
  unique() %>% 
  dplyr::mutate(cd_reg_id = str_to_lower(cd_reg_id), 
         id0 = str_replace_all(cd_reg_id, '^\\D+$', NA_character_),
         id0 = str_replace_all(id0, "n/a|n/a|^na$", NA_character_),
         id0 = str_replace_all(id0, "^[[:punct:]]+$", NA_character_)) %>% 
  tidyr::separate(id0, into=c("id1", "id2"), sep="/") %>%
  dplyr::mutate(id3 = if_else(str_detect(id1, "^fr"), str_replace_all(id1, "^fr\\d{2}(?=\\d+)", ""), id1),
         id3 = if_else(str_detect(id3, "^w|^fs"), id3, str_replace_all(id3, "\\D+", "")),
         id3 = str_replace_all(id3, " +", ""),
         id3 = if_else(id3=="", NA_character_, id3),
         
         id4 = if_else(str_detect(id2, "^fr"), str_replace_all(id2, "^fr\\d{2}(?=\\d+)", ""), id2),
         id4 = if_else(str_detect(id4, "^w|^fs"), id4, str_replace_all(id4, "\\D+", "")),
         id4 = str_replace_all(id4, " +", ""),
         id4 = if_else(id4=="", NA_character_, id4),
         
         id_p = if_else(str_detect(id3, "^w|^fs") | (nchar(id3)==9 | nchar(id3)==14), id3,  NA_character_),
         id_s = if_else(str_detect(id4, "^w|^fs") | (nchar(id4)==9 | nchar(id4)==14), id4,  NA_character_),
         
         cd_reg_vat_id = str_to_lower(cd_reg_vat_id),
         id5 = str_replace_all(cd_reg_vat_id, '^\\D+$', NA_character_),
         id5 = if_else(str_detect(id5, "^fr"), str_replace_all(id5, "^fr\\d{2}(?=\\d+)", ""), NA_character_),
         id_p = if_else(is.na(id_p) & (nchar(id5)==9 | nchar(id5)==14), id5, id_p),
         id5 = if_else(id5!=id_p, id5, NA_character_),
         id_p2 = if_else(nchar(id5)==9 | nchar(id5)==14, id5, NA_character_)) %>%
  select(-id1, -id3, -id2, -id4, -id5)  %>%
  unique() %>% 
  dplyr::mutate(across(.cols=c(id_p, id_p2, id_s), str_to_upper))
   

# modifier table pour le controle sirene

temp <- pic_id %>% 
  full_join(select(id_verif, c(participant_pic, id_p, id_s, id_p2, nb)),
            by = "participant_pic") %>% 
  mutate(id_temp = if_else(nchar(id_p)==14, str_sub(id_p, 1, 9), id_p)) %>% 
  unique()

temp <- temp %>% 
  mutate(id_temp=ifelse(id_temp %in% to_clear, NA_character_, id_temp),
        diff = if_else(!is.na(id_temp) & id_temp != id, 1, 0 )) 


# liste des id pour vérifier qu'ils sont toujours actifs
temp2 <- temp %>% 
  dplyr::filter(diff==1) %>% 
  select(id=id_temp)

identifiant_control <- temp %>% 
  select(id) %>% 
  dplyr::filter(!is.na(id)) %>% 
  bind_rows(temp2) %>% 
  unique()


############################################################
#tout participant 228 062

pic_id_temp <- participant %>% 
  select(participant_pic, name_source, website, address_source, post_code_source, city_source, country_name, country_code, project_id, stage) %>% 
  dplyr::mutate(across(c(name_source, address_source, city_source), str_to_lower)) %>% 
  #               across(c(name_source, address_source, city_source), replace_special_letters),
  #               across(c(name_source, address_source, city_source), clean_exo_char)) %>%
  group_by(participant_pic, stage) %>% 
  dplyr::mutate(nb = n_distinct(project_id)) %>% 
  ungroup() %>% 
  select(-project_id) %>%
  ungroup() %>% 
  unique() %>% 
  left_join(select(pic_id, c(participant_pic, id, country_code)), 
            by = c("participant_pic", "country_code")) %>% 
  unique()


akrun <- function(x) Filter(function(y) !all(is.na(y)), x) 

ajout <- anti_join(pic_id_orig, pic_id_temp, by=c("participant_pic", "country_code")) %>% 
  select(participant_pic, name_source, website, address_source, post_code_source, city_source,
         country_name, country_code, id, error=error_2021_11, last_control, source, project, proposal, comments) %>% 
  unique()
 
ajout <- akrun(ajout)
    
   

pic_id_temp <- bind_rows(pic_id_temp, ajout)

n_distinct(pic_id_temp$participant_pic)

pic_id_address <- pic_id_temp %>%
  select(participant_pic, name_source, address_source, post_code_source, city_source, country_name, country_code) %>%
  unique()


pic_id_new <- pic_id_temp %>%
  select(-address_source, -post_code_source) %>% unique() %>% 
  group_by(participant_pic, name_source, country_name, country_code) %>%
  mutate(website=str_c(na.omit(unique(website)), collapse=";"),
       city_source=str_c(na.omit(unique(city_source)), collapse=";"),
       source = ifelse(!is.na(id), last_control, NA_character_)) %>%
  unique() %>% 
  pivot_wider(names_from = stage, values_from = nb) %>% 
  group_by(participant_pic) %>% 
  mutate(nb_pic=n()) %>% 
  ungroup() %>%
  unique()

pic_id_new <- akrun(pic_id_new)
# mutate(website=ifelse(!is.na(website), str_c(na.omit(unique(website)), collapse=";"), website),
#        city_source=ifelse(!is.na(city_source), str_c(na.omit(unique(city_source)), collapse=";"), city_source))
#######################################################################################
# a intégrer à la table pic_id_new
link_h <- 
  read.csv(file= paste0("C:/Users/zfriant/Documents/OneDrive/PCRI/traitement/",livraison,"/bd/project/legal_entities_links.csv")) %>% 
  dplyr::mutate(participant_pic = as.character(CD_PART_PIC)) %>% unique() %>% 
  # dplyr::filter(LB_DATASET == "GRID") %>%
  dplyr::filter(LB_DATASET == "ROR" ) %>%
  mutate(id=paste0('R', CD_LINK_ID)) %>% 
  group_by(participant_pic) %>% 
  dplyr::mutate(nb=n_distinct(id), 
                id = paste(id, collapse = ","),
                source_link = paste0(livraison, '_LINK')) %>% 
  ungroup() %>% 
  dplyr::filter(nb==1) %>%
  select(participant_pic, id_link=id, source_link) %>% 
  unique()



pic_id_new <- pic_id_new %>% 
  left_join(link_h, by = "participant_pic") %>% 
  mutate(id = ifelse(is.na(id) & !is.na(id_link), id_link, id),
         source = source_link) %>% 
  select(-id_link, -source_link)

############################################################################


# table pour match participant EMPTY to GRID_ROR
id_empty <- pic_id_temp %>% dplyr::filter(is.na(id), country_code != "FR") %>% 
  mutate(country_code=if_else(country_code=="EL", "GR", country_code),
         country_code=if_else(country_code=="UK", "GB", country_code),
    empty = str_squish(str_replace_all(paste(name_source, city_source, country_code, sep = " "), '[:punct:]', ' '))) %>% 
  select(empty) %>% 
  dplyr::filter(!is.na(empty)) %>% 
  unique() %>% 
  write.table(file=paste0(identification, "a_match_ror_", date_sauve, ".csv"), 
             fileEncoding = "UTF-8", row.names = F, col.names = F, na = "")


##RETOUR DU MATCH ET DES ROR
#ATTENTION DEVELOPPER LE PROG POUR RECUPERER LE MATCH
#retour match grid validé et rorisé
# filename2 = "GRID_TO_ROR_2021-12-09T20-37.csv"
# match <- read.csv2(file=paste0("C:/Users/zfriant/Documents/OneDrive/referentiels/", filename2), na="", 
#                    colClasses = "character", encoding = "UTF-8")
# match <- match %>% 
#   dplyr::mutate(country_code=if_else(country.country_code=="GR", "EL", country.country_code),
#                 country_code=if_else(country.country_code=="GB", "UK", country_code),
#                 id_match = str_split(id, '(/)+'),
#                 id_match = unlist(map(id_match, dplyr::last(map(id_match, length))))) %>% 
#   select(grid_old, id_match, country_code) %>% 
#   unique()



pic_id_temp <- pic_id_temp %>% 
  left_join(match, by = "participant_pic") %>% 
  dplyr::mutate(id = ifelse(is.na(id) & !is.na(id_match), id_match, id),
                source = ifelse(!is.na(id_match) & is.na(source), paste0(livraison, '_MATCH'), source)) %>% 
  select(-id_match)



#########################################################################################################

id_temp2 <- pic_id_new %>% 
  dplyr::filter(!(id %in% to_clear)) %>% 
  select(id) %>% unique()

# liste des identifiants à controler pour savoir s'ils sont toujours actifs
identifiant_control <- bind_rows(identifiant_control, id_temp2) %>% 
  unique() %>%
  anti_join(supp_id_from_verif, by="id")


write.csv2(identifiant_control, file = paste0(identification, "a_control_id_", date_sauve, ".csv"), na="",
           row.names = F, fileEncoding = "UTF-8")
#####################################################################################
# retour controle

# 750925844 , 152000014 , 150000354, 811640184 non diffusible
# 152000014


filename="control_ID_2022-06-24T17-20.csv"
control_id <-
  read.csv2(file = paste0(identification, "corrected/", filename), 
            colClasses = "character", col.names=c('id', 'id_error'), na.strings = "") %>% 
  anti_join(identifiant_control, by='id') %>% 
  dplyr::filter(str_detect(id, pattern_rnsr, negate=T), !(id %in% to_clear))



pic_id_new <- pic_id_new %>% 
  mutate(id = ifelse(id %in% control_id$id, NA_character_, id))


#verification des ID FR différents entre les deux actu

temp3 <- temp %>% dplyr::filter(diff==1) %>% 
  mutate(id_new=ifelse(country_code=="FR" & 
                       str_detect(id, pattern_paysage, negate = T) & 
                       str_detect(id_temp, pattern_siren),
                       id_temp, id)) %>% 
  select(participant_pic, country_code, id_new)


pic_id_new <- pic_id_new %>% 
  left_join(temp3, by=c('participant_pic', 'country_code')) %>% 
  mutate(id=ifelse(!is.na(id_new), id_new, id),
         last_control=ifelse(!is.na(id), '2022_06', NA_character_)) %>% 
  select(-id_new)
  
ajout <- pic_id_orig %>% 
  dplyr::filter((!is.na(comments)|!is.na(error_2021_11)) & !is.na(id)) %>% 
  select(id, comments, error_2021_11 ) %>% 
  unique()


pic_id_new <- pic_id_new %>% 
  left_join(ajout, by='id')
  

write.csv2(pic_id_new, file=paste0(referentiel, 'pic_id_new.csv'), na="", 
           fileEncoding = "UTF-8", row.names = FALSE)

write.csv2(pic_id_address, file=paste0(referentiel, 'pic_id_ad.csv'), na="", 
           fileEncoding = "UTF-8", row.names = FALSE)

rm(list=c("pi_id", "pic_id_address", "pic_id_orig", "pic_id_temp", "pic_id", "diff", "control_id",
          "id_temp2", "id_verif", "identifiant_control", "link_h", "ajout", "supp_id_from_verif"))
rm(list=ls(pattern = "^temp"))

gc()
