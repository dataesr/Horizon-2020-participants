load(file=rdata_last_version(paste0(data_result, "data/"), "part_fr_p01_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "key_part_p01_"))


id_select=c("lid", "stage", "id", "participant_order", "participates_as")

x <- key_part %>% 
  dplyr::filter(str_detect(p, pattern_rnsr) & str_detect(participant_id, pattern_rnsr, negate = T))

# retransforme les participant_id identifié par paysage en nns ; mais concerne des structs prticulières

## annulation de cette étape pour l'instant sinon doublon d'affichage dans scanr 
# dans les tutelles apparaissent les tutelles de la struct en tant que nns
key_part <- key_part %>%
  dplyr::mutate(
    participant_nns = if_else(str_detect(p, pattern_rnsr),  p, NA_character_),
    participant_id = ifelse(!is.na(participant_nns), participant_nns, participant_id))


# remplissage des new vars avec les anciennes
key_part <- key_part %>% 
  dplyr::mutate(organizations_id=if_else(is.na(organizations_id), s, organizations_id),
         tutelle_id=if_else(is.na(tutelle_id), t, tutelle_id),
         participant_id=if_else(is.na(participant_id), p, participant_id)) %>% 
  select(all_of(id_select), s, p, organizations_id, participant_id, tutelle_id, participant_nns) %>%
  unique()


# traitement des keys nns + tutelle
#extraction des nns de participant_id mais pas de orga
#ajout de organizations_id aux tutelles et au participant_id si ne fait pas partie des tutelles nns

temp <- key_part %>% 
  dplyr::filter(!is.na(participant_nns), str_detect(organizations_id, pattern_rnsr, negate=T)) %>% 
  # dplyr::filter(str_detect(participant_id, pattern_rnsr), str_detect(organizations_id, pattern_rnsr, negate=T)) %>% 
  group_by(stage, id, participant_order, participates_as, organizations_id, participant_id) %>%
  dplyr::mutate(tutelle_id=str_c(unique(tutelle_id), collapse=","),
          # participant_id0=if_else(str_detect(tutelle_id, organizations_id, negate=T), paste(participant_id,organizations_id,sep=","), participant_id),
          tutelle_id=if_else(str_detect(tutelle_id, organizations_id, negate=T), str_c(tutelle_id,organizations_id,sep=","), tutelle_id),
          traite="OK") %>% 
  ungroup() %>% 
  unique()
# temp2 <- temp %>% filter(x==1) %>% select(stage, id, participant_order, participates_as, organizations_id) %>% unique()


key_part <- 
  anti_join(key_part, temp, by=c("stage", "id", "participant_order", "participates_as", "organizations_id", "participant_id"))
key_part <- 
  bind_rows(key_part, temp) %>% 
  unique()


## TRAITEMENT GRP HOSP 
#rectificatif de la tutelle_id pour les groupes hospitalier
temp <- key_part %>% 
  dplyr::filter(is.na(participant_nns) & (organizations_id %in% grp_hosp | str_sub(participant_id, 1, 9) %in% siren_gh)) %>% 
  dplyr::mutate(participant_id=if_else(is.na(participant_id), organizations_id, participant_id),
         tutelle_id=if_else(is.na(tutelle_id), participant_id, tutelle_id)) %>% 
  # group_by(stage, id, participant_order, participates_as, organizations_id) %>%
  dplyr::mutate(#tutelle_id=paste(tutelle_id,collapse=","),
         # x=if_else(str_detect(tutelle_id, organizations_id), 0,1),
         tutelle_id=if_else(str_detect(tutelle_id, organizations_id, negate=T), str_c(tutelle_id,organizations_id,sep=","), tutelle_id),
         # participant_id0=tutelle_id,
         # y=if_else(str_detect(participant_id, organizations_id), 0,1),
        #participant_id=if_else(str_detect(participant_id, organizations_id, negate=T), paste(participant_id,organizations_id,sep=","), participant_id),
         traite="OK") %>% 
  unique()

key_part <- anti_join(key_part, temp, by=c("stage", "id", "participant_order", "participates_as", "organizations_id", "participant_id"))
key_part <- bind_rows(key_part, temp) %>% 
  unique()


# autres entites
key_part <- key_part %>% 
  dplyr::mutate(
    # participant_id0=if_else(is.na(traite)&str_detect(organizations_id, pattern_rnsr), organizations_id, participant_id0),
    participant_id=if_else(is.na(traite)&is.na(participant_id), organizations_id, participant_id),
    tutelle_id=if_else(is.na(traite) & is.na(tutelle_id) & organizations_id!=participant_id, 
                str_c(organizations_id,participant_id, sep=","), tutelle_id),
    tutelle_id=if_else(is.na(traite) & is.na(tutelle_id), participant_id, tutelle_id)
    # participant_id0=if_else(is.na(traite)&is.na(participant_id0), tutelle_id, participant_id0)
    ) %>% 
  unique()

toString(colnames(key_part))


# suppression du lid
# key_part <- key_part %>% 
#   select(-s, -p, -lid, -traite) %>%
#   unique()

key_part <- key_part %>%
  group_by(stage, id, participant_order, participates_as, organizations_id, participant_id, tutelle_id) %>%
  dplyr::mutate(participant_nns=str_c(unique(participant_nns), collapse=",")) %>%
  ungroup() %>%
  unique()
key_part <- key_part %>%
  group_by(stage, id, participant_order, participates_as, organizations_id, participant_id, participant_nns) %>%
  dplyr::mutate(tutelle_id=str_c(unique(tutelle_id), collapse=",")) %>%
  ungroup() %>%
  unique()


# verification des doublons sans lid, s, p,...
test<- key_part %>% 
  select(stage, id, participant_order, participates_as, organizations_id, participant_id, tutelle_id, participant_nns) %>% 
  unique() %>% 
  dplyr::count(stage, id, participant_order, participates_as, organizations_id, participant_id) %>% 
  dplyr::filter(n>1)

if(nrow(test)>1){
  print("doublons à corriger")
}else{"aucun doublon"}


key_part <- key_part %>% 
  dplyr::mutate_at(c("organizations_id", "participant_id", "tutelle_id"), ~ifelse(is.na(.) , paste0("NonIdent", str_sub(id, -9)), .))


#extraction des lignes organizations_id pour les geolocaliser
test <- key_part %>% 
  dplyr::filter(organizations_id!=participant_id) %>% 
  dplyr::mutate(participant_id=organizations_id, tutelle_id=NA_character_) %>% 
  unique() %>% 
  left_join(select(part_fr,-participant_id), 
            by = c("stage", "id", "participant_order", "participates_as", "s"="organizations_id")) %>% 
  dplyr::mutate(lat=if_else(nchar(lat_source)<4 | is.na(lat_source), lat_dept, lat_source),
                lng=if_else(nchar(lng_source)<4 | is.na(lng_source), lng_dept, lng_source)) %>% 
  select(stage, id, participant_order, participates_as, organizations_id, participant_id, 
         tutelle_id, participant_nns, address_source_net, pcs_net, city_source_net, 
         country_dept, lat, lng) %>% 
  unique()


#####################################################

verif_ferme <- bind_rows(key_part %>% 
  dplyr::filter(str_detect(organizations_id, pattern_siren)) %>%
  select(siren=organizations_id),
  key_part %>% 
    dplyr::filter(str_detect(participant_id, pattern_siren)) %>%
    select(siren=participant_id)) %>% 
  unique() %>% 
  inner_join(ferme, by="siren")


###################################################################

# table avec geolocalisation par participant_id
key_geoloc <- key_part %>% 
  left_join(part_fr, by = c("stage", "id", "participant_order", "participates_as", "s"="organizations_id", "p"="participant_id")) %>% 
  dplyr::mutate(lat=if_else(nchar(lat_dept)<4 | is.na(lat_dept), lat_source, lat_dept), 
                lng=if_else(nchar(lng_dept)<4 | is.na(lng_dept), lng_source, lng_dept)) %>% 
  select(stage, id, participant_order, participates_as, organizations_id, participant_id, 
         tutelle_id, participant_nns, address_tag, address_net, address_tag, pc_net, city_net,
         city_tag, address_source_net, pcs_net, city_source_net, country_dept, lat, lng) %>% 
  unique()

key_geoloc <- bind_rows(key_geoloc, test)

# key_part sans s et p, lid, traite
key_part <- key_part %>% select(-s, -p, -traite) %>% unique()


save(key_part, file=paste0(data_result, "data/key_part_p02_", date_sauve, ".Rdata"))
save(key_geoloc, file=paste0(data_result, "data/geo_part_p02_", date_sauve, ".Rdata"))

rm(list= c("temp"))

