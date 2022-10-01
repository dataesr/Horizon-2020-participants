####################################################################################
####
### TRAITEMENT GEO
###
load(file=rdata_last_version(paste0(data_result, "data/"),"part_fr_p01_"))
load(file=rdata_last_version(paste0(data_result, "data/"),"key_part_p02_"))
load(file=rdata_last_version(paste0(data_result, "data/"),"geo_part_p02_"))

load(file=paste0(referentiel, "sirene_p6.Rdata"))
load(file=paste0(referentiel, "etab.Rdata"))

rnsr <- load_one_obj(rdata=paste0(referentiel, "rnsr.Rdata"), "rnsr") %>% 
  mutate(com_code = ifelse(str_detect(com_code, "^\\D+$"), NA_character_, com_code))

id_select=c("stage", "id", "participant_order", "participates_as")

x <- key_geoloc %>% 
  select(all_of(id_select),organizations_id, participant_id, tutelle_id, participant_nns) %>% 
  separate_rows(c(organizations_id, participant_id, tutelle_id, participant_nns), sep=",")
  
geo_r <- x %>%
  select(all_of(id_select), idr=organizations_id) %>% 
  dplyr::filter(str_detect(idr, pattern_rnsr)) %>% 
  bind_rows(x %>% 
              select(all_of(id_select), idr=participant_id) %>%
              dplyr::filter(str_detect(idr, pattern_rnsr)),
            bind_rows(x %>% 
                        select(all_of(id_select), idr=tutelle_id) %>%
                        dplyr::filter(str_detect(idr, pattern_rnsr))),
            bind_rows(x %>% 
                        select(all_of(id_select), idr=participant_nns) %>%
                        dplyr::filter(str_detect(idr, pattern_rnsr)))) %>% 
  unique()
 
n_rnsr=as.numeric(nrow(geo_r %>% select(idr) %>% unique())) #2 166


geoloc <- inner_join(geo_r, etab_rnsr, by = 'idr')

  
temp <- anti_join(geo_r, geoloc, by = c(id_select, 'idr')) %>% 
  left_join(rnsr, by = c("idr"="id")) %>% 
  select(all_of(id_select), idr, adresse, cp, city, com_code, gps, country_code) %>% 
  unique()
  
geoloc <- bind_rows(geoloc, temp)
  

# suite des rnsr non localisés
geo_r2 <- geoloc %>% 
  dplyr::filter(is.na(com_code) | nchar(com_code)!=5) %>% 
  unique()

geo_r2 %>% select(-id_select) %>% 
  unique() %>% 
  group_by(idr) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  relocate(idr, n) %>% 
  write.csv2(file=paste0(identification, "nns_adress_", date_sauve, ".csv"), 
             row.names = F, fileEncoding = "UTF-8", na = "")

# retour geoloc geo_r2
# paysage_geoloc <- 
#   read.csv2(file = paste0(referentiel, "etab_2022-01-16T18-06.csv"), na="", encoding = "UTF-8") %>%
#   mutate(gps = str_c(latitude, longitude, sep=",")) %>% 
#   separate_rows(id_autres) %>% 
#   dplyr::filter(str_detect(id_autres, pattern_rnsr), (!is.na(com_code)|!is.na(gps))) %>% 
#   select(idr=id_autres, com_code, com_nom, uucr_id, uucr_nom, dep_id, dep_nom, dep_num_nom, 
#          reg_id, reg_nom, adresse, gps) %>% 
#   group_by(idr) %>% 
#   mutate(n=n())
# 
# if (nrow(paysage_geoloc %>% dplyr::filter(n>1))>0){
#   print("verifier doublon dans la table paysage_geoloc")
# }else{print("passer à la suite")}
  

# geoloc <- bind_rows(geoloc, select(paysage_geoloc, -n))
# 
# geo_r3 <- geoloc %>% dplyr::filter(is.na(gps))
# 
# geo_r4 <- anti_join(geo_r, geoloc, by="idr") %>% 
#   left_join(rnsr, by=c("idr"="id")) %>% 
#   select(idr, sigles_rnsr, adresse, cp, city, com_code, gps) %>% 
#   bind_rows(geo_r3) %>%
#   unique()
# 
# 
# if (nrow(geo_r4)>0){
#   write.csv2(geo_r4,file=paste0(identification, "geo_rnsr_", date_sauve, ".csv"), row.names = F, na = "")
# }  


###################################################################################
#retour geolocalisation à la main
retour_geo_r <- 
  readxl::read_xlsx(paste0(identification, "corrected/_traitement_geo.xlsx"), 
                    sheet="geo_rnsr", na='') %>%
  dplyr::filter(idr %in% geo_r2$idr)
 
#provisoire, le retour est l'identification complète suite à un pb donc la prochaine fois
# traiter uniquement retour_geo_r en complément
temp <- inner_join(select(geo_r2, c(id_select, idr)), 
                   retour_geo_r, by="idr")

geoloc <- anti_join(geoloc, temp, by = c(id_select, 'idr')) %>% 
  bind_rows(temp)

geoloc <- geoloc %>% 
  mutate(country_code = ifelse(is.na(country_code), "FR", country_code),
         latitude = ifelse(is.na(latitude), str_extract(gps, "^.+\\.\\d{1,5}(?=\\d+,{1})"), latitude),
         longitude = ifelse(is.na(longitude),str_extract(gps, "(?<=,).+\\.\\d{1,5}"), longitude)) %>% 
  select(-gps)


test <- geoloc %>% select(idr) %>% unique() %>% 
  group_by(idr) %>% dplyr::count(idr) %>% dplyr::filter(n>1)
if (nrow(test)>0){
  print("verifier doublons dans geoloc")
}else{message("geoloc_rnsr ", nrow(geoloc %>% select(idr) %>% unique()) == n_rnsr)}

# lien avec les identifiants participants
geoloc_nns <- geoloc


if (nrow(geoloc_nns %>% dplyr::filter(is.na(com_code) & is.na(latitude)))>1){
  print("attention ! id non géolocalisés")
}else{print("ok")}

rm(list=c("test", "n_rnsr", "retour_geo_r", "paysage_geoloc", "geoloc"))
rm(list=ls(pattern = "^(geo_r)"))
gc()


# geoloc_nns %>% select(-stage, -id, -participant_order, -participates_as) %>% 
#   unique() %>% 
#   write.csv2(file=paste0(identification, "corrected/geoloc_nns_2022_01.csv"), row.names = F, na="")
###################################################################################################"
#####
####  Localisation des autres participants
###
geo_r <- key_geoloc %>%
  separate_rows(participant_id, sep=",") %>% 
  dplyr::filter(str_detect(participant_id, pattern_rnsr, negate=T)) %>%
  dplyr::mutate(adresse=if_else(is.na(address_net), address_source_net, address_net),
                cp=if_else(is.na(pc_net), pcs_net, pc_net),
                city=if_else(is.na(city_net), city_source_net, city_net)) %>% 
  select(all_of(id_select), idr=participant_id, adresse, cp, city, country_dept) %>%
  mutate_if(is.character, list(~na_if(.,""))) %>% 
  unique()

toString(colnames(geo_r))


# paysage
geoloc <- geo_r %>% 
  dplyr::filter(str_detect(idr, pattern_paysage)) %>% 
  unique() %>% 
  inner_join(select(etab, c(paysage, com_code, latitude, longitude)), 
             by=c("idr"="paysage"))
    # com_nom, uucr_id, uucr_nom, dep_id, dep_nom, dep_num_nom, reg_id, reg_nom, gps)), 

# siren
sir <- geo_r %>% 
  dplyr::filter(str_detect(idr, pattern_siren)) %>%
  unique() %>% 
  inner_join(
    select(sirene, c(siren, siege, com_code)), 
    by=c("idr"="siren")) %>% 
  dplyr::filter(siege=="True") %>% 
  select(-siege)

geoloc <- bind_rows(geoloc, sir)


# siret
sir <- geo_r %>% 
  dplyr::filter(str_detect(idr, pattern_siret)) %>%
  unique() %>% 
  inner_join(
    select(sirene, c(siret, com_code)), 
    by=c("idr"="siret"))

geoloc <- bind_rows(geoloc, sir) %>% 
  mutate(com_code = ifelse(com_code=="0None", NA_character_, com_code))


# ror
ror <- load_one_obj(rdata=paste0(referentiel, "ror.Rdata"), "ror") %>% 
  select(id, city_ror=city, country_ror=country_code, latitude, longitude) %>% 
  # mutate(latitude = str_extract(gps, "^.+\\.\\d{1,5}(?=\\d+,{1})"),
  #       longitude = str_extract(gps, "(?<=,).+\\.\\d{1,5}")) %>%
  dplyr::mutate_if(is.character, list(~na_if(.,""))) %>% 
  unique()

r <- geo_r %>% 
  dplyr::filter(str_detect(idr, pattern_ror)) %>% 
  unique() %>% 
  inner_join(ror, by=c("idr"="id"))


geoloc <- bind_rows(geoloc, r)

# attention bordel faire table avec geoloc nns et les autres ; à refelchir
temp <- geo_r %>% 
  # anti_join(geoloc, by = c("stage", "id", "participant_order", "participates_as", "idr")) %>%  
  anti_join(geoloc, by = c("stage", "id", "participant_order", "participates_as", "idr"))

geoloc <- bind_rows(geoloc, temp) %>% 
  mutate(city=ifelse(!is.na(city_ror), city_ror, city),
         country_code=ifelse(!is.na(country_ror), country_ror, country_dept)) %>% 
  select(-city_ror, -country_ror)

geoloc_full <- bind_rows(geoloc, geoloc_nns)

  
test <- anti_join(key_geoloc, geoloc_full, by = c("stage", "id", "participant_order", "participates_as"))  
if (nrow(test)>0){
  print("vérifier test : reste des participations non traitées")
}else{
  print('ok')
}
###################################################################################################
###################################################################################################
# temp <- geoloc %>% 
#   dplyr::filter(!is.na(com_nom)) %>% 
#   rename(idr=participant_id)
# 
# geoloc_full <- bind_rows(geoloc_full, temp)

# participation avec cp_ville sans infos
manquant <- geoloc %>% 
  # dplyr::filter(str_detect(participant_id, pattern_ror, negate=T)) %>% 
  dplyr::filter(is.na(cp) | is.na(city) | str_detect(idr, pattern_ror)) %>%
  dplyr::filter(str_detect(com_code, "^\\d{5}$", negate = T) | is.na(com_code))
write.csv2(manquant, file=paste0(identification, "geoloc_", date_sauve, ".csv"), 
           row.names = F, na = "")

retour_manquant <- readxl::read_xlsx(
  path=paste0(identification, "corrected/_traitement_geo.xlsx"), 
  sheet="retour_manquant", col_types = "text", na=c("", "#N/A", "0")) %>% 
  mutate(participant_order=as.numeric(participant_order),
         latitude = str_extract(gps, "^.+\\.\\d{1,5}(?=\\d+,{1})"),
         longitude = str_extract(gps, "(?<=,).+\\.\\d{1,5}")) %>% 
  select(-ends_with("old"), -gps) %>% 
  unique() %>% 
  inner_join(select(manquant, c(all_of(id_select), "idr")), by=c(id_select, "idr"))


geoloc_full <- anti_join(geoloc_full, retour_manquant,
        by = c("stage", "id", "participant_order", "participates_as", "idr")) %>% 
  bind_rows(retour_manquant)


#création de la liste cp_ville pour le com_code
#ajout adresse trouvée
cp_ville <- geoloc_full %>% 
  dplyr::filter(!is.na(cp) & !is.na(city) & is.na(com_code)) %>% 
  dplyr::filter(country_code=="FR") %>% 
  select(all_of(id_select), idr, adresse, cp, city, latitude, longitude, country_code) %>% 
  dplyr::mutate(nb = str_count(adresse, ","),
         pc_city = if_else(!is.na(cp), paste(cp, city, sep=" "), NA_character_),
         pc_city = if_else(is.na(pc_city), sub(".*, ", "", adresse), pc_city),
         pc_city = str_squish(tolower(pc_city))) %>% 
  select(-nb)


# Lancer apres avoir récuperer les cp_ville com_coder
cp_ville %>% select(pc_city) %>% unique() %>% 
write.csv2(file=paste0(identification, "cp_ville_", date_sauve, ".csv"),
           na="", fileEncoding = "UTF-8", row.names = F)

#######################################################################
# retour des cp_ville géocodés
  cp_v1 <- 
    readxl::read_xlsx(
      path=paste0(identification, "corrected/_cp_ville.xlsx"), 
      sheet="Complet", col_types = "text", na=c("", "#N/A", "0"), col_names = T) %>% 
    select_if(function(x){any(!is.na(x))})
  
  names(cp_v1) <- c("num_ligne", "pc_city", "com_code", "com_nom", "uucr_id", "uucr_nom", "dep_id", 
                "dep_nom", "dep_nom_num", "aca_id", "aca_nom", "reg_id", "reg_nom", "country_code2")
    
  cp_v1 <- cp_v1 %>% mutate(pc_city=str_replace_all(pc_city, '"', ""))
                    
  if (nrow(cp_v1 %>% group_by(pc_city) %>% mutate(n=n()) %>% dplyr::filter(n>1)>0)){
    print("vérifier les doublons dans le fichier excel")
    }else{print("passer à la suite")
    }
  
  cp_v2 <- cp_v1 %>% 
    select(pc_city, com_code2=com_code) %>% 
    # select(-num_ligne, -country_code2) %>% 
    select_if(function(x){any(!is.na(x))}) %>% 
    dplyr::filter(!is.na(com_code2)) %>% 
    dplyr::mutate_all(~str_squish(.)) %>% 
    unique()
  
  cp_country <- cp_v1 %>% 
    select(pc_city, country_code2) %>% 
    # select(-num_ligne, -country_code2) %>% 
    select_if(function(x){any(!is.na(x))}) %>% 
    dplyr::filter(!is.na(country_code2)) %>% 
    dplyr::mutate_all(~str_squish(.)) %>% 
    unique()


####################################################################################################  
####################################################################################################
# jointure avec retour com_code
cp_country <- inner_join(cp_ville, cp_country) %>% 
  select(id_select, idr, country_code2)

geoloc_full <- left_join(geoloc_full, cp_country, by=c(id_select, "idr")) %>% 
  mutate(country_code=ifelse(!is.na(country_code2), country_code2, country_code)) %>% 
  select(-country_code2)

cp_temp <- inner_join(cp_ville, cp_v2, by="pc_city") %>% 
  select(id_select, idr, com_code2)

geoloc_full <- left_join(geoloc_full, cp_temp, by = c(id_select, "idr")) %>% 
  mutate(com_code=ifelse(!is.na(com_code2), com_code2, com_code)) %>% 
  select(-com_code2)



test <- geoloc_full %>% 
  dplyr::filter(is.na(com_code) & country_code=="FR" & !is.na(adresse))
if (nrow(test)>0){
  print("verifier la table test car il ne reste des FR non localisés ; relancer cp_ville")
}

cp_ville <- geoloc_full %>% 
  dplyr::filter(!is.na(cp) & !is.na(city) & is.na(com_code)) %>% 
  dplyr::filter(country_code=="FR") %>% 
  select(all_of(id_select), idr, adresse, cp, city, latitude, longitude, country_code) %>% 
  dplyr::mutate(nb = str_count(adresse, ","),
                pc_city = if_else(!is.na(cp), paste(cp, city, sep=" "), NA_character_),
                pc_city = if_else(is.na(pc_city), sub(".*, ", "", adresse), pc_city),
                pc_city = str_squish(tolower(pc_city))) %>% 
  select(-nb)
cp_ville %>% select(pc_city) %>% unique() %>% 
  write.csv2(file=paste0(identification, "cp_ville_", date_sauve, ".csv"),
             na="", fileEncoding = "UTF-8", row.names = F)


# utilisation de base complete
# filename = "geocodage_full_2022-04-14T11-04"
# geo_full <- read.csv2(file=paste0(referentiel, filename, ".csv"), 
            # encoding = "UTF-8", colClasses = "character") %>% 
  # unique()



temp <- anti_join(geo_r, geoloc_full, by = c(id_select, "idr"))
if (nrow(temp)>0){
  print("il reste des participations dans geo_r oubliées")
  
}


# extraction par com_code du geocodage
com_code <- geoloc_full %>% select(com_code) %>% unique() %>% 
  write.csv2(file=paste0(identification, "com_code_", date_sauve, ".csv"),
             na="", fileEncoding = "UTF-8", row.names = F)

filename="geocod_2022-08-19T14-13"
geocod <- read.csv2(file=paste0(identification, "corrected/", filename, ".csv"), 
                    encoding = "UTF-8", colClasses = "character") %>% 
  unique() %>% 
  mutate(com_code=str_squish(com_code), 
         aca_id=ifelse(reg_id=="R28", "A70", aca_id), 
         aca_nom=ifelse(reg_id=="R28", "Normandie", aca_nom))

geoloc_full<- geoloc_full %>% 
  left_join(geocod, by = "com_code") %>% 
  unique()

#verif
test <- geoloc_full %>% dplyr::filter(!is.na(com_code) & is.na(com_nom))


geoloc_full <- geoloc_full %>% 
  mutate(country_code = ifelse(!is.na(com_code) & (is.na(country_code)| country_code=="ZZ"), "FR", country_code)) %>% 
  select(-country_dept)

test <- key_geoloc %>% select(all_of(id_select), idr=participant_id) %>% unique()
test <- anti_join(geoloc_full, test, by=c(id_select, "idr"))

save(geoloc_full, file=paste0(data_result, "data/geoloc_key_s01_", date_sauve, ".Rdata"))

##################################################################

################################################################################

#traitement des gps -> réduction des coordonnées

# geoloc_gps <- rnsr %>% select(idr=id, gps) %>% 
#   bind_rows(etab %>% select(idr=paysage, gps)) %>% 
#   bind_rows()
# 
# 
# 
# pattern_gps = "^.+\\.\\d{1,5}"
# test <- geoloc_full %>% 
#   select(latitude, longitude) %>% unique()
#   filter(!is.na(gps)) %>% 
#   separate(gps, into=c("t", "g"), sep=",", remove = F) %>% 
#   dplyr::mutate(lat=str_extract(t, pattern_gps), lng=str_extract(g, pattern_gps)) %>% 
#   select(-t,-g) %>% unique()
# 
# 
# geoloc_fin <- geoloc_fin %>% 
#   left_join(test, by="gps") %>% 
#   select(-gps, -id1, -nb) %>% 
#   dplyr::mutate(city = str_replace_all(city, "\\d+", ""),
#          id_geo = paste(address, pc, city, sep=" "),
#          id_geo = str_to_lower(str_replace_all(id_geo, "\\s", ""))) %>%
#   unite(address_pc, address, pc, city, sep= " ", na.rm=T, remove=F) %>% 
#   unite(address_city, address, city, sep=" ", na.rm=T, remove=F) 
# geoloc_fin <- geoloc_fin %>% 
#   dplyr::mutate_if(is.character, list(~na_if(.,""))) %>% 
#   dplyr::mutate_at(c("address_pc", "address_city"), str_to_lower) %>% 
#   dplyr::mutate_if(is.character, str_squish) %>% 
#   unique()
# 
# n_geo_lat=nrow(geoloc_fin %>% filter(!is.na(lat)))
# 
# gps <- geoloc_fin %>% 
#   filter(!is.na(lat)) %>% 
#   select(address_city, lt=lat, lg=lng) %>% 
#   unique() %>% group_by(address_city) %>% 
#   dplyr::mutate(n=n()) %>% filter(n<2)
# 
# geoloc_fin <- geoloc_fin %>% 
#   left_join(gps, by="address_city") %>% 
#   dplyr::mutate(lat=if_else(is.na(lat), lt, lat), 
#                 lng=if_else(is.na(lng), lg, lng)) %>% 
#   select(-lt, -lg, -n) %>% unique()
# 
# gps <- geoloc_fin %>% filter(!is.na(lat)) %>% select(address_pc, lt=lat, lg=lng) %>% 
#   unique() %>% group_by(address_pc) %>% dplyr::mutate(n=n()) %>% filter(n<2)
# 
# # essai <- geoloc_fin %>% filter(is.na(lat)) %>% 
# #   inner_join(gps, by="address_pc") %>% 
# #   dplyr::mutate(lat=if_else(is.na(lat), lt, lat), lng=if_else(is.na(lng), lg, lng)) %>% 
# #   select(-lt, -lg) %>% 
# #   unique()

#############################################################################################"
#lise adresse pour géoloc dans phyton ## DANS le paste régler le pb des NA
no_gps <- geoloc_fin %>% filter(is.na(lat)) %>% 
  select(com_code, address, address_pc, address_city, pc, city, id_geo) %>%
  # unite(address_pc, address, pc, city, sep= " ", na.rm=T, remove=F) %>% 
  # unite(address_city, address, city, sep=" ", na.rm=T, remove=F) %>% 
  dplyr::mutate_at(vars(-com_code), str_to_lower) %>% 
  dplyr::mutate_if(is.character, str_squish) %>% 
  rename_at(vars(-id_geo), ~ paste0(., '_orig')) %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>% 
  filter(!is.na(address_orig)) %>%
  unique()
write.csv2(test, file=paste0(identification, "adr_geo", date_sauve, ".csv"), 
           fileEncoding = "UTF-8", na="")

# retour geolocalisation au niveau de l'adresse
gps_retour <- 
  read.csv2(file=paste0(identification, "corrected/geoloc_datagouv_2021-05-29T23-03.csv"), 
            na.strings = "", encoding = "UTF-8", colClasses = "character")

# uniquement les réponses uniques
gps <- inner_join(gps_retour, no_gps, by=c("id"="address_city_orig")) %>% 
  unique() %>% 
  group_by(id_geo, com_code) %>% 
  filter(score==max(score)) %>% 
  select(id_geo, score, gps, id, com_code_orig, com_code, city_orig, city) %>% 
  ungroup() %>% unique()
  
gps_ok <- gps %>% 
  filter(com_code_orig==com_code) %>% 
  select(-score, -city, -city_orig, -com_code_orig, -id_geo) %>%
  unique() %>% 
  group_by(id, com_code) %>% 
  dplyr::mutate(n=n()) %>% 
  ungroup()

nrow(gps_ok %>% filter(n>1))

geoloc_fin <- left_join(geoloc_fin, select(gps_ok, c(id, gps, n)), by=c("address_city"="id")) %>%
  dplyr::mutate(lat=if_else(is.na(lat) & !is.na(gps), str_extract(gps, "(?<=\\[).+(?=,{1})"), lat),
         lng=if_else(is.na(lng) & !is.na(gps), str_extract(gps, "(?<=,).+(?=\\])"), lng)) %>%
  select(-n, -gps)


gps_a_verif <- anti_join(gps, gps_ok)

write.csv2(gps_a_verif, file=paste0(identification, "adr_geo", date_sauve, ".csv"), 
           fileEncoding = "UTF-8", na="")


gps <- readxl::read_xlsx(paste0(identification, "corrected/_traitement_geo.xlsx"), 
                         sheet="geo_gps", na='') %>% 
  dplyr::mutate(lt=str_extract(gps, "^.+\\.\\d{1,5}(?=\\d+,{1})"),
                lg=str_extract(gps, "(?<=,).+\\.\\d{1,5}")) %>% 
  select(id_geo, lt, lg)

geoloc_fin <- left_join(geoloc_fin, gps, by="id_geo") %>% 
  dplyr::mutate(lat=if_else(is.na(lat), lt, lat), lng=if_else(is.na(lng), lg, lng)) %>% 
  unite(gps, lat, lng, sep=",", na.rm=T) %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>% 
  select(-lt, -lg)

n_geo_gps=nrow(geoloc_fin %>% filter(!is.na(gps)))

test <- geoloc_fin %>% 
  select(stage, id, participant_order, participates_as, participant_id, com_code, gps) %>% 
  unique() %>% 
  group_by(stage, id, participant_order, participates_as, participant_id) %>% 
  dplyr::count()
##############################################################################"
#controle

# com_code seul
test <- geoloc %>% filter(is.na(gps)) %>% 
  select(address, com_code) %>% unique() %>% 
  write.csv2(file=paste0(identification, "no_gps.csv"), na="", row.names = F, fileEncoding = "UTF-8")
# si test n'est pas null, récupération des données com_code dans une autre base
if (nrow(test)>1){
  test[,c(14:ncol(test))]<- NULL
  
  geo_comcode <-
    read.csv2(file=paste0(identification, "corrected/geoloc_comcode_2021-03-14T16-52.csv"), 
              na = "", colClasses = "character", fileEncoding = "UTF-8") %>% unique() %>% select(-X)
  
  test <- inner_join(test, geo_comcode)
  
  geoloc <- 
    anti_join(geoloc_fin, test, by = c("stage", "id", "participant_order", "participates_as", "participant_id")) %>% 
    bind_rows(test)
  
  test <- geoloc %>% filter(!is.na(com_code) & is.na(com_nom)) %>% select(com_code) %>% unique()
  if (nrow(test)>0){
    write.csv2(test, file=paste0(identification, "geo_comcode.csv"), row.names = F)
  }
}else{
  print("ok")}

# test gps null et com_code null
test <- anti_join(geo_r, geoloc_fin, by="id")


verif <- anti_join(part_fr, geoloc_fin, by=c("stage","id", "participant_order", "participates_as"))

nrow(geoloc_fin %>% filter(stage=="project") %>% select(id, participant_order, participates_as) %>% unique())


# # essai pour récupérer géolocalisation avec api data.gouv ; pour l'instant pas terrible  
# part_fr %>% select(address_tag, city_tag, pc_net) %>% unique() %>%
#   write.csv2(file=paste0(identification, "geoloc_adresse.csv"), row.names = F, fileEncoding = "UTF-8")
# datagouv <- read.csv2(file=paste0(identification, "corrected/geoloc_datagouv_2021-03-14T15-29.csv"), 
#                       na = "", colClasses = "character") %>% select(-X) %>% unique()


save(geoloc_fin, file=paste0(data_result, "data/geoloc_key14_", date_sauve, ".Rdata"))


rm(list=c("paysage_geoloc", "rest_lid", "geo_code", "geo_r", "reste", "verif", "temp", "test", "sans_rien",
          "x", "verif1", "cp_v1", "cp_v2", "com_code_reg", "a_nett", "a_nett1", "cp_ville", "cp_ville1", 
          "geo_comcode", "grid", "new_rnsr", "rnsr", "old_geo_code", "rnsr_adresse", "reste2", "reste1",
          "key_geoloc", "geo", "geoc", "geo_comcode", "geo_c", "a_geoloc", "geoloc_nns", "geoloc",
          'gps', 'gps_ok', 'retour_manquant', 'manquant', "geocod"))

rm(list=ls(pattern = "^geo_r"))
rm(list=ls(pattern = "^cp_"))
gc()
# new_rnsr <- anti_join(rnsr, new_rnsr, by = "id") %>% 
#   bind_rows(new_rnsr)
# temp <- inner_join(select(geoloc, c("lid","participant_id0", "com_code")), 
#                    select(new_rnsr, c("id","gps", "adresse")),
#                    by=c("participant_id0"="id")) %>% unique()
# temp2 <- anti_join(geoloc, temp, by = "lid") %>%  
#   inner_join(part_fr, by = c("lid")) %>% 
#   # drop_na() %>% 
#   filter(!(is.na(com_code)&is.na(gps))) %>%
#   dplyr::mutate(adresse = paste(address_net, pc_net, city_tag, sep=", "),
#          adresse = str_replace_all(adresse, "(^|,)( ?)NA( ?)(,|$)", "")) %>% 
#   select(lid, com_code, adresse, gps)
# temp2[temp2=="NA"]<-NA_character_
# 
# temp <- bind_rows(temp, temp2)
# n_distinct(temp$lid)
# 
# temp2 <- temp %>% filter(is.na(gps))

