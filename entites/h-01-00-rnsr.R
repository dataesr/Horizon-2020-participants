#RNSR
load(file = rdata_last_version(paste0(data_result, "data/"),"part_fr_g01_"))


# suppression de labo ancien pour lesquels je n'ai pas trouvé de successeurs
# modification des labos dont le match_rnsr n'est pas bon
l_supp <- c("200118330Z", "200318373M", "200318386B", "200418323D", "200618025W")
part_fr <- part_fr %>% 
  dplyr::mutate(participant_id=if_else(participant_id %in% l_supp, NA_character_, participant_id),
                participant_id=ifelse(participant_id=="200817568R", "201220379D", participant_id),
                participant_id=ifelse(participant_id=="199812406Y", "201220322S", participant_id),
                participant_id=ifelse(participant_id=="200012189D", "200810703D", participant_id))


id_select=c("lid", "stage", "id", "participant_order", "participates_as")

key_part <- part_fr %>% 
  select(all_of(id_select), s=organizations_id, p=participant_id, id_temp) %>% 
  separate_rows(s, p, id_temp, sep=",")%>% 
  dplyr::mutate_if(is.character, str_squish)
# peur-etre appliquer l_supp à key_part au lieu de part_fr
#####################################################################################################
# cumul des id
cumul_id <- function(pat){
  temp <- key_part %>% dplyr::filter(str_detect(s, pat)) %>% 
    select(id=s) %>% unique()
  temp2 <- key_part %>% dplyr::filter(str_detect(id_temp, pat)) %>% 
    select(id=id_temp) %>% unique()
  maj_id <- key_part %>% dplyr::filter(str_detect(p, pat)) %>% 
    select(id=p) %>% bind_rows(temp,temp2) %>% unique() %>% 
    rename(id_find=id)
  return(maj_id)
}

maj_id <- cumul_id(pattern_rnsr)
############################################

## traitement SUCCESSION dans h-01-01-rnsr-successor -> SUCCESSION


succ_temp <- inner_join(maj_id, succession, by=c('id_find'='predecessor_id'))

succ_double <- succ_temp %>% 
  group_by(id_find) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  dplyr::filter(n>1)
succ_double %>% select(succession_type) %>% unique()
supp_succ = c('integration', 'eclatement', 'eclatement-fusion', 'essaimage', 'essaimage-fusion')
succ_double <- succ_double %>% dplyr::filter(succession_type %in% supp_succ)

succ_temp <- anti_join(succ_temp, succ_double, by = c("id", "id_find")) %>% 
  group_by(id_find) %>% 
  mutate(n=n()) %>% 
  ungroup() 
if (any((succ_temp$n)>1)){
  print('ok')
}

succ_temp2 <- inner_join(select(succ_temp, id_old=id), succession, by = c('id_old'='predecessor_id')) %>% 
  unique() %>% 
  group_by(id_old) %>% 
  mutate(n=n()) %>% 
  ungroup() 
succ_double <- succ_temp2 %>% 
  dplyr::filter(n>1)
succ_double %>% select(succession_type) %>% unique()
succ_double <- succ_double %>% dplyr::filter(succession_type %in% supp_succ)

succ_temp2 <- anti_join(succ_temp2, succ_double, by = c("id", "id_old")) %>% 
  group_by(id_old) %>% 
  mutate(n=n()) %>% 
  rename(id_new=id) %>% 
  ungroup() 
if (any((succ_temp2$n)>1)){
  print('ok')
}

succ_temp <- left_join(succ_temp, succ_temp2, by=c('id'='id_old')) %>% 
  select(id_find, id, id_new) %>% 
  mutate(id_new=ifelse(is.na(id_new), id, id_new)) %>% 
  unique()

# succ_temp3 <- inner_join(select(succ_temp2, id_old=id_new), succession, by = c('id_old'='predecessor_id')) 

maj_id <- succ_temp %>% select(id_find=id_new) %>% 
  bind_rows(maj_id) %>% 
  unique()

write.csv2(maj_id, file=paste0(identification, "id_nns_", date_sauve, ".csv"), row.names = FALSE)
####################################################################################################


"API Python RNSR pour nouvelle extraction des UR"
# retour API RNSR
filename = "rnsr_complet_2022-08-03T13-12"
rnsr <- 
  read.csv2(file = paste0(referentiel, filename,".csv"), 
            colClasses = "character", na.strings = "", encoding = 'UTF-8') %>% 
  unique() %>% 
  dplyr::rename(id_scanr=id, id=rnsr) %>% 
  relocate(id, name, date_end, predecessor)


maj_id <- left_join(maj_id, succ_temp, by='id_find') %>% 
  select(id=id_find, id_new) %>% 
  mutate(id_new=ifelse(is.na(id_new), id, id_new))


# traitement des NNS non actifs durant la période PROGRAMME
nns_close <- left_join(maj_id, rnsr, by=c("id_new"="id")) %>% 
  dplyr::filter(date_end < "2014") %>% 
  select(id_new, date_end) %>% 
  unique() %>% 
  arrange(id_new)

print(toString(nns_close$id_new))
modif <- data.frame(id_old=c("199814085Y", "200315014L", "200918439H", '200918477Z', '200717447N'),
                    id_modif=c('200612819M', '201119418N', '200311825V', '200311845S', '200919221H'))


maj_id <- left_join(maj_id, modif, by=c("id_new"="id_old")) %>% 
  mutate(id_new=ifelse(!is.na(id_modif), id_modif, id_new)) %>% 
  select(-id_modif) %>% 
  unique() %>% 
  left_join(rnsr, by=c("id_new"="id")) %>% 
  select(id, id_new, date_end)

# les new fermés typés eclatement... a voir comment les traiter, pour l'instant mis de côté
test <- maj_id %>% 
  dplyr::filter(!is.na(date_end)) %>% 
  select(id_new) %>% 
  inner_join(succession, by=c('id_new'='predecessor_id'))


# maj de key_part avec id_new
key_part <- key_part %>% left_join(maj_id, by = c("s"="id")) %>% 
  dplyr::mutate(s=if_else(!is.na(id_new), id_new, s)) %>% select(-id_new)
key_part <- key_part %>% left_join(maj_id, by = c("p"="id")) %>% 
  dplyr::mutate(p=if_else(!is.na(id_new), id_new, p)) %>% select(-id_new)
key_part <- key_part %>% left_join(maj_id, by = c("id_temp"="id")) %>% 
  dplyr::mutate(id_temp=if_else(!is.na(id_new), id_new, id_temp)) %>% select(-id_new)
key_part <- key_part %>% separate_rows(s,p,id_temp) %>% 
  select(-starts_with("date"))

# recalcule maj_id
maj_id <- cumul_id(pattern_rnsr) %>% 
  rename(id=id_find)

# controler que tous les maj_id sont dans rnsr , sinon relancer api
if(nrow(inner_join(maj_id, rnsr, by="id")) == nrow(maj_id)){
  rnsr <- inner_join(maj_id, rnsr, by="id")
  print('rnsr à jour')
}else{
  print('il manque des nns successeur dans rnsr: relancer API avce les nouveaux')
  anti_join(maj_id, rnsr, by="id") %>% 
    write.csv2(file=paste0(identification, "id_nns_", date_sauve, ".csv"),
               row.names = FALSE)
}

x <- rnsr %>% 
  select(id, name, date_end) %>% dplyr::filter(!(is.na(date_end) | date_end>2013))
if (nrow(x)>0){
  print("maj successeur avec la liste de x")
}else{
  print("ok")
}


# tutelles liées aux labos ci -dessus

tutelle <- rnsr %>%
  select(id, id_scanr, starts_with("tutelle")) %>% 
  separate_rows(starts_with("tutelle"), sep=';') %>% 
  dplyr::filter(tutelle_end >= '2014'| tutelle_end=="#") %>% 
  unique() %>% 
  dplyr::rename(tutelle_rnsr=tutelle_id)

# si NNS sans tutelle
x <- tutelle %>% dplyr::filter(str_detect(tutelle_name, "^#$")) %>% dplyr::rename(id1=id)
if (nrow(x)>0){
  # modif de maj_id en vérifiant la table temp du dessus pour identifier les labos encore actifs
  # maj_id <- maj_id %>% left_join(temp, by = c("id"="id1")) %>% 
  #   dplyr::mutate(id=if_else(!is.na(id1), id1, id)) %>% 
  #   select(-id1, -nb)
  print("nns absent des tutelles ; réimporter données rnsr ; relancer la requete RNSR et tutelle jusqu'à ec que TEMP soit à 0")
}else{
  print("ok")
}


# ajout d'un identifiant aux id manquants
tutelle_add <- 
  readxl::read_xlsx(paste0(identification, "corrected/_tutelle_rnsr.xlsx"), sheet="_tutelle_add", na ="") %>% 
  dplyr::filter(!is.na(id_temp))

# rapprochement pour compléter les id manquants avec tutelle_add (identifié à la main)
tutelle <- tutelle %>% 
  dplyr::mutate(tutelle_rnsr=if_else(str_detect(tutelle_rnsr,"^197534720"), "197534720", tutelle_rnsr),
                tutelle_rnsr=ifelse(tutelle_rnsr=="#"&str_detect(id_scanr, pattern_rnsr, negate = TRUE),
                id_scanr, tutelle_rnsr)) %>% 
  left_join(tutelle_add[c("id_temp", "tutelle_source")], by = "tutelle_source") %>% 
  dplyr::mutate(tutelle_rnsr=if_else(!is.na(id_temp), id_temp, tutelle_rnsr)) %>% 
  select(-id_temp) 

#####################################################################
# pour compléter des id manquants dans les tutelles RNSR
x <- tutelle %>% dplyr::filter(tutelle_rnsr=="#", tutelle_source!="323") %>% 
  select(tutelle_name, tutelle_source, tutelle_rnsr) %>% unique()
if (nrow(x)){
  x %>% dplyr::filter(tutelle_rnsr=="#", tutelle_source!="323") %>% 
  write.csv2(file=paste0(identification, "tutelle_rnsr_", date_sauve, ".csv"))
  print(paste(nrow(x),"tutelle(s) à identifier"))
}else{print("ok")}

#Lancer la recherche Python pour les grid à convertir en ROR

ror <- read.csv2(file=paste0(referentiels_chemin, "GRID_TO_ROR_2021-12-20T12-37.csv"),
                 colClasses = "character") %>% 
  dplyr::mutate(ror_id = paste0("R",as.character(map(str_split(id, "/+"), tail, 1)))) %>% 
  select(tutelle_source, ror_id)


tutelle <- tutelle %>% left_join(ror, by="tutelle_source") %>% 
  dplyr::mutate(tutelle_rnsr = ifelse(!is.na(ror_id), ror_id, tutelle_rnsr)) %>% 
  select(-ror_id)

x <- tutelle %>% dplyr::filter(str_detect(tutelle_rnsr, pattern_grid)) %>% 
  select(tutelle_name, tutelle_source, tutelle_rnsr) %>% unique()
if (nrow(x)){
  x %>% dplyr::filter(str_detect(tutelle_rnsr, pattern_grid)) %>% 
    write.csv2(file=paste0(identification, "tutelle_rnsr_grid_", date_sauve, ".csv"))
}else{print("ok")}
##########################################################################"

maj_id <- maj_id %>% left_join(tutelle[c('id', 'tutelle_rnsr', 'tutelle_source')], by = "id")

if (nrow(maj_id %>% dplyr::filter(tutelle_rnsr=="#"))>0){
  maj_id <- maj_id %>% dplyr::filter(!(tutelle_source=="323"))
  message(nrow(maj_id %>% dplyr::filter(tutelle_rnsr=="#")) ,
        " tutelles avec # à vérifier")
  maj_id <- maj_id %>% select(-tutelle_source)
}
  

#maj de key_part ; maj des nns et ajout des tutelles rnsr
key_part <- key_part %>% left_join(maj_id, by = c("p"="id"))

key_part <- key_part %>% dplyr::rename(t = tutelle_rnsr) %>% unique()

save(key_part, file=paste0(data_result, "data/key_part_h01_", date_sauve,".Rdata"))

save(list=c("tutelle", "rnsr", "succ_temp"), file=paste0(referentiel, "rnsr.Rdata"))

##################################################
# fichier pour recuperation des geolocs des labos
###################################################"
# temp <- tutelle %>% select(id) %>% unique() %>% 
# write.csv2(file=paste0(identification, "nns_api_", date_sauve, ".csv"), row.names = FALSE)



rm(list=c("tutelle_add", "maj_id", "test", "ror", "succession", "succ_type", "succ_double", "succ_temp",
         "succ_temp2", "modif", "x", "nns_close"))
gc()
