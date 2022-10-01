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
    select(id=p) %>% bind_rows(temp,temp2) %>% unique()
  return(maj_id)
}

maj_id <- cumul_id(pattern_rnsr)

write.csv2(maj_id, file=paste0(identification, "id_nns_", date_sauve, ".csv"), row.names = FALSE)



"API Python RNSR pour nouvelle extraction des UR"
# retour API RNSR
filename = "rnsr_2022-07-18T11-24"
rnsr <- 
  read.csv2(file = paste0(referentiel, filename,".csv"), 
            colClasses = "character", na.strings = "", encoding = 'UTF-8') %>% 
  unique() %>% 
  dplyr::rename(id_scanr=id, id=rnsr) %>% 
  relocate(id, name, date_end, predecessor)

if (exists("rnsr_add")){
  rnsr <- bind_rows(rnsr, rnsr_add)
  print("rnsr concaténés")
}else{print("passer à la suite")}

# POUR EXTRAIRE LES SUCCESSORS
# ECLATEMENT de predecessor -> id
# INTEGRATION de predecessor -> id
# ECLATEMENT-FUSION de predecessor -> id
#correction dans rnsr -> inversion dans les infos integration
#modicfication basée sur les infos en date de 12/2021
rnsr <- rnsr %>% 
  mutate(#predecessor=ifelse(id=="201320680B", "199318205Y", predecessor),
         #predecessor=ifelse(id=="201320681C", "200017663D;200717404S", predecessor),
         #succession_type=ifelse(id %in% c("201320681C"), "integration", succession_type),
         predecessor=ifelse(id %in% c("200017663D","200717404S","199318205Y"), NA_character_, predecessor),
         succession_type=ifelse(id %in% c("200017663D","200717404S","199318205Y"), NA_character_, succession_type))



succ <- rnsr %>% 
  select(id, predecessor, succession_type) %>% 
  separate_rows(predecessor, succession_type,  sep=";") %>% 
  unique() %>% 
  group_by(predecessor, succession_type) %>% 
  dplyr::mutate(nb_succ = n(), succession_type=replace_special_letters(succession_type)) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(predecessor))


# traitement des NNS non actifs durant la période PROGRAMME
nns_close <- left_join(maj_id, rnsr) %>% 
  dplyr::filter(date_end < "2014") %>% 
  select(id, date_end) %>% 
  left_join(succ, by=c("id"="predecessor"))

succ2 <- inner_join(select(rnsr, id_rnsr=id, date_end), succ, by=c('id_rnsr'='predecessor')) 

#remove les id éclatés en ++nns
succ2 <- succ2 %>% 
  dplyr::filter(!(str_detect(succession_type, "eclatement") & nb_succ>1))
#remove les id essaimés sans date de fin
succ2 <- succ2 %>% 
  dplyr::filter(!(str_detect(succession_type, "essaimage") & is.na(date_end)))

succ2 <- succ2 %>% select(-nb_succ) %>% 
  unique() %>% 
  group_by(id_rnsr) %>% 
  mutate(nb_succ=n()) %>% 
  ungroup()

#verif si reste des nb_succ > 1
if (nrow(succ2 %>% dplyr::filter(nb_succ>1)) > 1){
  verif <- succ2 %>% dplyr::filter(nb_succ>1)
  print('contrôler verif pour être sûre que ça ne crée pas de doublons')
}else{print("ok")}
  
succ2 <- succ2 %>% dplyr::filter(nb_succ < 2)       

modif <-
  data.frame(predecessors=c("199813981K","200014558D","200816917H","200417432K","200817568R","200918477Z","200716492A","200510789K","200216581U","199814085Y","200919734R","201622186A","201320568E","200815466F","201119732E"),
            id_modif=c("199813979H","201722392V","201220339K","201220322S","201220379D","200311845S","200919261B","199620141P","201220148C","200612819M","201522372G", "201122738X", "200711896E","200822710E", "201521685K"))

#ajout de id_modif (fait àla main) à maj_id ; a verifier après l'ajout des infos successeur
maj_id <- left_join(maj_id, modif, by =c("id"="predecessors"))


#jointure avec succ2 (avant 2 223 lignes)
maj_id <- maj_id %>% 
  left_join(select(succ2, c(id_succ=id, id_rnsr)), by=c("id"="id_rnsr")) %>% 
  unique()

maj_id <- maj_id %>% 
  mutate(id_succ=ifelse(is.na(id_succ)&!is.na(id_modif), id_modif, id_succ),
         test=ifelse(!is.na(id_succ)&!is.na(id_modif), 1, 0))

if (nrow(maj_id %>% dplyr::filter(id_modif!=id_succ, test==1))>0){
  print("verifier les lignes avec id_modif et id_succ remplis")
}else{
  maj_id <- maj_id %>% select(-test, -id_modif)
  print('ok')
}

# jointure avec rnsr pour recuperer annee de fermeture
maj_id <- maj_id %>% 
  left_join(select(rnsr, c(id, date_end)), by="id") %>% 
  left_join(select(rnsr, c(id, end_succ=date_end)), by=c("id_succ"="id")) %>% 
  unique()

# verification de la cohérence des dates de fermeture 
if (nrow(maj_id %>% 
    dplyr::filter((date_end>end_succ) | (is.na(date_end)&!is.na(end_succ))))>0){
verif <- maj_id %>% 
  dplyr::filter((date_end>end_succ) | (is.na(date_end)&!is.na(end_succ)))
print("verifier verif -> date_end > end_succ")
}else{
  maj_id <- maj_id %>% select(-date_end, -end_succ)
  print("ok")}
 
# si un probleme dans les successions avec la date ; suppression de la ligne
if (nrow(verif)>0){
  maj_id <- anti_join(maj_id, verif) 
    
}

# si plusieurs niveaux de succession
succ_boucle <- maj_id %>% 
  select(id1=id_succ) %>% 
  dplyr::filter(!is.na(id1)) %>% 
  left_join(maj_id, by = c("id1"="id")) %>% 
  dplyr::filter(!is.na(id_succ), id1!=id_succ) %>% 
  select(id1, id2=id_succ) %>% 
  left_join(maj_id, by = c("id2"="id"))
  
if (nrow(succ_boucle)>0){
  print("faire le code pour mettre à jour les id de maj_id")
# maj_id <- maj_id %>% 
#   left_join(succ_boucle[c("id1", "id2")], by = c("id"="id1")) %>% 
#   dplyr::mutate(id_new=if_else(!is.na(id2), id2, id_new)) %>% 
#   select(id, an, id_new) %>% unique()
}else{print('ok')}
  
  
  
# verif_eclate <- maj_id %>% #dplyr::filter(!is.na(id_new)) %>%
#   group_by(id) %>% 
#   dplyr::mutate(id_succ=paste(na.omit(id_succ), collapse=",")) %>% 
#   select(id, id_succ) %>% unique() %>% 
#   dplyr::filter(str_detect(id_succ, ","))
# 
# maj_id <- anti_join(maj_id, verif_eclate, by ="id") %>% 
#   bind_rows(verif_eclate) %>% 
#   dplyr::mutate(id_new=if_else(id_new==id, NA_character_, id_new)) %>% 
#   unique() %>% 
#   dplyr::mutate(id_new=str_replace_all(id_new, ",NA", ""), id_new=str_replace_all(id_new, "NA,", ""))
# 
# nrow(maj_id %>% select(id) %>% dplyr::count(id) %>% dplyr::filter(n>1))

maj_id <- maj_id %>% dplyr::filter(!is.na(id_succ)) %>% 
  group_by(id) %>% 
  mutate(nb=n()) %>% 
  ungroup() %>% 
  unique()
if (nrow(maj_id %>% dplyr::filter(nb>1)>0)){
  print("coder pour collapse id_succ")
}else{
  maj_id <- maj_id %>% select(-nb)
  print('ok')
}

# maj de key_part avec id_new
key_part <- key_part %>% left_join(maj_id, by = c("s"="id")) %>% 
  dplyr::mutate(s=if_else(!is.na(id_succ), id_succ, s)) %>% select(-id_succ)
key_part <- key_part %>% left_join(maj_id, by = c("p"="id")) %>% 
  dplyr::mutate(p=if_else(!is.na(id_succ), id_succ, p)) %>% select(-id_succ)
key_part <- key_part %>% left_join(maj_id, by = c("id_temp"="id")) %>% 
  dplyr::mutate(id_temp=if_else(!is.na(id_succ), id_succ, id_temp)) %>% select(-id_succ)
key_part <- key_part %>% separate_rows(s,p,id_temp)

# recalcule maj_id
maj_id <- cumul_id(pattern_rnsr)

# controler que tous les maj_id sont dans rnsr , sinon relancer api
if(nrow(inner_join(maj_id, rnsr, by="id")) == nrow(maj_id)){
  rnsr <- inner_join(maj_id, rnsr, by="id")
  print('rnsr à jour')
}else{
  print('il manque des nns successeur dans rnsr: relancer API avce les nouveaux')
  anti_join(maj_id, rnsr, by='id') %>% 
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
x <- tutelle %>% dplyr::filter(tutelle_rnsr=="#"|str_detect(tutelle_rnsr, pattern_grid)) %>% 
  select(tutelle_name, tutelle_source, tutelle_rnsr) %>% unique()
if (nrow(x)){
  x %>% dplyr::filter(tutelle_rnsr=="#") %>% 
  write.csv2(file=paste0(identification, "tutelle_rnsr_", date_sauve, ".csv"))
  x %>% dplyr::filter(str_detect(tutelle_rnsr, pattern_grid)) %>% 
    write.csv2(file=paste0(identification, "tutelle_rnsr_grid_", date_sauve, ".csv"))
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

save(list=c("tutelle", "rnsr", "succ"), file=paste0(referentiel, "rnsr.Rdata"))

##################################################
# fichier pour recuperation des geolocs des labos
###################################################"
# temp <- tutelle %>% select(id) %>% unique() %>% 
# write.csv2(file=paste0(identification, "nns_api_", date_sauve, ".csv"), row.names = FALSE)



rm(list=c("tutelle_add", "maj_id", "verif", "ror", "succ", "succ2", "succ_boucle",
          "modif", "x", "nns_close"))
gc()
