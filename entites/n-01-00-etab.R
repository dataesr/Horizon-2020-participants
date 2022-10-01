## chargement referentiel etab

# load(file=paste0(referentiel, "sirene_p6.Rdata"))
# load(file=paste0(referentiel, "rnsr.Rdata"))
# load(file=paste0(referentiel, "ror.Rdata"))
load(file = rdata_last_version(paste0(data_result, "data/"), "part_fr_m01_"))
load(file = rdata_last_version(paste0(data_result, "data/"), "key_part_k01_"))

## CHARGEMENT nouveau FICHIER
filename ="etab_2022-08-10T14-14"

etab_complet <-
  read.csv2(file = paste0(referentiel, filename, ".csv"),
            colClasses = "character", encoding = 'UTF-8', na.strings = "") %>% 
  unique()


if (exists("etab_add")){
  etab_complet <- bind_rows(etab_complet, etab_add) %>% 
    unique()
  print('etabs concatenés')
}else{"passer à la suite"}

pattern_gps = "^.+\\.\\d{1,5}"

etab_complet <- etab_complet %>% 
dplyr::rename(paysage = id) %>% 
  dplyr::mutate_if(is.character, str_squish) %>% 
  left_join(select(s_info, c(siret, cj_lib, siret_siege)), by="siret") %>% 
  mutate(Parent_source=Parent, 
         com_code=ifelse(!is.na(country_code), NA_character_, com_code),
         country_code = str_sub(country_code, 1,2),
         latitude = str_extract(latitude, pattern_gps),
         longitude = str_extract(longitude,pattern_gps)) %>% 
  relocate(sigle_etab, Parent, Enfant, cj_lib, siret_siege, .after=nom) %>% 
  select(-nomOfficiel, -sigle, -NomCourt, -type, -StatutJuridique, 
         -sitesiege) %>% 
  unique()

#########################################################################
# etab rnsr

etab_rnsr <- etab_complet %>% 
  separate_rows(id_autres) %>% 
  dplyr::filter(str_detect(id_autres, pattern_rnsr)) %>% 
  select(idr=id_autres, adresse, city, country_name, country_code, latitude, longitude, com_code) %>% 
  unique()

########################################################################

# suppression des iufm
etab <- etab_complet %>% 
  dplyr::filter(!(soustype %in% c("Entreprise", "Holding"))) %>% 
  dplyr::filter(!(!is.na(siren) & siret_siege%in%c('False', NA_character_) & is.na(Parent) & is.na(Successeur) & actif.inactif == "I"))


# 19751717000316 controler dans le prog
#####################################################################################""
#Traitement des cci / groupe hospices et de leurs ecoles
#hopitaux -> pour les groupes hopitaux chaque structure siege
hosp <- etab %>% 
  dplyr::filter(siren %in% siren_gh, siret_siege=='True', !is.na(siren)) %>% 
  select(paysage, siren) 
#267500452

for (i in 1:nrow(hosp)){
  etab <- etab %>%
    dplyr::mutate(Parent_source = ifelse(!(siret_siege %in% c('True')) & siren %in% c(hosp[i, 'siren']), hosp[i, 'paysage'], Parent_source))
}
  
etab <- etab %>% 
  dplyr::mutate(
    Parent_source = ifelse(!is.na(Parent), Parent, Parent_source),
    Parent = ifelse(Parent_source %in% grp_hosp, NA_character_, Parent_source),
    siret_siege = ifelse(siren %in% siren_gh, "True", siret_siege),
    siret_siege = ifelse(siren %in% siren_gh & !is.na(Parent), "False", siret_siege))


# cci -> suupression du niveau parent cci pour chaque structure ecole
cci_paysage <- etab %>% 
  dplyr::filter(soustype == "Chambre de commerce et d'industrie", siret_siege=="True") %>% 
  select(paysage) %>% 
  dplyr::pull(paysage)

temp <- etab %>% 
  dplyr::filter(siren %in% cci) %>% 
  select(paysage, Parent) %>% 
  dplyr::filter(!is.na(Parent)) %>% 
  separate_rows(Parent, sep=";") %>% 
  dplyr::filter(!(Parent %in% cci_paysage)) %>% 
  group_by(paysage) %>% 
  mutate(Parent=paste(Parent, collapse = ";")) %>% 
  ungroup() %>% 
  select(paysage, Ptemp=Parent)
#130017270

etab <- etab %>% left_join(temp, by="paysage") %>% 
  mutate(Parent=ifelse(!is.na(Ptemp), Ptemp, Parent))
         
ch <- etab %>% 
  dplyr::filter(siren %in% cci, siret_siege=='True', !is.na(siren)) %>% 
  select(paysage, siren) 

for (i in 1:nrow(ch)){
  etab <- etab %>%
    dplyr::mutate(Parent_source = ifelse(!(siret_siege %in% c('True')) & siren %in% c(ch[i, 'siren']), ch[i, 'paysage'], Parent_source))
}

etab <- etab %>% 
 mutate(
   Parent_source = ifelse(!is.na(Parent), Parent, Parent_source),
   Parent = ifelse(Parent %in% cci_paysage, NA_character_, Parent),
   siret_siege=ifelse(siren %in% cci, 'True', siret_siege),
   siret_siege=ifelse(siren %in% cci & !is.na(Parent), "False", siret_siege)) %>% 
  select(-Ptemp) %>% 
  unique()


#traitement autre siege
etab <- etab %>% 
  mutate(siret_siege = if_else(is.na(siret_siege), siege, siret_siege))

#traitement doublon siret
# suppression des doublons siret dont l'un est inactif
dup <- function(){
  return(etab %>% select(paysage, nom, siret, Parent, siret_siege, actif.inactif, fin_paysage) %>% 
    dplyr::filter(!is.na(siret)) %>% 
    group_by(siret) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    dplyr::filter(n>1))
}

#suppression inactif
supp_dup <- dup()
supp_dup <- supp_dup %>% dplyr::filter(actif.inactif=="I" | !is.na(fin_paysage))

for (i in 1:nrow(supp_dup)){
  pat = as.character(supp_dup[i,'paysage'])
 if (nrow(key_part %>%
    dplyr::filter_all(any_vars(str_detect(., pat)))) >0){
    message("Attention, le code paysage qui va être supprimer est présent dans la table key_part", "\n", 
            "appliquer le successeur" )
 }else{
   etab <- etab %>% dplyr::filter(!(paysage %in% supp_dup$paysage))
   message("Suppression des doublons siret inactif et fin paysage" )   
 }
}

#suppression siret si Parent
supp_dup <- dup()
supp_dup <- supp_dup %>% dplyr::filter(!is.na(Parent))

for (i in 1:nrow(supp_dup)){
  etab <- etab %>%
    arrange(paysage) %>% 
    dplyr::mutate(siret = ifelse(paysage %in% c(supp_dup[i, 'paysage']), NA_character_, siret),
                  siret_siege = ifelse(paysage %in% c(supp_dup[i, 'paysage']), 'False', siret_siege))
}

#dernier verification
supp_dup <- dup()
if (nrow(supp_dup)>0){
  print("analyser supp_dup et faire code")
}else{print('ok')}

# si besoin de supprimer des lignes en double
# etab <- etab[-678,]


# verification que les siren à ++siret et non siege ont bien un parent paysage 
test <- etab %>% 
  dplyr::filter(!is.na(siren)) %>% 
  group_by(siren) %>% 
  summarize(n=n()) %>% 
  dplyr::filter(n>1, !(siren %in% c(siren_gh, cci))) %>% 
  left_join(etab, by="siren") %>% 
  dplyr::filter(siret_siege != 'True' & is.na(Parent)) %>% 
  select(siren, Parent)


# MODIFICATION A LA MAIN : A vérifier à chaque fois

# ecole de météo-france / biblio sorbonne -> etab_special
etab <- etab %>%
  dplyr::mutate(
    siret_siege = ifelse(paysage %in% etab_special, 'True', siret_siege),
    Parent = ifelse(paysage %in% etab_special, NA_character_, Parent),
    siret=ifelse(paysage=="13E0w", "18008904700138", siret),
    siret=ifelse(paysage=="GSzlF", "18008904700146", siret),
    Parent=ifelse(paysage=="Ib4aA", "42F7G", Parent),
    siret_siege=ifelse(paysage=="Ib4aA", "False", siret_siege)) %>% 
  dplyr::filter(!(paysage%in%c("Xdjcs","lPOwc", "1f5la", "gGwb2"))) #bibliotheque municipale, univ, asso


####################################################
# traitement siege
siege_manquant <- etab %>% 
  dplyr::filter(!(siren %in% c(cci, siren_gh))) %>% 
  dplyr::filter(!is.na(siren), is.na(Parent), actif.inactif == "A") %>% 
  group_by(siren) %>% 
  mutate(nb=ifelse(siret_siege=='True', 1, 0)) %>% 
  dplyr::summarize(n=sum(nb)) %>% 
  dplyr::filter(n<1)

if (nrow(siege_manquant)>0){
  message(nrow(siege_manquant), ' siege(s) manquant(s)')
  # message('toujours ', nrow(siege_manquant))
  sir <- as.vector(siege_manquant$siren)
  etab <- etab %>%
    mutate(siret_siege=ifelse(siren %in% sir, 'True', siret_siege))
}

# siege en plus 
siege <- etab %>% 
  dplyr::filter(!is.na(siren), siret_siege=="True", !(siren %in% c(cci,siren_gh)), !(paysage %in% etab_special)) %>% 
  group_by(siren) %>% 
  mutate(nb=ifelse(siret_siege=='True', 1, 0)) %>% 
  dplyr::summarize(n=sum(nb)) %>% 
  dplyr::filter(n>1)

if (nrow(siege)>0){
  # message(nrow(siege %>% dplyr::filter(n>1)), ' siege(s) en trop')
  siege <- siege %>% 
    left_join(etab, by='siren') %>% 
    dplyr::filter(is.na(Parent) | siret_siege == 'True') %>% 
    select(siren, nom) %>% 
    left_join(sirene, by = "siren") %>%
    dplyr::filter(siege=='True', !is.na(siren)) %>%
    select(siren, siret) %>% unique()
  message(nrow(siege), ' siret siege')
  for (i in 1:nrow(siege)){
    sir <- as.vector(unique(siege$siren))
    st <- as.vector(unique(siege$siret))
    etab <- etab %>%
        mutate(siret_siege=ifelse(siren %in% sir & !(siret %in% st), 'False', siret_siege))
  }
}

siege <- etab %>% 
  dplyr::filter(!is.na(siren), siret_siege=="True", !(siren %in% c(cci,siren_gh)), !(paysage %in% etab_special)) %>% 
  group_by(siren) %>% 
  mutate(nb=ifelse(siret_siege=='True', 1, 0)) %>% 
  dplyr::summarize(n=sum(nb)) %>% 
  dplyr::filter(n>1)
#########################################################################

# verif <- etab_extract %>% dplyr::filter(str_detect(id, pattern_paysage)) %>% 
#   left_join(etab_complet, by=c("id"="paysage"))



##########################################################################
save(etab, etab_complet, etab_rnsr, file=paste0(referentiel, "etab.Rdata"))

rm(list=c("siege_manquant", "siege", "supp_dup", "test", "ch", "hosp"))
rm(list=ls(pattern = "^(temp)"))
gc()

######################################################################################################

# verification de l'evolution des organismes (fusion, autres...)
