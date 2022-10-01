load(file = rdata_last_version(paste0(data_result, "data/"),"part_key_k01_"))
load(file = rdata_last_version(paste0(data_result, "data/"), "part_fr_k01_"))
# load(file=paste0(chemin_pc, "participants/referentiels/sirene.Rdata"))


filename='sirene_extract_2022-08-04T09-05'
sirene <- read.csv2(file=paste0(referentiel, filename, '.csv'), encoding = 'UTF-8', 
                    colClasses = "character", na.strings = "")


#ajout de lignes extraites après
if (exists("sirene_add")){
  sirene <- bind_rows(sirene, sirene_add) %>%
    unique()
  print('etabs concatenés')
}else{"passer à la suite"}


# fonction qui teste si le nombre de caractères ert correct selon la variable
code_longueur <- function(var, longueur){
  return(paste0(str_dup(0, (longueur - nchar(var))), var))
}

sirene <- sirene %>% 
  mutate(siren=code_longueur(siren, 9), siret=code_longueur(siret, 14),
         com_code = ifelse(com_code=="0None", NA_character_, com_code)) %>% 
  select(siren, siret, siege, etat_ul, etat_et, sigle, nom, ens, nom_entier, nom_perso,
         nom_ul, cat, cat_an, cj, naf_ul, naf_et, rna,
         adresse, voie_comp, cp, ville, pays_code, com_code, ville_etr)

com_code <- sirene %>% dplyr::filter(nchar(com_code)!=5)
if (nrow(com_code)>0){
  print('vérifier les com_code')
}else{
  rm(com_code)
  print('ok')}

#VERIFICATION SIEGE
if (nrow(sirene %>% dplyr::filter(siege=="True") %>% dplyr::count(siren)) == nrow(sirene %>% select(siren) %>% unique())){
  print("ok")
} else if (nrow(sirene %>% dplyr::filter(siege=="True") %>% dplyr::count(siren)) > nrow(sirene %>% select(siren) %>% unique())){
  print("il n'y a des siege en doublon : verifier supp_siege")
  supp_siege <- sirene %>% dplyr::filter(siege=="True") %>% select(siren, siege) %>% 
    dplyr::count(siren, siege) %>% dplyr::filter(n>1)
} else if (nrow(sirene %>% dplyr::filter(siege=="True") %>% dplyr::count(siren)) < nrow(sirene %>% select(siren) %>% unique())){
  print("il y a des siren sans siege : verifier no_siege")
  no_siege <- sirene %>% select(siren, siege) %>% mutate(n=ifelse(siege=="True", 1,0)) %>% 
    group_by(siren) %>% summarize(sum(n))
}


# modifier  RNA par le siren si dispo 
verif <- sirene %>% 
  select(siren, rna) %>% 
  dplyr::filter(!is.na(rna)) %>% 
  unique() 

for (i in 1:nrow(verif)){
  key_part <- key_part %>%
   mutate(across(.cols=c(s, p, id_temp, t), .fns=~str_replace(.,verif[i,'rna'], verif[i,'siren'])))
  
  part_fr <- part_fr %>%   
   mutate(across(.cols=c(organizations_id), .fns=~str_replace(.,verif[i,'rna'], verif[i,'siren'])))
}

#############################################################################################################
#	Établissement d'hospitalisation
Hosp_temp <- sirene %>% 
  dplyr::filter(cj=="7364", naf_ul %in% c("85.1A", "86.10Z")) %>% 
  group_by(siren) %>%
  dplyr::summarise(nb =n()) %>% 
  unique()

if (length(Hosp_temp$siren)!=length(hospital)){
  message(length(Hosp_temp$siren), ",", length(hospital))
  paste0(sprintf("'%s'", Hosp_temp$siren), collapse = ", ")
}else{print('rien à ajouter pour les hopitaux')}

groupe_hospitalier <- Hosp_temp$siren[which(Hosp_temp$nb >= 25)]
print(toString(groupe_hospitalier))

#	Établissement CCI
CCI <- sirene %>% 
  dplyr::filter(cj=="7381") %>%
  select(siren, nom_ul) %>% 
  unique() %>% 
  arrange(siren)

if (length(CCI$siren)!=length(cci)){
  message(length(cci), ",", length(CCI$siren))
  paste0(sprintf("'%s'", CCI$siren), collapse = ", ")
}


##############################################################################################################
#traitement des cj 

cj <- read.csv2(file = paste0(participants, "exe/fichiers_parametrage/cj.csv"), 
       colClasses = "character", na.strings = "", encoding = 'UTF-8')


cj_new <- read.csv2(file = paste0(sirene_chemin, "nomenclature/cj_2020.csv"), 
           colClasses = "character", na.strings = "", encoding = 'UTF-8') %>% 
  select(-X) 

cj <- left_join(cj, cj_new) %>% 
  select(-uri, -intitule)

cj_ajout <- anti_join(cj_new, cj) %>% 
  select(-uri)
if (nrow(cj_ajout)>0){
  write.csv2(cj_ajout, file= paste0(participants, "exe/fichiers_parametrage/cj_ajout_", date_sauve,".csv"))
  
} else{
  print("ok")
}


# ajout d'un libelle maison au code cj [ETAT_AUT, ENT, ETAB_PUB...]
ej <- sirene %>% 
  select(siren, cj) %>% 
  unique()

#ajout cj_lib à sirene
sirene <- sirene %>% 
  left_join(cj, by = c("cj"="code"))

#correction gendarmerie
sirene <- sirene %>% 
  dplyr::mutate(cj_lib=if_else(siren == "786262410", "ETAT_AUT", cj_lib))

# part_fr <- part_fr %>% select(-cj, -cj_lib)
siren <- part_fr %>%
  dplyr::filter(str_detect(organizations_id, pattern_siren)|str_detect(organizations_id, pattern_siret)) %>%
  dplyr::mutate(s=str_sub(organizations_id, 1,9)) %>%
  left_join(ej, by=c("s"="siren")) %>%
  relocate(cj)


s_non_diffusible <- c('811640184', '152000014', '750925844')
oubli <- siren %>% 
  dplyr::filter(is.na(cj), !is.na(s), !(s %in% s_non_diffusible)) %>% 
  select(s) %>% 
  unique()
recup_source <- oubli %>% 
  inner_join(sirene, by=c("s"="siren"))

if (nrow(oubli)>0){
  write.csv2(oubli, file=paste0(identification, "identifiant_api.csv"), row.names = FALSE)
  # verifier si les siren ne font pas partis des siren non diffusibles listés dessous
  print(toString(oubli$s))
  }else{ print("ok")
  }


# les fermés
ferme <- sirene %>% 
  dplyr::filter(etat_ul=="cessée") %>% 
  select(siren, nom, cj_lib) %>% 
  dplyr::filter(cj_lib %in% c("EPSCP", "ETAT_AUT"))
#######################################################################
# vrification des cj par siren

verif <- sirene %>% 
  select(siren, cj_lib) %>% 
  unique() %>% 
  dplyr::count(siren) %>% 
  dplyr::filter(n>1) %>% 
  left_join(sirene)


# si rien à corriger jointure avec ej pour l'ajout du cj
part_fr <- part_fr %>% 
  left_join(ej, by=c("organizations_id"="siren"))

# siren non diffusible et correction du cj de la gendarmerie (à revoir)
part_fr <- part_fr %>% 
  dplyr::mutate(cj=if_else(organizations_id == "750925844", "1000", cj),
        cj=if_else(organizations_id == "152000014", "7160", cj),
        cj=if_else(organizations_id == "786262410", "7120", cj),
        cj=if_else(organizations_id == "811640184", "1000", cj))
        # cj=if_else(participant_pic=="905654081", "9220", cj),
        # cj=if_else(participant_pic=="949696252", "9220", cj))
        # organizations_id=if_else(participant_pic=="905654081", NA_character_, organizations_id),
        # organizations_id=if_else(participant_pic=="949696252", NA_character_, organizations_id))

# ajout du libelle maison cj_lib
part_fr <- part_fr %>% 
  left_join(cj, by=c("cj"="code")) %>% 
  relocate(id, cj_lib, cj, .after=participates_as)


# liste les siret siege ;  à conserver comme table pour la suite du prog
s_info <- sirene %>% 
  dplyr::filter(siege=="True") %>% 
  select(siren, siret, cj_lib, cat, siret_siege=siege) %>% 
  unique()
x <- s_info %>% 
  dplyr::count(siren, siret) %>% 
  dplyr::filter(n<1)


save("part_fr", file=paste0(data_result, "data/part_fr_m01_", date_sauve, ".Rdata"))

save(list=c("sirene","s_info"), file=paste0(referentiel, "sirene_p6.Rdata"))
###########################################################################################
rm(list=c("siren","cj_new", "cj_ajout", "ej", "oubli", "recup_source",  "x", 'sirene_add',
         'verif', "Hosp_temp", 'CCI'))
gc()



# CSV pour rapprochement 
write.csv2(siret[,c("NOM", "SIGLE", "NOMEN_LONG", "ADRESSE", "CODE_POSTAL", "VILLE", "SIRET")], "sirene.csv", row.names = FALSE)


