load(file=rdata_last_version(paste0(data_result, "data/"), "part_fr_p01_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "key_part_p02_"))
load(file=paste0(referentiel, "etab.Rdata"))
# load(file=paste0(participants, "referentiels/grid_pic.Rdata"))
load(file=paste0(participants, "referentiels/sirene_p6.Rdata"))
ref_info_old <- load_one_obj(rdata = paste0(referentiel, "ref_info_", livraison, ".Rdata"), 'ref_info')


######################################################################################
#####################################################################################
#####      
####       AJOUTER Le NIVEaAU GROUPE eTAB dans prog 12
###
load(file=paste0(data_result, "data/groupe_grp12.Rdata"))


# suppression du groupe centre du papier qui est un CTI ; à revoir
groupe <- groupe %>% dplyr::filter(id != "gent129")
################################################################


key_part <- key_part %>% dplyr::mutate_if(is.character, str_squish)


################################################################################
## CREATION LISTE REF + CAT + geo...
# affectation des infos dans nouvelles variables finales
# part_fr <- part_fr %>% 
#   mutate(organizations_id=ifelse(is.na(organizations_id), paste0("^NonIdent", participant_pic), organizations_id)) 

# extraction des structures  uniquement identifiées dans organizations_id et non rnsr dans participant_id
temp <- key_part %>% 
  dplyr::filter(str_detect(organizations_id, "^NonIdent", negate=TRUE)) %>% 
  dplyr::mutate(participant_id=if_else(str_detect(participant_id, pattern_rnsr), NA_character_, participant_id))


cumul_id <- function(data, v){
  x <-data %>% select(id=all_of(v)) %>% separate_rows(id) %>% unique()
  return(x)
}

# liste ID pour création du ref
ref_temp <- 
  data.frame(id=character(0)) %>% 
  rbind(cumul_id(temp,'organizations_id'), cumul_id(temp, 'participant_id'),
        cumul_id(temp, 'tutelle_id'), cumul_id(temp, 'participant_nns')) %>% 
  unique() %>% 
  dplyr::filter(!is.na(id)) %>% 
  dplyr::mutate(type=as.character(map(id, id_type)))


count_type <- function(){
  ref_temp %>% dplyr::filter(!is.na(id)) %>% dplyr::count(type)
}
count_type()

####################################################################################
#lien avec rnsr
rnsr <- load_one_obj(rdata=paste0(referentiel, "rnsr.Rdata"), "rnsr")
temp1 <- ref_temp %>% 
  dplyr::filter(type=="r") %>% 
  unique() %>% 
  inner_join(rnsr, by = "id") %>% 
  mutate(cj_lib = "STRUCT") %>% 
  select(type, id, libelle=name, acronym, nat, cj_lib) %>% 
  unique()
if (nrow(temp1)==ref_temp %>% 
    dplyr::count(type) %>% 
    dplyr::filter(type=="r") %>% 
    select(n)){
  print("ok")
}

# lien avec sirene SIREN
temp2 <- ref_temp %>% 
  dplyr::filter(type=="s") %>% 
  unique() %>%
  inner_join(sirene, by=c("id"="siren")) %>% 
  dplyr::filter(siege=="True") %>% 
  select(type, id, libelle=nom, acronym=sigle, nat=cat, cj_lib) %>% 
  unique()
if (nrow(temp2) == ref_temp %>% 
    dplyr::count(type) %>% 
    dplyr::filter(type=="s") %>% 
    select(n)){
  print("ok")
}else{
  x <- anti_join(ref_temp, temp2, by = "id") %>% 
    dplyr::filter(type=="s") %>% 
    select(id)
  print(paste(toString(x), "siren non diffusible, prendre infos dans part_fr"))
  x <- left_join(x, part_fr, by = c("id"="organizations_id")) %>% 
    select(id, libelle=name_source, acronym=acronym_source, nat=cj_lib) %>% 
    unique()
  temp2 <- bind_rows(temp2, x)
}

count_type()

# lien avec sirene SIRET
temp3 <- ref_temp %>% 
  dplyr::filter(type=="st") %>% 
  unique() %>% 
  inner_join(sirene, by=c("id"="siret")) %>% 
  select(type, id, libelle=nom, acronym=sigle, nat=cat, cj_lib) %>% 
  unique()
if (nrow(temp3) == ref_temp %>% 
    dplyr::count(type) %>% 
    dplyr::filter(type=="st") %>% 
    select(n)){
  print("ok")
}else{
  # x <- anti_join(ref, temp3, by = "id") %>% filter(type=="st") %>% select(id)
  print("prog à faire si diff de siret")
}

count_type()

# lien avec paysage
temp4 <- ref_temp %>% 
  dplyr::filter(type=="p") %>% 
  unique() %>% 
  inner_join(etab, by=c("id"="paysage")) %>% 
  select(type, id, libelle=nom, acronym=sigle_etab, nat=soustype, cj_lib, Secteur) %>% 
  unique()
if (nrow(temp4) == ref_temp %>% 
    dplyr::count(type) %>% 
    dplyr::filter(type=="p") %>% 
    select(n)){
  print("ok")
}else{
  x <- anti_join(ref_temp, temp4, by = "id") %>% 
    dplyr::filter(type=="p") %>% 
    select(id) %>% 
    unique()
  x <- left_join(select(x, c(paysage=id)), etab_complet, by="paysage")
  if (nrow(x)>0){
    etab <- bind_rows(etab, x)
    temp4 <- ref_temp %>% 
      dplyr::filter(type=="p") %>% 
      unique() %>% 
      inner_join(etab, by=c("id"="paysage")) %>% 
      select(type, id, libelle=nom, acronym=sigle_etab, nat=soustype, cj_lib, Secteur) %>% 
      unique()
    print(paste("extraction de x de etab_complet :", x$paysage))
    }else{
    print(paste("extraire etab manquant dans api paysage :", x))
    write.csv2(x, file=paste0(identification, "etab_api_", date_sauve, ".csv"), row.names = F)
    }
}

# lien avec assoc/finess/pic
temp5 <- ref_temp %>% 
  dplyr::filter(type %in% c("a", "f", "pi",NA_character_), !is.na(id)) %>% 
  unique() %>% 
  inner_join(part_fr, by=c("id"="organizations_id")) %>% 
  select(type, id, libelle=name_source, acronym=acronym_source, cj_lib) %>% 
  mutate(cj_lib = ifelse(str_detect(id, pattern = pattern_assoc), "ISBL", cj_lib)) %>% 
  unique()
if (nrow(temp5) == ref_temp %>% 
    dplyr::filter(type %in% c("a", "f", "pi", NA_character_), !is.na(id)) %>% 
    dplyr::count(type) %>% 
    select(n)%>% summarise(sum(n))){
  print("ok")
}else{
  x <- anti_join(ref_temp, temp3, by = "id") %>% filter(type=="st") %>% select(id)
  print("prog à faire si diff de part_fr")
}

#lien avec grid (ror_base)
ror <- load_one_obj(rdata=paste0(referentiel, "ror.Rdata"), "ror")
temp6 <- ref_temp %>% 
  dplyr::filter(type == "ro") %>%  
  unique() %>% 
  inner_join(ror, by="id") %>% 
  select(type, id, libelle=name, country_code, latitude, longitude) %>% 
  unique()
if (nrow(temp6) == ref_temp %>% 
    dplyr::count(type) %>% 
    dplyr::filter(type=="ro") %>% 
    select(n)){
  print("ok")
}else{
  x <- anti_join(ref_temp, temp6, by = "id") %>% 
    dplyr::filter(type=="ro") %>% 
    select(id)
  if (nrow(x)>0|nrow(x)==0){
    print("tous les ror son identifiés ; vérifier qu'il n'y a pas de doublons ror sur plusiuers pays")
    xx <- temp6 %>% filter(is.na(libelle))
    if (nrow(xx)>0){
      temp6 <- temp6 %>% 
        dplyr::filter(!is.na(libelle))
      print("suppression d'observations dans temp6 ; corriger à la source si possible")
    }
  }
}

count_type()

# rnsr, paysage, siren, siret, ror, assoc, finess, pic
x <- bind_rows(temp1,temp2,temp3,temp4, temp5,temp6)
verif <- anti_join(ref_temp, x, by="id") %>% 
  dplyr::filter(!is.na(id))
if (nrow(verif)==2){
  print("reste les non identifiés calédonie ")
}else{print("verifier la liste dans verif")
  }

ref <- anti_join(ref_temp,x,by='id') %>% 
  dplyr::filter(!is.na(id), id != "NC") %>% 
  left_join(part_fr, by=c('id'='organizations_id')) %>% 
  select(type, id, libelle=name_source, acronym=acronym_source, cj_lib) %>% 
  unique() %>% 
  bind_rows(x)


#########################################################################################
ref <- as_tibble(ref) %>% 
  rename_all(function(x){paste0("new.", x)})

# A revoir
ref_old <- load_one_obj(rdata=paste0(referentiel, "ref_info_2021_11.Rdata"), "ref_info")

# comparaison avec les ref précédentes

verif <- left_join(ref, ref_old, by=c("new.id"="id_ref")) %>% 
  mutate(diff = ifelse(new.cj_lib!=category, 1, 0)) %>% 
  dplyr::mutate(new.libelle=str_to_sentence(new.libelle)) %>% 
  select(new.id, new.libelle, new.acronym, new.cj_lib, new.nat, name_ref, name, acronym, category, category_2, sector, diff) %>% 
  left_join(select(groupe, c(siren, grp_id=id)), by=c("new.id"="siren")) %>% 
  relocate(grp_id, .after=new.nat)
if (nrow(verif)>0){
# export pour maj categorie et secteur libelle ...
write.csv2(verif, paste0(participants, "nomenclatures/ref_maj_",date_sauve,".csv"), na = "", 
           row.names = FALSE)#, fileEncoding = "UTF-8")
}else{print("ok")}

########################################################################################
#######################################################################################
# importation données structures nettoyées
# 
# ref_net <- read.csv2(file=paste0(participants, "nomenclatures/info_struct_2021_03_19.csv"), 
#                      na=c("", "#N/A", "0")) %>% select(-X)
ref_net <- 
  readxl::read_xlsx(
    paste0(participants, "nomenclatures/_traitement_info_struct.xlsx"), 
    sheet="ref_info", na=c("", "#N/A", "0"), col_types = "text")

# ref_net <- inner_join(ref_net, ref[c("id")], by = "id")
ref_net <- ref_net %>% 
  dplyr::mutate(nb=nchar(acronym), 
                sector=ifelse(category_2=="ENT", "PR", 
                        ifelse(category=="EPIC_AUT", "PU", sector))) %>% 
  select(id=new.id, name, acronym, category, sector, nb) %>% 
  dplyr::mutate_if(is.character,str_squish)
max(ref_net$nb, na.rm=T)

ref_net <- ref_net %>%
# ref_info <- ref_info %>%   
  dplyr::mutate(
    sector=if_else(is.na(sector) & category %in% c("ENT", "ISBL", "ENT_ETR"), "PR",
                  if_else(str_to_lower(sector)=="privé", "PR", 
                  if_else(str_to_lower(sector)=="public", "PU", 
                  if_else(sector== "Mixte", "PR",
                  if_else(sector== "PPP", "PR",
                  sector))))),
    sector_name=if_else(sector=="MIX", "Mixte", if_else(sector=="PU", "Public", 
                  if_else(sector=="PR", "Privé", NA_character_))))
    
    
ref_net <- ref_net %>% 
  mutate(acronym = ifelse(str_detect(acronym, "(\\.\\.\\.)"), NA_character_, acronym),
   test = 
    ifelse(str_detect(tolower(name), "(?<=\\b)sas(?=\\b)|(?<=\\b)s.a.s(?=\\b)|(?<=\\b)(s.a.s\\.)(?=\\b)|(?<=\\b)ass(?=\\b)|(?<=\\b)asso(?=\\b)"), 1, 0),
   name_temp = gsub("(?<=\\b)sas(?=\\b)|(?<=\\b)s.a.s(?=\\b)|(?<=\\b)(s.a.s\\.)(?=\\b)|(?<=\\b)ass(?=\\b)|(?<=\\b)asso(?=\\b)", "", name, 
                    ignore.case = TRUE, perl = TRUE),
   acronym = gsub("(?<=\\b)sas(?=\\b)|(?<=\\b)s.a.s(?=\\b)|(?<=\\b)(s.a.s\\.)(?=\\b)", "", acronym, 
                    ignore.case = TRUE, perl = TRUE),
   acronym = ifelse(is.na(acronym) & nchar(name_temp)<=25, name_temp,
              ifelse(is.na(acronym) & nchar(name_temp)>25, paste0(str_sub(name_temp,1,25), "."), acronym))
   ) %>% 
  dplyr::rename(id_ref=id, name_ref=name, acronym_ref=acronym) %>% 
  select(-test, -name_temp, -nb)

# identification des entreprises publiques ; maj secteur -> PUB
ent_pub <- readxl::read_excel(paste0(referentiel, "entreprises_publiques_2017p.xls"), na="") 
ent_pub <-as.character(ent_pub$SIREN)

#maj du secteur des entreprises publiques
ref_net <- ref_net %>% dplyr::mutate(ent_pub = if_else(id_ref %in% ent_pub, "Y", NA_character_))

#verification que les secteurs sont bien remplis
nrow(ref_net %>% dplyr::filter(is.na(sector)))



ref_info <- left_join(ref_net, groupe, by = c("id_ref"="siren"))

ref_info <- ref_info %>% 
  left_join(sirene[c("siren", "cat")], by = c("id_ref"="siren")) %>% 
  unique() %>% 
  dplyr::mutate(id_gref=if_else(is.na(id), id_ref, id),
         cat=if_else(cat=="GE", "GE_ENT", cat),
         category=if_else(category == "ENT" & category != "CTI" & !is.na(cat), cat, category),
         category=if_else(str_detect(id_gref, "^gent") & category != "CTI", "GE_ENT", category),
         name=if_else(is.na(name), name_ref, name),
         acronym=if_else(is.na(acronym), acronym_ref, acronym),
         sector_orig=sector, sector=ifelse(!is.na(sector_groupe), sector_groupe, sector)) %>% 
  select(-id, -sector_name, -sector_groupe)

# vérification du secteur pour les groupes ; si double sectorisation -> PU
temp <- ref_info %>% 
  dplyr::filter(category=="GE_ENT") %>% 
  select(id_gref, sector) %>% 
  unique() %>% 
  dplyr::count(id_gref) %>% 
  dplyr::filter(n>1)

if (nrow(ref_info)>1){
  temp <- temp$id_gref
  ref_info <- ref_info %>% 
    dplyr::mutate(sector=if_else(id_gref %in% temp, "PU", sector))
  print("doublons corrigés")
} else{print("ok")}


grp <- ref_info %>% 
  dplyr::filter(startsWith(id_gref, "gent")) %>% 
  select(id_gref, name) %>% 
  unique()

######################################################################################

# nomenclatures 

# cat <- ref_net %>% select(category) %>% unique()
# write.csv2(cat, file=paste0(participants, "nomenclatures/category.csv"), na="", row.names = F)
# 
# cat_sect <- ref_net %>% select(category, sector) %>% unique()
# write.csv2(cat_sect, file=paste0(participants, "nomenclatures/category_sect.csv"), na="", row.names = F)


# réimport après traitement
cat_name <- readxl::read_xlsx(paste0(participants, "nomenclatures/_category.xlsx"), 
            sheet="category_name", na="") %>% dplyr::mutate_if(is.character,str_squish) 
cat_level <- readxl::read_xlsx(paste0(participants, "nomenclatures/_category.xlsx"), 
            sheet="category", na="") %>% dplyr::mutate_if(is.character,str_squish) %>% unique()

maj_cat_name <- 
  left_join(cat_level, cat_name, by = c("category_4"="category")) %>% 
  dplyr::filter(is.na(category_4)) %>% 
  unique()
if (nrow(maj_cat_name)>0){
  print("category")
}else{print("ok")}

#########################################################################################
## ref_info + CAT

# pour centre technique de papier et ses filiales category cti rech
ref_info <- ref_info %>% 
  mutate(category=ifelse(id_gref=="gent129", "CTI", category))


ref_info <- left_join(ref_info, cat_level, by="category")

if (nrow(ref_info %>% select(id_ref) %>% 
    dplyr::count(id_ref) %>% dplyr::filter(n>1))>0){
message("ATTENTION ! doublons dans ref_info : \n",
        as.character(ref_info %>% 
                       select(id_ref) %>% 
                       dplyr::count(id_ref) %>% 
                       dplyr::filter(n>1))
)}
verif_na <- ref_info[apply(ref_info, 2, function(x) any(is.na(x)))]

# correction nomenclature participant_type de la commission
ref_info <- ref_info %>% 
  mutate(typage_temp=ifelse(category %in% c('COLLTER', 'EPIC_AUT'), 'Org. publics', 
                     ifelse(category %in% c('ETAB_HOSP', 'GIP') & sector=="PU", 'Org. publics',
                            ifelse(category %in% c('GE_ENT') & sector=="PU", 'Org. publics', NA_character_))),
         typage_temp=ifelse(category %in% c('ENT', 'ETI', 'PME'), 'Org. privés', 
                     ifelse(category %in% c('GE_ENT') & sector=="PR", 'Org. privés', typage_temp)),
         typage_temp=ifelse(category %in% c('EC_AUT', 'EC_COM', 'EC_ETR', 'EC_ING', 'EC_ING_AUT', 'ED', 'ENS', 'ETAB_2ND', 
                                     'GE', 'GE_AUT', 'UNIV', 'UT'), 'Ens. supérieur',
                     ifelse(category %in% c('CRITT', 'EPIC', 'CLCC', 'STRUCT', 'STRUCT_AUT', 'TGIR'), 'Recherche',
                            ifelse(category %in% c('COMPET'), 'Autres', typage_temp))))



save(ref_info, file=paste0(data_result, "data/ref_part_q01_", date_sauve, ".Rdata"))
save(ref_info, file=paste0(referentiel, "ref_info_", livraison, ".Rdata"))

rm(list=c("ref", "maj_cat_name", "cat", "ref_net", "verif", "ref_temp", "verif_na", "ref_old",
          "test", "groupe","cat_level"))
rm(list=ls(pattern = "^(temp)"))
gc()

######################################################
#maj supplémentaire si évolution des libellés
load(file=rdata_last_version(paste0(data_result, "data/"), "ref_part_q01_"))
id_paysage <- ref_info %>% 
  dplyr::filter(str_detect(id_ref, pattern_paysage))
write.csv2(id_paysage, file=paste0(identification, "etab_api_", date_sauve, ".csv"), fileEncoding = 'UTF-8',row.names = F)
###
#retour
temp <- read.csv2(paste0(referentiel, "denom_etab_2022-03-25T15-21.csv"), encoding = "UTF-8", na.strings = "") %>% 
  mutate(acronym_temp=if_else(!is.na(sigle),sigle, NomCourt)) %>% 
  select(id_ref=id, name_temp=nom, acronym_temp)

ref_info <- left_join(ref_info, temp, by='id_ref') %>%
  mutate(name_ref = ifelse(!is.na(name_temp), name_temp, name_ref),
         name = ifelse(!is.na(name_temp), name_temp, name),
         acronym_ref = ifelse(!is.na(acronym_temp), acronym_temp, acronym_ref),
         acronym = ifelse(!is.na(acronym_temp), acronym_temp, acronym)) %>% 
  select(-acronym_temp, -name_temp)
save(ref_info, file=paste0(data_result, "data/ref_part_q01_", date_sauve, ".Rdata"))
save(ref_info, file=paste0(referentiel, "ref_info_", livraison, ".Rdata"))
