# load(file=paste0(referentiel, "etab.Rdata"))
# load(file=paste0(referentiel, "sirene_p6.Rdata"))
load(file=paste0(referentiel, "rnsr.Rdata"))
load(file=paste0(referentiel, "ror.Rdata"))
load(file=rdata_last_version(paste0(data_result, "data/"), "key_part_k01_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "part_fr_m01_"))


key_part <- key_part %>% 
  dplyr::mutate_if(is.character, str_squish) %>% 
  dplyr::mutate_if(is.character, remove_space) %>% 
  dplyr::mutate(s=if_else(str_detect(s, pattern_siret), str_sub(s,1,9), s))


# REDRESSEMENT provisoire concernant les siret siege, problème de coh"rence entre sirene et etab
sirene <- sirene %>% dplyr::filter(siret!="13000285000134") %>% 
  mutate(siege=ifelse(siret=="13000285000019", 'True', siege))


# création de la liste des id de SOURCE_ID

# table qui liste les id et qui les type avec id_type()
id_s <- key_part %>% 
  select(s) %>% 
  unique() %>% 
  dplyr::filter(!is.na(s)) %>%
  dplyr::mutate(type=as.character(map(s, id_type)))
print(id_s %>% dplyr::count(type))


#####    ######     ######    ################
# S -> ORGANIZATIONS_ID
###############################
# gestion des struct nns qui sont des etabs sirenisés


# modifier les id ESR par paysage au niveau parent; création organizations_id

#liste les type siren présents dans la var paysage de etab ; 
#  siren en commun entre etab et id_s 
s_p <- etab %>% 
  dplyr::filter(!is.na(siren)) %>% 
  select(siren) %>% 
  unique() %>% 
  inner_join(id_s, by = c("siren"="s"))


#  lien entre s_p et sirene pour récupérer le siret siege
s_p <- left_join(s_p, sirene, by="siren") %>% 
  dplyr::filter(siege=="True") %>% 
  select(siren, siret)
if (nrow(s_p %>% dplyr::filter(is.na(siret)))>0){
  print("siren sans siege")
  s_p %>% dplyr::filter(is.na(siret))
}else{print("ok")}
##################################

#### S_PP -> LIEN ENTRE SIREN/SIRET SIEGE = PAYSAGE
#verif siren parent avec siret siege ; recuperation du num paysage 
s_pp <- left_join(s_p, etab) %>% 
  select(siren , paysage) %>% 
  unique()
if (nrow(s_pp %>% dplyr::count(siren) %>% dplyr::filter(n>1))>0){
  verif <- s_pp %>% dplyr::count(siren) %>% dplyr::filter(n>1)
  print("siren/siret siege -> plusieurs num paysage")
}else{"ok"}

## verifie si tous les siret de s_p sont présents dans etab
#integrer les changements dans etab
if (nrow(s_pp) == nrow(s_p)){
  print("ok")
}else{
  temp2 <- anti_join(s_p, s_pp, by="siren")
  print("continuer prog pour extraire les paysage des siren restants")}


######################################

### liste NUM PAYSAGES de key_part et ceux de s_pp lié aux siren/siret
list_p <- id_s %>% 
  dplyr::filter(type=="p") %>% 
  select(paysage=s) %>% 
  bind_rows(s_pp[c("paysage")]) %>% 
  unique()

# verfification des successeurs
#traitement s -> organizations_id
list_p <- list_p %>% 
  left_join(etab) %>% 
  select(paysage, succ=Successeur) %>% 
  dplyr::mutate(nb=str_count(succ, pattern=";")+1) %>% 
  separate_rows(succ) %>%
  left_join(etab[c("paysage", "actif.inactif", "Successeur")], by = c("succ"="paysage"))
if (nrow(list_p %>% dplyr::filter(actif.inactif=="I"))>0){
  list_p <- list_p %>% 
    select(-actif.inactif) %>% 
    left_join(etab[c("paysage", "actif.inactif")], by=c("Successeur"="paysage"))
  if (nrow(list_p %>% dplyr::filter(actif.inactif=="I"))>0){
    print("continuer de chercher les successeurs")
  }else{print("ok")}
}else{
  print("ok")
}

#création org_temp et maj des siren paysages actifs
list_p <- list_p %>% 
  dplyr::mutate(org_temp = if_else(!is.na(Successeur), Successeur,
                if_else(!is.na(succ), succ, paysage))) %>% 
  select(paysage, org_temp) 

#####   #####   #####   #####   #####
# AJOUT de ORGANIZATIONS_ID dans KEY_PART
#######################################

# maj du num paysage de S_PP (siren/paysage) avec list_p
s_pp <- left_join(s_pp, list_p, by = "paysage") %>% 
  select(siren, org_temp)

### création de organizations_id avec org_temp dans key_part
# maj des siren
key_part <- key_part %>% 
  left_join(s_pp, by = c("s"="siren")) %>% 
  dplyr::mutate(organizations_id = if_else(!is.na(org_temp), org_temp, NA_character_)) %>% 
  select(-org_temp)

# maj des num paysage
key_part <- key_part %>% 
  left_join(list_p, by = c("s"="paysage")) %>% 
  dplyr::mutate(organizations_id = if_else(!is.na(org_temp), org_temp, organizations_id)) %>% 
  select(-org_temp)


######   #####   #####   #####
# utilisation des id_autres
##############################
# chercher un lien entre s et id_autres de paysage
l_aut <- etab %>% 
  select(id_autres, paysage, nom, actif.inactif, Parent) %>% 
  separate_rows(id_autres) %>% unique() %>% 
  inner_join(key_part, by = c("id_autres"="s")) %>% 
  dplyr::filter(is.na(organizations_id), !is.na(id_autres)) %>% 
  dplyr::filter(is.na(Parent) | Parent!="6B2w8") %>% 
  select(id_autres, paysage, nom, actif.inactif, Parent) %>% 
  unique()
# retirer 6B2w8
if (nrow(l_aut)>0){
  x <- l_aut %>% dplyr::count(id_autres) %>% dplyr::filter(n>1)
  # message("id double à vérifier ", x$id_autres)
  # print(paste("id double à vérifier", x$id_autres))
  if (nrow(l_aut %>% dplyr::count(id_autres))>0){
    l_aut <- l_aut %>% dplyr::filter(!(id_autres %in% x$id_autres))
    print("doublons supprimés")
  }
  if (nrow(l_aut %>% dplyr::filter(actif.inactif=="I"))>0){
    x <- l_aut %>% dplyr::filter(actif.inactif=="I") %>% 
      left_join(etab, by="paysage") %>% 
      select(paysage, Successeur)
    l_aut <- l_aut %>% left_join(x, by="paysage") %>% 
      dplyr::mutate(paysage=if_else(!is.na(Successeur), Successeur, paysage)) %>% 
      select(-Successeur)
    print("succeseur : ok")
  }else{
    l_aut <- l_aut %>% select(-actif.inactif)
    print("actif.inactif supprimé de l_aut")
  }
}
if (nrow(l_aut %>% dplyr::filter(!is.na(Parent)))>0){
  l_aut <- l_aut %>% 
    left_join(etab[c("paysage", "actif.inactif")], by = c("Parent"="paysage"))
  if (nrow(l_aut %>% dplyr::filter(actif.inactif=="A"))>0){
    l_aut <- l_aut %>% dplyr::mutate(paysage = if_else(!is.na(Parent), Parent, paysage)) %>% 
      select(id_autres, paysage)
    print("paysage rempli avec Parent")
  }
}

key_part <- key_part %>% 
  left_join(select(l_aut, c(id_autres, paysage)), by = c("s"="id_autres")) %>% 
  dplyr::mutate(organizations_id = if_else(!is.na(paysage) & is.na(organizations_id), paysage, organizations_id)) %>% 
  select(-paysage)

#################################################################"
### liste les id siret de s
list_s <- id_s %>% dplyr::filter(type=="st") %>% select(siret=s) %>% unique()
if (nrow(list_s)>0){
  print("faire le code pour traiter les siret")
}else{
  print("passer à la suite")
}


##########################################################################
# controle les id restants
id_s <- key_part %>% 
  dplyr::filter(is.na(organizations_id) & !is.na(s)) %>% 
  select(s) %>% unique() %>% 
  dplyr::filter(!is.na(s)) %>%
  dplyr::mutate(type=as.character(map(s, id_type)))
print(id_s %>% dplyr::count(type))

# typage des siren restants dans s
x <- id_s %>% dplyr::filter(type=="s") %>% 
  left_join(sirene[c("siren", "cj_lib")], by = c("s"="siren")) %>% unique()

if (nrow(x %>% dplyr::filter(cj_lib %in% c("EPIC","EPSCP")))>0){
  message('EPIC ou/et EPSCP à chercher dans paysage')
}else{print(unique(x$cj_lib))}

###################################################
# traitement des assocs

l_aut <- id_s %>% dplyr::filter(type=="a") %>% 
  select(s) %>% 
  left_join(sirene, by=c("s"="rna")) %>% 
  dplyr::filter(siege=="True") %>% 
  select(s,siren)

if (nrow(l_aut)>0){
  key_part <- key_part %>% 
    left_join(l_aut, by="s") %>% 
    dplyr::mutate(organizations_id=if_else(!is.na(siren), siren, organizations_id)) %>% 
    select(-siren)
  print("maj effectuée de key_part, sirenisation des assocs")
}

id_s <- key_part %>% 
  dplyr::filter(is.na(organizations_id) & !is.na(s)) %>% 
  select(s) %>% 
  unique() %>% 
  dplyr::filter(!is.na(s)) %>%
  dplyr::mutate(type=as.character(map(s, id_type)))
print(id_s %>% dplyr::count(type))

l_aut <- id_s %>% dplyr::filter(type=="a") %>% 
  select(s) %>% 
  left_join(sirene, by=c("s"="rna")) %>% 
  dplyr::filter(siege=="True") %>% 
  select(s,siren)

##################################################################
#traitement des nns
l_aut <- id_s %>% dplyr::filter(type=="r") %>% 
  select(s) %>% 
  inner_join(rnsr, by=c("s"="id")) %>% 
  select(s)

if (nrow(id_s %>% dplyr::filter(type=="r"))==nrow(l_aut)){
  key_part <- key_part %>% 
    dplyr::mutate(organizations_id=if_else(str_detect(s, pattern_rnsr), s, organizations_id))
  print("effectué : ajout des nns dans organizations_id")
}else{
  id_s %>% dplyr::filter(type=="r") %>% 
    anti_join(l_aut) %>% select(s) %>% unique() %>% 
    write.csv2(file=paste0(identification, "id_nns_", date_sauve, ".csv"), row.names = F)
  print("attention, certains nns de key_part ne sont pas dans RNSR, revenir à l'étape 8 pour corriger")}

id_s <- key_part %>% 
  dplyr::filter(is.na(organizations_id) & !is.na(s)) %>% 
  select(s) %>% 
  unique() %>% 
  dplyr::filter(!is.na(s)) %>%
  dplyr::mutate(type=as.character(map(s, id_type)))
print(id_s %>% dplyr::count(type))

#ALLER CHERCHER REFERNTIEL ROR
l_aut <- id_s %>% 
  dplyr::filter(type=="ro") %>% 
  select(s) %>% 
  left_join(ror, by=c("s"="id")) %>% 
  select(s)

if (nrow(id_s %>% dplyr::filter(type=="ro"))==nrow(l_aut)){
  key_part <- key_part %>% 
    dplyr::mutate(organizations_id=if_else(str_detect(s, pattern_ror), s, organizations_id))
  print("effectué : ajout des ror dans organizations_id")
}else{print("attention, certains ror de key_part ne sont pas dans ror, verifier la validite du id")}

id_s <- key_part %>% 
  dplyr::filter(is.na(organizations_id) & !is.na(s)) %>% 
  select(s) %>% 
  unique() %>% 
  dplyr::filter(!is.na(s)) %>%
  dplyr::mutate(type=as.character(map(s, id_type)))
print(id_s %>% dplyr::count(type))

#### CONSERVER S_PP (lien siren/siege = paysage) et list_P (liste paysage et new_paysage)

# verification que les paysages n'ont pas de parent

pays_parent <- key_part %>% 
  dplyr::filter(str_detect(organizations_id, pattern_paysage)) %>% 
  select(paysage=organizations_id) %>% 
  unique() %>% 
  left_join(select(etab, c(paysage, nom, soustype, category_name, Parent)), by="paysage") %>% 
  dplyr::filter(!is.na(Parent)) %>% 
  left_join(select(etab, c(paysage, nomP=nom)), by=c("Parent"="paysage")) %>% 
  dplyr::filter(str_detect(soustype, "Structure relevant|Site secondaire")) %>% 
  select(paysage, Parent)


key_part <- key_part %>% 
  left_join(pays_parent, by = c("organizations_id"="paysage")) %>% 
  dplyr::mutate(organizations_id = if_else(!is.na(Parent), Parent, organizations_id)) %>% 
  select(-Parent)


##############################################################################
##############################################################################
##############################################################################
# MAJ TUTELLE RNSR
##############################################

# maj tut_id avec les paysages (siren) déjà listés ci-dessus
key_part <- key_part %>% 
  left_join(s_pp, by = c("t"="siren")) %>% 
  dplyr::mutate(tutelle_id = if_else(!is.na(org_temp), org_temp, NA_character_)) %>% 
  select(-org_temp)

key_part <- key_part %>% 
  left_join(list_p, by = c("t"="paysage")) %>% 
  dplyr::mutate(tutelle_id = if_else(!is.na(org_temp), org_temp, tutelle_id)) %>% 
  select(-org_temp)


# liste les id de tut_id sauf ceux déjà renseignés
id_t <- key_part %>% 
  dplyr::filter(!is.na(t) & is.na(tutelle_id)) %>% 
  select(t) %>% unique() %>% 
  dplyr::filter(!is.na(t)) %>%
  dplyr::mutate(type=as.character(map(t, id_type)))
id_t %>% dplyr::count(type)

# lien siren, siret, uai avec paysage
temp1 <- left_join(id_t, etab, by=c("t"="siret")) %>% 
  select(t, paysage, nom) %>% 
  dplyr::filter(!is.na(paysage))
temp2 <- left_join(id_t, etab, by=c("t"="siren")) %>% 
  select(t, paysage, nom) %>% 
  dplyr::filter(!is.na(paysage))
temp3 <- left_join(id_t, etab, by=c("t"="uai")) %>% 
  select(t, paysage, nom) %>% 
  dplyr::filter(!is.na(paysage))

# liste tous les paysages
temp <- id_t %>% 
  dplyr::filter(type=="p") %>% 
  select(paysage=t) %>% 
  bind_rows(temp1, temp2, temp3) %>% 
  dplyr::mutate(t=if_else(is.na(t), paysage, t)) %>% 
  unique()
# bind_rows(temp1[c("paysage")], temp2[c("paysage")], temp3[c("paysage")]) %>% unique()

# verfification des successeurs
#traitement 
test <- temp %>% 
  select(paysage) %>% 
  unique() %>% 
  left_join(etab) %>% 
  select(paysage, succ=Successeur) %>% 
  dplyr::mutate(nb=str_count(succ, pattern=";")+1) %>% 
  separate_rows(succ) %>%
  left_join(etab[c("paysage", "actif.inactif", "Successeur")], by = c("succ"="paysage"))
if (nrow(test %>% dplyr::filter(actif.inactif=="I"))>0){
  test <- test %>% select(-actif.inactif) %>% 
    left_join(etab[c("paysage", "actif.inactif")], by=c("Successeur"="paysage"))
  if (nrow(test %>% dplyr::filter(actif.inactif=="I"))>0){
    print("continuer de chercher les successeurs")
  }else{print("ok")}
}else{
  print("ok")
}

#création tut_temp et maj des id paysage actifs
test <- test %>% 
  dplyr::mutate(tut_temp = if_else(!is.na(Successeur), Successeur,
                if_else(!is.na(succ), succ, paysage))) %>% 
  select(paysage, tut_temp)

# verification des parents
test <- test %>% 
  left_join(etab, by = c("tut_temp"="paysage")) %>% 
  select(paysage, tut_temp, nom, Parent) %>% 
  dplyr::mutate(Parent=if_else(Parent %in% c("6B2w8", "6Th4v"), NA_character_, Parent),
                tut_temp=if_else(!is.na(Parent), Parent, tut_temp)) %>% 
  separate_rows(tut_temp) %>% select(paysage, tut_temp)


#####   #####   #####   #####   #####
# AJOUT à TUTELLE_ID dans KEY_PART
#######################################

temp <- left_join(temp, test, by = "paysage") %>% 
  select(t, tut_temp) %>% 
  unique()

#création de tutelle_id avec tut_temp dans key_part
key_part <- key_part %>% 
  left_join(temp, by = "t") %>% 
  dplyr::mutate(tutelle_id = if_else(!is.na(tut_temp), tut_temp, tutelle_id)) %>% 
  select(-tut_temp)


######   #####   #####   #####
# utilisation des id_autres
##############################
# chercher un lien entre t et id_autres de paysage
l_aut <- etab %>% 
  select(id_autres, paysage, nom, actif.inactif, Parent) %>% 
  separate_rows(id_autres) %>% 
  unique() %>% 
  inner_join(key_part, by = c("id_autres"="t")) %>% 
  dplyr::filter(is.na(tutelle_id), !is.na(id_autres)) %>% 
  select(id_autres, paysage, nom, actif.inactif, Parent) %>% 
  unique()

# modif de l_aut pour actualisation de 2019
l_aut <- l_aut %>% 
  dplyr::mutate(Parent=if_else(Parent %in% c("6Th4v"), paysage, Parent))
l_aut <- l_aut %>% dplyr::filter(paysage != "B8q5v") # supp de l'ecole meteo pour conserver le siren de meteo-fr

if (nrow(l_aut)>0){
  x <- l_aut %>% dplyr::count(id_autres) %>% dplyr::filter(n>1)
  if (nrow(x)>0){
    # message("id double à vérifier ", x$id_autres)
    print(paste("id double à vérifier", x$id_autres))
  }
  if (nrow(x %>% dplyr::count(id_autres))>0){
    l_aut <- l_aut %>% dplyr::filter(id_autres != x$id_autres)
    print("doublons supprimés")
  }
  if (nrow(l_aut %>% dplyr::filter(actif.inactif=="I"))>0){
    print("faire la requete si necesaire : aller chercher le successeur du paysage")
  }else{
    l_aut <- l_aut %>% select(-actif.inactif)
    print("actif.inactif supprimé de l_aut")
  }
}
if (nrow(l_aut %>% dplyr::filter(!is.na(Parent)))>0){
  l_aut <- l_aut %>% 
    left_join(etab[c("paysage", "actif.inactif")], by = c("Parent"="paysage"))
  if (nrow(l_aut %>% dplyr::filter(actif.inactif=="A"))>0){
    l_aut <- l_aut %>% dplyr::mutate(paysage = if_else(!is.na(Parent), Parent, paysage)) %>% 
      select(id_autres, paysage)
    print("maj de paysage avec Parent de l_aut effectuée")
  }else{print("actif.inactif = I : ajouter successeur")}
}else{print("aucun parent")}

key_part <- key_part %>% 
  left_join(select(l_aut, c(id_autres, paysage)), by = c("t"="id_autres")) %>% 
  dplyr::mutate(tutelle_id = if_else(!is.na(paysage) & is.na(tutelle_id), paysage, tutelle_id)) %>% 
  select(-paysage)

############################################"
# controle des id restants

id_t <- key_part %>% 
  dplyr::filter(!is.na(t) & is.na(tutelle_id)) %>% 
  select(t) %>% 
  unique() %>% 
  dplyr::filter(!is.na(t)) %>%
  dplyr::mutate(type=as.character(map(t, id_type)))
id_t %>% dplyr::count(type)

# typage des id siren restants dans t
x <- id_t %>% 
  dplyr::filter(str_detect(t, pattern_siren)) %>% 
  left_join(sirene[c("siren", "cj_lib")], by = c("t"="siren")) %>% unique()
unique(x$cj_lib)


# traitement des uai
temp <- id_t %>% dplyr::filter(type=="u") %>% 
  left_join(tutelle, by=c("t"="tutelle_rnsr")) %>% 
  select(t, tutelle_name) %>% 
  unique()

###############################################################################
###############################################################################
###############################################################################
# MAJ de p (siren et paysage) avec TEST et TEMP1
################################################

key_part <- key_part %>% 
  left_join(s_pp, by = c("p"="siren")) %>% 
  dplyr::mutate(participant_id = if_else(!is.na(org_temp), org_temp, NA_character_)) %>% 
  select(-org_temp)

key_part <- key_part %>% 
  left_join(list_p, by = c("p"="paysage")) %>% 
  dplyr::mutate(participant_id = if_else(!is.na(org_temp), org_temp, participant_id)) %>% 
  select(-org_temp)


# typage des participants
id_p <- key_part %>% 
  dplyr::filter(!is.na(p) & is.na(participant_id)) %>% 
  select(p) %>% 
  unique() %>% 
  dplyr::filter(!is.na(p)) %>%
  dplyr::mutate(type=as.character(map(p, id_type)))
id_p %>% dplyr::count(type)

# lien siren, siret, uai avec paysage
temp1 <- left_join(id_p, etab, by=c("p"="siret")) %>% 
  select(p, paysage) %>% 
  dplyr::filter(!is.na(paysage))
temp2 <- left_join(id_p, etab, by=c("p"="siren")) %>% 
  select(p, paysage) %>% 
  dplyr::filter(!is.na(paysage))
temp3 <- left_join(id_p, etab, by=c("p"="uai")) %>%
  select(p, paysage) %>% 
  dplyr::filter(!is.na(paysage))
temp4 <- etab %>% separate_rows(id_autres) %>% 
  dplyr::filter(str_detect(id_autres, pattern_rnsr)) %>% 
  select(p=id_autres, paysage) %>% 
  right_join(id_p[,"p"], by = "p") %>% 
  dplyr::filter(!is.na(paysage))
# liste tous les paysages
temp <- id_p %>% 
  dplyr::filter(type=="p") %>% 
  select(paysage=p) %>% 
  bind_rows(temp1, temp2, temp3,temp4) %>% 
  dplyr::mutate(p=if_else(is.na(p), paysage, p)) %>% 
  unique()


# verfification des successeurs
#traitement p -> participant_id
test <- temp %>% 
  select(paysage) %>% 
  unique() %>% 
  left_join(etab) %>% 
  select(paysage, succ=Successeur) %>% 
  dplyr::mutate(nb=str_count(succ, pattern=";")+1) %>% 
  separate_rows(succ) %>%
  left_join(etab[c("paysage", "actif.inactif", "Successeur")], by = c("succ"="paysage"))
if (nrow(test %>% dplyr::filter(actif.inactif=="I"))>0){
  test <- test %>% select(-actif.inactif) %>% 
    left_join(etab[c("paysage", "actif.inactif")], by=c("Successeur"="paysage"))
  if (nrow(test %>% dplyr::filter(actif.inactif=="I"))>0){
    print("continuer de chercher les successeurs")
  }else{print("ok")}
}else{
  print("ok")
}


#création part_temp et maj des id paysage actifs
test <- test %>% 
  dplyr::mutate(part_temp = if_else(!is.na(Successeur), Successeur,
                                    if_else(!is.na(succ), succ, paysage))) %>% 
  select(paysage, part_temp)

# verification des parents
test <- test %>% 
  left_join(etab, by = c("part_temp"="paysage")) %>% 
  select(paysage, part_temp, nom, Parent)

# Apres verification ; adapter la requete ci-dessous
test <- test %>%   
  dplyr::mutate(Parent=if_else(Parent %in% c("6B2w8", mines_tel, grp_hosp), NA_character_, Parent),
                part_temp=if_else(!is.na(Parent), Parent, part_temp)) %>% 
  separate_rows(part_temp) %>% select(paysage, part_temp) %>% 
  left_join(etab[c("paysage", "actif.inactif")], by = c("part_temp"="paysage")) %>% 
  dplyr::filter(actif.inactif == "A")


#####   #####   #####   #####   #####
# AJOUT à PARTICIPANT_ID dans KEY_PART
#######################################

temp <- left_join(temp, test, by = "paysage") %>% 
  select(p, part_temp) %>% 
  unique()

#création de participant_id avec tut_temp dans key_part
key_part <- key_part %>% left_join(temp, by = "p") %>% 
  dplyr::mutate(participant_id = if_else(!is.na(part_temp), part_temp, participant_id)) %>% 
  select(-part_temp)


######   #####   #####   #####
# utilisation des id_autres
###############################
###############################

# chercher un lien entre source_id et id_autres de paysage
l_aut <- etab %>% 
  select(id_autres, paysage, nom, actif.inactif, Parent) %>% 
  separate_rows(id_autres) %>% 
  unique() %>% 
  # inner_join(id_p, by = c("id_autres"="p")) %>% 
  # dplyr::filter(type %in% c("s", "st"))
  inner_join(key_part, by = c("id_autres"="p")) %>% 
  dplyr::filter(is.na(participant_id), !(str_detect(id_autres,pattern_rnsr))) %>% 
  select(id_autres, paysage, nom, actif.inactif, Parent) %>% unique()

# modif de l_aut pour actualisation de 2019
l_aut <- l_aut %>% 
  dplyr::mutate(Parent=if_else(Parent %in% c(mines_tel), paysage, Parent)) %>% 
  dplyr::filter(!is.na(Parent))

if (nrow(l_aut)>0){
  x <- l_aut %>% dplyr::count(id_autres) %>% dplyr::filter(n>1)
  if (nrow(x)>0){
    # message("id double à vérifier ", x$id_autres)
    print(paste("id double à vérifier", x$id_autres))
  }
  if (nrow(x %>% dplyr::count(id_autres))>0){
    l_aut <- l_aut %>% dplyr::filter(id_autres != x$id_autres)
    print("doublons supprimés")
  }
  if (nrow(l_aut %>% dplyr::filter(actif.inactif=="I"))>0){
    # l_aut <- l_aut %>% select(id_autres, paysage=Parent, Parent)
    print("remplacer le id_autres par le Parent à modifier avec successeur si c'est mieux")
  }else{
    l_aut <- l_aut %>% select(-actif.inactif)
    print("actif.inactif supprimé de l_aut")
  }
}else{print("ok")}

if (nrow(l_aut %>% dplyr::filter(!is.na(Parent)))>0){
  l_aut <- l_aut %>% 
    left_join(select(etab, c(paysage, actif=actif.inactif)), by = c("Parent"="paysage"))
  print("execution de la requête l_aut si parent existant")
  if (nrow(l_aut %>% dplyr::filter(actif=="A"))>0){
    l_aut <- l_aut %>% 
      dplyr::mutate(paysage = if_else(!is.na(Parent), Parent, paysage)) %>% 
      select(id_autres, paysage)
    print("execution de la requête si tout parent actif")
  }
}

# lancer requete si l_aut > 0
key_part <- key_part %>% 
  left_join(select(l_aut, c(id_autres, paysage)), by = c("p"="id_autres")) %>%
  dplyr::mutate(participant_id = if_else(!is.na(paysage) & is.na(participant_id), paysage, participant_id)) %>%
  select(-paysage)

####################################
# RELANCER ID_P #####
####################

id_p <- key_part %>% 
  dplyr::filter(!is.na(p) & is.na(participant_id)) %>% 
  select(p) %>% unique() %>% 
  dplyr::filter(!is.na(p)) %>%
  dplyr::mutate(type=as.character(map(p, id_type)))
id_p %>% dplyr::count(type)


x <- id_p %>% dplyr::filter(type=="s") %>% 
  left_join(s_info[c("siren", "cj_lib")], by = c("p"="siren")) %>% unique()
unique(x$cj_lib)

x <- id_p %>% dplyr::filter(type=="st") %>% 
  left_join(sirene[c("siret",  "nom","cj_lib")], by = c("p"="siret")) %>% unique()
unique(x$cj_lib)

#extraction du siren des orga et epscp

l_aut <- x %>% 
  dplyr::filter(cj_lib == "EPSCP" | str_sub(p, 1, 9) %in% orga ) %>% 
  dplyr::mutate(siren=str_sub(p, 1,9))
l_aut <- l_aut %>% select(siren, p) %>% unique() %>% 
  left_join(etab, by="siren") %>% 
  dplyr::filter(siret_siege=="True") %>% 
  select(p, org_temp=paysage)

if (sum(is.na(l_aut$org_temp))>0){
  print("verifier le siren manquant dans s_pp, cela veut dire qu'il manque un siren dans sirene")
}else{
  l_aut <- l_aut %>% select(p, org_temp) %>% unique()
  key_part <- key_part %>% 
    left_join(l_aut, by = "p") %>%
    dplyr::mutate(participant_id = if_else(!is.na(org_temp) & is.na(participant_id), org_temp, participant_id)) %>%
    select(-org_temp)
  print("maj de key_part effectuée")
}

################################################################################
################################################################################
################################################################################
# CONTROLE

test <- key_part %>% dplyr::filter(is.na(organizations_id)) %>% select(s) %>% unique()
temp <- test %>% dplyr::filter(str_detect(s, pattern_siren)) %>% 
  left_join(sirene, by = c("s"="siren")) %>% dplyr::filter(siege=="True") %>% 
  select(s, nom_ul, cj_lib)
if (nrow(temp %>% dplyr::filter(is.na(nom_ul)))>0){ 
  print("verifier le siren manquant dans sirenent")
}else{print("ok")}
if (nrow(temp)>0){
  x <- unique(temp$cj_lib)
  print(x)
  if (length(x %in% c("EPIC","EPSCP"))>0){
    temp1 <- temp %>% dplyr::filter(cj_lib %in% c("EPIC","EPSCP"))
    print(paste("si EPIC ou EPSCP dans siren -> modifier le id :",temp1$s, temp1$nom_ul))
  }
}

test <- key_part %>% dplyr::filter(is.na(participant_id)) %>% select(p) %>% unique()
temp <- test %>% dplyr::filter(str_detect(p, pattern_siren)) %>% 
  left_join(sirene, by = c("p"="siren")) %>% dplyr::filter(siege=="True") %>% 
  select(p, nom_ul, cj_lib)
if (nrow(temp %>% dplyr::filter(is.na(nom_ul)))>0){ 
  print("verifier le siren manquant dans sirene")
}else{print("ok")}
if (nrow(temp)>0){
  x <- unique(temp$cj_lib)
  print(x)
  if (length(x %in% c("EPIC","EPSCP"))>0){
    temp1 <- temp %>% dplyr::filter(cj_lib %in% c("EPIC","EPSCP"))
    print("si EPIC ou EPSCP dans siren -> modifier le id")
  }else{print("ok")}
}else{print("ok")}

test <- key_part %>% dplyr::filter(is.na(tutelle_id)) %>% select(t) %>% unique()
temp <- test %>% dplyr::filter(str_detect(t, pattern_siren)) %>% 
  left_join(sirene, by = c("t"="siren")) %>% dplyr::filter(siege=="True") %>% 
  select(t, nom_ul, cj_lib)
if (nrow(temp %>% dplyr::filter(is.na(nom_ul)))>0){ 
  print("verifier le siren manquant dans sirene")
}else{print("ok")}
if (nrow(temp)>0){
  x <- unique(temp$cj_lib)
  print(x)
  if (any(length(x %in% c("EPIC","EPSCP")))==TRUE){
    temp1 <- temp %>% dplyr::filter(cj_lib %in% c("EPIC","EPSCP"))
    print(paste("si EPIC ou EPSCP dans siren -> modifier le id :",temp1$t, temp1$nom_ul))
  }
}

#si besoin de supprimer des observations en fonction des id identifiés
#actu de 2019 -> supp de la tutelle 130021504  comue grand paris
key_part <- key_part %>% dplyr::filter(is.na(t) | t != "130021504")# NON 2021_11

# ajout du pic_id en id pour identifier les projets beneficiaires sans id référencé
verif <- key_part %>% dplyr::filter(stage=="project", participates_as=="beneficiary", is.na(s)) 
if (nrow(verif)>0){
  pic <- verif %>% 
    dplyr::mutate(participant_pic=str_extract(id, "(?<=-)\\d+$"), np=1) %>% 
    select(participant_pic, np) %>% unique()
  
  key_part <- key_part %>% 
    dplyr::mutate(participant_pic=str_extract(id, "(?<=-)\\d+$")) %>% 
    left_join(pic, by="participant_pic") %>% 
    dplyr::mutate(s=if_else(is.na(np), s, paste0("pic", participant_pic))) %>% 
    select(-np,-participant_pic)
  
  part_fr <- part_fr %>% 
    left_join(pic, by="participant_pic") %>% 
    dplyr::mutate(organizations_id=if_else(is.na(np), organizations_id, paste0("pic", participant_pic))) %>% 
    select(-np)
}

# A METTRE A LA FIN DU PROG QUAND TOUT EST PROPRE 
# verification que tous les siren sont dans sirene
siren <- etab %>% dplyr::filter(!is.na(siren), actif.inactif == "A", !(siren %in% s_non_diffusible)) %>% 
  select(siren) %>% unique() %>% 
  anti_join(sirene, by="siren") %>% 
  select(siren) %>% unique()
if (nrow(siren)>0){
  write.csv2(siren, file=paste0(identification, "siren_extract_", date_sauve, ".csv"), row.names = FALSE)
  print("interroger api si siren n'est pas null -> relancer prog m-01")
}else{
  rm(siren)
  print("ok")
}

###################################################
############################################################################################""
# key_part <- key_part %>% select( -actif.inactif, -Parent) %>% unique()


save(key_part, file=paste0(data_result, "data/key_part_p01_", date_sauve,".Rdata"))
save(part_fr, file=paste0(data_result, "data/part_fr_p01_", date_sauve,".Rdata"))

rm(list=c("pic","test", "s_p","s_pp","list_p","l_aut", "list_s","id_t","id_s","id_p", 
          "verif", "siren", "pays_parent", "x"))
rm(list=ls(pattern = "^(temp)"))
gc()

######################################################################################################


