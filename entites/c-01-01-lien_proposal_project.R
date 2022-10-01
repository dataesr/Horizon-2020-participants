##############################################################
#liste les id des deux bases et fait une jointure complete pour recupere la totalite des id
temp <- part %>% 
  select(id, ORDRE_PART, TYPE_PART, proj) %>% 
  group_by(id, ORDRE_PART, TYPE_PART) %>% 
  dplyr::mutate(np=n()) %>% 
  ungroup() %>% 
  unique() %>% 
  separate(., col = id, into=c("id1", "pic2", "pic1"), sep = "-", remove = F) %>% 
  dplyr::mutate_if(is.character,str_squish)
  

temp1 <- applicant %>% 
  select(id, ordre_prop=ORDRE_PART, type_prop=TYPE_PART, prop) %>% 
  group_by(id, ordre_prop, type_prop) %>% 
  dplyr::mutate(npp=n()) %>% 
  ungroup() %>% 
  unique() %>% 
  separate(., col = id, into=c("id1", "pic2", "pic1"), sep = "-", remove = F) %>% 
  dplyr::mutate_if(is.character,str_squish)

# jointure complète de tous les id
temp2 <- full_join(temp, temp1, by = c("id","id1", "pic2", "pic1")) %>% 
  unique() %>% 
  relocate(id, id1, pic1, pic2, ORDRE_PART, ordre_prop, TYPE_PART, type_prop, np, npp, proj, prop)

# 1 - jointure unique entre proposal et project
temp3 <- temp2 %>% filter(np==1 & npp==1) %>% 
  select(id, ORDRE_PART, TYPE_PART, ordre_prop, type_prop, proj, prop) %>% 
  unique()

# le reste
inter <- anti_join(temp2, temp3)

# 2 - numero ordre identique même si multi id identique dans le projet
np_1 <- inter %>% filter(np=="1" & !is.na(npp)) %>% 
  select(id, ORDRE_PART, TYPE_PART, ordre_prop, type_prop, proj, prop) %>% unique()

# construction table participant finale
temp3 <- bind_rows(temp3, np_1)

# le reste
# inter <- anti_join(inter, temp3)
# # inter <- anti_join(inter, temp3, by = c("id", 'ORDRE_PART', "ordre_prop"))
# 
# x <- inner_join(select(ordre_equal, id), inter, by = "id") %>% 
#   select(id, ORDRE_PART, TYPE_PART)
# 
# temp3 <- bind_rows(temp3, x)
# 
# # le reste
# inter <- anti_join(inter, temp3, by = c("id", "ORDRE_PART", "TYPE_PART"))
#########################################################
#### JOINTURE SUR DES ID PAS FORCEMENT EGAUX

### 3 - Seulement jointure sur le PIC1 + type_part
app <- inter %>% 
  select(id_prop=id, id1, pic1, ordre_prop, type_prop) %>% 
  filter(!is.na(ordre_prop)) %>% unique()

pic_ordre <- 
  inner_join(inter,
             app, by = c("id1", "pic1", "ORDRE_PART"="ordre_prop", "TYPE_PART"="type_prop")) %>% 
  select(id, ORDRE_PART, TYPE_PART, id_prop, proj) %>% 
  dplyr::mutate(ordre_prop=ORDRE_PART, type_prop=TYPE_PART, prop=1)

temp3 <- bind_rows(temp3, pic_ordre)


# le reste
inter <- anti_join(inter, temp3, by = c("id", 'ORDRE_PART', "TYPE_PART"))
inter <- anti_join(inter, temp3, by = c("id"="id_prop", 'ordre_prop', "type_prop"))

### 4 - seulement jointure sur le PIC2 + type_part
app <- inter %>% select(id_prop=id, id1, pic2, ordre_prop, type_prop) %>% 
  filter(!is.na(ordre_prop)) %>% unique()

pic_ordre <- 
  inner_join(inter, app, by = c("id1", "pic2", "ORDRE_PART"="ordre_prop", "TYPE_PART"="type_prop")) %>% 
  select(id, ORDRE_PART, TYPE_PART, id_prop, proj) %>% 
  dplyr::mutate(ordre_prop=ORDRE_PART, type_prop=TYPE_PART, prop=1)

temp3 <- bind_rows(temp3, pic_ordre)


# le reste
inter <- anti_join(inter, temp3, by = c("id", 'ORDRE_PART', "TYPE_PART"))
inter <- anti_join(inter, temp3, by = c("id"="id_prop", 'ordre_prop', "type_prop"))

### 5 - seulement jointure si np et npp = 1 + pic1 + ordre_part
app <- inter %>% filter(!is.na(ordre_prop), npp=="1") %>%
  select(id_prop=id, id1, pic1, ordre_prop, type_prop) %>% 
  unique()

pic_ordre <- 
  inner_join(select(inter, -type_prop), app, by = c("id1", "pic1", "ORDRE_PART"="ordre_prop")) %>% 
  filter(np=="1") %>% 
  select(id, ORDRE_PART, TYPE_PART, id_prop, type_prop, proj) %>% 
  dplyr::mutate(ordre_prop=ORDRE_PART, prop=1)

temp3 <- bind_rows(temp3, pic_ordre)


# le reste
inter <- anti_join(inter, temp3, by = c("id", 'ORDRE_PART', "TYPE_PART"))
inter <- anti_join(inter, temp3, by = c("id"="id_prop", 'ordre_prop', 'type_prop'))

### 6 - seulement jointure si np et npp = 1 + pic2 + ordre_part
app <- inter %>% filter(!is.na(ordre_prop), npp=="1") %>%
  select(id_prop=id, id1, pic2, ordre_prop, type_prop) %>% 
  unique()

pic_ordre <- 
  inner_join(select(inter, -type_prop),
             app, by = c("id1", "pic2", "ORDRE_PART"="ordre_prop")) %>% 
  filter(np=="1") %>% 
  select(id, ORDRE_PART, TYPE_PART, id_prop, type_prop, proj) %>% 
  dplyr::mutate(ordre_prop=ORDRE_PART, prop=1)

if(nrow(pic_ordre)>0){
  temp3 <- bind_rows(temp3, pic_ordre)
  # le reste
  inter <- anti_join(inter, temp3, by = c("id", 'ORDRE_PART', "TYPE_PART"))
  inter <- anti_join(inter, temp3, by = c("id"="id_prop", 'ordre_prop', 'type_prop'))
}else{print("passer à la suite")}


### 7 - seulement jointure si np et npp = 1 + pic1 + type_part
app <- inter %>% filter(!is.na(ordre_prop), npp=="1") %>%
  select(id_prop=id, id1, pic1, ordre_prop, type_prop) %>% 
  unique()

pic_ordre <- 
  inner_join(select(inter, -ordre_prop), app, by = c("id1", "pic1", "TYPE_PART"="type_prop")) %>% 
  filter(np=="1") %>% 
  select(id, ORDRE_PART, TYPE_PART, id_prop, ordre_prop, proj) %>% 
  dplyr::mutate(type_prop=TYPE_PART, prop=1)

temp3 <- bind_rows(temp3, pic_ordre)

# le reste
inter <- anti_join(inter, temp3, by = c("id", 'ORDRE_PART', "TYPE_PART"))
inter <- anti_join(inter, temp3, by = c("id"="id_prop", 'ordre_prop', "type_prop"))


### 8 - seulement jointure si np et npp = 1 + pic2 + type_part
app <- inter %>% filter(!is.na(ordre_prop), npp=="1") %>%
  select(id_prop=id, id1, pic2, ordre_prop, type_prop) %>% 
  unique()

pic_ordre <- 
  inner_join(select(inter, -ordre_prop), app, by = c("id1", "pic2", "TYPE_PART"="type_prop")) %>% 
  filter(np=="1") %>% 
  select(id, ORDRE_PART, TYPE_PART, id_prop, ordre_prop, proj) %>% 
  dplyr::mutate(type_prop=TYPE_PART, prop=1)

if(nrow(pic_ordre)>0){
  temp3 <- bind_rows(temp3, pic_ordre)
  inter <- anti_join(inter, temp3, by = c("id", 'ORDRE_PART', "TYPE_PART"))
  inter <- anti_join(inter, temp3, by = c("id"="id_prop", 'ordre_prop', "type_prop"))
}else{print("passer à la suite")}


### 9 - seulement jointure si np et npp = 1 + id
app <- inter %>% filter(!is.na(ordre_prop), npp=="1") %>%
  select(id_prop=id, ordre_prop, type_prop) %>% 
  unique()

pic_ordre <- 
  inner_join(select(inter, c(-ordre_prop, -type_prop)), app, by = c("id"="id_prop")) %>% 
  filter(np=="1") %>% 
  select(id, ORDRE_PART, TYPE_PART, ordre_prop, type_prop, proj) %>% 
  dplyr::mutate(prop=1)

if(nrow(pic_ordre)>0){
  temp3 <- bind_rows(temp3, pic_ordre)
  inter <- anti_join(inter, temp3, by = c("id", 'ORDRE_PART', "TYPE_PART"))
  inter <- anti_join(inter, temp3, by = c("id", 'ordre_prop', 'type_prop'))
}else{print("passer à la suite")}



### 9 - seulement jointure si npp > 1 + id
app <- inter %>% filter(!is.na(ordre_prop), npp!="1") %>%
  select(id_prop=id, ordre_prop, type_prop) %>% 
  unique()

pic_ordre <- 
  inner_join(select(inter, c(-ordre_prop, -type_prop)), app, by = c("id"="id_prop")) %>% 
  filter(!is.na(np)) %>% 
  select(id, ORDRE_PART, TYPE_PART, ordre_prop, type_prop, proj) %>% 
  dplyr::mutate(prop=1)

if(nrow(pic_ordre)>0){
  temp3 <- bind_rows(temp3, pic_ordre)
  inter <- anti_join(inter, temp3, by = c("id", 'ORDRE_PART', "TYPE_PART"))
  inter <- anti_join(inter, temp3, by = c("id", 'ordre_prop', 'type_prop'))
}else{print("passer à la suite")}
##########################################################################################
### 10 - seulement jointure sur id et identification des participants absents dans l'autre base

# applicant <- applicant %>% dplyr::mutate(ORDRE_PART=as.character(ORDRE_PART))
# part <- part %>% dplyr::mutate(ORDRE_PART=as.character(ORDRE_PART))

app <- inter %>% filter(!is.na(npp)) %>%
  left_join(select(part, c(NUM_PROJET, STADE_PROJET)), by = c("id1"="NUM_PROJET")) %>% 
  unique()

# uniquement proposal
only_npp <- app %>% #filter(is.na(STADE_PROJET)) %>% 
  select(id, ordre_prop, type_prop, prop) %>% unique()
# only_npp <- temp2 %>% filter(is.na(np)) %>% select(id, ordre_prop) %>% unique()

temp3 <- bind_rows(temp3, only_npp)

inter <- anti_join(inter, temp3, by = c("id", 'ordre_prop', "type_prop"))

### 11 - seulement id et identification des participants projets

# uniquement project
only_np <- inter %>% filter(!is.na(np)) %>% 
  left_join(select(applicant, c(NUM_PROJET, STADE_PROJET)), by = c("id1"="NUM_PROJET")) %>% 
  unique() %>% 
  select(id, ORDRE_PART, TYPE_PART, proj)

# regroupement des 3 tables
temp3 <- bind_rows(temp3, only_np)

inter <- anti_join(inter, temp3, by = c("id", 'ORDRE_PART', "TYPE_PART")) %>% unique()

###########################################################################################

# # np et npp indique le nb de participant existants pour un même id
# applicant <- applicant %>% dplyr::mutate(ORDRE_PART=as.character(ORDRE_PART))
# part <- part %>% dplyr::mutate(ORDRE_PART=as.character(ORDRE_PART))
# app <- inter %>% 
#   left_join(applicant, by = c("id", "TYPE_PART","ordre_prop"="ORDRE_PART")) %>% 
#   select(id, ROLE_PART, TYPE_PART, ordre_prop, ORDRE_PART) %>% unique()
# p <- inter %>% left_join(part, by = c("id", "ORDRE_PART", "TYPE_PART")) %>% 
#   select(id, ROLE_PART, TYPE_PART, ORDRE_PART, ordre_prop) %>% 
#   group_by(id, ordre_prop) %>% 
#   dplyr::mutate(t = paste0(TYPE_PART, collapse = ";")) %>% 
#   ungroup() %>% unique() %>% 
#   filter(!(str_detect(t, ";") & TYPE_PART=="thirdparty")) %>% 
#   select(-t, -ordre_prop)
# 
# # traitement dans project
# res <- p %>% #filter(np > 1) %>% 
#   inner_join(select(app, -ORDRE_PART), 
#              by=c("id", "ROLE_PART", "TYPE_PART")) %>% 
#   unique() %>% 
#   select(id, ROLE_PART, TYPE_PART, ORDRE_PART, ordre_prop) %>% unique()
# p <- anti_join(p, res, by=c("id", "ROLE_PART", "TYPE_PART", "ORDRE_PART"))
# app <- anti_join(app, res, by=c("id", "ROLE_PART", "TYPE_PART", "ordre_prop"))
# res <- inner_join(p, 
#                select(app, -ROLE_PART), 
#                by=c("id", "TYPE_PART", "ORDRE_PART")) %>% 
#   bind_rows(res) %>% 
#   unique()
# 
# p <- anti_join(p, res, by=c("id", "ROLE_PART", "TYPE_PART", "ORDRE_PART"))
# 
# if (nrow(p)>0){
#   res <- p %>% select(id, ORDRE_PART) %>% 
#     bind_rows(res)
# }
# 
# # ajout de res à temp3
# temp3 <-  res %>% select(id, ORDRE_PART, ordre_prop, TYPE_PART) %>% unique() %>% 
#   bind_rows(temp3)
# 
# temp4 <- anti_join(temp2, temp3, by="id") %>% 
#   anti_join(temp3, by = c("id"="id_prop"))
#   
# 
# temp3 <- bind_rows(temp3 , temp4)
# 
# # temp3 <- temp3 %>% 
# #   dplyr::mutate(id_prop = if_else(!is.na(ordre_prop), id, id_prop),
# #         type_prop = if_else(!is.na(id_prop)&is.na(type_prop), TYPE_PART, type_prop))
# 
# # suppression des lignes id qui sont dans id_prop et qui sont liés à un id part
# x <- temp3 %>% select(id_prop, ordre_prop, type_prop) %>% 
#   filter(!is.na(id_prop) | !is.na(ordre_prop) | !is.na(type_prop)) %>% unique()
# 
# temp3 <- anti_join(temp3, x, by = c("id"="id_prop", "ordre_prop", "type_prop"))
# 
# temp2 <- full_join(temp, temp1, by = c("id","id1", "pic2", "pic1","TYPE_PART")) %>% 
#   select(id, ordre_prop, ORDRE_PART, TYPE_PART) %>% unique()
# 
# temp2 <- anti_join(temp2, temp3, by = c("id", "ORDRE_PART", "TYPE_PART"))
# temp2 <- anti_join(temp2, temp3, by = c("id"="id_prop", "ordre_prop", "TYPE_PART"="type_prop"))
# temp2 <- anti_join(temp2, temp3, by = c("id", "ORDRE_PART"))
# temp2 <- anti_join(temp2, temp3, by = c("id"="id_prop", "ordre_prop"))
# temp2 <- anti_join(temp2, temp3, by = c("id"))


nrow(inner_join(part, temp3, by = c("id", "ORDRE_PART", 'TYPE_PART')))
temp3 <- temp3 %>% unique()

temp3 <- temp3 %>% 
  dplyr::mutate(id_prop=if_else(!is.na(prop) & is.na(id_prop), id, id_prop),
         ordre_prop=if_else(!is.na(prop) & is.na(ordre_prop), ORDRE_PART, ordre_prop),
         type_prop=if_else(!is.na(prop) & is.na(type_prop), TYPE_PART, type_prop),
         proposal=if_else(is.na(prop), "NO", "YES"),
         proposal=if_else(is.na(proposal) & prop==1, "YES", proposal))

subv_prop_tab <- temp3 %>% 
  left_join(select(applicant, c(id, ORDRE_PART, TYPE_PART, SUBV_PROP)), 
            by = c("id_prop"="id", "ordre_prop"="ORDRE_PART", 'type_prop'='TYPE_PART')) %>% 
  dplyr::mutate(ORDRE_PART=if_else(is.na(ORDRE_PART), ordre_prop, ORDRE_PART),
         TYPE_PART=if_else(is.na(TYPE_PART), type_prop, TYPE_PART)) %>% 
  unique()

# 
#   subv_prop_tab <- subv_prop_tab %>%
#     dplyr::mutate(ORDRE_PART=if_else(is.na(ORDRE_PART), ordre_prop, ORDRE_PART),
#         TYPE_PART=if_else(is.na(TYPE_PART), type_prop, TYPE_PART))

rm(list=ls(pattern="^temp"))
rm(list=c("supp", "x", "res", "pp", "p", "pic_ordre", "ordre_equal", "only_test", "only_np", 
          "only_npp", "one_to_one", "inter", "doublons", "app", "test", "np_1"))
gc()
