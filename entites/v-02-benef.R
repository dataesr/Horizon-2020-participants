load(file=paste0(data_result, "data/participants_etrangers.RData"))

prettyNum(sum(etr$subv, na.rm = T), big.mark = " ") # 60 219 755 579

benef_etr <- etr %>% 
  dplyr::filter(participates_as=="beneficiary") %>%
  select(stage, status, project_id, id_gref=organizations_id, name, role, participates_as,
         subv, subv_prop, country_code, country_name, country_level_1) %>%
  group_by(stage, status, project_id, id_gref, role, participates_as,
           country_code) %>% 
  mutate(nb_part=n(), subv=sum(subv, na.rm=T), subv_prop=sum(subv_prop, na.rm=T)) %>% 
  unique() %>% 
  ungroup()

  prettyNum(sum(benef_etr$subv, na.rm = T), big.mark = " ") # 60 221 485 015

  
etr_sign <- benef_etr %>% 
  dplyr::filter(status=='SIGNED') %>% 
  select(-subv_prop) %>% 
  rename(funding=subv)
prettyNum(sum(etr_sign$funding), big.mark = " ") # 60 221 485 015

etr_main <- benef_etr %>% 
  dplyr::filter(status %in% c('MAIN', 'SIGNED')) %>% 
  select(-subv) %>% 
  mutate(stage='proposal', status='MAIN') %>% 
  rename(funding=subv_prop)

etr_elig <- benef_etr %>% 
  mutate(stage='proposal', status="ELIGIBLE") %>% 
  select(-subv) %>% 
  rename(funding=subv_prop)

benef_etr <- bind_rows(etr_elig, etr_main, etr_sign)

###########################################################################################
load(file=rdata_last_version(paste0(data_result, "data/"), "participant_ID_d02_"))
load(file= paste0(data_result, "data/lien_id_proj.RData"))
load(file= paste0(data_result, "data/subvent.RData"))
load(file=rdata_last_version(paste0(data_result, "data/"), "ref_part_q01_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "prop_proj_"))
cat_name <- readxl::read_xlsx(paste0(participants, "nomenclatures/_category.xlsx"), 
                              sheet="category_name", na="") %>% dplyr::mutate_all(~str_squish(.))


# subventions par organizations principales
# niveau projets 
proj_benef <- subvent %>% 
  dplyr::filter(stage=="project", participates_as=="beneficiary") %>% 
  select(stage, project_id, id, participant_order, role, participates_as, subv) %>% 
  unique()
prettyNum(sum(proj_benef$subv), big.mark = " ") # tous les benef : 67 670 118 811

proj_benef <- proj_benef %>% 
  inner_join(lien_id_proj, by=c("stage", "project_id", "id", "participant_order", "participates_as")) %>% 
  select(stage, status, id, project_id, participant_order, role, participates_as, organizations_id, subv) %>% 
  unique() %>% 
  group_by(id, participant_order, participates_as) %>% 
  dplyr::mutate(nb_part=n(),
         nb_org=n_distinct(organizations_id),
         funding=subv/nb_org) %>% 
  ungroup() %>% 
  select(stage, status, project_id, id, participant_order, role, participates_as, 
         organizations_id, funding, nb_part)

  message("verifs données à conserver:", "\n",
        "les subv des third-paries étrangers dont benef français", "\n",
        prettyNum(
          proj_benef %>% 
          select(stage, project_id, participant_order) %>% 
          unique() %>% 
          left_join(participant) %>% 
          dplyr::filter(country_code !="FR") %>% 
          dplyr::summarise(sum(subv_net, na.rm=T)), big.mark = " ")
          ) # 126 548 499


  prettyNum(sum(proj_benef$funding), big.mark = " ") # 7 450 363 232
  prettyNum(proj_benef %>% select(id, participant_order, participates_as) %>% unique() %>% 
              n_distinct(), big.mark = " ") # 14 081
  prettyNum(sum(proj_benef$nb_part), big.mark = " ") # 14 081
  prettyNum(proj_benef %>% select(project_id) %>% unique() %>% 
              n_distinct(), big.mark = " ") # 7 783

if (nrow(proj_benef %>% 
         select(id, participant_order, organizations_id) %>% 
         dplyr::count(id, participant_order) %>% 
         dplyr::filter(n>1))>1){
  print("attention : vérifier les doublons dans x")
  x <- proj_benef %>% 
    select(id, participant_order, organizations_id) %>% 
    dplyr::count(id, participant_order) %>% 
    dplyr::filter(n>1)
}else{
  print("ok")
  }


#######################################################################################################
#prop tous les participants
prop_benef <- subvent %>% 
  dplyr::filter(participates_as=="beneficiary") %>%
  select(stage, id, project_id, participant_order, role, participates_as, subv, subv_prop) %>% 
  unique() 
# %>% 
#   left_join(statut, by="project_id") %>% dplyr::rename(stat=status)
  prettyNum(sum(prop_benef$subv_prop, na.rm=T), big.mark = " ") # 557 024 610 740
  prettyNum(prop_benef %>% select(id, participant_order, participates_as) %>% unique() %>% 
    n_distinct(), big.mark = " ") # 1 053 797
  prettyNum(prop_benef %>% select(project_id) %>% unique() %>% 
              n_distinct(), big.mark = " ") # 299 420

#####################################
# ELIGIBLE -> lien avec lien_id_proj (structures FR)
prop_benef1 <- prop_benef %>% 
  inner_join(lien_id_proj, by=c("stage", "id", "project_id", "participant_order", "participates_as")) %>% 
  # dplyr::mutate(subv_prop = if_else(proposal=="NO", subv, subv_prop)) %>% 
  select(proposal, stage, id, project_id, participant_order, role, participates_as, organizations_id, subv_prop) %>% 
  unique() %>% 
  group_by(id, participant_order, participates_as) %>% 
  dplyr::mutate(stage = "proposal", status = "ELIGIBLE",
         nb_part=n(),
         nb_org=n_distinct(organizations_id),
         funding=subv_prop/nb_org) %>% 
  select(proposal, stage, status, project_id, id, participant_order, role, participates_as, organizations_id, 
         funding, nb_part) %>% 
  ungroup()

  message("indicateurs proposals eligible avec rectificatif \n",
  "subv_prop \n",
  prettyNum(sum(prop_benef1$funding, na.rm=T), big.mark = " "), "\n",
  "nombre de paricipants \n",
  nrow(prop_benef1 %>% select(id, participant_order, participates_as) %>% unique()), "\n",
  "nombre de paricipants caluclés \n",
  prettyNum(sum(prop_benef1$nb_part, na.rm=T), big.mark = " "), "\n",
  "nombre de projets \n",
  n_distinct(prop_benef1$project_id), "\n\r",
  "subv prop sans rectificatif \n",
  prettyNum(prop_benef1 %>% dplyr::filter(proposal=="YES") %>% 
              summarize(sum(funding, na.rm=T)), big.mark = " "), "\n",
  "participants non rectifié \n",
  nrow(prop_benef1 %>% dplyr::filter(proposal=="YES") %>% 
              select(id, participant_order, participates_as) %>% unique()), "\n",
  "projets non rectifié \n",
  nrow(prop_benef1 %>% dplyr::filter(proposal=="YES") %>% 
         select(project_id) %>% unique())
  )

# nrow(anti_join(prop_benef1 %>% filter(proposal=="NO") %>% select(project_id) %>% unique(),
     # prop_benef1 %>% filter(proposal=="YES") %>% select(project_id) %>% unique(), by="project_id"))

###########################################################
#MAIN
prop_benef2 <- prop_benef %>% 
  inner_join(lien_id_proj, by=c("stage", "id", "project_id", "participant_order", "participates_as")) %>% 
  dplyr::filter(status %in% c("SIGNED", "MAIN")) %>% 
  # dplyr::mutate(subv_prop = if_else(proposal=="NO", subv, subv_prop)) %>% 
  select(proposal, stage, id, project_id, participant_order, role, participates_as, organizations_id, subv_prop) %>% 
  unique() %>% 
  group_by(id, participant_order, participates_as) %>% 
  dplyr::mutate(stage = "proposal", status = "MAIN",
         nb_part=n(),
         nb_org=n_distinct(organizations_id),
         funding=subv_prop/nb_org) %>% 
  select(proposal, stage, status, project_id, id, participant_order, role, participates_as, organizations_id, 
         funding, nb_part) %>% 
  ungroup()


  message("indicateurs proposals main avec rectificatif \n",
        "subv_prop \n",
        prettyNum(sum(prop_benef2$funding, na.rm=T), big.mark = " "), "\n",
        "nombre de paricipants \n",
        nrow(prop_benef2 %>% select(id, participant_order, participates_as) %>% unique()), "\n",
        "nombre de paricipants \n",
        prettyNum(sum(prop_benef2$nb_part, na.rm=T), big.mark = " "), "\n",
        "nombre de projets \n",
        n_distinct(prop_benef2$project_id), "\n\r",
        "subv prop sans rectificatif \n",
        prettyNum(prop_benef2 %>% dplyr::filter(proposal=="YES") %>% 
                    summarize(sum(funding, na.rm=T)), big.mark = " "), "\n",
        "participants non rectifié \n",
        nrow(prop_benef2 %>% dplyr::filter(proposal=="YES") %>% 
               select(id, participant_order, participates_as) %>% unique()), "\n",
        "projets non rectifié \n",
        nrow(prop_benef2 %>% dplyr::filter(proposal=="YES") %>% 
               select(project_id) %>% unique())
)

###############################################################
###############################################################
### concat les 3 tables
# lien avec ref_info
benef <- bind_rows(proj_benef, prop_benef1, prop_benef2) %>% 
  left_join(ref_info, by = c("organizations_id"="id_ref")) %>% 
  dplyr::mutate(id_gref=if_else(is.na(id_gref), organizations_id, id_gref)) %>% 
  select(-name_ref, -acronym_ref, -cat, -proposal) %>% 
  ungroup()

# verif status
unique(benef$status)

toString(colnames(benef))

# suppression du id et participant_order
structure_benef <- benef %>% 
  group_by(stage, status, id_gref, name, acronym, project_id, role, participates_as, 
           sector, category, category_2, category_3, category_4, category_5) %>% 
  dplyr::summarize(funding=sum(funding, na.rm = T), nb_part=sum(nb_part, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(country_code='FR', country_name='France', country_level_1="Pays membres") %>% 
  unique()

structure_benef <- bind_rows(structure_benef, benef_etr)

verif_na <- structure_benef[apply(structure_benef, 2, function(x) any(is.na(x)))]

  message("subv par statut :\n", 
      structure_benef %>% group_by(status) %>% 
        dplyr::summarize(sum(funding, na.rm=T)),"\n",
      structure_benef %>% group_by(status) %>% 
         dplyr::summarize(sum(nb_part)), "\n",
      structure_benef %>% group_by(status) %>% 
         dplyr::summarize(n_distinct(project_id))
      ) #7 318 583 978.17

##################################################################################################
##################################################################################################
### INFO niveau projet
# table proposal project avec info utile
temp <- prop_proj %>% 
  select(stage, project_id, year, call_code, topic_code, pilier, programme_abbr,programme_lib, 
         area_abbr, area_lib, action4_id, action4_lib,
         action3_id, action3_lib, action2_id, action2_lib, action1_id, action1_lib,
         panels_code_1, panels_name_1, panels_code_2, panels_name_2, msca_code, msca_name) %>% 
  unique()

#jointure structure_benef avec info project_id
structure_benef <-
  left_join(structure_benef, temp, by=c("project_id", "stage")) %>% unique()

verif_na <- structure_benef[apply(structure_benef, 2, function(x) any(is.na(x)))]

  message("subv par statut :\n",
        structure_benef %>% group_by(status) %>% 
          dplyr::summarize(sum(funding, na.rm=T)),
        structure_benef %>% group_by(status) %>% 
          dplyr::summarize(sum(nb_part)),
        structure_benef %>% group_by(status) %>% 
          dplyr::summarize(n_distinct(project_id))
  )


# aggrégation des données totales calcul du funding_t, proj_t
total <- structure_benef %>% 
  group_by(stage, status, year, call_code, topic_code, action4_id, action4_lib,
           pilier, programme_abbr,programme_lib, area_abbr, area_lib, 
           action3_id, action3_lib, action2_id, action2_lib, action1_id, action1_lib,
           panels_code_1, panels_name_1, panels_code_2, panels_name_2, msca_code, msca_name) %>% 
  dplyr::summarize(funding_tot=sum(funding, na.rm=T), part_t=sum(nb_part, na.rm=T), proj_t=n_distinct(project_id)) %>% 
  ungroup()


    prettyNum(total %>% dplyr::filter(stage=="proposal", status=="ELIGIBLE") %>% 
                summarize(sum(funding_tot)), big.mark = " ")
    prettyNum(total %>% dplyr::filter(stage=="proposal", status=="MAIN") %>% 
                summarize(sum(funding_tot)), big.mark = " ")
    prettyNum(total %>% dplyr::filter(stage=="project") %>% summarize(sum(funding_tot)), big.mark = " ")
    prettyNum(total %>% dplyr::filter(stage=="project") %>% summarize(sum(part_t)), big.mark = " ")


# croisement category et total
cat <- structure_benef %>% select(sector, starts_with("cate")) %>% 
  dplyr::filter(!is.na(category)) %>% unique() %>% 
  merge(total) %>% unique()

# concat total + structure benef + récupération libellé category
# structure_benef <- bind_rows(structure_benef, cat) %>% 
structure_benef <- structure_benef %>%
left_join(cat_name, by="category") %>% 
  dplyr::rename(category_1_name=category_name) %>% 
  left_join(cat_name, by=c("category_2"="category")) %>% 
  dplyr::rename(category_2_name=category_name) %>% 
  left_join(cat_name, by=c("category_3"="category")) %>% 
  dplyr::rename(category_3_name=category_name) %>% 
  left_join(cat_name, by=c("category_4"="category")) %>% 
  dplyr::rename(category_4_name=category_name) %>% 
  left_join(cat_name, by=c("category_5"="category")) %>% 
  dplyr::rename(category_5_name=category_name)



    prettyNum(structure_benef %>% dplyr::filter(stage=="proposal", status=="ELIGIBLE") %>% 
                summarize(sum(funding, na.rm=T)), big.mark = " ")
    prettyNum(structure_benef %>% dplyr::filter(stage=="proposal", status=="MAIN") %>% 
                summarize(sum(funding, na.rm=T)), big.mark = " ")
    prettyNum(structure_benef %>% dplyr::filter(stage=="project") %>% 
                summarize(sum(funding, na.rm=T)), big.mark = " ")


verif_na <- structure_benef[apply(structure_benef, 2, function(x) any(is.na(x)))]

# sauvegarde

structure_benef <- structure_benef %>% dplyr::filter(!is.na(project_id))

save(structure_benef, total, file=paste0(data_result, "structure_benef.Rdata"))

write.csv2(structure_benef, file=paste0(data_result, "structure_benef.csv"), na = "", 
           row.names = F, fileEncoding = "UTF-8")

rm(list=c("prop_benef", "prop_benef1", "prop_benef2", "benef", "total", "cat", 
          "temp", "structure_benef", "benef_etr", "etr_elig", "etr_main", "etr_sign"))
gc()
   