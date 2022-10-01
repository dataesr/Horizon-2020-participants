# subventions par tutelles labos + autres -> data pour tableau 
### PROJECT
load(file= paste0(data_result, "data/lien_id_proj.RData")) 
load(file= paste0(data_result, "data/subvent.RData"))

tut_proj <- lien_id_proj %>% 
  select(stage, status, id, participant_order, participates_as, organizations_id, participant_id, tutelle_id) %>% 
  unique() %>% 
  left_join(subvent, by = c("stage", "id", "participant_order", "participates_as")) %>% 
  dplyr::filter(stage=="project") %>% 
  unique() %>%
  group_by(stage, id, participant_order, participates_as, organizations_id, subv_net) %>% 
  dplyr::mutate(nb_p=n_distinct(participant_id), funding_p=subv_net/nb_p,
         nb_t=str_count(tutelle_id, pattern=",")+1, funding_tut_net=funding_p/nb_t) %>% 
  select(stage, project_id, status, id, participant_order, role, participates_as, organizations_id, funding=subv_net, 
         participant_id, funding_p, tutelle_id, funding_tut_net) %>% 
  separate_rows(tutelle_id) %>% 
  unique() %>% 
  ungroup()

###############################################
# répondre à l'ecart entre les fund beneficiaires et les subv net par tutelles
test <- proj_benef %>% 
  select(project_id, participant_order, participates_as, funding) %>% 
  group_by(project_id, participant_order) %>% 
  dplyr::summarise(f=sum(funding)) %>% 
  ungroup()
  
test2 <- tut_proj %>% 
  select(project_id, participant_order, participates_as, funding_tut_net) %>% 
  group_by(project_id, participant_order) %>% 
  dplyr::summarise(fnet=sum(funding_tut_net)) %>% 
  ungroup()

test3 <- left_join(test2, test) %>% 
  dplyr::mutate(diff=f-fnet) %>% 
  ungroup()
write.csv2(test3, file=paste0(identification, "compare_benef_fund_fund_tut.csv"), na="")
##########################################################"

prettyNum(sum(tut_proj$funding_tut_net, na.rm=T), big.mark = " ") # 7 368 081 756

message(
  "subv orga gestionnaire :\n",
  prettyNum(tut_proj %>% 
   select(id, participant_order, participates_as, organizations_id, funding) %>% 
   unique() %>% dplyr::summarize(sum(funding, na.rm=T)), big.mark = " "), "\n",
  "nombre de participants :\n",
  prettyNum(tut_proj %>% 
              select(id, participant_order, participates_as) %>% 
              unique() %>% 
              dplyr::summarize(n()), big.mark = " "), "\n",
  "nombre de projets :\n",
  prettyNum(tut_proj %>% select(project_id) %>% unique() %>% 
              dplyr::summarize(n()), big.mark = " "), "\n",
  
  "subv orga ident :\n",
  prettyNum(tut_proj %>% dplyr::filter(str_detect(organizations_id, "^NonIdent", negate = T)) %>% 
              select(id, participant_order, participates_as, organizations_id, funding) %>% 
              unique() %>% dplyr::summarize(sum(funding, na.rm=T)), big.mark = " "), "\n",
  "nombre de participants identifiés :\n",
  prettyNum(tut_proj %>% dplyr::filter(str_detect(organizations_id, "^NonIdent", negate = T)) %>% 
              select(id, participant_order, participates_as) %>% 
              unique() %>% dplyr::summarize(n()), big.mark = " "), "\n",
  "nombre de projets identifiés:\n",
  prettyNum(tut_proj %>% dplyr::filter(str_detect(organizations_id, "^NonIdent", negate = T)) %>% 
              select(project_id) %>% unique() %>% dplyr::summarize(n()), big.mark = " ")
) 
  
message(
  "thirdparty FR avec gestionnaire étranger :\n",
    prettyNum(tut_proj %>% dplyr::filter(participates_as!="beneficiary") %>% 
    select(stage, id, project_id, participant_order, funding_tut_net) %>% 
    left_join(participant , by =c("stage", "project_id", "participant_order")) %>% 
    dplyr::filter(participates_as=="beneficiary", country_code!="FR") %>% 
    dplyr::summarise(sum(funding_tut_net, na.rm=T)), big.mark = " ")
)
  


###############################################################
### PROPOSAL
# ELIGIBLE
tut_elig <- lien_id_proj %>% 
  select(stage, id, participant_order, participates_as, organizations_id, participant_id, tutelle_id) %>% 
  group_by(id, participant_order, participates_as, organizations_id) %>%
  dplyr::mutate(nb_p=n_distinct(participant_id), nb_t=str_count(tutelle_id, pattern=",")+1) %>%
  unique() %>% 
  left_join(subvent, by = c("stage", "id", "participant_order", "participates_as")) %>% 
  # dplyr::filter(stage=="proposal") %>% 
  dplyr::mutate(funding = if_else(proposal=="NO", subv_net, subv_prop), funding_p=funding/nb_p, 
         funding_tut_net=funding_p/nb_t, status="ELIGIBLE", stage="proposal") %>% 
  select(proposal, stage, status, id, participant_order, role, participates_as, organizations_id, 
         participant_id, tutelle_id, project_id, role, funding, funding_p, funding_tut_net) %>% 
  separate_rows(tutelle_id) %>% 
  ungroup() %>% 
  unique() 

# temp <- subv_prop_org %>% select(stage, id, participant_order, participates_as, organizations_id, funding) %>% 
#   count(stage, id, participant_order, participates_as, organizations_id, funding)

message(
  "subv tutelle :\n",
  prettyNum(sum(tut_elig$funding_tut_net, na.rm=T), big.mark = " "), "\n", # 34 624 182 559
  "subv tut identifiées \n",
  prettyNum(tut_elig %>% dplyr::filter(str_detect(organizations_id, "^NonIdent", negate = T)) %>% 
              dplyr::summarize(sum(funding_tut_net, na.rm=T)), big.mark = " "), "\n", 
  "nombre de participant :\n",
  prettyNum(tut_elig %>% 
    select(id, participant_order, participates_as) %>% unique() %>% 
      dplyr::summarize(n_distinct(id, participant_order, participates_as)), big.mark = " "), "\n",
  "nombre de participant idntifiés :\n",
  prettyNum(tut_elig %>% dplyr::filter(str_detect(organizations_id, "^NonIdent", negate = T)) %>% 
              select(id, participant_order, participates_as) %>% unique() %>% 
              dplyr::summarize(n_distinct(id, participant_order, participates_as)), big.mark = " "), "\n",
  "nombre de projets :\n",
  prettyNum(tut_elig %>% select(project_id) %>% unique() %>% dplyr::summarize(n()), big.mark = " "), "\n",
  "nombre de projets identifiés:\n",
  prettyNum(tut_elig %>% dplyr::filter(str_detect(organizations_id, "^NonIdent", negate = T)) %>% 
              select(project_id) %>% unique() %>% dplyr::summarize(n()), big.mark = " ") 
)
message( "valeurs sans rectification ; prendre la valeur à YES \n(rectification correspond à l'ajout de participations manquantes, un projet peut donc être comptabilisé 2x: \n",
lapply(tut_elig %>% select(proposal, funding_tut_net) %>% 
  group_by(proposal) %>% dplyr::summarize(sum(funding_tut_net, na.rm=T)), FUN = prettyNum, big.mark = " "), "\n",
tut_elig %>% select(proposal, id, participant_order, participates_as) %>% unique() %>% 
  group_by(proposal) %>% dplyr::summarize(n_distinct(id, participant_order, participates_as)), "\n",
tut_elig %>% select(proposal, project_id) %>% unique() %>% 
  group_by(proposal) %>% dplyr::summarize(n())
)
#########################################################################
## MAIN

tut_main <- lien_id_proj %>%
  dplyr::filter(status %in% c("MAIN", "SIGNED")) %>% 
  select(stage, id, participant_order, participates_as, organizations_id, participant_id, tutelle_id) %>% 
  group_by(id, participant_order, participates_as, organizations_id) %>%
  dplyr::mutate(nb_p=n_distinct(participant_id), nb_t=str_count(tutelle_id, pattern=",")+1) %>%
  unique() %>% 
  left_join(subvent, by = c("stage", "id", "participant_order", "participates_as")) %>% 
  # dplyr::filter(stage=="proposal") %>% 
  dplyr::mutate(funding = if_else(proposal=="NO", subv_net, subv_prop), funding_p=funding/nb_p, 
         funding_tut_net=funding_p/nb_t, status="MAIN", stage="proposal") %>% 
  select(proposal, stage, id, participant_order, role, participates_as, organizations_id, participant_id, tutelle_id, 
         project_id, role, funding, funding_p, funding_tut_net, status) %>% 
  separate_rows(tutelle_id) %>% 
  ungroup() %>% 
  unique() 


message(
  "subv tutelle :\n",
  prettyNum(tut_main %>% dplyr::summarize(sum(funding_tut_net, na.rm=T)), big.mark = " "), "\n",# 5 981 373 288
  "subv tutelle identifiée:\n",
  prettyNum(tut_main %>% dplyr::filter(str_detect(organizations_id, "^NonIdent", negate = T)) %>% 
              dplyr::summarize(sum(funding_tut_net, na.rm=T)), big.mark = " "), "\n",
  "nombre de participant :\n",
  prettyNum(tut_main %>% 
    select(id, participant_order, participates_as) %>% unique() %>% 
     dplyr::summarize(n_distinct(id, participant_order, participates_as)), big.mark = " "), "\n",
  "nombre de participant identifiés :\n",
  prettyNum(tut_main %>% dplyr::filter(str_detect(organizations_id, "^NonIdent", negate = T)) %>% 
              select(id, participant_order, participates_as) %>% unique() %>% 
              dplyr::summarize(n_distinct(id, participant_order, participates_as)), big.mark = " "), "\n",
  "nombre de projets :\n",
  prettyNum(tut_main %>% select(project_id) %>% unique() %>% dplyr::summarize(n()), big.mark = " "), "\n",
  "nombre de projets identifiés:\n",
  prettyNum(tut_main %>% dplyr::filter(str_detect(organizations_id, "^NonIdent", negate = T)) %>% 
              select(project_id) %>% unique() %>% dplyr::summarize(n()), big.mark = " ") 
)

message("valeurs non rectifiées \n",
  lapply(tut_main %>% select(proposal, funding_tut_net) %>% 
    group_by(proposal) %>% dplyr::summarize(sum(funding_tut_net, na.rm=T)), FUN=prettyNum, big.mark = " ") , "\n",
  tut_main %>% select(proposal, id, participant_order, participates_as) %>% unique() %>% 
    group_by(proposal) %>% dplyr::summarize(n_distinct(id, participant_order, participates_as)), "\n",
  tut_main %>% select(proposal, project_id) %>% unique() %>% 
    group_by(proposal) %>% dplyr::summarize(n_distinct(project_id))
)
######################################################################################################
######################################################################################################
tut <- bind_rows(tut_proj, tut_elig, tut_main) %>%  
  # separate_rows(tutelle_id) %>% 
  left_join(ref_info, by=c("tutelle_id"="id_ref")) %>% 
  dplyr::mutate(cat=if_else(category_5=="ENT" & str_detect(id_gref, "^gent"), "GE_ENT", cat),
                id_gref=if_else(is.na(id_gref), organizations_id, id_gref),
                funding_tut_net=if_else(str_detect(id_gref, "^NonIdent"), funding_p, funding_tut_net)) %>% 
  select(-proposal) %>% 
  unique()
  # left_join(select(part2, -status), by = c("stage", "project_id", "id", 'participant_order', "participates_as")) %>% 
  # select(stage, status, id, participant_order, participates_as, sector, starts_with("cat"), 
  #        organizations_id, participant_id, id_gref, tutelle_id, name, acronym, name_ref, acronym_ref, 
  #        project_id, role, country_code, funding, funding_p, funding_tut_net)
# prettyNum(sum(tut$funding_tut_net, na.rm=T), big.mark = " ")

#niveau groupe entreprise des participants
structure_tut <- tut %>% 
  group_by(stage, status, id_gref, name, acronym, project_id, role, participates_as, 
           sector, category, category_2, category_3, category_4, category_5, cat) %>% 
  dplyr::summarise(funding_share=sum(funding_tut_net)) %>% 
  ungroup() 

structure_tut %>% group_by(status) %>% 
  dplyr::summarize(sum(funding_share, na.rm=T))

############################################################################################
############################################################################################

### INFO niveau projet
# table proposal project avec info utile
temp <- prop_proj %>% 
  select(stage, project_id, year, call_code, topic_code, pilier, programme_abbr,programme_lib, 
         area_abbr, area_lib, action4_id, action4_lib,
         action3_id, action3_lib, action2_id, action2_lib, action1_id, action1_lib,
         panels_code_1, panels_name_1, panels_code_2, panels_name_2, msca_code, msca_name) %>% 
  unique()

#jointure structure_benef avec info project_id
structure_tut <- left_join(structure_tut, temp, by=c("project_id", "stage")) %>% unique()

########################################################################################################
# aggrégation des données totales calcul du funding_t, proj_t
total <- structure_tut %>% 
  group_by(stage, status, year, call_code, topic_code, action4_id, action4_lib,
           action3_id, action3_lib, action2_id, action2_lib, action1_id, action1_lib,
           pilier, programme_abbr,programme_lib, area_abbr, area_lib, 
           panels_code_1, panels_name_1, panels_code_2, panels_name_2, msca_code, msca_name) %>% 
  dplyr::summarize(funding_t=sum(funding_share, na.rm=T), proj_t=n_distinct(project_id)) %>% 
  ungroup()
##############################################################################
prettyNum(total %>% 
            dplyr::filter(stage=="proposal", status=="ELIGIBLE") %>% 
            dplyr::summarize(sum(funding_t, na.rm=T)), big.mark = " ")
prettyNum(total %>% 
            dplyr::filter(stage=="proposal", status=="MAIN") %>% 
            dplyr::summarize(sum(funding_t, na.rm=T)), big.mark = " ")
prettyNum(total %>% 
            dplyr::filter(stage=="project") %>% 
            dplyr::summarize(sum(funding_t, na.rm=T)), big.mark = " ")
prettyNum(total %>% 
            dplyr::filter(stage=="project") %>% 
            dplyr::summarize(sum(proj_t, na.rm=T)), big.mark = " ")

###################################################################################
# croisement category et total
cat <- structure_tut %>% 
  select(sector, starts_with("cate")) %>% 
  dplyr::filter(!is.na(category)) %>% 
  unique() %>% 
  merge(total) %>% 
  unique()

# concat total + structure benef + récupération libellé category
structure_tut <- bind_rows(structure_tut, cat) %>% 
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


    prettyNum(structure_tut %>% 
                dplyr::filter(stage=="proposal", status=="ELIGIBLE") %>% 
                dplyr::summarize(sum(funding_share, na.rm=T)), big.mark = " ")
    prettyNum(structure_tut %>% 
                dplyr::filter(stage=="proposal", status=="MAIN") %>% 
                dplyr::summarize(sum(funding_share, na.rm=T)), big.mark = " ")
    prettyNum(structure_tut %>% 
                dplyr::filter(stage=="project") %>% 
                dplyr::summarize(sum(funding_share, na.rm=T)), big.mark = " ")
    
    prettyNum(structure_tut %>% 
                dplyr::filter(stage=="proposal", status=="ELIGIBLE", str_detect(id_gref, "^NonIdent", negate=T)) %>% 
                dplyr::summarize(sum(funding_share, na.rm=T)), big.mark = " ")
    prettyNum(structure_tut %>% dplyr::filter(stage=="proposal", status=="MAIN", str_detect(id_gref, "^NonIdent", negate=T)) %>% 
                dplyr::summarize(sum(funding_share, na.rm=T)), big.mark = " ")
    prettyNum(structure_tut %>% dplyr::filter(stage=="project", str_detect(id_gref, "^NonIdent", negate=T)) %>% 
                dplyr::summarize(sum(funding_share, na.rm=T)), big.mark = " ")
    prettyNum(structure_tut %>% dplyr::filter(stage=="project", str_detect(id_gref, "^NonIdent", negate=T)) %>% 
                dplyr::summarize(sum(proj_t, na.rm=T)), big.mark = " ")
#########################

# sauvegarde
save(structure_tut, tut, cat, file=paste0(data_result, "structure_tut.Rdata"))

structure_tut <- structure_tut %>% dplyr::filter(!is.na(project_id))

write.csv2(structure_tut, file=paste0(data_result, "structure_tut.csv"), 
           na = "", row.names = F, fileEncoding = "UTF-8")

rm(list=c("tut_elig","tut_main", "tut_proj", "total", "cat", "structure_tut",
          "verif", "x", "temp", "cat_name"))
rm(list=ls(pattern="^test"))
gc()
