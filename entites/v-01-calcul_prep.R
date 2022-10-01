#PARTICIPANTS
load(file=rdata_last_version(paste0(data_result, "data/"), "participant_ID_d02_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "key_part_p02_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "part_fr_p01_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "ref_part_q01_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "prop_proj_"))
cat_name <- readxl::read_xlsx(paste0(participants, "nomenclatures/_category.xlsx"), 
                              sheet="category_name", na="") %>% dplyr::mutate_all(~str_squish(.))


message("doublons ",
nrow(test <- participant %>% #filter(country_code=="FR") %>%
  select(project_id, participant_pic, applicant_pic, participates_as, participant_order) %>% 
  dplyr::count(project_id, participant_pic, applicant_pic, participates_as, participant_order) %>% 
  dplyr::filter(n>1)) )


message("doublons de id dans les projets part: ",
      nrow(participant %>% 
      dplyr::filter(stage=="project") %>% 
      select(stage, project_id, participant_pic, applicant_pic, participant_order) %>%  
      dplyr::count(project_id, participant_pic, applicant_pic, participant_order) %>% 
      dplyr::filter(n>1)))

message("nombre de participations FR dans projets : \n",
        nrow(participant %>% 
            dplyr::filter(country_code=="FR", stage=="project") %>% 
            select(project_id, participant_pic, applicant_pic, participant_order, participates_as) %>% 
              unique()),"\n",
       "nombre de participations FR main : \n",
       nrow(participant %>% 
              dplyr::filter(country_code=="FR", status %in% c("MAIN", "SIGNED", "UNDER_PREPARATION")) %>% 
              select(project_id,participant_pic, applicant_pic, participant_order, participates_as) %>% 
              unique()),"\n",
       "nombre de participations FR eligible : \n",
       nrow(participant %>% 
              dplyr::filter(country_code=="FR") %>% 
              select(project_id,participant_pic, applicant_pic, participant_order, participates_as) %>% 
              unique())
       
)
# 17 903 avec participates_as

subvent <- participant %>% 
  select(proposal, stage, project_id, id, participant_order, role, participates_as, country_code, subv, subv_net, subv_prop) %>% 
  dplyr::mutate(#subv_prop = as.double(subv_prop), subv = as.double(subv), subv_net = as.double(subv_net),
         subv_net = if_else(subv_net<0, 0, subv_net),
         subv_net = if_else(is.na(subv_net), subv, subv_net),
         subv_prop = if_else(proposal=="NO", subv_net, subv_prop)
  ) 

subvent <- subvent %>% 
  group_by(stage, project_id, id, participant_order, role, participates_as, country_code) %>% 
  dplyr::mutate(nb=n()) %>% 
  ungroup()


message("subventions tot projets : \n",
prettyNum(subvent %>% 
            dplyr::filter(stage=="project") %>% #67 670 118 811
            select(subv) %>% summarize(sum(subv)), big.mark=" "), "\n",
"subventions nettes par participant (thirdparty,...) : \n",
prettyNum(subvent %>% 
            dplyr::filter(stage=="project") %>% 
            select(subv_net) %>% summarize(sum(subv_net)), big.mark=" "), "\n",
"subventions propositions avec le rectificatif des projets non présents dans proposal : \n",
  prettyNum(subvent %>% select(subv_prop) %>% summarize(sum(subv_prop, na.rm=T)), big.mark=" ")
) # 563 098 141 629



if (subv_tot==subv_sh){
  print("ok")
}else{
  ecart_subv_subv_sh <- subvent %>% 
    dplyr::filter(stage=="project") %>% 
    select(stage, project_id, ordre_part, subv, subv_net) %>%
    group_by(project_id, ordre_part) %>% 
    summarise(subv=sum(subv), subv_net=sum(subv_net), diff=sum(subv)-sum(subv_net)) %>% 
    filter(floor(diff)>0) %>% 
    ungroup()
  if (nrow(ecart_subv_subv_sh)>0){
    message("subv différent de subv_net ; vérifier l'écart ;\nl'écart s'explique par les participants étrangers thirdparty... à vérifier")
  }else{
    print("ok")
    rm(ecart_subv_subv_sh)}
}

################################################################################################
unique(part_fr$status)
unique(prop_proj$status_orig)
#création table avec tous les id pour jointure avec subvent
lien_id_proj <- part_fr %>% #filter(stage=="project") %>% 
  select(stage, project_id, status, proposal, id, participant_order, participates_as) %>% 
  unique() %>% 
  inner_join(key_part, by=c("stage", "id", "participant_order", "participates_as")) %>% 
  dplyr::mutate(status_orig=status,
         status=if_else(status_orig=="UNDER_PREPARATION", "SIGNED", status_orig)) %>% 
  dplyr::mutate_if(is.character, str_squish) %>% 
  unique()


# lien avec ls infos projets proposals
lien_id_proj <- prop_proj %>% 
  select(project_id, stage, programme_abbr) %>% 
  unique() %>% 
  inner_join(lien_id_proj)

# suppression des participations non françaises avec un code country FR
# etr <- lien_id_proj %>% filter(str_detect(organizations_id, "^grid.(?!449847.2)"))
# lien_id_proj <- anti_join(lien_id_proj, etr, by="organizations_id")

verif <- inner_join(lien_id_proj, subvent, by=c("stage", "id", "participant_order", "participates_as"))
if (nrow(verif)==nrow(lien_id_proj)){print("ok")
  }else{print("vérifier le lien lien_id_proj et subvent")}


message(
  "nombre de participations signées absentes des proposals : \n",
  lien_id_proj %>% 
    dplyr::filter(proposal=="NO") %>% 
    summarise(n_distinct(id, participant_order, participates_as)), "\n",
  "concerne les projets : \n",
  lien_id_proj %>% 
    dplyr::filter(proposal=="NO") %>% 
    summarise(n_distinct(project_id)), "\n",
  "dont projets non français dans proposals : \n",
  nrow(lien_id_proj %>% 
         dplyr::filter(proposal=="NO") %>% 
         select(project_id) %>% 
         unique())-
  nrow(lien_id_proj %>% 
         dplyr::filter(proposal=="NO") %>% 
         select(project_id) %>% 
         unique() %>% 
      left_join(select(lien_id_proj, c(project_id, proposal)), by="project_id") %>% 
      unique() %>% 
      dplyr::filter(proposal=="YES"))
)

message(
  "subv_prop eligible \n",
  prettyNum(sum(subvent$subv_prop, na.rm = T), big.mark= " "), "\n",
  "nombre de participations proposals : \n",
  nrow(subvent %>% #filter(proposal=="YES") %>% 
    select(id, participant_order, participates_as) %>% unique()), "\n",
  "concerne les projets : \n",
  nrow(subvent %>% #filter(proposal=="YES") %>% 
    select(project_id) %>% unique()), "\n",
  "\r",
  "subv_prop main \n",
  participant %>% dplyr::filter(status %in% c("MAIN", "SIGNED")) %>% 
           left_join(subvent) %>% summarize(sum(subv_prop, na.rm = T)), "\n",
  "nombre de participations main : \n",
  nrow(participant %>% dplyr::filter(status %in% c("MAIN", "SIGNED")) %>% 
         select(id, participant_order, participates_as) %>% unique()), "\n",
  "concerne les projets main: \n",
  nrow(participant %>% dplyr::filter(status %in% c("MAIN", "SIGNED")) %>% 
         select(project_id) %>% unique()),"\n",
  "\r",
  "subv_prop proj \n",
  participant %>% dplyr::filter(status %in% c("SIGNED")) %>% 
    left_join(subvent) %>% summarize(sum(subv_prop, na.rm = T)), "\n",
  "nombre de participations main : \n",
  nrow(participant %>% dplyr::filter(status %in% c( "SIGNED")) %>% 
         select(id, participant_order, participates_as) %>% unique()), "\n",
  "concerne les projets main: \n",
  nrow(participant %>% dplyr::filter(status %in% c("SIGNED")) %>% 
         select(project_id) %>% unique())
)
#############################################################################################
### non identifié
lien_id_proj %>% 
  dplyr::filter(startsWith(organizations_id, "NonIdent")) %>% 
  select(status, project_id, participant_order, participates_as) %>% 
  group_by(status) %>% 
  summarize(n_distinct(project_id, participant_order, participates_as))

lien_id_proj %>% 
  dplyr::filter(startsWith(organizations_id, "NonIdent")) %>% 
  select(status, project_id) %>% 
  group_by(status) %>% 
  summarize(n_distinct(project_id))

lien_id_proj %>% 
  dplyr::filter(startsWith(organizations_id, "NonIdent")) %>% 
  left_join(subvent, by = c("stage", "id", "participant_order", "participates_as", "project_id")) %>% 
  select(status, starts_with("subv")) %>% 
  group_by(status) %>% 
  summarize(s=sum(subv, na.rm=T), sn=sum(subv_net, na.rm=T), sp=sum(subv_prop, na.rm=T))


save("lien_id_proj", file= paste0(data_result, "data/lien_id_proj.RData"))
save("subvent", file= paste0(data_result, "data/subvent.RData"))

rm(list=c("verif", "participant"))
gc()

part_fr <- part_fr %>% 
  relocate(status, stage, project_id, participant_order, participates_as, participant_pic, 
           applicant_pic, entite)

