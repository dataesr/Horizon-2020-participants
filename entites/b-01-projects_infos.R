############################################################################"

# récupération de la dernière actualisation des projets pour avoir les infos actuelles à afficher
# proj_new <- load_one_obj(rdata=paste0(chemin_pc, "traitement/2021_04/donnees/datas.Rdata"), "project") %>% 
#   dplyr::mutate(stage = "project", status = "SIGNED") %>%
#   select(-STADE_PROJET, -programme, -area, -panel_2, -panel_1, -panel1_tri, -panel2_tri, -programme_tri, -area_tri, -pilier_tri,
#          -action_1, -action_2, -action_3, -action2_tri, -action1_tri, -action3_tri, -pilier_abbr)


#chargement des datas nettoyées
proposal <- load_one_obj(paste0(data_source,"donnees/datas.RData"), "proposal")
project <- load_one_obj(paste0(data_source,"donnees/datas.RData"), "project")


#ajustement dans libellé variables pour faire correspondre proposal et project + ajout variables à project
proposal <- proposal %>% 
  dplyr::mutate(stage = "proposal", status_orig=STATUT_EVAL, status=if_else(STATUT_EVAL=="MAIN", "MAIN", "ELIGIBLE")) %>% 
  select(-ELIGIBLE, -STATUT_EVAL, -programme, -area, -panel_2, -panel_1, -panel1_tri, -panel2_tri, 
         -programme_tri, -area_tri, -pilier_tri, -pilier_abbr,
         -action_1, -action_2, -action_3, -action_4, -action2_tri, -action1_tri, -action3_tri, -action4_tri)
colnames(proposal) <- tolower(colnames(proposal))

project <- project %>%
  dplyr::mutate(stage = "project", status = "SIGNED", status_orig = STADE_PROJET) %>%
  select(-STADE_PROJET, -programme, -area,  -programme_tri, -area_tri, -pilier_tri, -pilier_abbr,
         -panel_2, -panel_1, -panel1_tri, -panel2_tri, -action_1, -action_2, -action_3, -action_4,
          -action2_tri, -action1_tri, -action3_tri, -action4_tri)
colnames(project) <- tolower(colnames(project))
###########################################################################################
# INFOS pour SCANR
# diff entre new contrats et anciens
# temp <- anti_join(project, proj_new, by="NUM_PROJET")
# temp2 <- key_part %>%  dplyr::mutate(NUM_PROJET=as.numeric(str_sub(id, 1,6))) %>% 
#   inner_join(temp, by="NUM_PROJET") %>% select(NUM_PROJET, programme_abbr) %>% unique() %>% 
#     group_by(programme_abbr) %>% summarize(n_distinct(NUM_PROJET))
# 
# 
# proj_scan <- inner_join(select(project, NUM_PROJET), proj_new, by="NUM_PROJET")

# project <- proposal %>% select(num_projet, date_call, key_words) %>% unique() %>%
# right_join(project, by = "num_projet")
  
proj_scan <- project %>%
  dplyr::filter(status_orig=="SIGNED") %>% 
  dplyr::mutate(id=as.character(num_projet), type="H2020",
         source_url = paste0("https://cordis.europa.eu/project/id/", id), 
         panels = if_else(programme_abbr == "ERC", panel2_id, NA_character_),
         msca_code = if_else(programme_abbr == "MSCA", panel2_id, NA_character_),
         msca_name = if_else(programme_abbr == "MSCA", panel2_lib, NA_character_),
         duration = round(duration, digits = 0),
         nb_part = as.integer(nb_part)) %>% 
  select(id, type, stage, status, name=titre_projet, description=abstract, acronym=acronym_projet, 
         keywords_en=key_words, year=annee, signature_date=date_signe, start_date=date_start, 
         end_date=date_end, budget_total=cout_projet, budget_financed=cout_part, 
         dg_code=code_dg, duration, source_url, number_participant=nb_part, call_code=appel, 
         call_date=date_call, topic_code=cd_topic, topic_name=lib_topic, 
         pilier, programme_abbr, programme_lib, area_abbr, area_lib, msca_code, msca_name, panels,
         action2_id, action2_lib, action1_id, action1_lib, action3_id, action3_lib,
         action4_id, action4_lib) %>% 
  unique()

verif_na <- proj_scan[apply(proj_scan, 2, function(x) any(is.na(x)))]
nrow(proj_scan %>% select(id) %>% unique())
#provisoire : juste pour charger les projest dans scanr


###############################################################################################"

#création dataframe complet proposal + projets
prop_proj <- bind_rows(project,proposal) %>% 
  dplyr::mutate(type = "H2020",
         project_id = as.character(num_projet),
         panels_code_1 = if_else(programme_abbr == "ERC", panel1_id, NA_character_),
         panels_name_1 = if_else(programme_abbr == "ERC", panel1_lib, NA_character_),
         panels_code_2 = if_else(programme_abbr == "ERC", panel2_id, NA_character_),
         panels_name_2 = if_else(programme_abbr == "ERC", panel2_lib, NA_character_),
         msca_code = if_else(programme_abbr == "MSCA", panel2_id, NA_character_),
         msca_name = if_else(programme_abbr == "MSCA", panel2_lib, NA_character_),
         duration = round(duration, digits = 0),
         number_participant = as.integer(nb_part)) %>% 
  dplyr::rename(name_project = titre_projet, acronym_projet = acronym_projet, 
                keywords_en = key_words, year = annee,
                signature_date = date_signe, start_date = date_start, end_date = date_end, 
                budget_total = cout_projet, budget_financed = cout_part,
                dg_code = code_dg, call_code = appel, call_date = date_call,
                topic_code = cd_topic, topic_name = lib_topic, description = abstract) %>% 
  select(-panel1_id, -panel2_lib, -panel2_id, -panel1_lib, -num_projet)

# table project_id + statut projet
statut <- prop_proj %>% filter(stage=='project') %>% select(project_id, status) %>% unique()
statut <- prop_proj %>% anti_join(statut, by="project_id") %>% select(project_id, status) %>% 
  unique() %>% 
  bind_rows(statut)

# provisoire pour caler la nomenclature des actions qui sera corrigée à la prochaine actu
# actions <- read.csv2(paste0(chemin_pc, "traitement/exe/nomenclature/actions_gen.csv"))
# modif_action <- function(tab){
#   tab <- tab %>% dplyr::rename(type_fi=action3_id) %>% select( -starts_with("action")) %>% 
#           left_join(select(actions, 
#                    c(action4_id, action4_lib, action3_id, action3_lib, action2_id, 
#                     action2_lib, action1_id, action1_lib)),
#             by=c("type_fi"="action4_id")) %>% 
#           dplyr::rename(action4_id=type_fi)
# return(tab)
# }
# prop_proj <- modif_action(prop_proj)
# proj_scan <- modif_action(proj_scan)

save(prop_proj, file = paste0(data_result,"data/prop_proj_", date_sauve, ".RData"))

write.csv2(proj_scan,file = paste0(data_result, "data/projects_", livraison, ".csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

rm(list= c("proj_scan","project", "proposal"))
gc()
