structure_benef <- load_one_obj(rdata =paste0(data_result, "data/calcul_structures_2021-03.Rdata"), "structure_benef" )

structure_benef <- projects_proposal %>% filter(stage=="project") %>% 
  select(-keywords_en, -description, -DT_LAST_UPDATE, -call_date, -dg_code, 
         -start_date, -end_date, -signature_date,-number_participant, -duration, 
         -topic_code, -topic_name, -source_url, ) %>%
  right_join(structure_benef, by = "project_id")

temp2 <- structure_benef %>% 
  select(-acronym_projet, -name_project, -budget_total, -budget_financed,
         -id, -ordre_part, -sector, -starts_with("cat"), -name, -acronym, -id_gref,
         -pic_id, -pic_id_2, -role, -participant_type_code, -participant_type_name, -country_code) %>%
  group_by_at(vars(-project_id, -funding_benef)) %>%
  summarise(tot_funding=sum(funding_benef), tot_project=n_distinct(project_id)) %>%
  ungroup()
prettyNum(sum(temp2$tot_funding), big.mark = " ")
prettyNum(sum(temp2$tot_project), big.mark = " ")

# pour verif des totaux
x <- part_fr %>% filter(stage=="project", type_part=="beneficiary") %>% 
  select(status,project_id) %>% unique()

structure_benef <- bind_rows(structure_benef, temp2)

#######################################################################################################
#######################################################################################################
structure_tut <- 
  load_one_obj(rdata =paste0(data_result, "data/calcul_structures_2021-03.Rdata"), "structure_tut" )

structure_tut <- projects_proposal %>% filter(stage=="project") %>% 
  select(-keywords_en, -description, -DT_LAST_UPDATE, -call_date, -dg_code, 
         -start_date, -end_date, -signature_date,-number_participant, -duration, 
         -topic_code, -topic_name, -source_url, ) %>%
  right_join(structure_tut, by = "project_id")

temp2 <- structure_tut %>% 
  select(-acronym_projet, -name_project, -budget_total, -budget_financed,
         -id, -ordre_part, -sector, -starts_with("cat"), -name, -acronym, -id_gref, -participates_as,
         -pic_id, -pic_id_2, -role, -participant_type_code, -participant_type_name, -country_code) %>%
  group_by_at(vars(-project_id, -funding_share)) %>%
  summarise(tot_funding=sum(funding_share), tot_project=n_distinct(project_id)) %>%
  ungroup()
prettyNum(sum(temp2$tot_funding), big.mark = " ")
prettyNum(sum(temp2$tot_project), big.mark = " ")

structure_tut <- bind_rows(structure_tut, temp2)

#########################################################################################################
########################################################################################################
structure_geoloc <- 
  load_one_obj(rdata =paste0(data_result, "data/calcul_structures_2021-03.Rdata"), "structure_geoloc" )

structure_geoloc <- structure_geoloc %>% mutate(project_id=str_sub(id, 1, 6))

structure_geoloc <- projects_proposal %>% filter(stage=="project") %>% 
  select(-keywords_en, -description, -DT_LAST_UPDATE, -call_date, -dg_code, 
         -start_date, -end_date, -signature_date,-number_participant, -duration, 
         -topic_code, -topic_name, -source_url, ) %>%
  right_join(structure_geoloc, by = "project_id")

save(structure_benef, structure_geoloc, structure_tut, 
     file=paste0(data_result, "data/datas_projects_2021-03.Rdata"))
