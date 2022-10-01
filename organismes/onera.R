# RETOUR ONERA

onera_b <- base %>% 
  dplyr::filter(str_detect(org, "ONERA") | organizations_id=="775722879") %>% 
  mutate(org_id=if_else(org_id=="775722879", NA_character_, org_id), org="ONERA") %>% 
  separate_rows(org_id, sep=",") %>% 
  unique() %>% 
  group_by(id, stage, participant_order, participates_as) %>% 
  mutate(n=n()) %>% 
  ungroup()



onera <- read_excel(paste0(source_org, "retour_org/ONERA.xlsx"), col_types = "text", sheet="Réponse_OV") %>%
  dplyr::rename(org_id=participant_id) %>% 
  select(stage, id, project_id, participant_order, participates_as, org_id) %>% 
  mutate(participant_order=as.numeric(participant_order), org_temp=if_else(org_id=="775722879", NA_character_, org_id)) %>% 
  separate_rows(org_temp,  sep=",") %>% 
  select(-org_id) %>% 
  unique()


test <- inner_join(onera_b,  onera, 
        by = c("stage", "id", 'project_id', 'participant_order', 'participates_as')) %>% 
  unique()


non_onera <- anti_join(onera_b, onera, 
                   by = c("stage", "id", 'project_id', 'participant_order', 'participates_as')) %>% 
  unique()

onera <- left_join(onera_b, onera,
                  by = c("stage", "id", 'project_id', 'participant_order', 'participates_as')) %>% 
  mutate(org_id=ifelse(!is.na(org_temp), org_temp, org_id), org="ONERA") %>%
  select(-org_temp, -role, -organizations_id, -year) %>%
  unique()
  # group_by(id, stage, project_id, participant_order, participates_as) %>% 
  # mutate(org_id=ifelse(!is.na(org_id), str_c(na.omit(unique(org_id)), collapse = ","), NA_character_)) %>% 
  # ungroup() %>% 
  # unique()


#non utilisé
# pivot <- onera_b %>% select(id, stage, org, org_id) %>% unique()
# temp <- pivot_wider(pivot, names_from = "org", values_from = 'org_id', names_repair = "check_unique", values_fn = toString)
# onera <- temp %>% dplyr::rename(org_id=ONERA) %>% mutate(org="ONERA")
