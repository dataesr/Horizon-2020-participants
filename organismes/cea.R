# RETOUR CEA

cea_b <- base %>% 
  dplyr::filter(str_detect(org, "CEA") | organizations_id=="775685019") %>% 
  mutate(org_id=if_else(org_id=="775685019", NA_character_, org_id), org="CEA") %>% 
  separate_rows(org_id, sep=",") %>% 
  unique() %>% 
  group_by(id, stage, participant_order, participates_as) %>% 
  mutate(n=n()) %>% 
  ungroup()

# cea_ss <- cea_b %>% dplyr::filter(is.na(org_id) | org_id=="775685019") %>% 
#   select(-organizations_id)

cea <- read_excel(paste0(source_org, "retour_org/CEA.xlsx"), col_types = "text") %>% 
  unique() %>% 
  select(project_id, org_temp=participant_id) %>% 
  unique()

test <- inner_join(cea_b, cea, by="project_id") # ok

# remplacer directement après verification pas de particularité
cea <- left_join(cea_b, cea, by = "project_id") %>% 
  mutate(org_id=ifelse(!is.na(org_temp), org_temp, org_id)) %>%
  select(-org_temp, -n, -role, -organizations_id, -year) %>%
  unique()
  # group_by(id, stage, project_id, participant_order, participates_as) %>% 
  # mutate(org_id=ifelse(!is.na(org_id), str_c(na.omit(unique(org_id)), collapse = ","), NA_character_)) %>% 
  # ungroup() %>% 
  # unique()


# test <- anti_join(cea, cea_b, by="project_id") %>% 
#   inner_join(base, by="project_id") %>% 
#   inner_join(participant, by = c("project_id", "year", "id", "stage", "participant_order", "participates_as", "role"))

# non utilisé
# cea_list <- cea %>% select(SIG_LABO, participant_id) %>% unique() %>% 
#   write.csv2(file=paste0(source_org, "cea_liste.csv"), row.names=FALSE)
# 
# temp <- read.csv2(file=paste0(source_org, "cea_liste.csv")) %>% unique()
# 
# cea <- cea %>% left_join(temp, by = "SIG_LABO")
# 
# cea <- cea %>% mutate(org_id = if_else(is.na(org_id), "775685019", org_id))
# x <- cea %>% count(id, stage)

# cea <- org_all %>% select(id, stage, org, org_id) %>% filter(org== "CEA")
