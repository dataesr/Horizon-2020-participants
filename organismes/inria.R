# RETOUR INRIA

# 
inria_b <- base %>% 
  dplyr::filter(str_detect(org, "INRIA") | organizations_id=="180089047") %>% 
  mutate(org_id=if_else(org_id=="180089047", NA_character_, org_id), org="INRIA") %>% 
  separate_rows(org_id, sep=",") %>% 
  unique() %>% 
  group_by(id, stage, participant_order, participates_as) %>% 
  mutate(n=n()) %>% 
  ungroup()


inria <-  read_excel(paste0(source_org, "retour_org/INRIA.xlsx"), col_types = "text") %>% 
  mutate(participant_order=as.numeric(participant_order),
         org_temp=ifelse(str_detect(RNSR, "^\\w+$"), RNSR, NA_character_)) %>% 
  unique()

temp <- inria %>% count(id, participant_order) %>% dplyr::filter(n>1)


inria <- inner_join(inria, base, 
            by = c("stage", "id", "participant_order", "participates_as")) %>% 
  mutate(org_id=ifelse(!is.na(org_temp), org_temp, org_id), org="INRIA",
        org_id=if_else((org_id=="180089047" | org_id == "NC"), NA_character_, org_id)) %>%
  # group_by(id, stage, project_id, participant_order, participates_as) %>% 
  # mutate(org_id=ifelse(!is.na(org_id), str_c(na.omit(unique(org_id)), collapse = ","), NA_character_),
  #        ) %>% 
  # ungroup() %>% 
  select(id, stage, project_id, participant_order, participates_as, org, org_id) %>% 
  unique()
  

# temp <- inria %>% select(SIG_LABO, org_id) %>% unique() %>% 
#   write.csv2(file=paste0(source_org, "inria_liste.csv"), row.names=FALSE)
# 
# temp <- read.csv2(file=paste0(source_org, "inria_liste.csv"))

# inria <- inria %>% select(-org_id) %>% 
#   left_join(temp, by = "SIG_LABO") %>% 
#   mutate(org_id=if_else(is.na(org_id), "180089047", org_id))
# 
# inria <- org_all %>% select(id, stage, org, org_id) %>% filter(org== "INRIA") %>% unique()
# x <- inria %>% count(id, stage)
