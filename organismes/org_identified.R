#########################################################################
org_identified <- bind_rows(inra, cnrs, cea, inria, onera) %>% 
  dplyr::filter(!is.na(org_id))



test <- org_identified %>% 
  group_by(id, stage, project_id, participant_order, participates_as) %>% 
  mutate(n=n()) %>% 
  ungroup()

org_identified <- org_identified %>% 
  group_by(id, stage, project_id, participant_order, participates_as) %>% 
  mutate(org=ifelse(!is.na(org), str_c(na.omit(unique(org)), collapse=",")),
         org_id=ifelse(!is.na(org_id), str_c(na.omit(unique(org_id)), collapse=","))) %>% 
  unique()


save(org_identified, 
     file=paste0(source_org,"org_identified.RData"),
     compress=FALSE)


# non utilisé
x <- org_identified %>% count(id, stage) %>% filter(n>1)
pivot <- inner_join(org_identified, x, by = c("id", "stage")) %>% select(-n)
nom_cols <- c("id", "stage")
v <- c('org','org_id')
temp <- pivot_wider(pivot, id_cols = nom_cols, values_from = v, names_repair = "check_unique", values_fn = toString) %>% 
  dplyr::rename(org_id = org_id_, org=org_) %>% unique()

org_identified <- anti_join(org_identified, x)
org_identified <- bind_rows(org_identified, temp)   
y <- org_identified %>% count(id, stage) %>% filter(n>1)

save(org_identified, file=paste0(chemin, livraison,"/identification_org/org_identified.RData"),
     compress=FALSE)