# RETOUR CNRS

# load(paste0(chemin_bd, "participant_ID_d02_2022-01-24-T15-33.Rdata"))

# 647315-999997930-999997930

id_stage <- base %>% select(project_id, stage) %>% unique()


cnrs_r <- readxl::read_xlsx(paste0(source_org, "retour_org/CNRS_2021_11.xlsx"), sheet="CNRS", col_types = "text")
names(cnrs_r) <- tolower(names(cnrs_r)) 
cnrs <- cnrs_r %>%
  select(id, stage, project_id=projet_id, participant_order,  
         cnrs_participates=participates_as, labo, nom_labo, sig_labo, ville_labo) %>%
  mutate(across(.cols=everything(), .fns=str_to_lower),
         participant_order=as.numeric(participant_order)) %>% 
  dplyr::filter(str_detect(stage, "_fpa", negate=T)) %>%
  unique() # 27 646


cnrs <- cnrs %>% inner_join(id_stage) %>% 
  group_by(id, participant_order) %>% 
  mutate(n_stage=n_distinct(stage)) %>% 
  ungroup() %>% 
  group_by(stage, id, participant_order) %>% 
  mutate(n_type=n_distinct(cnrs_participates), nb=n()) %>% 
  ungroup() # 24 571

#suppression des lignes proposal si la ligne project existe 
test <- cnrs %>% dplyr::filter(n_stage>1)
vec <- unique(test$cnrs_participates)
for (i in vec){
  test1 <- test %>% dplyr::filter(cnrs_participates==i) %>% 
    select(id, participant_order) %>% unique()
  test2 <- left_join(test1, test, by = c("id", "participant_order"))
  if (nrow(test2 %>% 
           dplyr::filter(!(is.na(cnrs_participates)|cnrs_participates==i)))>0){
    print("a verifier")
  }else{
    print("ok")
    test1 <- test1 %>% mutate(stage="proposal")
    cnrs <- anti_join(cnrs, test1, by = c("id", "stage", "participant_order")) %>% 
      group_by(id, participant_order) %>% 
      mutate(n_stage=n_distinct(stage)) %>% 
      ungroup()# 24 059
    }
}


temp <- cnrs %>% #dplyr::filter(is.na(cnrs_participates)) %>% # 19 217
  inner_join(select(base, c(stage, id, participant_order, participates_as)), 
            by=c("stage", "id", "participant_order")) %>% 
  mutate(cnrs_participates=participates_as) %>% 
  select(-participates_as)

test <- temp %>% dplyr::filter(is.na(cnrs_participates))

cnrs <- anti_join(cnrs, temp, by=c("stage", "id", "participant_order"))
cnrs <- bind_rows(cnrs, temp) 

# ajout des anciens org_id
temp <- cnrs %>% 
  inner_join(base, by=c("stage", "project_id", "id", "participant_order", "cnrs_participates"="participates_as")) %>% 
  separate_rows(org_id, sep=",") %>% 
  mutate(org_id=ifelse(org_id=="180089013", NA_character_, org_id), org="CNRS")
temp <- data.frame(lapply(temp, function(x) {gsub("\\+|/", "#", x)})) %>% 
  separate_rows(labo, sig_labo, nom_labo, sep="#")

nrow(temp %>% select(labo) %>% unique())

id_cnrs <- temp %>% 
  select(year, labo, sig_labo, nom_labo, ville_labo) %>% 
  mutate(supervisor="cnrs") %>% 
  unique() %>% 
  write.csv2(file=paste0("C:/Users/zfriant/Documents/OneDrive/PCRI/participants/identification/cnrs_liste.csv"), row.names = F, fileEncoding = "UTF-8") 


cnrs_retour <- read.csv("C:/Users/zfriant/Documents/OneDrive/PCRI/participants/identification/corrected/id_org_2022-06-16.csv", 
                         na.strings = "", sep=";")

cnrs_retour <- cnrs_retour %>% 
  mutate(match=str_squish(match),
    match2= gsub("\\['|'|'\\]", "", match)) %>% 
  select(year, labo, sig_labo, nom_labo, ville_labo, match2) %>% 
  separate_rows(match2, sep=',') %>% 
  unique() %>% 
  dplyr::filter(!is.na(match2)) %>% 
  # group_by(labo) %>% 
  # mutate(org_new=str_c(na.omit(unique(match2)), collapse = ",")) %>% 
  # ungroup() %>% 
  select(labo, match2) %>% 
  unique()

test <- anti_join(temp, cnrs_retour, by="labo") %>% 
  select(labo) %>% unique()

nrow(cnrs_retour %>% select(labo) %>% unique())
cnrs <- left_join(temp, cnrs_retour, by = "labo")

cnrs <- cnrs %>% 
  select(id, stage, project_id, participant_order, participates_as=cnrs_participates, org_id, match2) %>% 
  unique() %>% 
  mutate(org_id=ifelse(!is.na(match2), match2, org_id),
         participant_order=as.numeric(participant_order),
         org="CNRS") %>%
  select(-match2) %>% 
  unique()


# cnrs <- cnrs %>% 
#   select(id, stage, project_id, participant_order, participates_as=cnrs_participates, org_id) %>% 
#   unique()
  # group_by(id, stage, project_id, participant_order, participates_as) %>% 
  # mutate(org_id=ifelse(!is.na(org_id), str_c(na.omit(unique(org_id)), collapse = ","), NA_character_),
  #        org="CNRS") %>% 
  # ungroup() %>% 
  # unique()

# cnrs1 <- read_excel(paste0(source_org, "CNRS.xlsx"), 
#                     sheet = "CNRS_ProposalsH2020_Oct2019", col_types = "text") %>% 
#   dplyr::rename(id=NUM_PROJET_part_id_CD_APPL_PIC, org=organismes) %>% 
#   mutate(stage="proposal") %>% 
#   select(id, NUM_PROJET, stage, org, LABO, NOM_LABO, SIG_LABO, VILLE_LABO)
# 
# cnrs2 <- read_excel(paste0(source_org, "CNRS.xlsx"),
#                     sheet = "CNRS_GrantsH2020_Oct2019", col_types = "text") %>% 
#   dplyr::rename(id=NUM_PROJET_part_id_CD_APPL_PIC, org=organismes) %>% 
#   mutate(stage="project") %>% 
#   select(id, NUM_PROJET, stage, org, LABO, NOM_LABO, SIG_LABO, VILLE_LABO)

# doublons <- cnrs1 %>% count(id, stage) %>%  filter(n>1)


# temp <- anti_join(cnrs1, cnrs2, by = c("id", "LABO"))
# cnrs <- bind_rows(cnrs2, temp)


# 
# id_cnrs <- read.csv2(file = paste0(chemin, "2019_11/identification_org/verif_org_ident.csv"), encoding = "UTF-8") %>%
#   select(LABO, match_rnsr) %>%  unique() %>%
#   inner_join(cnrs, by = "LABO") %>%
#   select(LABO, match_rnsr) %>% unique() %>%
#   group_by(LABO) %>%
#   mutate(nb=n()) %>%
#   write.csv2(file=paste0(source_org, "cnrs_liste.csv"), row.names = FALSE)
# 
# 
# temp <- read.csv2(file=paste0(source_org, "cnrs_liste.csv")) %>% 
#   select(LABO, match_rnsr) %>% unique()
# cnrs <- left_join(cnrs, temp, by = "LABO") %>% 
#   dplyr::rename(org_id=match_rnsr)
# 
# pivot <- cnrs %>% select(id, stage, org, org_id) %>% unique()
# temp <- pivot_wider(pivot, names_from = "org", values_from = 'org_id', names_repair = "check_unique", values_fn = toString)
# cnrs <- temp %>% dplyr::rename(org_id=CNRS) %>% mutate(org="CNRS")
# 
# x <- cnrs %>% count(id, stage)
