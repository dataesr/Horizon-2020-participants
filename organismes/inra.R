# RETOUR INRA


inra <- base %>% 
  dplyr::filter(str_detect(org, "INRAE") | organizations_id=="180070039") %>% 
  select(-role, -organizations_id, -year) %>% 
  separate_rows(org_id, sep=",") %>% 
  mutate(org_id=if_else(org_id=="180070039", NA_character_, org_id), org="INRAE") %>%
  unique()




#non utilisé pas de retour inrae
inra <-  read_excel(paste0(source_org, "INRA.xlsx"), col_types = "text") %>% 
  dplyr::rename(NUM_PROJET=CD_PROJ_ID) %>% 
  select(id, stage, NUM_PROJET, Unite, Centre)


inra <- separate(inra, Unite, into = paste0("col_", 1:20), sep = " +")%>% 
  dplyr::rename(SIG_LABO = col_4, LABO = col_3) %>% 
  unite(NOM_LABO, col_5:col_20, sep = " ", na.rm = TRUE) %>% 
  select(-col_1, -col_2) %>% 
  mutate(org="INRA")

inra <- inra %>% mutate(NOM_LABO=if_else(NOM_LABO == "" & is.na(LABO), paste(org,Centre,sep=" "), NOM_LABO),
                        NOM_LABO = str_replace_all(NOM_LABO, "\\?", ""),
                        NOM_LABO = str_squish(NOM_LABO))


temp1 <- inra %>%  select(id, LABO, SIG_LABO, NOM_LABO) %>% unique()
  
temp <- ident %>% select(LABO, match_rnsr) %>% filter(LABO!="") %>% unique

inra_liste <- left_join(temp1, temp, by = "LABO") %>% 
  group_by(id) %>% mutate(nb=n()) %>% ungroup() %>% unique() %>% 
write.csv2(file=paste0(source_org, "inra_liste.csv"), row.names=FALSE)

temp <- read.csv2(file=paste0(source_org, "inra_liste.csv")) %>% 
  select(id, match_rnsr)

inra <- left_join(inra, temp, by ="id") %>% dplyr::rename(org_id=match_rnsr)

# inra <- org_all %>% select(id, stage, org, org_id) %>% filter(org== "INRA")
# x <- inra %>% count(id, stage)
