alloc <- read.csv2(file="C:/Users/zfriant/OneDrive/PCRI/participants/referentiels/alloc_modele.csv",
                   encoding = 'UTF-8')

etab <- read.csv2(file="C:/Users/zfriant/OneDrive/PCRI/participants/identification/corrected/etab_2021-05-29T15-13.csv",
                  encoding = 'UTF-8')

alloc <- left_join(alloc, etab, by= 'uai') %>% 
  filter(actif.inactif == "A") %>% 
  select(id, uai, nom) %>%
  unique()


load(file= "C:/Users/zfriant/OneDrive/PCRI/participants/2021_04/structure_tut.Rdata")

x <- tut %>% select(tutelle_id, funding_tut_net, status, project_id) %>% 
  inner_join(alloc, by = c("tutelle_id" = "id")) %>% 
  group_by(tutelle_id, status, uai, nom) %>% 
  dplyr::mutate(nb_proj=n_distinct(project_id), subv=sum(funding_tut_net)) %>% 
  select(-project_id, -funding_tut_net) %>% 
  ungroup() %>% 
  unique()

x <- pivot_wider(x, names_from=status, values_from = c(nb_proj, subv), values_fn = sum)
write_excel_csv2(x, path="C:/Users/zfriant/OneDrive/PCRI/participants/2021_04/tableau/alloc_modele_2021_04.csv", delim=';')
