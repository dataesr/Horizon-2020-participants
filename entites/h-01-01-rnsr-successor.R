filename= 'rnsr_succession_2022-08-03T09-54'
succession <- 
  read.csv2(file = paste0(referentiel, filename,".csv"), 
            colClasses = "character", na.strings = "", encoding = 'UTF-8') %>%
  separate_rows(predecessor_id, succession_type, succession_date, sep=",") %>% 
  mutate(succession_type=str_to_lower(remove_space(replace_special_letters(succession_type)))) %>% 
  unique()


succ_type <- succession %>% select(succession_type) %>% unique() %>% arrange(succession_type)


succession <- succession %>%
  group_by(id, predecessor_id) %>% 
  mutate(
         succession_type=str_c(unique(succession_type), collapse=","),
         succession_date=str_c(unique(succession_date), collapse=",")) %>% 
  ungroup() %>% 
  unique()


# POUR EXTRAIRE LES SUCCESSORS
# ECLATEMENT de predecessor en id
# ECLATEMENT-FUSION de predecessor -> id
# ESSAIMAGE de predecessor -> id
# INTEGRATION de predecessor -> id 
##(ne veut pas dire que la structure n'existe plus, peut vouloir dire participe à 200017663D)
# FUSION de predecessor en id 
## attention infos attribuées aussi aux predecessors ce qui créer une confusion (201220379D)

# succession de predecessor en id

succession <- succession %>% 
  dplyr::filter(!(predecessor_id %in% c("201220379D", '201421281A', '200311864M', '199814085Y')))

# 200311864M se mord la queue
