filename = "rnsr_2022-01-25T08-59"
rnsr_add <- 
  read.csv2(file = paste0(referentiel, filename,".csv"), 
            colClasses = "character", na.strings = "", encoding = 'UTF-8') %>% 
  unique() %>% 
  dplyr::rename(id_scanr=id, id=rnsr) %>% 
  relocate(id, name, predecessor)
