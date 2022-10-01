
filename ="etab_2022-01-23T17-35"

etab_add <-
  read.csv2(file = paste0(referentiel, filename, ".csv"),
            colClasses = "character", encoding = 'UTF-8', na.strings = "") 


# si les etab add sont des structures de recherche extraite spour la geoloc
etab_add <- etab_add %>% 
  separate_rows(id_autres) %>% 
  dplyr::filter(str_detect(id_autres, pattern_rnsr)) %>% 
  select(idr=id_autres, latitude, longitude, adresse, city, com_code, country_name=country, country_code) %>% 
  unique()


test <- etab_add %>% 
  group_by(idr) %>% 
  mutate(n=n()) %>% 
  dplyr::filter(n > 1)
  

etab_add <- etab_add %>% 
  dplyr::filter(!(idr %in% test$idr & is.na(latitude))) %>% 
  mutate(com_code=ifelse(!is.na(country_code), NA_character_, com_code),
         country_code = str_sub(country_code, 1,2))
