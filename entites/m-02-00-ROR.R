filename = "ROR_2022-08-04T09-51"
ror <- read.csv2(file=paste0(referentiel, filename, ".csv"), encoding = 'UTF-8',na = c("", 'None')) %>% 
  mutate(country_code = ifelse(country_code=='GB', 'UK', 
                               ifelse(country_code=='GR', 'EL', country_code)))


save(ror, file=paste0(referentiel, 'ror.RData'))
