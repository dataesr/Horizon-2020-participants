filename=c('sirene_extract_2022-08-04T13-10', 'sirene_extract_2022-08-04T15-32','sirene_extract_2022-08-11T09-52')


sirene_add <- data.frame()
for (i in filename){ 
  file_desc=paste0(referentiel, i, ".csv")
  temp <- read.csv2(file=file_desc, encoding = 'UTF-8',  colClasses = "character", na.strings = "")
  sirene_add <- bind_rows(sirene_add, temp) %>% unique()
  }
