load(file = rdata_last_version(paste0(data_result, "data/"),"key_part_h01_"))
load(file = rdata_last_version(paste0(data_result, "data/"),"part_fr_g01_"))


key_part <- key_part %>% mutate_if(is.character, str_squish)

#verification de la présence de certains id
if (any(sapply(key_part, function(x) c("0133794D","130002793", "687994274") %in% x))){
  print("maj key_part")
}else{print("ok")}
#provisoire à vérifier à chaque fois
key_part <- key_part %>% mutate(across(c(s,p,id_temp,t), .fns=~str_replace(., "130002793", "130026222")))
part_fr <- part_fr %>% mutate(across(c(organizations_id), .fns=~str_replace(., "130002793", "130026222")))
key_part <- key_part %>% mutate(across(c(s,p,id_temp,t), .fns=~str_replace(., "687994274", "498448596")))
part_fr <- part_fr %>% mutate(across(c(organizations_id), .fns=~str_replace(., "687994274", "498448596")))
key_part <- key_part %>% mutate(across(c(s,p,id_temp,t), .fns=~str_replace(., "784543480", "775682990")))
part_fr <- part_fr %>% mutate(across(c(organizations_id), .fns=~str_replace(., "784543480", "775682990")))

# liste tous les id pour recuperation info dans referentiels 
t <- key_part %>% select(id=s) %>% dplyr::filter(str_detect(id, pattern_rnsr, negate=TRUE)) %>% unique()
t2 <- key_part %>% select(id=p) %>% dplyr::filter(str_detect(id, pattern_rnsr, negate=TRUE)) %>% unique()
t3 <- key_part %>% select(id=t) %>% dplyr::filter(str_detect(id, pattern_rnsr, negate=TRUE)) %>% unique()
etab_extract <- bind_rows(t, t2, t3) %>% unique() %>% dplyr::filter(!is.na(id))


#recuperation des ror etranger
load(file = rdata_last_version(paste0(data_result, "data/"), "participant_ID_d02_"))
participant <- participant %>% 
  dplyr::filter(country_code != "FR", !is.na(organizations_id)) %>% 
  select(id=organizations_id, country_code) %>% 
  unique()

etab_extract <- etab_extract %>% bind_rows(participant) %>% 
  mutate(country_code=ifelse(is.na(country_code), "FR", country_code))

write.csv2(etab_extract, file=paste0(identification, "etab_api_", date_sauve, ".csv"), row.names = FALSE)


save(key_part, file=paste0(data_result, "data/key_part_k01_", date_sauve,".Rdata"))
save(part_fr, file=paste0(data_result, "data/part_fr_k01_", date_sauve,".Rdata"))

rm(list=c("t", "t2", "t3","etab_extract", "participant"))
gc()
