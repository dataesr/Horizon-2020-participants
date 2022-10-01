load(file=rdata_last_version(paste0(data_result, "data/"), "part_fr_p01_"))
load(file=paste0(participants, "referentiels/ror.Rdata"))
load(file=rdata_last_version(paste0(data_result, "data/"), "participant_ID_d02_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "ref_part_q01_"))
load(file=paste0(data_result, "data/subvent.Rdata"))
load(file=paste0(data_result, "data/lien_id_proj.Rdata"))
load(file=rdata_last_version(paste0(data_result, "data/"), "prop_proj_"))
etr <- load_one_obj(rdata=paste0(data_result, "data/participants_etrangers.Rdata"), "etr")
load(file=paste0(data_result, "data/perso_erc.RData"))
pays <- load_one_obj(rdata=paste0(chemin, "traitement/exe/nomenclature/pays.rdata"), "pays_fr")
cat_name <- readxl::read_xlsx(paste0(participants, "nomenclatures/_category.xlsx"), 
                sheet="category_name", na="") %>% mutate_if(is.character,str_squish) 


# chargement des bases personnes ERC ecorda
# load(file=paste0(data_result, "data/peronne_contact.RData"))
# colnames(perso_erc) <- str_to_lower(colnames(perso_erc))

temp <- lien_id_proj %>% 
  dplyr::filter(programme_abbr=="ERC") %>% 
  select(project_id, stage, proposal, id, participant_order, participates_as, 
         organizations_id, participant_id, status, status_orig) %>% 
  mutate(participant_id=ifelse(str_detect(organizations_id, pattern_rnsr), organizations_id, participant_id)) %>% 
  # group_by(stage, id, participant_order, participates_as, participant_id, status, status_orig) %>% 
  # ungroup() %>% 
  unique()

# subv <- subvent %>% 
#   select(project_id, id, participant_order, role, participates_as, subv, subv_net, subv_prop) %>% 
#   unique()

proj_fr <- temp %>% 
  dplyr::filter(status %in% "SIGNED") %>% 
  left_join(subvent %>% 
              dplyr::select(project_id, id, participant_order, role, participates_as, subv, subv_net, subv_prop) %>% 
              unique(), 
            by=c("project_id", "id", "participant_order","participates_as"))

main_fr <- temp %>% 
  dplyr::filter(status %in% c("SIGNED", "MAIN")) %>% 
  left_join(subvent %>% 
              dplyr::select(project_id, id, participant_order, role, participates_as, subv, subv_net, subv_prop) %>% 
              unique(), 
            by=c("project_id", "id", "participant_order","participates_as")) %>% 
  mutate(status="MAIN", stage="proposal")

elig_fr <- temp %>%
  left_join(subvent %>% 
              dplyr::select(project_id, id, participant_order, role, participates_as, subv, subv_net, subv_prop) %>% 
              unique(), 
            by=c("project_id", "id", "participant_order","participates_as")) %>% 
  mutate(status="ELIGIBLE", stage="proposal")  

erc_fr <- bind_rows(proj_fr, main_fr, elig_fr) %>% 
  select(-status_orig) %>% 
  mutate(country_code="FR", country_name="France", country_level_1="Pays membres", country_level_2="Pays membres (UE15)")
erc_fr <- ref_info %>% 
  select(participant_id=id_ref, participant_name=name_ref, 
         participant_acronym=acronym_ref, sector, category, cat, category_2, category_5) %>% 
  left_join(cat_name, by="category") %>% 
  dplyr::rename(category_1_name=category_name) %>% 
  left_join(cat_name, by=c("category_2"="category")) %>% 
  dplyr::rename(category_2_name=category_name) %>% 
  left_join(cat_name, by=c("category_5"="category")) %>% 
  dplyr::rename(category_5_name=category_name) %>% 
  right_join(erc_fr, by="participant_id") %>% 
  left_join(ref_info %>% select(organizations_id=id_ref, organizations_name=name_ref),
            by = "organizations_id")

if (nrow(erc_fr %>% dplyr::filter(str_detect(participant_id, "^NonIdent")))>0){
erc_fr <- 
  left_join(erc_fr,
            participant %>% 
            dplyr::select(id, participant_order, participates_as, participant_pic, name_source), 
            by = c("id","participant_order", "participates_as")) %>% 
  mutate(participant_name=ifelse(is.na(participant_name), str_to_sentence(name_source), participant_name),
  ) %>% 
  select(-name_source, -participant_pic) %>% 
  unique()
}

verif <- erc_fr %>% 
  select(status, id, participant_order, participates_as, participant_id) %>%
  dplyr::count(status, id, participant_order, participates_as) %>% 
  dplyr::filter(n>1)


#ETRANGER
temp2 <- etr %>% 
  dplyr::filter(programme_abbr=="ERC") %>% 
  select(id, stage, status, project_id, participant_order, participates_as, role,
         organizations_id, organizations_name=name, subv, subv_net, subv_prop, 
         country_code, country_name, country_level_1, country_level_2, proposal) %>% 
  mutate(participant_id=organizations_id, participant_name=organizations_name)


proj <- temp2 %>% 
  dplyr::filter(stage=="project") %>% 
  mutate(status="SIGNED")
main <- temp2 %>% 
  dplyr::filter(status %in% c("MAIN", "SIGNED")) %>% 
  mutate(status="MAIN", stage="proposal")
elig <- temp2 %>% 
  mutate(status="ELIGIBLE", stage="proposal")

erc_etr <- bind_rows(proj, main, elig)
verif <- erc_etr %>% 
  select(status, id, participant_order, participates_as, organizations_id) %>%
  dplyr::count(status, id, participant_order, participates_as) %>% 
  dplyr::filter(n>1)


# table finale erc fr+etr ; traitement funding/2 pour id_etr CH/UK
erc <- bind_rows(erc_fr, erc_etr) %>% 
  left_join(prop_proj %>%
  select(stage, status_orig, year, call_code, project_id, acronym_projet, starts_with("action"), starts_with("panel")),
  by= c("project_id", "stage")) %>% 
  group_by(status, id, participant_order, participates_as) %>% 
  dplyr::mutate(n_participation=n(), funding=if_else(stage=="project", subv, subv_prop), 
                funding=funding/n_participation) %>% 
  ungroup()

nrow(erc %>% dplyr::filter(country_code=="FR") %>% unique()) # 9 944
erc %>% dplyr::filter(country_code=="FR") %>% 
  group_by(status) %>% 
  dplyr::summarize(sum(funding), na.rm=T)
# ELIGIBLE   11 992 937 568
# MAIN        1 836 955 010
# SIGNED      1 538 215 428
erc %>% group_by(status) %>% dplyr::summarize(sum(funding), na.rm=T)
# ELIGIBLE  109 764 418 832 
# MAIN       14 136 339 015
# SIGNED     13 420 647 254
#########################################################################################################
colnames(erc)
unique(erc$action3_id)==unique(erc$action4_id)

erc <- erc %>%   
  separate(., col = id, into=c("id_p", "applicant_pic", "participant_pic"), sep = "-", remove = F) %>% 
  select(-id_p, -starts_with("action4_")) 

verif_na <- erc[apply(erc, 2, function(x) any(is.na(x)))]

#############################################################
tot <- erc %>% 
  group_by(year, call_code, stage, status,
           action3_id, action3_lib, action2_id, action2_lib, action1_id, action1_lib,
           panels_code_1, panels_name_1, panels_code_2, panels_name_2) %>% 
  dplyr::summarize(n_project=n_distinct(project_id), n_participation=sum(n_participation, na.rm=T), 
                   funding=sum(funding, na.rm=T)) %>% 
  select(year, call_code, stage, starts_with("action"), starts_with("panel"),
         status, n_project, funding, n_participation) %>% 
  ungroup() %>% 
  unique()

tot %>% group_by(status) %>% dplyr::summarize(sum(funding, na.rm=T)) #109 764 418 832
tot %>% group_by(status) %>% summarize(sum(n_participation, na.rm=T))
tot %>% group_by(status) %>% dplyr::mutate(n=sum(n_project)) %>% select(status, n) %>% unique()

p <- 
  erc %>% 
  select(country_code, country_name, country_level_2, country_level_1) %>% 
  unique() %>% 
  merge(tot)

erc <- bind_rows(erc, p)

save(erc, file=paste0(data_result, "data/ERC_z00_", date_sauve, ".RData"))

#############################################################################################################
# GEOLOC f#
#provisoire
pays_rg <- pays %>% select(country_code=CODE_PAYS, country_name=NOM_PAYS) %>% unique()

load(file=rdata_last_version(paste0(data_result, "data/"), "geoloc_key_s01_"))
geoloc_full <-  geoloc_full %>% 
  select(-country_name) %>% 
  mutate(country_loc=ifelse(!is.na(com_code), "FR", country_code),
         country_code="FR",
         project_id=str_split_fixed(id, "-", 3)[,1])
  


###
proj_fr <- proj_fr %>%
  select(id, stage, status, participant_order, participates_as,
         organizations_id, participant_id, subv, subv_net) %>%
  unique() %>%
  left_join(geoloc_full, by=c("id", "stage", "participant_order", "participates_as", "participant_id"="idr")) %>%
  select(project_id, stage, status, id, participant_order, participates_as,
         organizations_id, participant_id, com_code, com_nom,
         uucr_id, uucr_nom, dep_id, dep_nom, dep_num_nom, aca_id, aca_nom, reg_id, reg_nom,
         latitude, longitude, country_loc, country_code, funding=subv, funding_net=subv_net) %>%
  mutate(loc=if_else(!is.na(com_code), com_code, country_loc)) %>%
  unique()

main_fr <- main_fr %>%
  select(id, stage, status, participant_order, participates_as,
         organizations_id, participant_id, subv_prop) %>%
  unique() %>%
  left_join(select(geoloc_full, -stage), 
            by=c("id", "participant_order", "participates_as", "participant_id"="idr")) %>%
  select(project_id, stage, status, id, participant_order, participates_as,
         organizations_id, participant_id, com_code, com_nom,
         uucr_id, uucr_nom, dep_id, dep_nom, dep_num_nom, aca_id, aca_nom, reg_id, reg_nom,
         latitude, longitude, country_loc, country_code, funding=subv_prop) %>%
  mutate(loc=if_else(!is.na(com_code), com_code, country_loc)) %>%
  unique()

elig_fr <- elig_fr %>%
  select(id, stage, status, participant_order, participates_as,
         organizations_id, participant_id, subv_prop) %>%
  unique() %>%
  left_join(select(geoloc_full, -stage), 
            by=c("id", "participant_order", "participates_as", "participant_id"="idr")) %>%
  select(project_id, stage, status, id, participant_order, participates_as,
         organizations_id, participant_id, com_code, com_nom,
         uucr_id, uucr_nom, dep_id, dep_nom, dep_num_nom, aca_id, aca_nom, reg_id, reg_nom,
         latitude, longitude, country_loc, country_code, funding=subv_prop) %>%
  mutate(loc=if_else(!is.na(com_code), com_code, country_loc)) %>%
  unique()

test <- erc_geo %>% dplyr::filter(is.na(loc))
if (nrow(test)>0){
  print("vérifier les participations non localisées dans test")
}else{print('ok')}

test <- erc_geo %>% select(id, participant_order, participates_as) %>% 
  unique() %>% 
  dplyr::count(id, participant_order, participates_as) %>% 
  dplyr::filter(n>1)
if (nrow(test)>0){
  print("vérifier plusieurs localisations pour une participation dans test")
}else{print('ok')}

# proj <- erc_geo %>% 
#   dplyr::filter(country_code=="FR", status=="SIGNED") %>% 
#   mutate(funding=subv_net) %>% 
#   unique()
# 
# main <- erc_geo %>% 
#   dplyr::filter(country_code=="FR", status %in% c("MAIN","SIGNED")) %>% 
#   mutate(status = "MAIN", stage="proposal", funding=subv_prop) %>% 
#   unique()
# 
# elig <- erc_geo %>% 
#   dplyr::filter(country_code=="FR") %>% 
#   mutate(status = "ELIGIBLE", stage="proposal", funding=subv_prop) %>% 
#   unique()

# denombrement des acteurs par participation, et des localisations par part + orga
erc_geo <- bind_rows(proj_fr, main_fr, elig_fr) %>% 
  group_by(status, id, participant_order, participates_as) %>% 
  dplyr::mutate(n_organization=n_distinct(participant_id)) %>%
  group_by(status, id, participant_order, participates_as, organizations_id, participant_id) %>% 
  dplyr::mutate(n_location=n()) %>%
  dplyr::mutate(funding_location=funding/n_organization/n_location, 
                funding_net_location=funding_net/n_organization/n_location) %>%
  ungroup() %>% 
  unique() 

erc_geo %>% group_by(status) %>% 
  dplyr::summarize(sum(funding_location), na.rm=T)

erc_geo <- ref_info %>% 
  select(participant_id=id_ref, participant_name=name_ref, 
         participant_acronym=acronym_ref, sector, category, cat, category_2, category_5) %>% 
  left_join(cat_name, by="category") %>% 
  dplyr::rename(category_1_name=category_name) %>% 
  left_join(cat_name, by=c("category_2"="category")) %>% 
  dplyr::rename(category_2_name=category_name) %>% 
  left_join(cat_name, by=c("category_5"="category")) %>% 
  dplyr::rename(category_5_name=category_name) %>% 
  right_join(erc_geo, by="participant_id") %>% 
  left_join(ref_info %>% select(organizations_id=id_ref, organizations_name=name_ref),
            by = "organizations_id")

if (nrow(erc_geo %>% dplyr::filter(is.na(participant_name)))>0){
  print("reprendre code + haut pour aller chercher les noms dans la table participant")
}



# geo etranger
temp2 <- etr %>% 
  dplyr::filter(programme_abbr=="ERC") %>% 
  # mutate(latitude = str_extract(gps, "^.+\\.\\d{1,5}(?=\\d+,{1})"),
  #        longitude = str_extract(gps, "(?<=,).+\\.\\d{1,5}")) %>% 
  select(id, stage, status, project_id, participant_order, participates_as,
         organizations_id, organizations_name=name, subv_net, subv_prop, 
         country_code, country_name, geo_admin1_code, geo_admin1_name, geo_admin1_name_ascii,
         geo_nuts1_code, geo_nuts1_name, latitude, longitude)


proj <- temp2 %>% 
  dplyr::filter(stage=="project") %>% 
  mutate(status="SIGNED", funding=subv_net)
main <- temp2 %>% 
  dplyr::filter(status %in% c("MAIN", "SIGNED")) %>% 
  mutate(status="MAIN", stage="proposal", funding=subv_prop)
elig <- temp2 %>% 
  mutate(status="ELIGIBLE", stage="proposal", funding=subv_prop)

etr_geo <- bind_rows(proj, main, elig) %>% 
  select(-starts_with("subv")) %>% 
  group_by(status, id, participant_order, participates_as) %>% 
  dplyr::mutate(n_organization=n_distinct(organizations_id)) %>%
  group_by(status, id, participant_order, participates_as, organizations_id) %>% 
  dplyr::mutate(n_location=n()) %>%
  dplyr::mutate(funding_location=funding/n_organization/n_location) %>%
  ungroup() %>% 
  unique() %>% 
  mutate(participant_id=organizations_id, participant_name=organizations_name)

erc_geo <- bind_rows(erc_geo, etr_geo)

erc_geo <- prop_proj %>% 
  select(stage, project_id, call_code, year, starts_with("action"), starts_with("panel")) %>% 
  right_join(erc_geo, prop_proj, by= c("project_id", "stage")) %>% 
  select(-starts_with("action4_")) 


save(erc_geo, file=paste0(data_result, "data/ERC_geo_z00_", date_sauve, ".RData"))
#################################################################################






# proj <- proj %>% 
#   # charger_rdata(paste0(data_source,"datas.RData"), "project") %>% 
#   # filter(area_abbr=="ERC") %>%
#   select(stage, project_id=NUM_PROJET, year=ANNEE, panel1_id, panel1_lib, panel2_id, panel2_lib, action2_id, action2_lib) %>% 
#   mutate(project_id=as.character(project_id), prog="H2020")
# colnames(proj) <- stringr::str_to_lower(colnames(proj))

#######
erc_h <- inner_join(erc, proj, by = c('project_id','stage')) %>% select(-pic_id2) %>% unique()

s1 <- erc_h %>% select(project_id, pic_id, id=organizations_id) %>% unique()
s2 <- erc_h %>% filter(!is.na(participant_id)) %>% select(project_id, pic_id, id=participant_id) %>% unique()
struct <- bind_rows(s1, s2) %>% unique() %>% separate_rows(id, sep=",")

struct <- left_join(struct, etab[c("paysage", "nom_uai")], by = c("id"="paysage")) %>% 
  dplyr::rename(nom=nom_uai)
struct <- left_join(struct, siren_ul, by = c("id"="siren")) %>% 
  mutate(nom=if_else(!is.na(nom_ul), nom_ul, nom)) %>% select(-cj, -nom_ul)                                                          
struct <- left_join(struct, rnsr, by = "id") %>% 
  mutate(nom=if_else(!is.na(libelle), libelle, nom)) %>% 
  select(id, nom) %>% unique()


erc_h <- erc_h %>% left_join(struct, by = c("organizations_id" = "id")) %>% unique() %>% 
  dplyr::rename(organizations_name=nom) 
erc_h<- erc_h %>% 
  left_join(struct, by = c("participant_id" = "id")) %>% unique() %>% 
  dplyr::rename(participant_name=nom)

write.csv2(erc_h, file=paste0(data_result, "erc_", date_sauve, ".csv"), na="", row.names = FALSE)

# concat ERC H2020 + FP7 pour Emmanuel
FP7_erc <- read.csv2(file="C:/Users/zfriant/OneDrive/PCRI/FP7/2019/fp7_erc_2020_12_06.csv", na="", colClasses = "character")
panel <- erc_h %>% select(panel1_id,panel1_lib, panel2_id) %>% unique() 
FP7_erc <- FP7_erc %>% mutate(erc_code=if_else(erc_code=="PoC", "PC", erc_code)) %>% 
  left_join(panel, by = c("erc_code"="panel2_id"))
FP7_erc <- FP7_erc %>% mutate(last_name = tolower_noblank(last_name), first_name = tolower_noblank(first_name),
                              title_name = tolower_noblank(title_name), last_name = replace_special_letters(last_name), 
                              first_name = replace_special_letters(first_name), title_name = replace_special_letters(title_name),
                              last_name = clean_exo_char(last_name), first_name = clean_exo_char(first_name), 
                              title_name = clean_exo_char(title_name),
                              contact = paste(last_name,first_name, sep = " "),
                              role = tolower_noblank(role), role = if_else(role=="participant", "partner", role),
                              prog="FP7", stage="project")


FP7_erc <- FP7_erc %>% dplyr::rename(action2_id=area_abbr, action2_lib=area_lib, panel2_id=erc_code, panel2_lib=erc_panel)

erc_full <- bind_rows(erc_h, FP7_erc) %>% mutate(contact=paste(last_name, first_name, sep=" ")) %>% 
  relocate(prog, stage, year, contact, title_name, last_name, first_name, idref, participant_id, participant_name, organizations_id, organizations_name,
           nationality, country_residence, action2_id, action2_lib, panel1_id, panel1_lib, panel2_id, panel2_lib) %>% 
  mutate_all(~str_squish(.))

write.csv2(erc_full, file=paste0(data_result, "erc_full_", date_sauve, ".csv"), na="", row.names = FALSE)

##############################################################################################
#retour traitement erc
erc_cor <- readxl::read_xlsx(path=paste0(identification, "corrected/erc_full_2020_12_07.xlsx")) %>% 
  mutate(temp=paste0("idref",idref))
# erc_cor <- left_join(erc_cor, erc_affi, by = c("temp"="idref")) %>% 
#   mutate(participant_id = if_else(is.na(participant_id), struct, participant_id)) %>% 
#   select(-end, -struct)
# 
# erc2 <- erc_cor %>% select(project_id, pic_id, organizations_id, part=participant_id, role) %>% unique()
# 
# temp <- part_fr %>% left_join(erc2)
# 
# x <- temp %>% count(lid) %>% filter(n>1)
#######################################################################################################
#fichier pour paysage ERC

temp <- erc_cor %>% 
  select(project_id, pic_id, organizations_id, participant_id, last_name, newid=idref, newlien=lien_idref, notes_Emmanuel) %>% 
  mutate(last_name = tolower_noblank(last_name),last_name = replace_special_letters(last_name), 
  last_name = clean_exo_char(last_name), retour="1", ) %>% unique() %>% 
  mutate_all(~str_squish(.))
x <- temp %>% count(project_id, pic_id, organizations_id, participant_id, last_name) %>% filter(n>1)

test <- left_join(erc_full, temp, by = c("project_id", "pic_id", "organizations_id", "participant_id", "last_name"))

erc <- test %>% 
  mutate(idref=if_else(!is.na(newid), str_extract(newid, "(?<=idref)\\w+"), idref),
         lien_idref=if_else((idref!=""|!is.na(idref)), paste0("http://www.idref.fr/", str_squish(idref)), NA_character_),
         idref=if_else((idref==""|is.na(idref)) & retour=="1", "x", idref)) %>% 
  select(-newid, -newlien)

x <- erc %>% count(contact) %>% filter(n>1)
test <- left_join(x, erc, by = "contact") %>% arrange(contact, idref)
write.csv2(erc, file=paste0(data_result, "erc_full_", date_sauve, ".csv"), na="", row.names = FALSE)

# ajout des erc après 2018 pour paysage, concatener avec FP7 pour un fichier complet
# proj <- project %>% filter(programme_abbr=="ERC") %>% 
#   select(NUM_PROJET, ANNEE, panel1_id, panel1_lib, panel2_id, panel2_lib, action2_id, action2_lib) %>% 
#   left_join(participant, by = "NUM_PROJET") %>% filter(CODE_PAYS=="FR") %>% 
#   mutate(prog="H2020",stage="project", NUM_PROJET=as.character(NUM_PROJET), 
#          CODE_ORG=as.character(CODE_ORG),
#          PIC_ID2=as.character(PIC_ID2)) %>% 
#   dplyr::rename(project_id=NUM_PROJET,country_code=CODE_PAYS, country=pays_lib, year=ANNEE, 
#                 role=ROLE_PART, pic_id=CODE_ORG, pic_id2=PIC_ID2) %>% 
#   unique() %>% 
#   inner_join(perso_erc) %>% 
#   mutate(last_name = tolower_noblank(last_name),last_name = replace_special_letters(last_name), 
#          last_name = clean_exo_char(last_name), retour="1", ) %>% unique() %>% 
#   mutate_all(~str_squish(.)) %>% 
#   select(prog, stage, year, project_id, pic_id, pic_id2, panel1_id, panel1_lib, panel2_id, panel2_lib, 
#          action2_id, action2_lib, organizations_name=NOM_LONG, role,nationality=lb_main_nationality,
#          country_residence=lb_residence_country, country_origin=lb_origin_country, 
#          contact_type, contact,	title_name,	last_name,	first_name) %>% 
#   left_join(pic_id_fr) %>% 
#   dplyr::rename(organizations_id=source_id) %>% 
#   left_join(erc_cor[c('stage','project_id', 'pic_id', 'participant_id', "participant_name", 'role', 'last_name', "idref", 'notes_Emmanuel')], 
#             by = c('stage','project_id', 'pic_id', 'role', 'last_name'))
# 
# temp <- proj %>% filter(is.na(idref)) %>% select('project_id', 'pic_id', "pic_id2", 'role', 'last_name', 'first_name') %>% 
# write.csv2(file = paste0(identification, "erc_idref_", date_sauve, ".csv"), na="", row.names = FALSE)
# 
# x <- erc_id %>% count(project_id, pic_id, pic_id2, role, last_name)
# 
# proj <- proj %>% left_join(erc_id) %>% 
#   mutate(idref=if_else(is.na(idref) & !is.na(idtemp), idtemp, idref),
#          lien_idref=if_else(!is.na(idref) | idref != "x", paste0("http://www.idref.fr/", str_squish(idref)), NA_character_)) %>% 
#   relocate(prog, stage, year, contact, title_name, last_name, first_name, idref, participant_id, participant_name, organizations_id, organizations_name,
#            nationality, country_residence, action2_id, action2_lib, panel1_id, panel1_lib, panel2_id, panel2_lib) %>% 
#   mutate_all(~str_squish(.))
# 
# 
# write.csv2(proj, file=paste0(data_result, "erc_full_", date_sauve, ".csv"), na="", row.names = FALSE)
