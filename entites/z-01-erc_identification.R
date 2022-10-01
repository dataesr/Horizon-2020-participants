# load(file=paste0(data_result, "data/part_fr_labo7_2020-12-06-T17-13.Rdata"))
load(file=paste0(data_result, "data/lien_id_proj.Rdata"))
load(file=paste0(data_result, "data/perso_erc.RData"))
load(file=rdata_last_version(paste0(data_result, "data/"), "ref_part_q01_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "prop_proj_"))
pi <- read.csv(paste0(data_result, "perso/project_erc_pi.csv"))
load(file=rdata_last_version(paste0(data_result, "data/"), "ERC_z00_"))

erc <- erc %>% 
  dplyr::filter(!is.na(id)) %>% 
  unique()

# erc individuel avec orcid
erc_pi <- erc %>% 
  dplyr::filter(role=="coordinator") %>% 
  select(project_id, stage, country_code, panels_name_1, panels_name_2) %>% 
  unique() %>% 
  inner_join(
    perso_erc %>% 
      dplyr::filter(action2_id!="ERC-SyG"), 
    by=c("project_id", "stage")) %>% 
  group_by(contact) %>% 
  fill(orcid_id, .direction = "updown") %>% 
  ungroup() %>% 
  unique()

# write.csv2(temp, file=paste0(identification, "erc_", date_sauve, ".csv"), na="",
#            fileEncoding = "UTF-8", row.names = F)

test <- inner_join(perso_erc %>% dplyr::filter(action2_id!="ERC-SyG"), erc_pi, 
                  by = c("stage", "project_id", "participant_pic", "applicant_pic", "contact")) %>% 
  unique()

#ERC idéréfisé -> verification du dernier fichier iderefisé
# filename="erc_identifie_2021-11-19-T16-22.csv"
filename="erc_2022_2-03.csv"
erc_id <-  read.csv2(file=paste0(identification, "corrected/", filename), na.strings = "" ,colClasses = "character") %>% 
  select(action3_id, project_id, stage, status, participant_pic, applicant_pic, contact, id_perso) %>% 
  dplyr::filter(action3_id!="ERC-SyG") %>% 
  unique() %>% 
  mutate(uncertain=ifelse(str_detect(id_perso, '^\\?'), "?", NA_character_),
    id_perso=ifelse(str_detect(id_perso, '^\\?'), str_replace(id_perso, "^\\?(?=.+)", ""), id_perso)) %>% 
  group_by(project_id) %>% 
  mutate(n=n_distinct(id_perso)) %>% 
  ungroup()


# extraction des erc FR ou chercheurs FR
temp <- erc_pi %>% 
  dplyr::filter(country_code=="FR" | nationality=="FR")


# lien erc FR avec erc identifié par idref et à la main
temp <- temp %>% 
  left_join(erc_id %>% select(project_id, contact, id_perso) %>% unique(),
  by = c("project_id", 'contact')) %>% 
  mutate(id_perso=ifelse(orcid_id==id_perso, NA_character_, id_perso)) %>% 
  left_join(erc_id %>% select(project_id, contact, investigator_id=id_perso) %>% unique(),
            by = c("project_id", 'contact_signe'='contact')) %>%
  mutate(investigator_id=ifelse(orcid_id==investigator_id, NA_character_, investigator_id),
         investigator_id=ifelse(is.na(investigator_id), id_perso, investigator_id),
         investigator_id=ifelse(is.na(investigator_id), orcid_id, investigator_id),
         investigator_id=str_squish(investigator_id)) %>% 
  select(-id_perso) %>% 
  group_by(contact) %>% 
  fill(investigator_id, .direction = "downup") %>% 
  ungroup() %>% 
  group_by(stage, project_id, contact) %>% 
  mutate(long=ifelse((nchar(investigator_id)==9|nchar(investigator_id)==19), 0, 1),
    investigator_id=ifelse(!is.na(investigator_id), str_c(na.omit(unique(investigator_id)), collapse = ","), NA_character_)
         ) %>% 
  ungroup() %>% 
  unique()
  
# liste erc non identifiés pour export 
no_id <- temp %>% dplyr::filter(is.na(investigator_id)| long==1) %>% 
  left_join(select(erc, c(project_id, participant_id, organizations_name, participant_name)), 
            by="project_id") %>% 
  select(last_name, first_name, contact, contact_signe, old.idref=investigator_id, organizations_name, participant_name, participant_id, panels_name_1, panels_name_2) %>% 
  unique()
write.csv2(no_id, file=paste0(identification, "erc_fr_nofr_", date_sauve, ".csv"), na="",
             fileEncoding = "UTF-8", row.names = F)

##########
# retour identification erc
retour_no_id <- readxl::read_xlsx(path = paste0(identification, "corrected/_contact_idref.xlsx"), sheet = "h2020",
                                    col_types = "text", na=c("", "#N/A", "0")) %>% 
  select(contact, panels_name_1, idref, new.gender=gender) %>% 
  unique() %>% 
  group_by(panels_name_1, contact) %>% 
  mutate(n=n(), 
         idref=str_replace(idref, "idref", ""),
         long=ifelse((nchar(idref)==9|nchar(idref)==19), 0, 1)) %>% 
  ungroup()

if ((nrow(retour_no_id %>% dplyr::filter(n>1))>0)|(nrow(retour_no_id %>% dplyr::filter(long==1))>0)){
  print("vérifier les idref ; soit plusieurs idref pour un même nom (n>1) ou idref erronés (long=1)")
}else{print("ok")}


# ajout des nouveaux identifiants à TEMP
temp <- temp %>% select(-long) %>% 
  left_join(retour_no_id, by = c("panels_name_1", "contact")) %>% 
  mutate(investigator_id=ifelse(!is.na(idref), idref, investigator_id),
         new.gender=ifelse(is.na(new.gender), gender, new.gender)) %>% 
  select(-n, -long, -gender, -idref) %>% 
  group_by(stage, project_id, contact) %>% 
  mutate(investigator_id=ifelse(!is.na(investigator_id), str_c(na.omit(unique(investigator_id)), collapse = ","), NA_character_)) %>% 
  select(project_id, stage, contact, investigator_id, new.gender) %>% 
  unique()
 

erc_pi <- left_join(erc_pi, temp, by = c("project_id", "stage", "contact")) %>% 
  mutate(gender=ifelse(!is.na(new.gender), new.gender, gender),
         gender=ifelse((gender=="F"|gender=="Female"), "F", "M" )) %>% 
  select(project_id, stage, country_code, contact, last_name, first_name, nationality, gender, investigator_id, orcid_id) %>% 
  unique()

# verification du gender -> "F" et "M"
if (length(unique(erc_pi$gender))==2){
  print("ok")
}else{message("corriger gender : ", unique(erc_pi$gender))}

if (length(is.na(erc_pi$nationality))>0){
  print("ok")
}else{message("corriger nationality : info manquante")}


erc_tot <- erc %>% 
  left_join(erc_pi,
             by = c("project_id", "stage", "country_code")) %>% 
  group_by(project_id, contact) %>% 
  fill(c(last_name, first_name, contact, investigator_id, orcid_id, gender, nationality), .direction = "updown") %>% 
  ungroup() %>% 
  select(organizations_id, organizations_name, participant_id, participant_name, participant_acronym,
         country_code, country_name, participates_as, role, 
         project_id, stage, status, year, call_code, acronym_projet, action2_id, action2_lib, 
         panels_code_1, panels_name_1, panels_code_2, panels_name_2, contact,
         last_name, first_name, nationality, gender, investigator_id, orcid_id, funding) %>%
  group_by(organizations_id, organizations_name, participant_id, participant_name, participant_acronym,
           country_code, country_name, participates_as, role,
           project_id, stage, status, year, call_code, acronym_projet, action2_id, action2_lib, 
           panels_code_1, panels_name_1, panels_code_2, panels_name_2,  
           last_name, first_name, contact, nationality, gender, investigator_id, orcid_id) %>% 
  dplyr::mutate(funding=sum(funding, na.rm=T)) %>% 
  ungroup() %>% 
  unique()



save(erc_tot, file=paste0(data_result, "data/ERC_id_genre_nat_z01_", date_sauve, ".RData"))


#fichier pour eric et emmanuel
erc_signe <- erc_tot %>% 
  dplyr::filter(status=="SIGNED")


write.csv2(erc_signe, file=paste0(chemin, "ERC/ERC_H2020_", date_sauve, ".csv"), na="",
           fileEncoding = "UTF-8", row.names = F)

load(file=rdata_last_version(paste0(data_result, "data/"), "geoloc_key_s01_"))
erc_signe_geo <- erc_geo %>% 
  dplyr::filter(status=="SIGNED") %>% 
  select(status, project_id, participates_as, country_code, country_name,
         organizations_id, organizations_name, participant_id, participant_name, participant_acronym,
         com_code, com_nom, uucr_id, uucr_nom, dep_id, dep_nom, dep_num_nom, reg_id, reg_nom,
         latitude, longitude, geo_admin1_code, geo_admin1_name, geo_admin1_name_ascii,
         geo_nuts1_code, geo_nuts1_name) %>% 
  unique()

write.csv2(erc_signe_geo, file=paste0(chemin, "ERC/ERC_geo_H2020_", date_sauve, ".csv"), na="",
           fileEncoding = "UTF-8", row.names = F)
###############################################"
#nouveau erc à iderefiser

erc_proj <- erc %>% dplyr::filter(!is.na(id)) %>% 
  select(project_id, acronym_projet, status, stage, id, participant_order, 
         participates_as, organizations_id, organizations_name, participant_id, 
         participant_name, participant_acronym, action3_id) %>% 
  unique() %>% 
  inner_join(perso_erc, by = c("project_id", "stage")) %>% 
  mutate(across(.cols = everything(), .fns = ~str_replace(.,"^NA$", NA_character_))) %>% 
  purrr::discard(~all(is.na(.)))

  
temp4 <- left_join(erc_proj, erc_id, by = c("project_id","status", "contact", "organizations_id"))  %>% 
  relocate(id_perso, nationality) %>% 
  mutate(id_perso=ifelse(is.na(id_perso)|id_perso=="x", orcid_id, id_perso),
         nationality=ifelse(is.na(nationality), cd_main_nationality, nationality)) %>% 
  unique() %>% 
  group_by(contact) %>% 
  dplyr::mutate(n=n()) %>% 
  fill(c(id_perso, nationality), .direction = "downup") %>% 
  ungroup() %>% 
  unique()


write.csv2(temp4, file=paste0(identification, "erc_identification_", date_sauve, '.csv'), 
           na="", row.names = FALSE, fileEncoding = "utf8") 

#########
#RETOUR iderefisation
filename = "contact_idref_2022-01-25T14"
erc_new <- read.csv2(file=paste0(identification, "corrected/", filename, ".csv"), 
            na="", encoding = "UTF-8", colClasses=c("project_id"="character"))

erc_new <- erc_new %>% 
  select(status, id, participant_order, participates_as, organizations_id, participant_id, 
         contact, id_perso, idref, nationality) %>% 
  unique()




###
# 

gender <- erc %>% 
  dplyr::filter(!is.na(project_id)) %>% 
  inner_join(perso_erc, 
            by = c("project_id", "participant_pic", "applicant_pic", "stage", "status")) %>% 
  select(stage, status, action3_id, project_id, first_name, last_name, gender) %>% 
  unique() %>% 
  dplyr::filter(!is.na(last_name))

test <- gender %>% 
  select(status, project_id, first_name, last_name) %>% 
  unique() %>% 
  group_by(status, project_id, last_name) %>%
  mutate(n_last=n_distinct(last_name), n_first=n()) %>% 
  ungroup()
    
    
# gender <- temp4 %>% select(first_name, last_name, nationality, gender) %>% 
#   mutate_if(is.character,str_squish) %>%
#   unique()

gender_code <- 
readxl::read_xlsx(paste0(identification, "corrected/_gender.xlsx"), 
                  sheet="gender", na="") %>% 
  mutate_if(is.character,str_squish) %>% 
  unique() %>% 
  group_by(first_name) %>% 
  mutate(n=n())

gender <- left_join(gender, gender_code, by = "first_name") %>% 
  unique()

gender_a_trouver <- gender %>% 
  dplyr::filter(is.na(gender)) %>% 
  select(first_name, last_name, nationality)
write.csv2(gender_a_trouver, file=paste0(identification, "erc_gender_", date_sauve, ".csv"),
           row.names = F, fileEncoding = "UTF-8")


###############################################################
#erc etranger
erc_etr <- 
  separate(erc_etr, col = id, into=c("id_p", "applicant_pic", "participant_pic"), sep = "-", remove = F) %>% 
  left_join(perso_erc, by=c("project_id", "participant_pic", "applicant_pic", "status", "stage")) %>% 
  select(-id_p, -starts_with("action4_"))






######################################################################################
# selection des ERC signés
erc <- left_join(erc_proj, erc_id, by = c("project_id", "pic_id", "pic_id2", "contact")) %>% unique() %>% 
  mutate(temp=paste0("idref", idref))

# liste des ERC non affiliés
erc_affi <- erc %>% filter(is.na(participant_id) & !is.na(idref)) %>% select(idref) %>% unique() %>% 
  write.csv2(file=paste0(identification, "affil_", date_sauve, '.csv'), row.names = FALSE)

# retour des ERC affiliés
erc_affi <- read.csv2(file=paste0(identification, "corrected/erc_affil_2020-11-17T15-25.csv"))

erc <- left_join(erc, erc_affi, by = c("temp"="idref")) %>% 
  mutate(participant_id = if_else(is.na(participant_id), struct, participant_id)) %>% 
  select(-end, -struct, -temp)

#########################################################################################################

proj <- 
  charger_rdata(paste0(data_source,"datas.RData"), "project") %>% 
  select(project_id=NUM_PROJET, year=ANNEE, panel1_id, panel1_lib, panel2_id, panel2_lib, action2_id, action2_lib) %>% 
  mutate(project_id=as.character(project_id), stage="project")
prop <- 
  charger_rdata(paste0(data_source,"datas.RData"), "proposal") %>% 
  select(project_id=NUM_PROJET, year=ANNEE, panel1_id, panel1_lib, panel2_id, panel2_lib, action2_id, action2_lib) %>% 
  mutate(project_id=as.character(project_id), stage="proposal")

prop <- anti_join(prop, proj, by= "project_id")
proj <- bind_rows(proj, prop) %>% mutate(prog="H2020")

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
