##################################################################################
##################################################################################

#########
####### subventions par geoloc
#####
load(file=rdata_last_version(paste0(data_result, "data/"), "geoloc_key_s01_"))
load(file=rdata_last_version(paste0(data_result, "data/"), "prop_proj_"))
tut <- load_one_obj(rdata=paste0(data_result, "structure_tut.Rdata"), "tut")
load(file=rdata_last_version(paste0(data_result, "data/"), "ref_part_q01_"))


#selection des vars utiles pour calculs
# toString(colnames(geoloc))

geo <- geoloc_full %>% 
  select(stage, id, participant_order, participates_as, participant_id=idr,  
         latitude, longitude, country_code, com_code, com_nom, aca_id, aca_nom, 
         uucr_id, uucr_nom, dep_id, dep_nom, dep_num_nom, reg_id, reg_nom) %>% 
  unite('gps', latitude, longitude, sep=",", remove=F, na.rm=T) %>%
  dplyr::mutate(loc=if_else(!is.na(com_code), com_code, gps)) %>% 
  group_by(stage, id, participant_order, participates_as) %>% 
  dplyr::mutate(nb_p=n_distinct(participant_id)) %>%
  group_by(stage, id, participant_order, participates_as, participant_id) %>% 
  dplyr::mutate(nb_l=n_distinct(loc), nb_l2=n_distinct(loc, com_code, gps)) %>%
  ungroup() %>% 
  unique()

geo[geo == ""] <- NA_character_

nrow(geo %>% dplyr::filter(!is.na(gps)))

message(
  "nombre de participants :\n",
  prettyNum(geo %>% dplyr::filter(stage=="project") %>% 
              select(id, participant_order, participates_as) %>%
              unique() %>% dplyr::summarize(n()), big.mark = " "), "\n",
  "nombre de projets : \n",
  prettyNum(geo %>% dplyr::filter(stage=="project") %>% 
              dplyr::mutate(project_id=str_split_fixed(id, "-", 3)[,1]) %>% select(project_id) %>%
              unique() %>% dplyr::summarize(n()), big.mark = " ")
)

#######################################################################################
### PROJECTS
geo_proj <- tut %>% dplyr::filter(stage=="project") %>% 
  select(stage, status, project_id, id, participant_order, role, participates_as, 
         participant_id, tutelle_id, funding_tut_net) %>% 
  left_join(geo, by=c("stage", "id", "participant_order", "participates_as", "participant_id")) %>% 
  select(stage, status, project_id, id, participant_order, role, participates_as, participant_id, nb_p, 
         tutelle_id, funding_tut_net, loc, nb_l) %>% 
  unique() %>% 
  group_by(id, participant_order, participates_as, participant_id, tutelle_id) %>% 
  dplyr::mutate(funding_tut_geo=funding_tut_net/nb_l, 
                funding_tut_geo=if_else(is.na(funding_tut_geo), funding_tut_net, funding_tut_geo)) %>% 
  ungroup() %>% 
  unique()
# write.csv2(structure_geoloc, file=paste0(identification, "verif_geoloc.csv"))
  
# prettyNum(sum(structure_geoloc$funding_tut_net, na.rm=T), big.mark = " ")  
        prettyNum(sum(geo_proj$funding_tut_geo, na.rm=T), big.mark = " ")
        message(
          "subv :\n",
          prettyNum(geo_proj %>% 
                      select(id, participant_order, participates_as, participant_id, tutelle_id, funding_tut_net, funding_tut_geo) %>% 
                      unique() %>% dplyr::summarize(sum(funding_tut_net, na.rm=T)), big.mark = " "), "\n",
          "nombre de participants :\n",
          prettyNum(geo_proj %>% dplyr::filter(!is.na(loc)) %>%
                      select(id, participant_order, participates_as) %>% 
                      unique() %>% dplyr::summarize(n()), big.mark = " "), "\n",
          "nombre de projets :\n",
          prettyNum(geo_proj %>% dplyr::filter(!is.na(loc)) %>%
                      select(project_id) %>% unique() %>% dplyr::summarize(n()), big.mark = " ")
        )
        prettyNum(geo_proj %>% dplyr::filter(is.na(loc)) %>% # non geolocalisé
                    select(stage, status, project_id, id, participant_order, role, participates_as, participant_id, 
                           tutelle_id, funding_tut_net) %>% unique() %>% dplyr::summarize(sum(funding_tut_net, na.rm=T))
                  ,big.mark = " ")
        prettyNum(geo_proj %>% dplyr::filter(is.na(loc)) %>% 
                    select(project_id) %>% 
                    dplyr::summarize(n_distinct(project_id)), big.mark = " ") 
        prettyNum(geo_proj %>% dplyr::filter(is.na(loc)) %>% 
                    select(project_id, id, participant_order, role, participates_as) %>% 
                    unique() %>% 
                    dplyr::summarize(n_distinct(project_id, id, participant_order, role, participates_as))
                  ,big.mark = " ") 
#######################################################################################
### PROPOSAL
## ELIGIBLE
geo_elig <- tut %>% dplyr::filter(stage=="proposal", status=="ELIGIBLE") %>% 
  select(stage, status, project_id, id, participant_order, role, participates_as, participant_id, 
         tutelle_id, funding_tut_net) %>% 
  left_join(select(geo, -stage), by=c("id", "participant_order", "participates_as", "participant_id")) %>% 
  select(stage, status, project_id, id, participant_order, role, participates_as, participant_id, nb_p, 
         tutelle_id, funding_tut_net, loc, nb_l) %>% 
  unique() %>% 
  group_by(id, participant_order, participates_as, participant_id, tutelle_id) %>% 
  dplyr::mutate(funding_tut_geo=funding_tut_net/nb_l,
         funding_tut_geo=if_else(is.na(participant_id), funding_tut_net, funding_tut_geo),
         funding_tut_geo=if_else(is.na(funding_tut_geo), funding_tut_net, funding_tut_geo)) %>% 
  ungroup() %>% 
  unique()

# write.csv2(geo_elig, file=paste0(identification, "verif_geoloc.csv")) #"34 624 182 559" avec les non idnetifiés

        prettyNum(sum(geo_elig$funding_tut_geo, na.rm=T), big.mark = " ")
        message("données géolocaliées :", "\n",
                "particpations : " , "\n",
            prettyNum(geo_elig %>% dplyr::filter(!is.na(loc)) %>% 
                        select(project_id, id, participant_order, role, participates_as) %>% 
                        unique() %>% 
                        dplyr::summarize(n_distinct(project_id, id, participant_order, role, participates_as))
                      ,big.mark = " ") , "\n",
            "project : " , "\n",
            prettyNum(geo_elig %>% dplyr::filter(!is.na(loc)) %>%
                        select(project_id) %>% unique() %>% 
                        dplyr::summarize(n_distinct(project_id)), big.mark = " "), "\n",
            "subventions : " , "\n",
            prettyNum(geo_elig %>% dplyr::filter(!is.na(loc)) %>% 
                        select(stage, status, project_id, id, participant_order, role, participates_as, participant_id, 
                        tutelle_id, funding_tut_net) %>% unique() %>% dplyr::summarize(sum(funding_tut_net, na.rm=T))
                    ,big.mark = " ")  #"32 178 044 284"
        )
        message("données non géolocaliées :", "\n",
                "particpations : " , "\n",
                prettyNum(geo_elig %>% dplyr::filter(is.na(loc)) %>% 
                            select(project_id, id, participant_order, role, participates_as) %>% 
                            unique() %>% 
                            dplyr::summarize(n_distinct(project_id, id, participant_order, role, participates_as))
                          ,big.mark = " ") , "\n",
                "project : " , "\n",
                prettyNum(geo_elig %>% dplyr::filter(is.na(loc)) %>%
                            select(project_id) %>% unique() %>% 
                            dplyr::summarize(n_distinct(project_id)), big.mark = " "), "\n",
                "subventions : " , "\n",
                prettyNum(geo_elig %>% dplyr::filter(is.na(loc)) %>% 
                            select(stage, status, project_id, id, participant_order, role, participates_as, participant_id, 
                                   tutelle_id, funding_tut_net) %>% unique() %>% dplyr::summarize(sum(funding_tut_net, na.rm=T))
                          ,big.mark = " ")  #"32 178 044 284"
        )
#######################################################################################
### PROPOSAL
## MAIN
geo_main <- tut %>% dplyr::filter(stage=="proposal", status=="MAIN") %>% 
  select(stage, status, project_id, id, participant_order, role, participates_as, participant_id, 
         tutelle_id, funding_tut_net) %>% 
  left_join(select(geo, -stage), by=c("id", "participant_order", "participates_as", "participant_id")) %>% 
  select(stage, status, project_id, id, participant_order, role, participates_as, participant_id, nb_p, 
         tutelle_id, funding_tut_net, loc, nb_l) %>% 
  unique() %>% 
  group_by(id, participant_order, participates_as, participant_id, tutelle_id) %>% 
  dplyr::mutate(funding_tut_geo=funding_tut_net/nb_l,
         funding_tut_geo=if_else(is.na(participant_id), funding_tut_net, funding_tut_geo),
         funding_tut_geo=if_else(is.na(funding_tut_geo), funding_tut_net, funding_tut_geo)) %>% 
  ungroup() %>% 
  unique()

# write.csv2(geo_main, file=paste0(identification, "verif_geoloc.csv"), na="") 

      message("données géolocaliées :", "\n",
        "particpations : " , "\n",
        prettyNum(geo_main %>% dplyr::filter(!is.na(loc)) %>% 
                    select(project_id, id, participant_order, role, participates_as) %>% 
                    unique() %>% 
                    dplyr::summarize(n_distinct(project_id, id, participant_order, role, participates_as))
                  ,big.mark = " ") , "\n",
        "project : " , "\n",
        prettyNum(geo_main %>% dplyr::filter(!is.na(loc)) %>%
                    select(project_id) %>% unique() %>% 
                    dplyr::summarize(n_distinct(project_id)), big.mark = " "), "\n",
        "subventions : " , "\n",
        prettyNum(geo_main %>% dplyr::filter(!is.na(loc)) %>% 
                    select(stage, status, project_id, id, participant_order, role, participates_as, participant_id, 
                           tutelle_id, funding_tut_net) %>% unique() %>% dplyr::summarize(sum(funding_tut_net, na.rm=T))
                  ,big.mark = " ")  #"32 178 044 284"
      )
      message("données non géolocaliées :", "\n",
        "particpations : " , "\n",
        prettyNum(geo_main %>% dplyr::filter(is.na(loc)) %>% 
                    select(project_id, id, participant_order, role, participates_as) %>% 
                    unique() %>% 
                    dplyr::summarize(n_distinct(project_id, id, participant_order, role, participates_as))
                  ,big.mark = " ") , "\n",
        "project : " , "\n",
        prettyNum(geo_main %>% dplyr::filter(is.na(loc)) %>%
                    select(project_id) %>% unique() %>% 
                    dplyr::summarize(n_distinct(project_id)), big.mark = " "), "\n",
        "subventions : " , "\n",
        prettyNum(geo_main %>% dplyr::filter(is.na(loc)) %>% 
                    select(stage, status, project_id, id, participant_order, role, participates_as, participant_id, 
                           tutelle_id, funding_tut_net) %>% unique() %>% dplyr::summarize(sum(funding_tut_net, na.rm=T))
                  ,big.mark = " ")  #"32 178 044 284"
      )
###########################################################################################################"
#####################################################################################

######
#     ATTENTION : CONSERVER LES CATEGORIES      
geo <- bind_rows(geo_proj, geo_elig, geo_main) %>% 
  left_join(ref_info, by=c("tutelle_id"="id_ref")) %>% 
  dplyr::mutate(id_gref=if_else(is.na(id_gref), participant_id, id_gref)) %>% 
  select(-cat) %>% 
  unique()

toString(colnames(geoloc_full))
geo <- geoloc_full %>% 
  select(com_code, com_nom, uucr_id, uucr_nom, aca_id, aca_nom, dep_id, dep_nom, dep_num_nom, dep_nom_num, reg_id, reg_nom) %>% 
  unique() %>% 
  right_join(geo, by=c("com_code"="loc")) %>% 
  dplyr::mutate(gps=if_else(str_detect(com_code, ","), com_code, NA_character_))

#niveau groupe entreprise des participants
structure_geo <- geo %>% 
  group_by(stage, status, id_gref, name, acronym, project_id, role, participates_as, gps,
           dep_id, dep_nom, dep_num_nom, dep_nom_num, aca_id, aca_nom, reg_id, reg_nom) %>% 
  dplyr::summarize(funding_share=sum(funding_tut_geo)) %>% 
  ungroup()

 
  structure_geo %>% group_by(status) %>% 
    dplyr::summarize(sum(funding_share, na.rm=T))

rm(list=c("geo_elig","geo_main","geo_proj","geoloc"))
gc()


############################################################################################
############################################################################################

### INFO niveau projet
# table proposal project avec info utile
temp <- prop_proj %>% 
  select(stage, project_id, year, call_code, topic_code, pilier, programme_abbr,programme_lib, 
         area_abbr, area_lib, action4_id, action4_lib,
         action3_id, action3_lib, action2_id, action2_lib, action1_id, action1_lib,
         panels_code_1, panels_name_1, panels_code_2, panels_name_2, msca_code, msca_name) %>% 
  unique()

#jointure structure_benef avec info project_id
structure_geo <- left_join(structure_geo, temp, by=c("project_id", "stage")) %>% unique()

rm(list=c("temp", "prop_proj"))
gc()


########################################################################################################
# aggrégation des données totales calcul du funding_t, proj_t
total <- structure_geo %>% 
  group_by(stage, status, year, call_code, topic_code, action4_id, action4_lib,
           action3_id, action3_lib, action2_id, action2_lib, action1_id, action1_lib,
           pilier, programme_abbr,programme_lib, area_abbr, area_lib, 
           panels_code_1, panels_name_1, panels_code_2, panels_name_2, msca_code, msca_name) %>% 
  dplyr::summarize(funding_t=sum(funding_share, na.rm=T), proj_t=n_distinct(project_id)) %>% 
  ungroup() %>% unique()

  message(
  prettyNum(total %>% dplyr::filter(stage=="proposal", status=="ELIGIBLE") %>% 
              dplyr::summarize(sum(funding_t)), big.mark = " "), "\n",
  prettyNum(total %>% dplyr::filter(stage=="proposal", status=="MAIN") %>% 
              dplyr::summarize(sum(funding_t)), big.mark = " "), "\n",
  prettyNum(total %>% dplyr::filter(stage=="project") %>% 
              dplyr::summarize(sum(funding_t)), big.mark = " "), "\n",
  prettyNum(total %>% dplyr::filter(stage=="project") %>% 
              dplyr::summarize(sum(proj_t)), big.mark = " ")
  )

# croisement category et total
# load(file = paste0(participants, "exe_sur_fixe.Rdata"))

# toString(colnames(structure_geo))

loc <- structure_geo %>% 
  dplyr::mutate(loc_code=if_else(!is.na(dep_id), dep_id, gps)) %>% 
  select(loc_code, gps, dep_id, dep_nom, dep_num_nom, aca_id, aca_nom, reg_id, reg_nom) %>% 
  dplyr::filter(!is.na(loc_code)) %>% unique()

if(nrow(loc %>% select(loc_code) %>% unique())==nrow(loc)){
  print("ok")
}else{
    print("pb doublon dans loc")
  }

#pdt cartesien loc + total
total <- total %>%
  group_by(programme_abbr) %>%
  merge(loc, by=NULL) %>%
  ungroup() 

message(
  prettyNum(total %>% dplyr::filter(status=="SIGNED") %>%  
   select(year, call_code, topic_code, pilier, programme_abbr, programme_lib, area_abbr, area_lib, 
          panels_code_1, panels_name_1, panels_code_2, panels_name_2, msca_code, msca_name, funding_t) %>% 
          unique() %>% 
     dplyr::summarize(sum(funding_t)), big.mark = " "), "\n",
  prettyNum(total %>% 
    dplyr::filter(status=="SIGNED") %>%  
    select(year, call_code, topic_code, pilier, programme_abbr, programme_lib, area_abbr, area_lib, 
           panels_code_1, panels_name_1, panels_code_2, panels_name_2, msca_code, msca_name, proj_t) %>% 
    unique() %>% dplyr::summarize(sum(proj_t)), big.mark = " ")
)  

# concat total + structure benef + récupération libellé category
# structure_geo <- bind_rows(structure_geo, total)


# sauvegarde
save(structure_geo, geo, total, file=paste0(data_result, "structure_geo.Rdata"))

cat_name <- readxl::read_xlsx(paste0(participants, "nomenclatures/_category.xlsx"), 
            sheet="category_name", na="") %>% dplyr::mutate_all(~str_squish(.))
ref <- ref_info %>% select(id_gref, category_5) %>% unique() %>% 
  group_by(id_gref) %>% mutate(n=n())
structure_geo <-  structure_geo %>% 
  dplyr::filter(!is.na(project_id)) %>% 
  left_join(ref, by = "id_gref") %>% 
  left_join(cat_name, by=c("category_5"="category"))

write.csv2(structure_geo, file=paste0(data_result, "structure_geo.csv"), na = "", 
           row.names = F, fileEncoding = "UTF-8")

rm(list=c("tut", "total", "loc", "ref_info"))
gc()

#########################
library(geojsonio)
library(sf)
library(rgdal)
geo_poly <- geojson_read(paste0(tableau, "geocodage/fr-en-contour-academies-2020.geojson"),
                         parse=T, what = "sp")


geo_poly <- st_read(paste0(tableau, "geocodage/fr-en-contour-academies-2020.geojson"), drivers = "GeoJSON") %>% 
  # geo_poly@data$code_academie <- paste0("A", geo_poly@data$code_academie)
  mutate(aca_id=ifelse(nchar(code_academie)==1, paste0("A0", code_academie),
                       paste0("A", code_academie))) %>%
  select(aca_id, geometry)

# geo_poly <- read.csv2(file=paste0(tableau, "geocodage/fr-en-contour-academies-2020.csv"), encoding = "UTF-8") %>%
#   mutate(code_pays="FR", aca_id=ifelse(nchar(code_academie)==1, paste0("A0", code_academie),
#            paste0("A", code_academie))) %>%
#   select(code_pays, aca_id, aca_name=name, geo_shape) %>%
#   unique()
# write.csv2(geo_poly, file=paste0(data_result, "geo_shape.csv"), fileEncoding = "UTF-8")
# geo_shape_sf = st_as_sf(geo_shape, coords = c("geo_shape"), crs = 28992)
#   st_write(geo_shape, paste0(data_result, "geo_shape.geojson"))  


structure_geo <- left_join(structure_geo, geo_poly, by="aca_id")

fn <- paste0(data_result, "structure_geo.geojson")
#Check its existence
if (file.exists(fn)) {
  #Delete file if it exists
  file.remove(fn)
}

st_write(structure_geo, paste0(data_result, "structure_geo.geojson"), delete_dsn=TRUE)

