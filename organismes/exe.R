livraison = "2021_11"
chemin = paste0("C:/Users/zfriant/Documents/OneDrive/PCRI/participants/", livraison)
source_org = paste0(chemin, "/org/")
chemin_bd = paste0(chemin, "/data/")


load(paste0(chemin_bd, "participant_org_e01_2022-08-09-T14-52.Rdata"))
load(paste0(source_org, "org_identified_2019_11.RData"))


base <- participant %>% 
  select(organizations_id, year, id, stage, project_id, participant_order, participates_as, 
         role, org, org_id) %>% 
  unique()



# fichier créé pour le recodage des orgas, il contient toutes les participations
# signées et évaluées. Partir de celui -ci pour les traitements
part_all <-  read.csv2(paste0(chemin, "2019_11/reperage_participations.txt"), sep = ";", 
                        dec = ",", na.strings = NA_character_) %>% 
  mutate(CD_TYPE = ifelse(stage == "proposal" & CD_ROLE %in% c("Partner", "Coordinator"), "BENEFICIARY",
                            ifelse(stage == "proposal" & CD_TYPE == "", CD_ROLE, CD_TYPE)))
#  filter(CD_TYPE == "BENEFICIARY")

# year <- read_excel(paste0(chemin, livraison, "/PROPOSAL_YEAR.xlsx"), 
#                    col_types = "text") %>% dplyr::rename(NUM_PROJET = CD_PROP_ID)

# concatenations des retours organismes
# org_all <- bind_rows(inra, cnrs, cea, inria, onera)  %>% 
#   left_join(., year, by = "NUM_PROJET") %>% 
#   mutate(org_id = if_else(nchar(org_id)==9&!is.na(siret), siret,org_id))

org_pour_api <- org_all %>% filter(is.na(id_org)) %>% 
  select(LABO, SIG_LABO, NOM_LABO, VILLE_LABO, year, org, renseigne) %>% unique()

load(paste0(chemin, livraison,"/identification_org/org_all.RData"))

temp <- org_all %>% select(id, stage, org, org_id) %>% unique()






save(org_all, file=paste0(chemin, livraison,"/identification_org/org_all.RData"),
     compress=FALSE)
write.csv2(org_pour_api, file=paste0(chemin, livraison,"/identification_org/org_pour_api.csv"),
           fileEncoding = "UTF-8", row.names = FALSE)


# ajout des données tables DEPT

part_dept <- read.csv2(paste0(chemin_bd, "PARTICIPANTS_DEPT.txt")) %>% 
  mutate(id = paste0(CD_PROJ_ID, "-", CD_PART_PIC, "-", CD_APPL_PIC), stage = "project") %>% 
  dplyr::rename(NUM_PROJET = CD_PROJ_ID) %>% 
  select(id, stage, NUM_PROJET, LB_DEPT_NAME, LB_DEPT_ADRS, CD_DEPT_POST, LB_DEPT_CITY, CD_DEPT_COUNTRY)


app_dept <- read.csv2(paste0(chemin_bd, "APP_DEPT.txt")) %>% 
  mutate(id = paste0(CD_PROP_ID, "-", CD_PART_PIC, "-", CD_APPL_PIC),
         stage = "proposal") %>% 
  dplyr::rename(NUM_PROJET = CD_PROP_ID) %>% 
  select(id, NUM_PROJET, stage,
         LB_DEPT_NAME = LB_DEPARTMENT_NAME, 
         LB_DEPT_ADRS = LB_DEPARTMENT_STREET, 
         CD_DEPT_POST = CD_DEPARTMENT_POSTAL_CODE, 
         LB_DEPT_CITY = LB_DEPARTMENT_CITY, 
         CD_DEPT_COUNTRY = CD_DEPARTMENT_COUNTRY)

part_dept <- bind_rows(part_dept, app_dept)

temp <- part_all %>% left_join(part_dept, by = c("id", "stage"))

pattern <- "(^| )(umrs|(umr( )?)+|ea|u|unit|unite|unity|gdr|fre|fr|frc|fed|je|upr|upesa|ifr|umr a|umemi|epi|eac|ums|ertint|inserm|insermu|ur|ups|uar|ert|usr|ura|umr d|rtra|ue|ers|cic|ep|umi|umr( )?cnrs|cnrs( )?umr|msh|umr ma|umr s|umr e|us|erm|erl|inra|umr t|dept|umr c)( )?(\\d+)\b"

x <- temp %>% mutate(LABO = str_extract(LB_DEPT_NAM, pattern))

x <- grep(pattern, temp$LB_DEPT_NAM)
# if _N_ = 1 then ExpressionID = prxparse(&labo_exp.);
# retain ExpressionID;