livraison = "2021_11"
chemin_pc = "C:/Users/zfriant/Documents/OneDrive/"
chemin = paste0(chemin_pc, "PCRI/")
participants = paste0(chemin, "participants/")
prog = paste0(chemin, "participants/exe/entites/")
data_result = paste0(participants, livraison, "/")
data_source = paste0(chemin, "traitement/", livraison, "/")
identification = paste0(participants, "identification/")
nomenclature = paste0(participants, "nomenclatures/")
referentiel = paste0(participants, "referentiels/")
sirene_chemin = paste0(chemin_pc, "sirene/")
referentiels_chemin = paste0(chemin_pc, "referentiels/")
tableau = paste0(participants, "tableau/")
data_old = paste0(participants, "2021_04/data/")
# global special_symb no_special


source(file=paste0(participants, "exe/fichiers_parametrage/parameters_import.R"))
source(file=paste0(prog, "a-01-function.R"))

date_sauve = format(Sys.time(), "%Y-%m-%d-T%H-%M")

# id_select=c("stage", "id", "participant_order", "participates_as")

pays_fr = c("FR","BL","CP","GF","GP","MF","MQ","NC","PF","PM","RE","TF","WF","YT","ZZ")

hospital = c('200023059', '200030013', '200034528', '200042166', '200047835', '200082105', '260300264', 
             '260500046', '260600705', '260900162', '261000020', '261300081', '261400931', '262100076', 
             '262200066', '262501760', '263000036', '263100125', '263305823', '263305849', '263400160', 
             '263500076', '263700189', '263800302', '263900060', '264200304', '264400136', '264500091', 
             '264900036', '265001073', '265100057', '265400101', '265400119', '265906719', '265906735', 
             '265907014', '266307461', '266500180', '266700574', '266900273', '267200160', '267500452', 
             '267500643', '267601680', '267802718', '268000148', '268300126', '268600012', '268708518', 
             '269301016', '269301248', '269401154', '269500153', '269710414')
grp_hosp = c("dUyiC", "8M9Gd", "FL57b", "ymJ4r", "Y4l0C", "Py0K5")
siren_gh = c("263100125", "267500452", "261300081", "264200304", "264400136", "266900273")

orga = c("130012024", "130013428", "130018310", "180006025", "180036048", "180037020", "180043036", "180070013", 
         "180070039", "180089013", "180089047", "18036048", "330715368", "331596270", "381984921", "385290309", 
         "390199669", "582056149", "775664113", "775665912", "775684897", "775685019", "775688229", "775722879", 
         "775729155", "775741101","783696834", "784257164")


mines_tel <- c("6Th4v")
etab_special <- c("B8q5v", '3RygZ')

etab_remove <- c("Entreprise", "Holding") 
                 # "Administration publique",
                 # "Rectorat", "Gouvernement", "Direction d'administration centrale", 
                 # "Service d'administration centrale",
                 # "Mission d'administration centrale", "Délégation d'administration centrale",
                 # "Instance du CNESER", "Unité de gestion administrative et des ressources humaines", 
                 # "Sous-direction d'administration centrale",
                 # "Service à compétence nationale", "Département / Bureau d'administration centrale")

cj_etat_etab <- c("EPSCP", "EPIC_AUT",  "ETAT_AUT", "ISBL", "ETAB_HOSP")

cci_paysage <- c("H1TgQ", "IuFFK", "i7Vmu", "S0Jbc")
cci <- c('130007503', '130008139', '130013105', '130013170', '130013543', '130014087', '130017270', '130020878', '130021603', '130021645', '130021660', '130021678', '130021710', '130021744', '130021801', '130022023', '130022031', '130022478', '130022494', '130022569', '130022668', '130022692', '130022718', '130023815', '130027923', '130027949', '130028012', '180070047', '180600017', '180600025', '180710014', '181300013', '181300021', '181300047', '181400052', '181600016', '181700014', '182100040', '182400010', '182630012', '183100023', '183200013', '183300052', '183400035', '183500016', '183500040', '183830017', '184401289', '184401347', '184401354', '184500114', '184500130', '184800019', '185300456', '185422037', '185522034', '185722030', '186400024', '186400032', '186400057', '186930020', '187100045', '187500020', '188002513', '188008502', '188200026', '188300016', '188300024', '188400014', '188400022', '188702021', '188822035', '188822118', '189720022', '189742117')
############################################################################################################
date_livraison <- data.frame(date_pub  = "novembre 2021")
write.csv2(date_livraison, file=paste0(data_result, "date_livraison.csv"), row.names = F)
rm(date_livraison)
gc()

