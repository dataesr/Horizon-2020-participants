# import fichiers de paramétrages pour le nettoyage

## création vecteur de remplacement
add_abbr <- read.table(paste0(participants,"exe/fichiers_parametrage/adresse_abreviation.csv"),
                       sep = ";", header=TRUE)
a <- pull(add_abbr,abrev_adr)
b <- pull(add_abbr, long_adr)


word_remplacement <- 
  read.table(paste0(participants,"exe/fichiers_parametrage/Liste_mots_remplacement.csv"), 
            sep = ";", header=TRUE) %>% 
  dplyr::filter(pays %in% c("all", "fr")) %>% 
  dplyr::mutate(word=str_squish(word), newword=str_squish(newword))
c <- pull(word_remplacement,word)
d <- pull(word_remplacement,newword)

word <- c(a, c)
word <- str_squish(word)
word_new <- c(b, d)
word_new <- str_squish(word_new)

# lettres spciales : accentués, oe, ae,...
# exolist <- read.csv2(paste0(chemin_pc,"participants/exe/fichiers_parametrage/lettres_remplacement.csv"))
# special_symb <- pull(exolist, symbol)
# no_special <-  pull(exolist, replace)



## mots vides à supprimer
stop_word <- read.table(paste0(participants,"exe/fichiers_parametrage/Liste_mots_vides.csv"), 
                        sep = ";", header=,TRUE) %>% 
  dplyr::filter(pays%in%c("all", "fr"))
no_word <- pull(stop_word,word)

word_del <- c("batiment", "bureau(x?)", "immeuble", "dactivite", "administrative", "bat")
word_del_tag <- c("quartier","parc","professeur","rue","route","place","lieutenant","regiment","zone_activite",
                  "clos","centre","activite","cours","impasse","division","docteur","domaine(universitaire)?","general",
                  "zone_industrielle", "zone_portuaire", "zone_industrielle_portuaire", "rond point( echangeur)?",
                  "zac","za","lieu dit","allee","avenue","adjudant","batiment","boulevard","bp","commandant", "chemin",
                  "boulevard","quai","lieudit","lieu-dit", "parvis", "place", "route")

# Sigles labos
llab <- c("umr", "ua", "umrs", "umr s","ea", "u", "gdr", "fre", "fr", "frc", "fed", "je", "us", "ums",
        "upr","upesa","ifr","umr a","umemi","epi","eac", "ertint", "ur", "ups", "umr m", "umr t",
        "uar","ert","usr","ura","umr d","rtra","ue","ers","cic","ep","umi", "unit")


# Pattern organisations pour qualification; création vars organismes, societe
pattern_ifremer = "(ifremer|in.* fran.* re.* ex.* mer)"
pattern_cnrs =   "(ce.* na.* (de )?(la )?re.* sc.[a-z]*|fr.* na.* sc.* re.* ce.[a-z]*|cnrs|c\\.n\\.r\\.s|c n r s)"
pattern_inria =  "(^| |\\(|\\.)(in.* na.* (de )?re.* (en )?in.* (et )?(en )?au.[a-z]*|inria|i\\.n\\.r\\.i\\.a|i n r i a)"
pattern_inrae =   "(^| |\\(|\\.)(in.* na.* (de )?re.* ag.[a-z]*|inra|inrae|irstea|i\\.n\\.r\\.a|i n r a)"
pattern_inserm = "(^| |\\(|\\.)(in.* na.* (de )?(la )?sa.* (et )?(de )?(la )?re.* me.[a-z]*|inserm|i\\.n\\.s\\.e\\.r\\.m|i n s e r m)"
pattern_cea =    "(^| |\\(|\\.)(co.* (a )?l?\\'?en.* at.[a-z]*|cea|c\\.e\\.a|c e a)"
pattern_ens =    "(^| |\\(|\\.)(ec.* no.* sup.[a-z]*|ens\\b|e\\.n\\.s|e n s)"
pattern_fnsp =   "(^| |\\(|\\.)(fo.* na.* (des )?sc.* po.[a-z]*|fnsp|sciences po|f\\.n\\.s\\.p|f n s p)"
pattern_cirad =  "(^| |\\(|\\.)(ce.* (de )?co.* in.* (en )?re.* ag.* (pour )?(le )?dev.[a-z]*|cirad|c\\.i\\.r\\.a\\.d|c i r a d)"
pattern_ird =    "(^| |\\(|\\.)(in.* (de )?re.[a-z]* (pour )?(le )?dev.[a-z]*|ird|i\\.r\\.d|i r d)"
pattern_chu =    "(^| |\\(|\\.)(ce.* hos.* univ.[a-z]*|univ.* hosp.[a-z]*( of)?|chu|c\\.h\\.(r\\.)?u|c h (r )?u)"
pattern_universite =   "(^| |\\(|\\.|.)(univ(ersite|ersity|ersitaire| |\\.|-)( of)?)"
pattern_pasteur =   "(^| |\\(|\\.)(ins([a-z]*|\\.*) pasteur( de)?( lille)?|pasteur inst([a-z]*|\\.*))"
pattern_curie =    "(^| |\\(|\\.|.)(inst([a-z]*|\\.*) curie|curie inst([a-z]*|\\.*))"
pattern_irsn =   "(^| |\\(|\\.)(in.* (de )?radio.[a-z]* (et )?(de )?sur.[a-z]* nuc.[a-z]*|irsn|i\\.r\\.s\\.n|i r s n)"
pattern_onera =  "(^| |\\(|\\.)(onera|off.* na.* (d )?etu.* (et )?(de )?rech.* aero.*)"
pattern_agrocampus = "(^| |\\(|\\.)(agrocampus)"
pattern_ecole = "(^| |\\(|\\.)(ecole)"
lpattern <- c("cnrs", "inria", "inrae", "inserm", "cea", "ens", "fnsp", "cirad", "ird", "chu", "ifremer",
           "universite", "pasteur", "curie", "irsn", "onera", "agrocampus", "ecole")




rm(list=c("a","b","c","d","add_abbr", "word_remplacement", 'stop_word'))
gc()

pattern_gps = "^.+\\.\\d{1,5}"

pattern_grid = "^grid."
pattern_rnsr = "\\b(\\d{9}\\D{1})\\b"
pattern_siret = "\\b(\\d{14})\\b"
pattern_siren = "\\b(\\d{9})\\b"
pattern_paysage = "^([[:alnum:]]{5})$"
pattern_uai = "\\b(\\d{7}\\D{1})\\b"
pattern_assoc = "\\b(W{1}\\w{9})\\b"
pattern_finess = "^F{1}\\d{9}"
pattern_pic = "^pic"
pattern_ror = "^R0(\\w){6}[0-9]{2}$"


special_symb <<- c("Œ","Ž","š","œ","ž","Ÿ","À","Á","Â","Ã","Ä","Å","Æ","Ç","È","É","Ê","Ë","Ì","Í","Î","Ï","Ñ",
                   "Ò","Ó","Ô","Õ","Ö","×","Ù","Ú","Û","Ü","Ý","ß", "à","á","â","ã","ä","å","æ","ç","ç", "è","é","ê", "è",
                   "ë","ì","í","î","ï","ñ","ò","ó","ô","õ","ö","ø","ù","ú","û","ü","ý","ÿ")

no_special <<- c("OE","Z","s","oe","z","y","A","A","A","A","A","A","AE","C","E","E","E","E","I","I","I","I","N",
                 "O","O","O","O","O","x","U","U","U","U","Y","ss","a","a","a","a","a","a","ae","c", "c", "e","e","e","e",
                 "e","i","i","i","i","n","o","o","o","o","o","o","u","u","u","u","y","y")