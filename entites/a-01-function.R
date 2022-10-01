#import de la dernière rdata créé
rdata_last_version <- function(chemin, filename){
  data_files <- file.info(Sys.glob(path=paste0(chemin, filename, "*")))
  row.names(data_files)[which.max(data_files[["mtime"]])]
  print(row.names(data_files)[which.max(data_files[["mtime"]])])
}


## fonction LOAD avec choix d'importer un objet specifique
load_one_obj <- function(rdata, nom_objet = NULL){
   
   if (!file.exists(rdata)) {
      stop(paste0("Le fichier \"", rdata, "\" n'existe pas."), call. = FALSE)
   }
   
   env = new.env()
   load(file = rdata, envir = env)
   
   if (!is.null(nom_objet)) {
      charger_rdata <- env[[nom_objet]]
   } else {
      charger_rdata <- as.list(env)
   }
   
   return(charger_rdata)
}


# # minuscule + suppr. [space, \t, \n] -> left / right / keep one whitespace inside

tolower_noblank <- function(v){
   v <- str_to_lower(v)
   v <- str_squish(v)
   return(v)
}

remove_space <- function(x){
   x <- str_replace_all(x, "\\s", "")
   return(x)
}

# # sans accent 
# Unaccent <- function(v) {
#    v <- gsub("['`^~\"¨]", " ", v)
#    v <- iconv(v, to="ASCII//TRANSLIT//IGNORE")
#    v <- gsub("['`^~\"¨]", "", v)
#    return(v)
# }


# lettres spéciales et/ou accentuées minuscules et majuscules, oe, ae,...
replace_special_letters <- function(x){
   
  special_symb <<- c("ð","Œ","Ž","š","œ","ž","Ÿ","À","Á","Â","Ã","Ä","Å","Æ","Ç","È","É","Ê","Ë","Ì","Í","Î","Ï","Ñ",
                     "Ò","Ó","Ô","Õ","Ö","×","Ù","Ú","Û","Ü","Ý","ß", "à","á","â","ã","ä","å","æ","ç","ç", "è","é","ê", "è",
                     "ë","ì","í","î","ï","ñ","ò","ó","ô","õ","ö","ø","ù","ú","û","ü","ý","ÿ", "ț", "ș")
  
  no_special <<- c("o","OE","Z","s","oe","z","y","A","A","A","A","A","A","AE","C","E","E","E","E","I","I","I","I","N",
                   "O","O","O","O","O","x","U","U","U","U","Y","ss","a","a","a","a","a","a","ae","c", "c", "e","e","e","e",
                   "e","i","i","i","i","n","o","o","o","o","o","o","u","u","u","u","y","y", "t", "s")
  
   if (length(special_symb)!=length(no_special)){
      return(print("error length vectors")) 
   }
   else{
      for (i in seq_along(special_symb)){
         x <- gsub(special_symb[i], no_special[i], x, fixed = TRUE)
      }
      return(x)
   }
}  


# Attention !!!! utiliser la fonction replace_special_letters() avant clean_exo (qui supprime les lettres accentuées)
# sup tous les caractères spéciaux y compris lettre accentuée hors alpha, num, /, space, 
clean_exo_char <- function(x){
   x <- str_replace_all(x, "[^0-9A-Za-z\\/ ]", " ")
   x <- str_squish(x)
   return(x)  
}    


del_empty_var <- function(x){
   mask_drop <- colnames(x[colSums(x!='') == 0] == TRUE)
   print(mask_drop, quote=TRUE)
   # copier la liste ci-dessous en supprimant celles qui doivent Ãªtre gardÃ©es
   x <- x[ , !names(x) %in% mask_drop]
   return(x)
}


###########################################################################################################
###########################################################################################################

# nettoyage post_code
clean_post_code  <- function(x){

      x2 = str_extract(x, "(\\d){5}")
      x_net = if_else(is.na(x2), str_replace(str_extract(x, "\\d{2} \\d{3}"), "\\s", ""), x2)
      return(x_net)
}

# # nettoyage ville

clean_city <- function(x){
   x <- str_replace_all(x, "(\\.|[[:cntrl:]])+", " ")
   x <- str_replace_all(x, "(\\d)+( )*(er|eme|ème|ier|°|e)+( )*"," ")
   x <- str_replace_all(x, "(fr|f)( )*(-|,)+", " ")
   x <- str_replace_all(x, "(cedax|cedex)+( )*(\\d)*( )*(-|,|\\.|\\()*( )*(france)( )*(\\)|\\.)*", " ")
   x <- str_replace_all(x, "(cedax|cedrex|cdexe|cdex|credex|cedex|cédex|cedx|cede|ceddex|cdx|cex|cexex|edex)+( )*(\\d)*"," ")
   x <- str_replace_all(x, "(\\(|-|,)*( )*(b p|bp)( )*(\\d)*( )*(-|,|\\))*", " ")
   x <- str_replace_all(x, "(\\(|-|,)*( )*\\b(case|cs|cx|ct)( )*(\\d)*( )*(-|,|\\))*", " ")
   x <- str_replace_all(x, "cdg|cgd", " ")
   x <- str_replace_all(x, "\\d+( )*(-)", " ")
   x <- if_else(str_detect(x ,"(en|de|-)( )*(france)")==FALSE, str_replace_all(x, "( )+france", " "), x)
   x <- if_else(str_detect(x ,"^france( |-|,)+")==TRUE, str_replace_all(x, "^france( |-|,)+", " "), x)
   x <- str_replace_all(x, "^france$|^-$", " ")
   # x <- str_replace_all(x, "-( )*$", " ")
   # x <- str_replace_all(x, "\\(.*\\)", " ")
   # x <- str_replace_all(x, "(-|,)*( )*(fr|f)( )*$", " ")
   x <- str_replace_all(x, "\\d+",  " ")
   x <- str_squish(x)
   
   x <- paste(" ", x, sep=" ")
   x <- str_replace_all(x, " st-", ' saint-')
   x <- str_replace_all(x, " st ", ' saint ')
   x <- str_replace_all(x, " ste-", ' sainte-')
   x <- str_replace_all(x, " ste ", ' sainte ')
   x <- str_replace_all(x, "( )*rp( )*$", ' ')
   x <- str_replace_all(x, "^ce ", ' ')
   x <- gsub("\\/|s/|s/s", " sur ", x)
   x <- str_replace_all(x, "\\?|,", " ")
   x <- str_replace_all(x, "[[:punct:]]", " ")
   x <- str_squish(x)
   x <- str_replace(x, "NA", NA_character_)
   return(x)
}

word_stop_word <- function(x){
   for (i in no_word) {
      i_paste = paste0("\\b",i,"\\b")
      x <- str_replace_all(x, i_paste, " ") 
      x <- str_squish(x)
   }
   return(x)
}

word_to_delete <- function(x, del_string){
   
      for (i in del_string) {
      i_paste = paste0("\\b",i,"\\b")
      x <- str_replace_all(x,i_paste," ")
      x <- str_squish(x)
   }
   return(x)
}


word_to_replace <- function(x){
   for (i in word) {
      i_paste = paste0("\\b",i,"\\b")
      loc = which(word==i)
      x <- str_replace_all(x,i_paste,word_new[loc])
      x <- str_squish(x)
   }
   return(x)
}


# Suppression des points dans les mots s.a, s.a.r.l et autres acronymes
remove_point <- function(x){
   pattern <- "\\b(?:[a-zA-Z]\\.){1,}"
   loc <- map(x, str_detect, pattern)
   y <- if_else(loc == TRUE, str_replace_all(x, "\\.", ""), x)
   return(y)
}
# temp <- temp %>% dplyr::mutate(sortie = map(name_source,remove_point))

clean_address <- function(x){
   x <- str_squish(str_replace_all(x, "\\b(c/o|c/|s/n)\\b(^| ?)([[:graph:]])*", " " ))
   x <- str_squish(str_replace_all(x, "(\\d)+ ?(er|ieme|eme|ème|ier|e)\\b"," "))
   x <- str_squish(str_replace_all(x, "\\b(eq|pc|box|credex|cedex|cédex|cedx|cede|ceddex|cdx|cex|cexex|edex|cs|cx|bp|case|etage(s?)|etag|tsa|cc|cp|ps|bt)+( )?(\\d)*"," "))
   x <- str_squish(str_replace_all(x, "(\\d+( ?|\\b)(bis|ter)\\b)", " ")) #suppr: 15bis, 15 bis
   x <- str_squish(str_replace_all(x, "\\bzone ", "zone_"))
   x <- str_squish(str_replace_all(x, "(boite courrier|point courier|point courrier|courrier) ?(\\d)*", " " ))
   
   x <- str_squish(str_replace_all(x, "\\b(pavillon)\\b ?([[:graph:]]+|\\s*)", " " ))
   x <- str_squish(str_replace_all(x, "(immeuble(s?))\\b(^| ?)([[:graph:]])*", " " ))
   x <- str_squish(str_replace_all(x, "(batiment(s?)) ?([[:graph:]])+ *", " " ))
   x <- str_squish(str_replace_all(x, "(espace)(^| ?)([[:graph:]]+|\\s*)", " " ))
   x <- str_squish(str_replace_all(x, "\\b(tour)\\b ?([[:graph:]])+ *", " " ))
   x <- str_squish(str_replace_all(x, "\\b(porte)\\b ?([[:digit:]])+", " " ))   
   x <- str_squish(str_replace_all(x, "\\b(bureau)\\b ?([[:digit:]])+", " " ))
   return(x)
}


finition <- function(x){
   x <- str_squish(str_replace_all(x, "\\d+", " " ))
   x <- str_squish(str_replace_all(x, "/", " " ))
   x <- str_squish(str_replace_all(x, "\\b[a-z]{1}\\b", " " ))   
   x <- str_squish(str_replace_all(x, "^[a-z]{1,3}$", " " ))
   return(x)
}



labo_extraction <- function(x){
   x = str_replace_all(x, "(umr( )?inra|inra( )?umr|inserm( )?urm|inserm( )?umr|umr( )?cnrs|cnrs( )?umr|umr( )?inserm|umr()?ird)", "umr")
   x = str_replace_all(x, "(umr( )?n( )?(?=\\d+))", "umr")
   x = str_replace_all(x, "(cic( )?inserm|inserm( )?cic( )?ec)", "cic")
   x = str_replace_all(x, "(inserm unite de service)", "us")
   x = str_replace_all(x, "\\b(inserm um|inserm research unit|inserm s|inserm|insermu|inserm unit)(?=(\\s?)\\d+|\\s?)", "u")
   x = str_replace_all(x, "\\b(jru|urm)(?=\\d+|\\s?)", "umr")
   x = str_squish(x)
   
   lsigles <- c("")
   for (i in llab) {
      pattern = paste0("\\b(",i,")( )?(\\d+)")
      y <- map_chr(str_extract_all(x, pattern), toString)
      lsigles = str_c(lsigles, y, sep=',')
      lsigles = str_remove_all(lsigles, "(,$+)|(^,+)")
      lsigles = str_squish(str_replace_all(lsigles, "\\s+", ""))
   }
   return(lsigles)
}


qualif_organisation<-function(x){    
  org <- c("")
  for (j in lpattern){
     pattern <- get(paste0("pattern_", j))
     y <- map(x, str_count, pattern)
     z <- if_else(y>0, j, "")
     z <- str_squish(z)
     org = str_c(org, z, sep=',')
     org = str_remove_all(org, "(,$+)|(^,+)")
  }
   return(org)
}


qualif_societe <- function(x){
   societe = "sa sarl sas eurl sci srl eirl sasu snc gie ltd limited bv sro nv gmbh gesmbh spa sl doo aisbl asbl sprl vzw cie entreprise compagnie societe"
   vec <- strsplit(societe, " ")[[1]]
   inst = 0
   for (i in vec){
      pattern = paste0("\\b", i,"\\b")  
      y <- map(x, str_detect, pattern)
      z <- if_else(y==TRUE, 1, 0)
      inst = sum(inst, z)
   } 
   return(inst)
}

email_domain <- function(x){
   pattern = "(?<=@)([[:alnum:]]+|([[:alnum:]]+(\\.|-)[[:alnum:]]+)+)(?=\\.[a-z]{2,6}$)"
   y = str_extract(x, pattern)

   del_dom <- c("gmail", "yahoo", "hotmail", "wanadoo", "aol", "free", "freemail", "freenet", "skynet", "orange")   
   for (i in del_dom){
      pat = paste0("^\\b", i, "\\b")
      y = if_else(str_detect(y,pat), NA_character_, y)
   }
   return(y)
}

# Normalisation des numéros de téléphone
tel_clean <- function(x){
   x = as.character(str_replace_all(x, "\\s+", ""))
   x = clean_exo_char(x)
   y = as.character(str_replace_all(x, "\\/|\\s+", ""))
   
   z = if_else(str_detect(y, "^0033"), str_sub(y, 5, nchar(y)),
               if_else(str_detect(y, "^033"), str_sub(y, 4, nchar(y)),        
                       if_else(str_detect(y, "^33"), str_sub(y, 3,  nchar(y)),
                               if_else(nchar(y) == 10 & str_detect(y, "^0{1}\\d{9}"), y,
                                       NA_character_))))
   
   z = if_else(str_sub(z, 1, 1) != "0" & nchar(z)==9, paste0("0", z),
               if_else(str_sub(z, 1, 2) == "00", str_replace(z, "^0{1}", ""),
                       if_else(str_detect(z, "^0") & nchar(z) > 10, str_sub(z, 1, 10),
                               if_else(str_detect(z, "^0") & nchar(z) != 10, NA_character_, 
                                       if_else(str_detect(z, "^0{3,}"), NA_character_, z)))))
   return(z)
}

#typage des id (paysage, siret, siren...)

id_type <- function(var){
  
  pat <- c(pattern_siren, pattern_siret, pattern_paysage, pattern_rnsr, pattern_uai,
           pattern_assoc, pattern_grid, pattern_finess, pattern_pic, pattern_ror)
  pat_symb <- c("s", "st", "p", "r", "u", "a", "g", "f", "pi", "ro")   

  for (i in seq_along(pat)){
    if (!is.na(var) & str_detect(var, pat[i])==TRUE){
      return(pat_symb[i])
    }
  }
}



# liste de prénoms genrés
gender_datas <- function(){
   perso_gender <- 
      readxl::read_xlsx(paste0(identification, "corrected/_gender.xlsx"), sheet="gender") %>% 
      mutate(gender = str_to_upper(gender))
   
   
   toString(unique(perso_gender$gender))
   g1 <- c("F", "HS", "M", "MALE", "UNKNOWN", "FEMALE")
   
   
   perso_gender %>% 
      dplyr::filter(!(gender %in% c("UNKNOWN", "HS"))) %>% 
      mutate(gender_temp=if_else(str_detect(gender, "^F"), "Female", if_else(str_detect(gender, "^M"), "Male", NA_character_))) %>% 
      select(-gender) %>% 
      unique() 
   
}