

#extract day month year

jahr.to.date <- function(x){
  x <- str_replace(x, "(^[0-9]{4}$)", "01/01/\\1")
  x <- str_replace(x, "ab ", "")
  x <- str_replace(x, " ", "")
  x <- as.Date(x, "%d/%m/%Y")
}

#convert German date to dmy

german_date_to_date <- function(x){


x <- str_replace(x, "Januar", "01.")
x <- str_replace(x, "Februar", "02.")
x <- str_replace(x, "März", "03.")
x <- str_replace(x, "April", "04.")
x <- str_replace(x, "Mai", "05.")
x <- str_replace(x, "Juni", "06.")
x <- str_replace(x, "Juli", "07.")
x <- str_replace(x, "August", "08.")
x <- str_replace(x, "September", "09.")
x <- str_replace(x, "Oktober", "10.")
x <- str_replace(x, "November", "11.")
x <- str_replace(x, "Dezember", "12.")

x <- gsub(" ", "", x)
x <- gsub("\\.", "/", x)

x <- as.Date(x, "%d/%m/%Y")

}


names_extract <- function(x){

x <- gsub("Präsidentin", "", x)   
x <- gsub("Präsidenten", "", x)   
x <- gsub("Vizepräsidenten", "", x)   
x <- gsub("die", "", x) 
x <- gsub("den","",  x)  
x <- gsub("und","",  x)  

x <- gsub("Richterinnen", "", x)  
x <- gsub("Richterin", "", x)  
x <- gsub("Richter", "", x) 

x <- gsub("gerichtsdurch", "", x) 
x <- gsub("durch", "", x)  


x <- gsub(" ", "", x)
x <- gsub("\\,", "", x)
}

#clean more:

#manual fixing:

clean_names2 <- function(x){
  
  
  x <- gsub("derLübbe-Wolff", "Lübbe-Wolff", x)
  x <- gsub("derOsterloh", "Osterloh", x)
  
  x <- gsub("dessMellinghoff", "Mellinghoff", x)
  x <- gsub("enBroß", "Broß", x)
  x <- gsub("enHassemer", "Hassemer", x)
  x <- gsub("Gerhard", "Gerhardt", x)
  x <- gsub("Gerhard", "Gerhardt", x)
  x <- gsub("gerichtsBryde", "Bryde", x)
  x <- gsub("HaasHömigBryde", "Haas,Hömig,Bryde", x)
  x <- gsub("ndBritz", "Britz", x)
  x <- gsub("ndBaer", "Baer", x)

  x <- gsub("gerichtsBryde", "Bryde", x)
  
  x <- gsub("Hassemer,Osterloh,MellinghoffS.,1473)am30.,Oktober", "Hassemer,Osterloh,Mellinghoff", x)
  x <- gsub("HassemerBroßMellinghoff", "Hassemer,Broß,Mellinghoff", x)
  x <- gsub("JaegerHömigBryde", "Jaeger,Hömig,Bryde", x)
  x <- gsub("JentschBroßLübbe-Wolff", "Jentsch,Broß,Lübbe-Wolff", x)
  
  x <- gsub("JentschBroßLübbe-Wolff", "Jentsch,Broß,Lübbe-Wolff", x)
  
  x <- gsub("KirchhofMasingBaer", "Kirchhof,Masing,Baer", x)
  x <- gsub("Lübbe-Wollf", "Lübbe-Wolff", x)
  
  x <- gsub("Gerhardttt", "Gerhardt", x)
  x <- gsub("Gerhardtt", "Gerhardt", x)
  x <- gsub("inHohmann-Dennhardt", "Hohmann-Dennhardt", x)
  x <- gsub("PapierSteinerHoffmann-Riem", "Papier,Steiner,Hoffmann-Riem", x)
  
  x <- gsub("PräsitinLimbachHassemerMellinghoff", "Limbach,Hassemer,Mellinghoff", x)
  x <- gsub("ndBritz", "Britz", x)
  
  
  x <- gsub("PräsitinLimbach", "Limbach", x)
  x <- gsub("RicherSommer", "Sommer", x)
  
  x <- gsub("SommerBroßMellinghoff", "Sommer,Broß,Mellinghoff", x)
  x <- gsub("SommerDiFabioLübbe-Wolff", "Sommer,DiFabio,Lübbe-Wolff", x)
  x <- gsub("LimbachKruisWinter", "Limbach,Kruis,Winter", x)
  
  x <- gsub("Kirchhof,Masing,Baer,Kirchhof,Masing", "Kirchhof,Masing,Baer", x)


  x <- gsub("VoßkuhleGerhardtLandau", "Voßkuhle,Gerhardt,Landau", x)
 
  
  x <- gsub("VizepräsitPapier", "Papier", x)
  
 
  
  x <- gsub(",,", ",", x)
  
  
  
}


# split richter strings
richter.split <- function(x){
  x <- str_trim(str_replace_all(x, "\\.|,|;", " "))
  x <- str_split(x, " +")
  return(x)
}

clean.names <- function(x){
  x <- str_replace_all(x, "\\s", " ")
  x <- str_replace_all(x, "Di Fabio", "DiFabio")
  x <- str_replace_all(x, "Vossk", "Voßk")
  x <- str_replace_all(x, "Hasseme( |,|$)", "Hassemer ")
  x <- str_replace_all(x, "- ", "-")
  x <- str_replace_all(x, "Mellinghofff", "Mellinghoff")
  x <- str_replace_all(x, "Kuehling", "Kühling")
  x <- str_replace_all(x, "Bross", "Broß")
  x <- str_replace_all(x, "Grasshof", "Graßhof")
  x <- str_replace_all(x, "Gerhard( |,|$)", "Gerhardt ")
  x <- str_replace_all(x, "Hoemig", "Hömig")
  x <- str_replace_all(x, "Lübbe-Woff", "Lübbe-Wolff")
  x <- str_replace_all(x, "Kessal-Wulff", "Kessal-Wulf")
  x <- str_replace_all(x, "Steine ", "Steiner")
  x <- str_replace_all(x, "Hohmann-Dennhard ", "Hohmann-Dennhardt ")
  x <- str_replace_all(x, "Judge.?:", "")
  
  return(x)
}



# test name extraction
# (bla <- names_extract(as.character(chamber_decisions$judge1 )))
# (bla2 <- names_extract(as.character(chamber_decisions$judge2 )))
# (bla3 <- names_extract(as.character(chamber_decisions$judge3 )))










