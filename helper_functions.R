

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

x <- gsub("Präsidenten", "", x)   
x <- gsub("Vizepräsidenten", "", x)   
x <- gsub("die", "", x) 
x <- gsub("den","",  x)  
x <- gsub("und","",  x)  

x <- gsub("Richterinnen", "", x)  
x <- gsub("Richterin", "", x)  
x <- gsub("Richter", "", x)  


x <- gsub(" ", "", x)
x <- gsub("\\,", "", x)
}


# test name extraction
# (bla <- names_extract(as.character(chamber_decisions$judge1 )))
# (bla2 <- names_extract(as.character(chamber_decisions$judge2 )))
# (bla3 <- names_extract(as.character(chamber_decisions$judge3 )))










