read_cancer <- function(theYear, theSex, theRangeLower="A8", theRangeUpper="AJ77"){
  x <- read_xlsx(paste0("./Data/INCIDENCIA_", theYear, 
                        "_DIFERENTES_CARACTERISTICAS.xlsx"), 
                 range = paste0("LOC. Y GR. EDAD ", substitute(theSex), "!", 
                                substitute(theRangeLower), ":", 
                                substitute(theRangeUpper)), 
                 col_names = FALSE)
  cbind(year = theYear, x)
}



# cancerMales2009  <-  read_cancer(2009, VARONES)

# cancerMales2013  <-  read_cancer(2013, VARONES, A9, AJ78)


