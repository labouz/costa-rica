#one year age adjusted incidence and mortaltiy rates 2009-2014
#for 10 cancers 
#for each sec and both

library(tidyverse)
library(epitools)
library(readxl)

#####FUNCTIONS
#function to extract cases and pop at risk by year and sex
getCancer <- function(theYear, theSex) {
  cancer_cases <- read_excel(paste0("data/raw/INCIDENCIA_", theYear, "_DIFERENTES_CARACTERISTICAS.xlsx"),
                             sheet = paste0("10 MAS FREC. GR. EDAD ", theSex),
                             range = "B6:AJ19")
  cancer_cases <- cancer_cases %>% 
    #remove all the unadjusted rate columns
    select(-TOTAL,- c(seq(from = 3, to = 35, by = 2))) %>%
    filter(!LOCALIZACION %in% c("TOTAL", NA))
  
  #transform from wide to long
  cancer_cases2 <-  gather(cancer_cases, key = "agegrp", value = "cases", -LOCALIZACION) %>% 
    mutate(year = theYear, sex = theSex)
  
}

getPopAtRisk <- function(theYear, theSex) {
  #pop at risk
  popAtRisk <- read_excel(paste0("data/raw/INCIDENCIA_", theYear, "_DIFERENTES_CARACTERISTICAS.xlsx"),
                          sheet = paste0("10 MAS FREC. GR. EDAD ", theSex),
                          range = "AQ7:BF8")
  #transorm to long
  popAtRisk2 <- gather(popAtRisk, key = "agegrp", value = "pop") %>% 
    mutate(year = theYear, sex = theSex)

}

args <- expand.grid(theYear = 2009:2014, theSex = c("VARONES", "MUJERES"))

#####Cases and at risk pops
cancerCases_bysex <- map2_dfr(args$theYear, args$theSex, getCancer) %>% 
  mutate(cases = as.numeric(cases))

popAtRisk_bysex <- map2_dfr(args$theYear, args$theSex, getPopAtRisk)


#####Create everyone groups

cancerCases_todos <- cancerCases %>% 
  group_by(LOCALIZACION, agegrp, year) %>% 
  summarise(cases = sum(as.numeric(cases))) %>% 
  mutate(sex = "TODOS")

#bind to cases
cancerCases <- bind_rows(cancerCases_bysex, cancerCases_todos)

popAtRisk_todos <- popAtRisk_bysex %>% 
  group_by(agegrp, year) %>% 
  summarise(pop = sum(pop)) %>% 
  mutate(sex = "TODOS")

#bind to at risk
popAtRisk <- bind_rows(popAtRisk_bysex, popAtRisk_todos)

saveRDS(popAtRisk, "./data/popAtRisk_2009-2014")
saveRDS(cancerCases, "./data/cancerCases_2009-2014")
