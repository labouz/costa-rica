#one year age adjusted incidence and mortaltiy rates 2009-2014
#for all cancers 
#for each sex and both

library(tidyverse)
library(epitools)
library(readxl)
library(lettercase)

#####FUNCTIONS
#function to extract cases and pop at risk by year and sex
getCancer <- function(theYear, theSex) {
  cancer_cases <- read_excel(paste0("data/raw/INCIDENCIA_", theYear, "_DIFERENTES_CARACTERISTICAS.xlsx"),
                             sheet = paste0("LOC. Y GR. EDAD ", theSex),
                             range = "A6:AJ77")
  cancer_cases <- cancer_cases %>%
    #remove all the unadjusted rate columns
    select(-TOTAL,- c(seq(from = 4, to = 36, by = 2)), "CIE" = 'CIE-O-3',
           "site" = "LOCALIZACION") %>%
    filter(!site %in% c(NA)) %>% 
    #give a C# to the Total group
    mutate(CIE = if_else(is.na(CIE) == TRUE, "Cxx", CIE))
  
  #remove the unspecified sites
  cancer_cases2 <- cancer_cases %>% 
    filter(!CIE %in% c("C02","C06","C08","C14","C26","C39","C41","C44","C55",
                        "C57","C63","C68","C75","C76","C77","C78","C79","C80",
                        "C88","C94","C95","C96","C97"))

  #transform from wide to long
  cancer_cases3 <-  gather(cancer_cases2, key = "agegrp", value = "cases", 
                           -site, -CIE) %>%
    mutate(year = theYear, sex = theSex)

}



# getPopAtRisk <- function(theYear, theSex) {
#   #pop at risk
#   popAtRisk <- read_excel(paste0("data/raw/INCIDENCIA_", theYear, "_DIFERENTES_CARACTERISTICAS.xlsx"),
#                           sheet = paste0("10 MAS FREC. GR. EDAD ", theSex),
#                           range = "AQ7:BF8")
#   #transorm to long
#   popAtRisk2 <- gather(popAtRisk, key = "agegrp", value = "pop") %>%
#     mutate(year = theYear, sex = theSex)
# 
# }

args <- expand.grid(theYear = 2009:2014, theSex = c("VARONES", "MUJERES"))

#####Cases and at risk pops
cancerCases_bysex <- map2_dfr(args$theYear, args$theSex, getCancer) %>%
  mutate(cases = as.numeric(cases))
# 
# popAtRisk_bysex <- map2_dfr(args$theYear, args$theSex, getPopAtRisk)


#####Create everyone groups

cancerCases_todos <- cancerCases_bysex %>%
  group_by(CIE, site, agegrp, year) %>%
  summarise(cases = sum(as.numeric(cases))) %>%
  mutate(sex = "TODOS")

#bind to cases
 cancerCases <- bind_rows(cancerCases_bysex, cancerCases_todos) %>% 
   mutate(site = str_ucfirst(str_decapitalize(site)))
# 
# popAtRisk_todos <- popAtRisk_bysex %>% 
#   group_by(agegrp, year) %>% 
#   summarise(pop = sum(pop)) %>% 
#   mutate(sex = "TODOS")

#bind to at risk
# popAtRisk <- bind_rows(popAtRisk_bysex, popAtRisk_todos)
# 
# saveRDS(popAtRisk, "./data/popAtRisk_2009-2014.rds")
# saveRDS(cancerCases, "./data/cancerCases_2009-2014.rds")

cancerCases <- readRDS("./data/cancerCases_2009-2014.rds")
popAtRisk <- readRDS( "./data/popAtRisk_2009-2014.rds")

#standard population
standPop <- readRDS("./data/standPop.rds")


#####Age-adjusted rates

rateArgs <- expand.grid(theCancer = unique(cancerCases$site),
                        theYear = 2009:2014, 
                        theSex = c("VARONES", "MUJERES", "TODOS"),
                        stringsAsFactors = FALSE)

makeRate <- function(theCancer, theYear, theSex){
  
  cancer <- cancerCases %>% 
    filter(site == theCancer,
           year == theYear,
           sex == theSex)
  atRisk <- popAtRisk %>% 
    filter(year == theYear,
           sex == theSex)
  
  c(unlist(round(10^5 * (ageadjust.direct(count = cancer$cases,
                                          pop = atRisk$pop,
                                          stdpop = standPop$`Recalculation to add to 1,000,000`,
                                          conf.level = 0.95)), 2)
           
  ),
  site = theCancer,
  sex = theSex,
  year = theYear,
  count = sum(cancer$cases),
  ratePop = sum(atRisk$pop)
  )
  
}

CR_incRates <- lapply(1:nrow(rateArgs), function(row){
  makeRate(theCancer = rateArgs[row, 1],
           theYear = rateArgs[row, 2],
           theSex = rateArgs[row, 3])
})


CR_incRates_df <- data.frame(matrix(unlist(CR_incRates,
                                           use.names = TRUE),
                                    nrow = 1008, byrow = TRUE), stringsAsFactors = FALSE) %>% 
  mutate_at(vars(X1:X4), funs(as.numeric)) 

names(CR_incRates_df) <- names(CR_incRates[[1]])

#filter rows  n<10
CR_incRates_df <- CR_incRates_df %>% 
  mutate(count = as.numeric(count)) %>% 
  filter(count > 10)

saveRDS(CR_incRates_df, "./data/CR_incRates.rds")
