#one year age adjusted incidence and mortaltiy rates 2009-2014
#for all cancers 
#for each sex and both

library(tidyverse)
library(epitools)
library(readxl)
library(lettercase)

#####FUNCTIONS
#function to extract cases and pop at risk by year and sex
getMort <- function(theYear, theSex) {
  mort_cases <- read_excel(paste0("data/raw/Mortality/MORT.", theSex, " POR LOCALIZ.Y GRUP.EDAD ", theYear,".xls"),
                             sheet = "Número", range = "A1:T124")

  mort_cases <- mort_cases %>%
    select(-TOTAL, -'...3', "site" = '...2',"CIE" = "LOCALIZACION") %>%
    filter(!site %in% c(NA)) %>% 
    #give a C# to the Total group
    mutate(CIE = if_else(CIE == "CIE-10", "Cxx", CIE))
  
  #remove the unspecified sites
  mort_cases2 <- mort_cases %>% 
    filter(!CIE %in% c("C02","C06","C08","C14","C26","C39","C41","C44","C55",
                       "C57","C63","C68","C75","C76","C77","C78","C79","C80",
                       "C88","C94","C95","C96","C97"))
  
  #the names of several cancers are spread on multiple rows: collapsing them
  mort_cases2 <- mort_cases2 %>% 
    group_by(CIE) %>% 
    mutate(site = str_c(site, collapse = " ")) %>%
    #omitting the duplicate rows of NA
    na.omit('0-4') 

  #transform from wide to long
  mort_cases3 <-  gather(mort_cases2, key = "agegrp", value = "cases", 
                         -site, -CIE) %>%
    mutate(year = theYear, sex = theSex)

}

#add 2015 to popatRisk data
#popAtRisk <- readRDS("./data/popAtRisk_2009-2014.rds")


#pop at risk FEMALES
# femAtRisk2015 <- read_excel(paste0("data/raw/Mortality/MORT.MUJERES POR LOCALIZ.Y GRUP.EDAD 2015.xls"),
#                             sheet = "Número y tasa",
#                             range = "AS8:BH10")
# femAtRisk2015 <- femAtRisk2015[-1,]
# #transorm to long
# femAtRisk2015_long <- gather(femAtRisk2015, key = "agegrp", value = "pop") %>%
#   mutate(year = 2015, sex = "MUJERES")

#pop at risk MALES
# maleAtRisk2015 <- read_excel(paste0("data/raw/Mortality/MORT.HOMBRES POR LOCALIZ.Y GRUP.EDAD 2015.xls"),
#                              sheet = "Número y tasa",
#                              range = "AT8:BI10")
# maleAtRisk2015 <- maleAtRisk2015[-1,]
# #transorm to long
# maleAtRisk2015_long <- gather(maleAtRisk2015, key = "agegrp", value = "pop") %>%
#   mutate(year = 2015, sex = "VARONES")
#   
#bind males and females and make everyone group
# todoAtRisk2015 <- bind_rows(femAtRisk2015_long, maleAtRisk2015_long) %>% 
#   group_by(agegrp, year) %>%
#   summarise(pop = sum(pop)) %>%
#   mutate(sex = "TODOS")

#bind 2015 to rest of years popatrsik data
# popAtRisk <- bind_rows(popAtRisk, maleAtRisk2015_long, femAtRisk2015_long, todoAtRisk2015) %>% 
#   mutate(agegrp = if_else(agegrp == "75 +", "75+", agegrp))

#save new popatrsik data
#saveRDS(popAtRisk, "./data/popAtRisk_2009-2015.rds")


args <- expand.grid(theYear = 2012:2015, theSex = c("HOMBRES", "MUJERES"))

#####Cases and at risk pops
mortCases_bysex <- map2_dfr(args$theYear, args$theSex, getMort) %>%
  mutate(cases = as.numeric(cases)) %>% 
  mutate(agegrp = if_else(agegrp == "75 +", "75+", agegrp))


#####Create everyone groups

mortCases_todos <- mortCases_bysex %>%
  group_by(CIE,site, agegrp, year) %>%
  summarise(cases = sum(as.numeric(cases))) %>%
  mutate(sex = "TODOS")

#bind to cases
mortCases <- bind_rows(mortCases_bysex, mortCases_todos) %>% 
  mutate(sex = if_else(sex == "HOMBRES", "VARONES", sex))%>% 
  mutate(site = str_ucfirst(str_decapitalize(site)))

#saveRDS(mortCases, "./data/mortCases_2012-2015.rds")

mortCases <- readRDS("./data/mortCases_2012-2015.rds")
popAtRisk <- readRDS( "./data/popAtRisk_2009-2015.rds")

#standard population
standPop <- readRDS("./data/standPop.rds")


#####Age-adjusted rates

rateArgs <- expand.grid(theCancer = unique(mortCases$site),
                        theYear = 2012:2015, 
                        theSex = c("VARONES", "MUJERES", "TODOS"),
                        stringsAsFactors = FALSE)

makeRate <- function(theCancer, theYear, theSex){
  
  cancer <- mortCases %>% 
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

CR_mortRates <- lapply(1:nrow(rateArgs), function(row){
  makeRate(theCancer = rateArgs[row, 1],
           theYear = rateArgs[row, 2],
           theSex = rateArgs[row, 3])
})


CR_mortRates_df <- data.frame(matrix(unlist(CR_mortRates,
                                           use.names = TRUE),
                                    nrow = 804, byrow = TRUE), stringsAsFactors = FALSE) %>% 
  mutate_at(vars(X1:X4), funs(as.numeric))

names(CR_mortRates_df) <- names(CR_mortRates[[1]])

#filter rows  n<10
CR_mortRates_df <- CR_mortRates_df %>% 
  mutate(count = as.numeric(count)) %>% 
  filter(count > 10)

saveRDS(CR_mortRates_df, "./data/CR_mortRates.rds")
