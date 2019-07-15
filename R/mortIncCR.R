#calculate 4 year mortality rate for the country

library(tidyverse)
library(epitools)

#read cases and popatrisk
mortCases <- readRDS("./data/mortCases_2012-2015.rds")
popAtRisk <- readRDS( "./data/popAtRisk_2009-2015.rds")

#standard population
standPop <- readRDS("./data/standPop.rds")


#combine cases
mortCases2 <- mortCases %>% 
  group_by(CIE, site, agegrp, sex) %>% 
  summarize(cases = sum(cases))

#combine leukemias
leuk <- mortCases2 %>% 
  filter(site %in% c("Leucemia Linfoide", "Leucemia Mieloide")) %>% 
  group_by(agegrp, sex) %>% 
  summarise(cases = sum (cases)) %>% 
  mutate(CIE = "C91", site = "Leucemias")

#bind to data
mortCases3 <- bind_rows(mortCases2, leuk) %>% 
  filter(!site %in% c("Leucemia Linfoide", "Leucemia Mieloide"))


#####Age-adjusted rates

rateArgs <- expand.grid(theCancer = unique(mortCases3$site),
                        theSex = c("VARONES", "MUJERES", "TODOS"),
                        stringsAsFactors = FALSE) %>% 
  arrange(theCancer, theSex)

makeRate <- function(theCancer, theSex){
  
  cancer <- mortCases3 %>% 
    filter(site == theCancer,
           sex == theSex)
  atRisk <- popAtRisk %>% 
    filter(year %in% c(2012,2013,2014,2015),
           sex == theSex) %>% 
    group_by(agegrp) %>% 
    summarise(n = sum(pop))
  
  c(unlist(round(10^5 * (ageadjust.direct(count = cancer$cases,
                                          pop = atRisk$n,
                                          stdpop = standPop$`Recalculation to add to 1,000,000`,
                                          conf.level = 0.95)), 2)
           
  ),
  site = theCancer,
  sex = theSex,
  count = sum(cancer$cases),
  ratePop = round(sum(atRisk$n)/4)
  )
  
}

CR_4yrMortRates <- lapply(1:nrow(rateArgs), function(row){
  makeRate(theCancer = rateArgs[row, 1],
           theSex = rateArgs[row, 2])
})


CR_4yrMortRates_df <- data.frame(matrix(unlist(CR_4yrMortRates,
                                            use.names = TRUE),
                                     nrow = 198, byrow = TRUE), 
                                 stringsAsFactors = FALSE) %>% 
  mutate_at(vars(X1:X4), funs(as.numeric))

names(CR_4yrMortRates_df) <- names(CR_4yrMortRates[[1]])

#filter rows  n<10
CR_4yrMortRates_df <- CR_4yrMortRates_df %>% 
  mutate(count = as.numeric(count)) %>% 
  filter(count > 10)

saveRDS(CR_4yrMortRates_df, "./data/CR_4yrMortRates.rds")
