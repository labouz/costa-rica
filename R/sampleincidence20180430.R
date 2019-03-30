#setwd("c:/Users/rrb28.CGCENT/Box Sync/Global Oncology")
load("./myWorkspace.RData")

library(epitools)
library (tidyverse)
library(popEpi)

str(whoStandPop)
unique(whoStandPop$`Age Group`)

#collapses the age groups over 75 from whoStandPop
over75 <- whoStandPop %>% 
  filter(`Age Group` %in% c("75-79","80-84","85-89","90-94","95-99","100+")) %>% 
  select(-`Age Group`) %>% #select - equals removing age group column
  summarise_all(funs(sum)) %>% #funs = tells which function we're going to use to summrize - can be multiple functions
  bind_cols(data.frame("AgeGroup" = "75+"), .) %>%  #period indicates where the argument will be piped into
  mutate(AgeGroup = as.character(AgeGroup)) #overwrite age group column from a factor to a character column

#binds the row created above to the original agebins <75 and assigns it to whoShortPop
whoShortPop <- whoStandPop %>% 
  filter(!(`Age Group` %in% 
             c("75-79","80-84","85-89","90-94","95-99","100+","Total"))) %>% 
  rename(AgeGroup =`Age Group`) %>% #rename is the dplyr version of colnames(x)<-"char"
  bind_rows(over75)





# cancerCount <- cancerFemales2009_2014
# cancerName <-  "COLON-RECTO"
# countAtRisk <- atRiskFemales2009_2014


makeRate <- function(cancerCount, cancerName, countAtRisk){
  
 # browser()
  #subset one cancer and just counts and transpose
  site <- subset(cancerCount, 
                 X__2 == cancerName, # & year == 2009, 
                 select= c(g0_4,g5_9,g10_14,g15_19,g20_24,g25_29,g30_34,g35_39,
                           g40_44,g45_49,g50_54,g55_59,g60_64,g65_69,g70_74,g75)) %>% 
    add_column(year = 2009:2014) 
  
  site_df <- gather(site, agegrp, count, c(g0_4,g5_9,g10_14,g15_19,g20_24,g25_29,g30_34,g35_39,
                                  g40_44,g45_49,g50_54,g55_59,g60_64,g65_69,g70_74,g75))
  
  # site_df <- data.frame(X2 = names(site), cancer = t(site[,]))
  colnames(site_df) <- c("Year","AgeGroup", "Cancer") #colnames grabs the column names of a dataframe and with assignment will allow you to rename them - this is better when you want to rename all at once
  rownames(site_df) <- NULL #whatevers here, remove it -- rownames grabs the rownames and assigns it to null -- renames all at once
  
  # Collapse the years into a single cancer count per age bin
  site_df <- site_df %>% 
    group_by(AgeGroup) %>% 
    summarise(Cancer = sum(Cancer))
  
  # this is so wrong
  # #collapse who age group to end with 75+
  # whostandpop2 <- if (whoStandPop$`Age Group` %in% c("75-79","80-84","85-89","90-94","95-99","100+")) {whoStandPop$`Age Group` == "75+"}
  
  atrisk <- subset(countAtRisk, 
                   select= c(g0_4,g5_9,g10_14,g15_19,g20_24,g25_29,g30_34,
                             g35_39,g40_44,g45_49,g50_54,g55_59,g60_64,g65_69,g70_74,g75)) %>% 
    add_column(year = 2009:2014)
  
  #females_df <- data.frame(X2 = names(femalesatrisk), atRisk = t(femalesatrisk[1,]))
  atrisk_df <- gather(atrisk, agegrp, atrisk, c(g0_4,g5_9,g10_14,g15_19,g20_24,g25_29,g30_34,g35_39,
                                      g40_44,g45_49,g50_54,g55_59,g60_64,g65_69,g70_74,g75))
  
  colnames(atrisk_df) <- c("Year", "AgeGroup", "atRisk") #colnames grabs the column names of a dataframe and with assignment will allow you to rename them - this is better when you want to rename all at once
  rownames(atrisk_df) <- NULL #whatevers here, remove it -- rownames grabs the rownames and assigns it to null -- renames all at once
  
  
  # Collapse the years into a single cancer count per age bin
  atrisk_df <- atrisk_df %>% 
    group_by(AgeGroup) %>% 
    summarise(atRisk = sum(atRisk))
  
  library(stringr)
  # #removing the initial g
  # str_sub(site_df$AgeGroup,1,1) <- ""
  # str_sub(females_df$AgeGroup,1,1) <- ""
  # #replacing the _ for -
  # str_replace(site_df$AgeGroup,"_","-")
  
  site_df <- site_df %>% 
    mutate(AgeGroup = str_replace(AgeGroup,"g","")) %>%  #replace any g with nothing
    mutate(AgeGroup = str_replace(AgeGroup,"_","-")) #replace any _ with -
  
  site_df[nrow(site_df), "AgeGroup"] <- "75+" #nrow returns the number of rows-- replace the last row in the agegroup column to 75+
  
  #this chunk is the same as above but uses %<>%:
  # site_df %<>% 
  #   mutate(AgeGroup = str_replace(AgeGroup,"g","")) %>% 
  #   mutate(AgeGroup = str_replace(AgeGroup,"_","-"))
  
  atrisk_df <- atrisk_df %>% 
    mutate(AgeGroup = str_replace(AgeGroup,"g","")) %>%  #replace any g with nothing
    mutate(AgeGroup = str_replace(AgeGroup,"_","-")) #replace any _ with -
  
  atrisk_df[nrow(atrisk_df), "AgeGroup"] <- "75+" 
  
  
  #joining cancer counts, atrisk, and who to one table:
  CR_site <- inner_join(site_df,atrisk_df, by = "AgeGroup")
  CR_site <- inner_join(CR_site,whoShortPop, by = "AgeGroup") %>%
    add_column(site = cancerName)
  
  

  # site_inc <- round(10^5 * (ageadjust.direct(CR_site$Cancer,
  #                                            CR_site$atRisk,
  #                                            stdpop = CR_site$`Recalculation to add to 1,000,000`, 
  #                                            conf.level = 0.95)), 2)
  
  # return(c(cancerName, site_inc))
}




makeRates <- function(theCancer) {
  
  theCancerFemale <- makeRate(cancerFemales2009_2014, theCancer, atRiskFemales2009_2014)
  print(paste(theCancer, "Females"))
  female <- 
  round(10^5 * (ageadjust.direct(theCancerFemale$Cancer,
                                 theCancerFemale$atRisk,
                                 stdpop = theCancerFemale$`Recalculation to add to 1,000,000`,
                                 conf.level = 0.95)), 2)
  print(female)
  
  
  theCancerMale <- makeRate(cancerMales2009_2014, theCancer, atRiskMales2009_2014)
  print(paste(theCancer, "Males"))
  male <- 
  round(10^5 * (ageadjust.direct(theCancerMale$Cancer,
                                       theCancerMale$atRisk,
                                 stdpop = theCancerMale$`Recalculation to add to 1,000,000`,
                                 conf.level = 0.95)), 2)
  print(male)
  
  theCancerBoth <- rbind(theCancerFemale, theCancerMale)
  theCancerBoth %>% 
    group_by(AgeGroup) %>% 
    summarise(cancer = sum(Cancer), atRisk = sum(atRisk), stdpop = sum(`Recalculation to add to 1,000,000`)/2)
  
  
  print(paste(theCancer, "People"))
  both <- 
      round(10^5 * (ageadjust.direct(theCancerBoth$Cancer,
                                   theCancerBoth$atRisk,
                                   stdpop = theCancerBoth$`Recalculation to add to 1,000,000`,
                                   conf.level = 0.95)), 2)
  print(both)
  
  list(cancer = theCancer, female = female, male = male, both = both)

}




#standardized incidence ratio 

makeSIR <- function(theCancer) {
  
  theCancerFemale <- makeRate(cancerFemales2009_2014, theCancer, atRiskFemales2009_2014)
  theCancerMale <- makeRate(cancerMales2009_2014, theCancer, atRiskMales2009_2014)

  
  theCancer_df <- rbind(theCancerFemale, theCancerMale)
  theCancer_df %>% 
    group_by(AgeGroup) %>% 
    summarise(cancer = sum(Cancer), atRisk = sum(atRisk), stdpop = sum(`Recalculation to add to 1,000,000`)/2)
  
  # print(paste(theCancer, "SIR - Female Ref"))
  # 
  # print(
  #   sir(coh.data = thyroidMale, coh.obs = "Cancer", coh.pyrs = "atRisk", 
  #       ref.data = thyroidFemale, ref.obs = "Cancer", ref.pyrs = "atRisk",
  #       adjust = "AgeGroup")
  # )
  
  print(paste(theCancer, "SIR - Male Ref"))
  
  print(
    sir(coh.data = theCancerFemale, coh.obs = "Cancer", coh.pyrs = "atRisk", 
        ref.data = theCancerMale, ref.obs = "Cancer", ref.pyrs = "atRisk",
        adjust = "AgeGroup")
  )
}


# makeRates("GLANDULA TIROIDES")

stuff <- map( unique(cancerFemales2009_2014$X__2)[c(-69)], makeRates)
rateDF  <- data.frame(matrix(unlist(stuff, use.names = TRUE),
                             nrow = 50, byrow = TRUE), stringsAsFactors = FALSE)

names(rateDF) <- c("Cancer", "Female crude", "Female adjusted", "Female LCI", "Female UCI",
                 "Male crude", "Male adjusted", "Male LCI", "Male UCI",
                 "People crude", "People adjusted", "People LCI", "People UCI")

#convert factor variables to numeric for app
rateDF[,2:13] <- lapply(rateDF[,2:13], as.numeric)



#3/30/19 adjusting cancer names to be consistent with cancer names in canton files
rateDF <- rateDF %>% 
  mutate(Cancer = case_when(Cancer == "COLON-RECTO" ~ "COLON",
                            Cancer == "GLANDULA TIROIDES" ~ "TIROIDES",
                            Cancer == "CUELLO UTERINO" ~ "CUELLO DEL UTERO",
                            Cancer == "CUERPO UTERINO" ~ "CUERPO DEL UTERO",
                            Cancer == "GANGLIOS LINFATICOS" ~ "GANGLIOS LINF.",
                            Cancer == "GLANDULA PROSTATICA" ~ "PROSTATA",
                            Cancer == "VEJIGA URINARIA" ~ "VEJIGA",
                            Cancer == "SISTEMAS HEMATOPOYETICO Y RETICULOENDOTELIAL" ~ "Y RETICULOEND.",
                            Cancer == "COLON-RECTO" ~ "RECTO",
                            TRUE ~ Cancer
                            ))

saveRDS(rateDF, file = "Data/rateDF.Rda")

#stuff <- map( unique(cancerFemales2009_2014$X__2)[c(-69)], makeSIR)

#as.data.frame(matrix(unlist(stuff), nrow = 4, byrow=TRUE))

# colon <-   makeRate(cancerFemales2009_2014, "COLON", atRiskFemales2009_2014)


# females <- 
# pmap(list(cancerCount=list(cancerFemales2009_2014), 
#           cancerName=list("GLANDULA TIROIDES", "COLON"), 
#           countAtRisk=list(atRiskFemales2009_2014)),
#      makeRate)
# 
# 
# for (i in 1:length(females)){
#   print(
#     round(10^5 * (ageadjust.direct(females[[i]]$Cancer,
#                                females[[i]]$atRisk,
#                                stdpop = females[[i]]$`Recalculation to add to 1,000,000`, 
#                                conf.level = 0.95)), 2)
# )
# }



#standardization


#for RR
# sir(coh.data = ovary_MD, coh.obs = "events", coh.pyrs = "at_risk", 
#     ref.data = ovary_USA, ref.obs = "events", ref.pyrs = "at_risk",
#     adjust = "age_group")
