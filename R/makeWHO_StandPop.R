#make WHO standard pop
library(tidyverse)


whoStandPop <- read_xlsx("./data/raw/whoStandardPop.xlsx", range = "Sheet1!A1:C23")

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

saveRDS(whoShortPop, "./data/standPop.rds")
