#Emily Maloney
#July 6 2019
#Nelson Replication Project
#Reading in Data

#libraries
library(tidyverse)
library(haven)
library(naniar)
library(janitor)

#reading in the different data files
        #Mturk studies:
        nelsonrep <- read_dta("Data/Nelson_Replication - Complete - Cleaned.dta")
        nelsonos <- read_dta("Data/Nelson_Replication - Oversample B&O - Cleaned.dta")
        
        #didn't get a clean file of this from Emi & Brent
        nelsonms <- read_dta("Data/Nelson_Replication - Minus Sentence.dta")
      
        #Lab studies:
        nelsonclean <- read_dta("Data/Nelson_Replication - Cleaned.dta")
        nelsonic <- read_dta("Data/Nelson_Replication - InClass - Complete - Cleaned.dta")
        
        #original Nelson study:
        nelsonog <- read_dta("Data/Nelson_Original.dta")

        #the deflection of the different sentences
        deflection <- readxl::read_xlsx("Data/Nelson Deflections Table.xlsx")

#clean names of the deflection file
deflection <- clean_names(deflection) %>% 
              mutate(Condition = condition,
                     q_num = case_when(str_detect(question, "1") ~ 1,
                                      str_detect(question, "2") ~ 2,
                                      str_detect(question, "3") ~ 3,
                                      str_detect(question, "4") ~ 4,
                                      str_detect(question, "5") ~ 5,
                                      str_detect(question, "6") ~ 6)) %>% 
              select(Condition, question, q_num, event, overall_deflection, actor_deflection,
              behavior_deflection, object_deflection, abo, a_bo, ab_o) 


#function to reshape the data 
new_data_format <- function(d){
        d <- replace_with_na_all(data = d, condition = ~.x %in% c("."))
        
        d_new <- d %>% pivot_longer(names_to = "question",
                              values_to = "replaced",
                              cols = P2C1Q1A:P2C6Q6O) %>% 
          select(MTurkCode, EndTime, Attention, question, replaced, Condition,
                 idnum, question, replaced, Age, female, RBlack, RWhite, 
                 RHispanic, RNative, RAsian, RPacific, ROther, Educ,
                 Inc, Home1, Home2, Spiritual, UrbRur, Region) %>% 
          mutate(condq = case_when(str_detect(question, "C1") ~ 1,
                                   str_detect(question, "C2") ~ 2,
                                   str_detect(question, "C3") ~ 3,
                                   str_detect(question, "C4") ~ 4,
                                   str_detect(question, "C5") ~ 5,
                                   str_detect(question, "C6") ~ 6),
                 element_replaced = case_when(str_detect(question, "A") ~ "A",
                                              str_detect(question, "B") ~ "B",
                                              str_detect(question, "O") ~ "O"),
                 q_num = case_when(str_detect(question, "Q1") ~ 1,
                                   str_detect(question, "Q2") ~ 2,
                                   str_detect(question, "Q3") ~ 3,
                                   str_detect(question, "Q4") ~ 4,
                                   str_detect(question, "Q5") ~ 5,
                                   str_detect(question, "Q6") ~ 6)) %>% 
          filter(Condition == condq & !is.na(replaced))
      
        return(d_new)
}

nelsonrep_long <- new_data_format(nelsonrep)
nelsonos_long <- new_data_format(nelsonos)


#merge deflection information
nelsonrep1 <- left_join(nelsonrep1, deflection, by = c("Condition", "question"))
nelsonrep2 <- left_join(nelsonrep2, deflection, by = c("Condition", "question"))
nelsonrep3 <- left_join(nelsonrep3, deflection, by = c("Condition", "question"))
nelsonrep4 <- left_join(nelsonrep4, deflection, by = c("Condition", "question"))
nelsonrep5 <- left_join(nelsonrep5, deflection, by = c("Condition", "question"))
nelsonrep6 <- left_join(nelsonrep6, deflection, by = c("Condition", "question"))

#join all together
nelsonrep_long <- rbind(nelsonrep1, nelsonrep2, nelsonrep3, nelsonrep4, nelsonrep5, nelsonrep6)

#save final data
write.csv(nelsonrep_long, "nelsonrep_em.csv")
