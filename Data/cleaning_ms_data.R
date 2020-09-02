#libraries
library(tidyverse)
library(haven)
library(naniar)
library(janitor)

#read in the data
nelsonms <- read_dta("Data/Nelson_Replication - Minus Sentence.dta")

#fix variable names
nelsonms_clean <- nelsonms %>% 
                  mutate(idnum = V1,
                         MTurkCode = mTurkCode,
                         StartTime = V8,
                         EndTime = V9,
                         InstrP1 = Q3,
                         ExampleP1 = Q4,
                         P2C1Q1A = Q14_1_1_TEXT,
                         P2C1Q1B = Q14_1_2_TEXT,
                         P2C1Q1O = Q14_1_3_TEXT,
                         P2C1Q2A = Q15_1_1_TEXT,
                         P2C1Q2B = Q15_1_2_TEXT,
                         P2C1Q2O = Q15_1_3_TEXT,
                         P2C1Q3A = Q16_1_1_TEXT,
                         P2C1Q3B = Q16_1_2_TEXT,
                         P2C1Q3O = Q16_1_3_TEXT,
                         P2C1Q4A = Q17_1_1_TEXT,
                         P2C1Q4B = Q17_1_2_TEXT,
                         P2C1Q4O = Q17_1_3_TEXT,
                         P2C1Q5A = Q18_1_1_TEXT,
                         P2C1Q5B = Q18_1_2_TEXT,
                         P2C1Q5O = Q18_1_3_TEXT,
                         P2C1Q6A = Q19_1_1_TEXT,
                         P2C1Q6B = Q19_1_3_TEXT,
                         P2C2Q1A = Q20_1_1_TEXT,
                         P2C2Q1B = Q20_1_2_TEXT,
                         P2C2Q1O = Q20_1_3_TEXT,
                         P2C2Q2A = Q21_1_1_TEXT,
                         P2C2Q2B = Q21_1_2_TEXT,
                         P2C2Q2O = Q21_1_3_TEXT,
                         P2C2Q3A = Q22_1_1_TEXT,
                         P2C2Q3B = Q22_1_2_TEXT,
                         P2C2Q3O = Q22_1_3_TEXT, 
                         P2C2Q4A = Q23_1_1_TEXT,
                         P2C2Q4B = Q23_1_2_TEXT,
                         P2C2Q4O = Q23_1_3_TEXT,
                         P2C2Q5A = Q24_1_1_TEXT,
                         P2C2Q5B = Q24_1_2_TEXT,
                         P2C2Q5O = Q24_1_3_TEXT,
                         P2C2Q6A = Q25_1_1_TEXT,
                         P2C2Q6B = Q25_1_2_TEXT,
                         P2C2Q6O = Q25_1_3_TEXT,
                         P2C3Q1A = Q26_1_1_TEXT,
                         P2C3Q1B = Q26_1_2_TEXT,
                         P2C3Q1O = Q26_1_3_TEXT,
                         P2C3Q2A = Q27_1_1_TEXT,
                         P2C3Q2B = Q27_1_2_TEXT,
                         P2C3Q2O = Q27_1_3_TEXT,
                         P2C3Q3A = Q28_1_1_TEXT,
                         P2C3Q3B = Q28_1_2_TEXT,
                         P2C3Q3O = Q28_1_3_TEXT,
                         P2C3Q4A = Q29_1_1_TEXT,
                         P2C3Q4B = Q29_1_2_TEXT,
                         P2C3Q4O = Q29_1_3_TEXT,
                         P2C3Q5A = Q30_1_1_TEXT,
                         P2C3Q5B = Q30_1_2_TEXT,
                         P2C3Q5O = Q30_1_3_TEXT,
                         P2C3Q6A = Q31_1_1_TEXT, 
                         P2C3Q6B = Q31_1_2_TEXT,
                         P2C3Q6O = Q31_1_3_TEXT,
                         P2C4Q1A = Q32_1_1_TEXT,
                         P2C4Q1B = Q32_1_2_TEXT, 
                         P2C4Q1O = Q32_1_3_TEXT,
                         P2C4Q2A = Q33_1_1_TEXT,
                         P2C4Q2B = Q33_1_2_TEXT,
                         P2C4Q2O = Q33_1_3_TEXT,
                         P2C4Q3A = Q34_1_1_TEXT,
                         P2C4Q3B = Q34_1_2_TEXT,
                         P2C4Q3O = Q34_1_3_TEXT,
                         P2C4Q4A = Q35_1_1_TEXT,
                         P2C4Q4B = Q35_1_2_TEXT,
                         P2C4Q4O = Q35_1_3_TEXT,
                         P2C4Q5A = Q36_1_1_TEXT,
                         P2C4Q5B = Q36_1_2_TEXT,
                         P2C4Q5O = Q36_1_3_TEXT,
                         P2C4Q6A = Q37_1_1_TEXT,
                         P2C4Q6B = Q37_1_2_TEXT,
                         P2C4Q6O = Q37_1_3_TEXT,
                         P2C5Q1A = Q38_1_1_TEXT,
                         P2C5Q1B = Q38_1_2_TEXT,
                         P2C5Q1O = Q38_1_3_TEXT,
                         P2C5Q2A = Q39_1_1_TEXT,
                         P2C5Q2B = Q39_1_2_TEXT, 
                         P2C5Q2O = Q39_1_3_TEXT,
                         P2C5Q3A = Q40_1_1_TEXT,
                         P2C5Q3B = Q40_1_2_TEXT,
                         P2C5Q3O = Q40_1_3_TEXT, 
                         P2C5Q4A = Q41_1_1_TEXT, 
                         P2C5Q4B = Q41_1_2_TEXT,
                         P2C5Q4O = Q41_1_3_TEXT,
                         P2C5Q5A = Q42_1_1_TEXT,
                         P2C5Q5B = Q42_1_2_TEXT,
                         P2C5Q5O = Q42_1_3_TEXT,
                         P2C5Q6A = Q43_1_1_TEXT,
                         P2C5Q6B = Q43_1_2_TEXT, 
                         P2C5Q6O = Q43_1_3_TEXT,
                         P2C6Q1A = Q44_1_1_TEXT, 
                         P2C6Q1B = Q44_1_2_TEXT,
                         P2C6Q1O = Q44_1_3_TEXT,
                         P2C6Q2A = Q45_1_1_TEXT, 
                         P2C6Q2B = Q45_1_2_TEXT, 
                         P2C6Q2O = Q45_1_3_TEXT,
                         P2C6Q3A = Q46_1_1_TEXT,
                         P2C6Q3B = Q46_1_2_TEXT,
                         P2C6Q3O = Q46_1_3_TEXT, 
                         P2C6Q4A = Q47_1_1_TEXT, 
                         P2C6Q4B = Q47_1_2_TEXT, 
                         P2C6Q4O = Q47_1_3_TEXT, 
                         P2C6Q5A = Q48_1_1_TEXT, 
                         P2C6Q5B = Q48_1_2_TEXT, 
                         P2C6Q5O = Q48_1_3_TEXT, 
                         P2C6Q6A = Q49_1_1_TEXT, 
                         P2C6Q6B = Q49_1_2_TEXT, 
                         P2C6Q6O = Q49_1_3_TEXT, 
                         female = if_else(Q51 == 2, 1, 0),
                         age = Q52,
                         RBlack = Q53_1, 
                         RWhite = Q53_2, 
                         RHispanic = Q53_3,
                         RNative = Q53_4, 
                         RAsian = Q53_5, 
                         RPacific = Q53_6, 
                         ROther = Q53_7, 
                         Student = if_else(Q54 == 1, 1, 0),
                         Educ = Q55, 
                         Income = Q56,
                         Home1 = Q57, 
                         Home2 = Q58, 
                         Spiritual = Q59, 
                         UrbRur = Q60,
                         Region = Q61, 
                         InstrDebrief = Q63, 
                         Attention = Q112) %>% 
                  select(c(idnum:Attention, MTurkCode))

nelsonms_long <- nelsonms_clean %>% pivot_longer(names_to = "question",
                                             values_to = "replaced",
                                             cols = P2C1Q1A:P2C6Q6O) %>% 
                 filter(replaced != "") %>% 
                 mutate(Condition = case_when(str_detect(question, "C1") ~ 1,
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
                                          str_detect(question, "Q6") ~ 6))

#remove people who answered the attention checks incorrectly
#list of people who didn't follow directions
bad_ids <- c(2396644, 5745850, 8804486, 571458, 4421702, 8158025,
                   9218801, 5743504, 8153549, 2092680)

nelsonms_long <- nelsonms_long %>% 
                  filter(Attention == 2) %>% 
                  filter(!(MTurkCode %in% bad_ids))

saveRDS(nelsonms_clean, "Data/nelsonms_clean.RDS")

nelsonms_long <- left_join(nelsonms_long, deflection, by = c("Condition", "q_num"))

nelsonms_byid <- nelsonms_long %>% 
                 group_by(MTurkCode) %>% 
                 summarise(answered = n())

nelsonms_


