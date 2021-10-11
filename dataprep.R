## clean NCES school level data to be used for SAS national norming code ### 
library(tidyverse)
options("scipen"=100, "digits"=4)
rm(list=ls())
data_path<- "C:\\Other\\Presentation\\data\\"

## table number = 170545

NCES_sch <- readxl::read_excel(paste(data_path, "NCES\\ELSI_excel_export_6375702093736198749192", ".xlsx", sep=""))

colnames(NCES_sch) <-c("School_Name", 
                       "State_Name",  
                       "COMBOKEY", 
                       "School_Level", 
                       "flunch",
                       "rlunch", 
                       
                       "total_student", 
                       "a_native",
                       "as",
                       "hi",
                       "bl",
                       "wh",
                       "native", 
                       "two_race",
                       
                       "a_native_male",
                       "a_native_female",
                       "as_male",
                       "as_female",
                       "hi_male",
                       "hi_female",
                       "bl_male",
                       "bl_female",
                       "wh_male",
                       "wh_female",
                       "native_male", 
                       "native_female", 
                       "tworace_male",
                       "tworace_female",
                       
                       "G1G8",
                       "G9G12",
                       "eth_total",
                       
                       "male",
                       "female")

#Northeast#
region1<- str_to_title(c("Maine","New Hampshire","Vermont","Massachusetts", "Rhode Island","Connecticut",
                         "New York","New Jersey","Pennsylvania"))
#Midwest#
region2<- str_to_title(c("Ohio","Indiana","Illinois","Michigan", "Wisconsin",
                         "Minnesota","Iowa","Missouri","North Dakota", "South Dakota", "Nebraska","Kansas"))
#South#
region3<- str_to_title(c("Delaware","Maryland","District of Columbia","Virginia", "West Virginia","North Carolina","South Carolina","Georgia","Florida",
                         "Kentucky","Tennessee","Alabama","Mississippi",
                         "Arkansas","Louisiana","Oklahoma","Texas"))
#West#
region4<- str_to_title(c("Montana","Idaho","Wyoming","Colorado", "New Mexico","Arizona","Utah","Nevada",
                         "Washington","Oregon","California","Alaska","Hawaii"))



NCES_sch <- NCES_sch %>%
  #filter(eth_total>0)  %>%
  mutate(
         frlunch = flunch+ rlunch,
         flunch_per = round(flunch/eth_total*100, 1),
         rlunch_per = round(rlunch/eth_total*100, 1),
         COMBOKEY = ifelse(nchar(COMBOKEY)== 11, paste("0", COMBOKEY, sep=""),
                          ifelse(nchar(COMBOKEY)== 12, paste(COMBOKEY, sep=""), NA)),
         State_Name = str_to_title(State_Name),
         ses_sch = ifelse((flunch_per+rlunch_per)<25, "H",
                          ifelse((flunch_per+rlunch_per)>=25 & (flunch_per+rlunch_per)<50, "MH",
                                 ifelse((flunch_per+rlunch_per)>=50 & (flunch_per+rlunch_per)<75, "ML",
                                        ifelse((flunch_per+rlunch_per)>=75 & (flunch_per+rlunch_per)<=100, "L", NA)))),
         region = ifelse(State_Name %in% region1, "Northeast",
                         ifelse(State_Name %in% region2, "Midwest",
                                ifelse(State_Name %in% region3, "South",
                                       ifelse(State_Name %in% region4, "West", NA)))))%>%
  distinct(COMBOKEY, .keep_all = T)



round(prop.table(table(NCES_sch$ses_sch))*100, 2)
round(prop.table(table(NCES_sch$region)) *100, 2)

saveRDS(NCES_sch, "C:\\Other\\Presentation\\data\\NCES\\nces_1718.rds")

################################################################################
################################################################################



Enrollment<- read.csv(paste(data_path, "Enrollment", ".csv", sep=""), 
                      na.strings=c("-3","-5","-6","-8","-9", "-11",
                                   "-3.0","-5.0","-6.0","-8.0","-9.0", "-11.0",
                                   "-3.00","-5.00","-6.00","-8.00","-9.00", "-11.00"))%>% 
                select(COMBOKEY, 
                       SCH_ENR_HI_M,
                       SCH_ENR_HI_F,
                       SCH_ENR_AM_M,
                       SCH_ENR_AM_F,
                       SCH_ENR_AS_M,
                       SCH_ENR_AS_F,
                       SCH_ENR_HP_M,
                       SCH_ENR_HP_F,
                       SCH_ENR_BL_M,
                       SCH_ENR_BL_F,
                       SCH_ENR_WH_M,
                       SCH_ENR_WH_F,
                       SCH_ENR_TR_M,
                       SCH_ENR_TR_F,
                       TOT_ENR_M,
                       TOT_ENR_F) %>%
                  mutate(SCH_ENR_BL = SCH_ENR_BL_M + SCH_ENR_BL_F,
                         SCH_ENR_WH = SCH_ENR_WH_M + SCH_ENR_WH_F,
                         bl_per = (SCH_ENR_BL_M + SCH_ENR_BL_F)/(TOT_ENR_M + TOT_ENR_F),
                         wh_per = (SCH_ENR_WH_M + SCH_ENR_WH_F)/(TOT_ENR_M + TOT_ENR_F),
                         male_per = TOT_ENR_M/ (TOT_ENR_M + TOT_ENR_F))

Offenses<- read.csv(paste(data_path, "Offenses", ".csv", sep=""), 
                    na.strings=c("-3","-5","-6","-8","-9", "-11",
                                 "-3.0","-5.0","-6.0","-8.0","-9.0", "-11.0",
                                 "-3.00","-5.00","-6.00","-8.00","-9.00", "-11.00")) %>%
                select(COMBOKEY, 
                       SCH_OFFENSE_RAPE,
                       SCH_OFFENSE_BATT,
                       SCH_OFFENSE_ROBWW,
                       SCH_OFFENSE_ROBWX,
                       SCH_OFFENSE_ROBWOW,
                       SCH_OFFENSE_ATTWW,
                       SCH_OFFENSE_ATTWX,
                       SCH_OFFENSE_ATTWOW,
                       SCH_OFFENSE_THRWW,
                       SCH_OFFENSE_THRWX,
                       SCH_OFFENSE_THRWOW,
                       SCH_OFFENSE_POSSWX) %>%
                mutate(n_discipline =  SCH_OFFENSE_RAPE+
                                       SCH_OFFENSE_BATT+
                                       SCH_OFFENSE_ROBWW+
                                       SCH_OFFENSE_ROBWX+
                                       SCH_OFFENSE_ROBWOW+
                                       SCH_OFFENSE_ATTWW+
                                       SCH_OFFENSE_ATTWX+
                                       SCH_OFFENSE_ATTWOW+
                                       SCH_OFFENSE_THRWW+
                                       SCH_OFFENSE_THRWX+
                                       SCH_OFFENSE_THRWOW+
                                       SCH_OFFENSE_POSSWX)

School_Characteristics<- read.csv(paste(data_path, "School Characteristics", ".csv", sep=""), 
                                  na.strings=c("-3","-5","-6","-8","-9", "-11",
                                               "-3.0","-5.0","-6.0","-8.0","-9.0", "-11.0",
                                               "-3.00","-5.00","-6.00","-8.00","-9.00", "-11.00")) %>%
                          select(COMBOKEY, 
                                 SCH_STATUS_SPED,
                                 SCH_STATUS_MAGNET,
                                 SCH_STATUS_CHARTER,
                                 SCH_STATUS_ALT) 

School_Support<- read.csv(paste(data_path, "School Support", ".csv", sep=""), 
                          na.strings=c("-3","-5","-6","-8","-9", "-11",
                                       "-3.0","-5.0","-6.0","-8.0","-9.0", "-11.0",
                                       "-3.00","-5.00","-6.00","-8.00","-9.00", "-11.00")) %>%
                  select(COMBOKEY, 
                         SCH_FTETEACH_TOT,
                         SCH_FTETEACH_CERT,
                         SCH_FTETEACH_FY,
                         SCH_FTETEACH_SY,
                         SCH_FTESERVICES_NUR,
                         SCH_FTESERVICES_PSY,
                         SCH_FTESERVICES_SOC,
                         SCH_FTESECURITY_LEO,
                         SCH_FTESECURITY_GUA,
                         SCH_FTECOUNSELORS) 

Corporal_Punishiment<- read.csv(paste(data_path, "Corporal Punishment", ".csv", sep=""), 
                                na.strings=c("-3","-5","-6","-8","-9", "-11",
                                             "-3.0","-5.0","-6.0","-8.0","-9.0", "-11.0",
                                             "-3.00","-5.00","-6.00","-8.00","-9.00", "-11.00")) %>%
                        select(COMBOKEY, 
                               SCH_CORPINSTANCES_WODIS,
                               SCH_CORPINSTANCES_WDIS) 

Suspensions<- read.csv(paste(data_path, "Suspensions", ".csv", sep=""), 
                       na.strings=c("-3","-5","-6","-8","-9", "-11",
                                    "-3.0","-5.0","-6.0","-8.0","-9.0", "-11.0",
                                    "-3.00","-5.00","-6.00","-8.00","-9.00", "-11.00"))%>%
                select(COMBOKEY, 
                       SCH_DISCWODIS_ISS_HI_M,
                       SCH_DISCWODIS_ISS_HI_F,
                       SCH_DISCWODIS_ISS_AM_M,
                       SCH_DISCWODIS_ISS_AM_F,
                       SCH_DISCWODIS_ISS_AS_M,
                       SCH_DISCWODIS_ISS_AS_F,
                       SCH_DISCWODIS_ISS_HP_M,
                       SCH_DISCWODIS_ISS_HP_F,
                       SCH_DISCWODIS_ISS_BL_M,
                       SCH_DISCWODIS_ISS_BL_F,
                       SCH_DISCWODIS_ISS_WH_M,
                       SCH_DISCWODIS_ISS_WH_F,
                       SCH_DISCWODIS_ISS_TR_M,
                       SCH_DISCWODIS_ISS_TR_F,
                       TOT_DISCWODIS_ISS_M,
                       TOT_DISCWODIS_ISS_F,
                       SCH_DISCWODIS_ISS_LEP_M,
                       SCH_DISCWODIS_ISS_LEP_F,
                       SCH_DISCWODIS_SINGOOS_HI_M,
                       SCH_DISCWODIS_SINGOOS_HI_F,
                       SCH_DISCWODIS_SINGOOS_AM_M,
                       SCH_DISCWODIS_SINGOOS_AM_F,
                       SCH_DISCWODIS_SINGOOS_AS_M,
                       SCH_DISCWODIS_SINGOOS_AS_F,
                       SCH_DISCWODIS_SINGOOS_HP_M,
                       SCH_DISCWODIS_SINGOOS_HP_F,
                       SCH_DISCWODIS_SINGOOS_BL_M,
                       SCH_DISCWODIS_SINGOOS_BL_F,
                       SCH_DISCWODIS_SINGOOS_WH_M,
                       SCH_DISCWODIS_SINGOOS_WH_F,
                       SCH_DISCWODIS_SINGOOS_TR_M,
                       SCH_DISCWODIS_SINGOOS_TR_F,
                       TOT_DISCWODIS_SINGOOS_M,
                       TOT_DISCWODIS_SINGOOS_F,
                       SCH_DISCWODIS_SINGOOS_LEP_M,
                       SCH_DISCWODIS_SINGOOS_LEP_F,
                       SCH_DISCWODIS_MULTOOS_HI_M,
                       SCH_DISCWODIS_MULTOOS_HI_F,
                       SCH_DISCWODIS_MULTOOS_AM_M,
                       SCH_DISCWODIS_MULTOOS_AM_F,
                       SCH_DISCWODIS_MULTOOS_AS_M,
                       SCH_DISCWODIS_MULTOOS_AS_F,
                       SCH_DISCWODIS_MULTOOS_HP_M,
                       SCH_DISCWODIS_MULTOOS_HP_F,
                       SCH_DISCWODIS_MULTOOS_BL_M,
                       SCH_DISCWODIS_MULTOOS_BL_F,
                       SCH_DISCWODIS_MULTOOS_WH_M,
                       SCH_DISCWODIS_MULTOOS_WH_F,
                       SCH_DISCWODIS_MULTOOS_TR_M,
                       SCH_DISCWODIS_MULTOOS_TR_F,
                       TOT_DISCWODIS_MULTOOS_M,
                       TOT_DISCWODIS_MULTOOS_F,
                       SCH_DISCWODIS_MULTOOS_LEP_M,
                       SCH_DISCWODIS_MULTOOS_LEP_F)


Referrals_Arrests<- read.csv(paste(data_path, "Referrals and Arrests", ".csv", sep=""), 
                             na.strings=c("-3","-5","-6","-8","-9", "-11",
                                          "-3.0","-5.0","-6.0","-8.0","-9.0", "-11.0",
                                          "-3.00","-5.00","-6.00","-8.00","-9.00", "-11.00")) %>%
                      select(COMBOKEY,
                             SCH_DISCWODIS_ARR_HI_M,
                             SCH_DISCWODIS_ARR_HI_F,
                             SCH_DISCWODIS_ARR_AM_M,
                             SCH_DISCWODIS_ARR_AM_F,
                             SCH_DISCWODIS_ARR_AS_M,
                             SCH_DISCWODIS_ARR_AS_F,
                             SCH_DISCWODIS_ARR_HP_M,
                             SCH_DISCWODIS_ARR_HP_F,
                             SCH_DISCWODIS_ARR_BL_M,
                             SCH_DISCWODIS_ARR_BL_F,
                             SCH_DISCWODIS_ARR_WH_M,
                             SCH_DISCWODIS_ARR_WH_F,
                             SCH_DISCWODIS_ARR_TR_M,
                             SCH_DISCWODIS_ARR_TR_F,
                             TOT_DISCWODIS_ARR_M,
                             TOT_DISCWODIS_ARR_F,
                             SCH_DISCWODIS_ARR_LEP_M,
                             SCH_DISCWODIS_ARR_LEP_F,
                             SCH_DISCWODIS_REF_HI_M,
                             SCH_DISCWODIS_REF_HI_F,
                             SCH_DISCWODIS_REF_AM_M,
                             SCH_DISCWODIS_REF_AM_F,
                             SCH_DISCWODIS_REF_AS_M,
                             SCH_DISCWODIS_REF_AS_F,
                             SCH_DISCWODIS_REF_HP_M,
                             SCH_DISCWODIS_REF_HP_F,
                             SCH_DISCWODIS_REF_BL_M,
                             SCH_DISCWODIS_REF_BL_F,
                             SCH_DISCWODIS_REF_WH_M,
                             SCH_DISCWODIS_REF_WH_F,
                             SCH_DISCWODIS_REF_TR_M,
                             SCH_DISCWODIS_REF_TR_F,
                             TOT_DISCWODIS_REF_M,
                             TOT_DISCWODIS_REF_F,
                             SCH_DISCWODIS_REF_LEP_M,
                             SCH_DISCWODIS_REF_LEP_F) %>%
                mutate(SCH_DISCWODIS_ARR_BL = SCH_DISCWODIS_ARR_BL_F + SCH_DISCWODIS_ARR_BL_M,
                       SCH_DISCWODIS_ARR_WH = SCH_DISCWODIS_ARR_WH_F + SCH_DISCWODIS_ARR_WH_M,
                       SCH_DISCWODIS_REF_BL = SCH_DISCWODIS_REF_BL_F + SCH_DISCWODIS_REF_BL_M,
                       SCH_DISCWODIS_REF_WH = SCH_DISCWODIS_REF_WH_F + SCH_DISCWODIS_REF_WH_M)

Restraint_Seclusion<- read.csv(paste(data_path, "Restraint and Seclusion", ".csv", sep=""), 
                               na.strings=c("-3","-5","-6","-8","-9", "-11",
                                            "-3.0","-5.0","-6.0","-8.0","-9.0", "-11.0",
                                            "-3.00","-5.00","-6.00","-8.00","-9.00", "-11.00")) %>%
                        select(COMBOKEY,
                               SCH_RS_NONIDEA_MECH_HI_M,
                               SCH_RS_NONIDEA_MECH_HI_F,
                               SCH_RS_NONIDEA_MECH_AM_M,
                               SCH_RS_NONIDEA_MECH_AM_F,
                               SCH_RS_NONIDEA_MECH_AS_M,
                               SCH_RS_NONIDEA_MECH_AS_F,
                               SCH_RS_NONIDEA_MECH_HP_M,
                               SCH_RS_NONIDEA_MECH_HP_F,
                               SCH_RS_NONIDEA_MECH_BL_M,
                               SCH_RS_NONIDEA_MECH_BL_F,
                               SCH_RS_NONIDEA_MECH_WH_M,
                               SCH_RS_NONIDEA_MECH_WH_F,
                               SCH_RS_NONIDEA_MECH_TR_M,
                               SCH_RS_NONIDEA_MECH_TR_F,
                               TOT_RS_NONIDEA_MECH_M,
                               TOT_RS_NONIDEA_MECH_F,
                               
                               SCH_RS_NONIDEA_MECH_LEP_M,
                               SCH_RS_NONIDEA_MECH_LEP_F,
                               SCH_RS_NONIDEA_MECH_504_M,
                               SCH_RS_NONIDEA_MECH_504_F,
                               SCH_RS_NONIDEA_PHYS_HI_M,
                               SCH_RS_NONIDEA_PHYS_HI_F,
                               SCH_RS_NONIDEA_PHYS_AM_M,
                               SCH_RS_NONIDEA_PHYS_AM_F,
                               SCH_RS_NONIDEA_PHYS_AS_M,
                               SCH_RS_NONIDEA_PHYS_AS_F,
                               SCH_RS_NONIDEA_PHYS_HP_M,
                               SCH_RS_NONIDEA_PHYS_HP_F,
                               SCH_RS_NONIDEA_PHYS_BL_M,
                               SCH_RS_NONIDEA_PHYS_BL_F,
                               SCH_RS_NONIDEA_PHYS_WH_M,
                               SCH_RS_NONIDEA_PHYS_WH_F,
                               SCH_RS_NONIDEA_PHYS_TR_M,
                               SCH_RS_NONIDEA_PHYS_TR_F,
                               TOT_RS_NONIDEA_PHYS_M,
                               TOT_RS_NONIDEA_PHYS_F)



data <- NCES_sch %>%
        inner_join(Enrollment, by= "COMBOKEY") %>%
        inner_join(Corporal_Punishiment, by= "COMBOKEY") %>%
        inner_join(Offenses, by= "COMBOKEY") %>%
        inner_join(Referrals_Arrests, by= "COMBOKEY") %>%
        inner_join(Restraint_Seclusion, by= "COMBOKEY") %>%
        inner_join(Suspensions, by= "COMBOKEY") %>%
        inner_join(School_Characteristics, by= "COMBOKEY") %>%
        inner_join(School_Support, by= "COMBOKEY") %>%
        mutate(COMBOKEY = ifelse(nchar(COMBOKEY)== 11, paste("0", COMBOKEY, sep=""),
                                ifelse(nchar(COMBOKEY)== 12, paste(COMBOKEY, sep=""), NA)))

  


saveRDS(data, "C:\\Other\\Presentation\\data\\data_combined.rds")





