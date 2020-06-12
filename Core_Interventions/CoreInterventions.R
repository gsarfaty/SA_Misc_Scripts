library(tidyverse)


#HFR RETENTION ------------------------------------------------------------

tx_currr<-read_tsv("Processed_Files\\HFR\\HFR_pd3to8_20200524.txt") %>% 
  filter(indicator %in% c("TX_CURR")) %>% 
  group_by(date,psnu,indicator) %>% 
  summarise_at(vars(val), sum, na.rm=TRUE) %>% 
  filter(date >= as.Date("2020-04-04")) %>% 
  spread(date,val) %>% 
  rename(t1="2020-04-06",
         t2="2020-05-04") %>% 
  mutate(NN= t2-t1) %>% 
  select(-c("t1","t2")) %>% 
  mutate(indicator="TX_NET_NEW") %>% 
  rename(val=NN)


TX_NEW<-read_tsv("Processed_Files\\HFR\\HFR_pd3to8_20200524.txt") %>% 
  filter(indicator=="TX_NEW",
         date > as.Date("2020-04-06")) %>% 
  group_by(psnu,indicator) %>% 
  summarise_at(vars(val), sum, na.rm=TRUE) %>%

TX_NEW<-TX_NEW %>%
  rename(val=TX_NEW)


ret<-bind_rows(tx_currr,TX_NEW)

# write_tsv(ret,"Processed_Files\\HFR\\HFR_May_Retention_20200604.txt",na="")

#CORE INTERVENTIONS APPT & TRACING --------------------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)

columns<-(c("Province","District","Partner","Total Facilities","Total Siyzena Facilities","AppointmentSystem_Siyenza",
            "AppointmentSystem_non-Siyenza","SMS_WhatsApp_reminder_Siyenza","SMS_WhatsApp_reminder_non-Siyenza",
            "Phone_reminder_Siyenza","Phone_reminder_non-Siyenza","2mo_TEE_Siyenza","2mo_TEE_non-Siyenza",
            "3mo_TLD_Siyenza","3mo_TLD_non-Siyenza","CaseMgr_Siyenza","CaseMgr_non-Siyenza","SMS_WhatsApp_missed_Siyenza",
            "SMS_WhatsApp_missed_non-Siyenza","Phone_missed_Siyenza","Phone_missed_non-Siyenza",
            "PhysicalTracing_Siyenza","PhysicalTracing_non-Siyenza"))

ret_int<-read_excel("Core_Interventions\\USAID_Retention interventions summary_Disaggregated_06082020.xlsx", 
                    sheet="Appointments and Tracing", range="A3:W53",na="") %>% 
  set_names(nm=columns)


ret_int<-ret_int %>% 
  filter(!is.na(`Total Facilities`)) %>% 
  mutate(`Total_non-Siyenza_Facilities`=`Total Facilities`-`Total Siyzena Facilities`) %>% 
  mutate(psnu=case_when(
    str_detect(District,"Alfred") ~ "ec Alfred Nzo District Municipality",
               str_detect(District,"Buffalo") ~ "ec Buffalo City Metropolitan Municipality",
               str_detect(District,"Lej") ~ "fs Lejweleputswa District Municipality",
               str_detect(District,"Thabo") ~ "fs Thabo Mofutsanyane District Municipality",
               str_detect(District, "Johannesburg") ~ "gp City of Johannesburg Metropolitan Municipality",
               str_detect(District, "Sedibeng") ~ "gp Sedibeng District Municipality",
               str_detect(District, "eThekwini") ~ "kz eThekwini Metropolitan Municipality",
               str_detect(District, "Harry") ~ "kz Harry Gwala District Municipality",
               str_detect(District, "King") ~ "kz King Cetshwayo District Municipality",
               str_detect(District, "Ugu") ~ "kz Ugu District Municipality",
               str_detect(District, "Capricorn") ~ "lp Capricorn District Municipality",
               str_detect(District, "Mopani") ~ "lp Mopani District Municipality",
               str_detect(District, "Ehlanzeni") ~ "mp Ehlanzeni District Municipality",
               str_detect(District, "Gert") ~ "mp Gert Sibande District Municipality",
               str_detect(District, "Nkangala") ~ "mp Nkangala District Municipality",
               str_detect(District, "Cape Town") ~ "wc City of Cape Town Metropolitan Municipality",
                          TRUE ~ District
               )) %>% 
  gather(indicator,val,`Total Facilities`:`Total_non-Siyenza_Facilities`) %>% 
  mutate(Siyenza_status=case_when(
    str_detect(indicator,"non-Siyenza") ~ "non-Siyenza",
    str_detect(indicator, "Siyenza") ~ "Siyenza",
    TRUE ~ "NA"
  ))

ret_test<-ret_int %>% 
  mutate(indicator_short=
    str_remove(indicator, "_Siyenza")) %>% 
  mutate(indicator_short=
    str_remove(indicator_short, "_non-Siyenza")) %>% 
  mutate(Siyenza_status=case_when(
    str_detect(indicator, "Total Siyzena Facilities") ~ "Siyenza",
    TRUE ~ Siyenza_status
  )) %>% 
  mutate(indicator_short=case_when(
    indicator=="Total_non-Siyenza_Facilities" ~"Total Facilities",
    indicator=="Total Siyzena Facilities" ~ "Total Facilities",
    TRUE ~indicator_short
  )) %>% 
 spread(indicator_short,val)

# 
# ret_int_wide<-ret_test %>% 
#   spread(indicator,val)

# final<-bind_rows(ret,ret_int) %>% 
#   mutate(indicator_2=indicator,
#          val_2=val) %>% 
#   spread(indicator_2,val_2)


write_tsv(ret_test,"Processed_Files\\CoreIn_HFR\\CoreIn_HFR_CoreIntervention_semiwide_SiyenzaStatus_06092020.txt")

# CORE INTERVENTIONS -- APPT & TRACING FOR GIS -------------------------------------------
library(ICPIutilities)

df<-read_tsv("Genie\\fy20q2\\Genie_TXsubset_20200610.txt")

curr<-df %>% 
  group_by(psnu,fiscal_year,indicator) %>% 
  summarize_at(vars(targets:cumulative),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period_type=="results") %>% 
  unite(ind_unite,indicator,period,sep="_",remove=TRUE) %>% 
  spread(ind_unite,val) %>% 
  select(psnu,TX_CURR_FY19Q4,TX_CURR_FY20Q1,TX_CURR_FY20Q2,TX_NEW_FY19Q4,TX_NEW_FY20Q1,TX_NEW_FY20Q2)



GIS<-ret_int %>% 
  left_join(curr,by="psnu")


write_csv(GIS,"Processed_Files\\CoreIn_HFR\\CoreInterventions_GIS_20200610.csv", na="")

#CORE INTERVENTIONS ACCESS TO MEDS --------------------------------------------------------
columns<-(c("Province","District","Partner","Total Facilities","Total Siyzena Facilities",
            "External_PuP_Siyenza_1","External_PuP_non-Siyenza_1","External_PuP_Siyenza","External_PuP_non-Siyenza",
            "Facility_PuP_Siyenza_1","Facility_PuP_non-Siyenza_1","Facility_PuP_Siyenza","Facility_PuP_non-Siyenza",
            "Adherence_Siyenza_1","Adherence_non-Siyenza_1","Adherence_Siyenza","Adherence_non-Siyenza",
            "Lockers_Siyenza_1","Lockers_non-Siyenza_1","Lockers_Siyenza","Lockers_non-Siyenza",
            "Mobile_Siyenza_1","Mobile_non-Siyenza_1","Mobile_Siyenza","Mobile_non-Siyenza",
            "CHW_Siyenza_1","CHW_non-Siyenza_1","CHW_Siyenza","CHW_non-Siyenza",
            "Other_Siyenza","Other_non-Siyenza"))

meds_int<-read_excel("Core_Interventions\\USAID_Retention interventions summary_Disaggregated_06082020.xlsx", 
                    sheet="Access to meds", range="A5:AE55",na="") %>% 
  set_names(nm=columns)


meds_int<-meds_int %>% 
  filter(!is.na(`Total Facilities`)) %>% 
  mutate(`Total_non-Siyenza_Facilities`=`Total Facilities`-`Total Siyzena Facilities`) %>% 
  mutate(psnu=case_when(
    str_detect(District,"Alfred") ~ "ec Alfred Nzo District Municipality",
    str_detect(District,"Buffalo") ~ "ec Buffalo City Metropolitan Municipality",
    str_detect(District,"Lej") ~ "fs Lejweleputswa District Municipality",
    str_detect(District,"Thabo") ~ "fs Thabo Mofutsanyane District Municipality",
    str_detect(District, "Johannesburg") ~ "gp City of Johannesburg Metropolitan Municipality",
    str_detect(District, "Sedibeng") ~ "gp Sedibeng District Municipality",
    str_detect(District, "eThekwini") ~ "kz eThekwini Metropolitan Municipality",
    str_detect(District, "Harry") ~ "kz Harry Gwala District Municipality",
    str_detect(District, "King") ~ "kz King Cetshwayo District Municipality",
    str_detect(District, "Ugu") ~ "kz Ugu District Municipality",
    str_detect(District, "Capricorn") ~ "lp Capricorn District Municipality",
    str_detect(District, "Mopani") ~ "lp Mopani District Municipality",
    str_detect(District, "Ehlanzeni") ~ "mp Ehlanzeni District Municipality",
    str_detect(District, "Gert") ~ "mp Gert Sibande District Municipality",
    str_detect(District, "Nkangala") ~ "mp Nkangala District Municipality",
    str_detect(District, "Cape Town") ~ "wc City of Cape Town Metropolitan Municipality",
    TRUE ~ District
  )) %>% 
  gather(indicator,val,`Total Facilities`:`Total_non-Siyenza_Facilities`) %>% 
  mutate(Siyenza_status=case_when(
    str_detect(indicator,"non-Siyenza") ~ "non-Siyenza",
    str_detect(indicator, "Siyenza") ~ "Siyenza",
    TRUE ~ "NA"
  )) %>% 
  mutate(indicator=
           str_remove(indicator,"_1")) %>%
  mutate(val=as.numeric(val)) %>% 
  group_by(Province,District,Partner,psnu,indicator,Siyenza_status) %>% 
  summarize_at(vars(val),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  mutate(indicator_short=
           str_remove(indicator, "_Siyenza")) %>% 
  mutate(indicator_short=
           str_remove(indicator_short, "_non-Siyenza")) %>% 
  mutate(Siyenza_status=case_when(
    str_detect(indicator, "Total Siyzena Facilities") ~ "Siyenza",
    TRUE ~ Siyenza_status
  )) %>% 
  mutate(indicator_short=case_when(
    indicator=="Total_non-Siyenza_Facilities" ~"Total Facilities",
    indicator=="Total Siyzena Facilities" ~ "Total Facilities",
    TRUE ~indicator_short
  )) %>% 
  spread(indicator_short,val)


write_tsv(meds_int,"Processed_Files\\CoreIn_HFR\\CoreIntervention_AccesstoMeds_semiwide_SiyenzaStatus_20200610.txt")
