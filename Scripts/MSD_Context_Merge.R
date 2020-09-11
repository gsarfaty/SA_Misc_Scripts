library(tidyverse)
library(readxl)
library(ICPIutilities)
library(here)
library(glamr)

memory.limit(size=500000)

#GLOBALS
msds<-here("MSD")
genies<-here("Genie")
user<-"insert email"
folder_id<-"insert folder id"
folder_save<-(genies)



#genie pull from drive
access_files(user,folder_id,folder_save)
genie_files<-list.files(genies,pattern="Genie")

genie<-here("Genie",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind)


# #MSD
msd_files<-list.files(msds,pattern="MER")

msd<-here("MSD",msd_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind)



#subset & merge
msd<-msd %>%
  filter(fiscal_year %in% c("2017", "2018","2019"))

final<-rbind(genie,msd)

rm(genie,msd)

##CONTEXT FILES
dsp<-read_excel("ContextFiles\\UserFriendly_PartnerName_DSPcolumn.xlsx") %>% 
  rename(mech_code=MechanismID,
         DSP=DSP_18_19)

ethk<-read_excel("ContextFiles\\eThekwiniSiteShifts.xlsx") %>% 
  filter(Transitionstat=="USAIDtoCDC") %>% 
  rename(fy19q1_sitetransition=Transitionstat,
         orgunituid=Facilityuid) %>% 
  select(orgunituid,fy19q1_sitetransition)

siyenza<-read_tsv("ContextFiles\\siyenza_att_uid_20200626.txt") %>%
  select(-c(facility))


factype<-read_excel("ContextFiles\\Facility_Type.xlsx") %>% 
  rename(facility=Facility)


#MSD/Genie/Context Merge ##
final<-final %>% 
  left_join(dsp, by="mech_code") %>% 
  left_join(ethk,by="orgunituid") %>% 
  left_join(factype, by="facility") %>% 
  left_join(siyenza, by="orgunituid")

rm(dsp,ethk,factype,siyenza)


#CONTEXT & LOOKBACK
final <-final%>% 
  mutate(HIGHBURDEN=case_when(
    psnu=="gp City of Johannesburg Metropolitan Municipality" ~ "YES",
    psnu=="gp City of Tshwane Metropolitan Municipality" ~ "YES",
    psnu=="gp Ekurhuleni Metropolitan Municipality" ~ "YES",
    psnu=="kz eThekwini Metropolitan Municipality" ~ "YES",
    psnu=="wc City of Cape Town Metropolitan Municipality" ~ "YES",
    TRUE ~ "NO"
  )) %>% 
  unite(DSPID,mech_code,psnu,sep="_",remove=FALSE) 


final_test<-final %>% 
  mutate(DSP_lookback=case_when(
    DSP=="YES" ~ "YES",
    TRUE ~ "NO")
  ) %>%
  mutate(partner_lookback=case_when(
    DSPID %in% c("17020_gp City of Johannesburg Metropolitan Municipality",
                "17037_gp City of Johannesburg Metropolitan Municipality",
                "70310_gp City of Johannesburg Metropolitan Municipality",
                "17023_gp Sedibeng District Municipality",
                "70310_gp Sedibeng District Municipality",
                "17036_lp Capricorn District Municipality",
                "70310_lp Capricorn District Municipality",
                "17046_wc City of Cape Town Metropolitan Municipality",
                "70310_wc City of Cape Town Metropolitan Municipality",
                "70288_wc City of Cape Town Metropolitan Municipality")~ "ANOVA HEALTH INSTITUTE",
    DSPID %in% c("17036_mp Nkangala District Municipality",
                 "70287_mp Nkangala District Municipality") ~ "Broadreach",
    DSPID %in% c("18481_fs Lejweleputswa District Municipality",
                 "70301_fs Lejweleputswa District Municipality") ~ "Wits Reproductive Health& HIV Institute",
    DSPID %in% c("17037_nw Dr Kenneth Kaunda District Municipality",
                 "18484_nw Dr Kenneth Kaunda District Municipality",
                 "17046_kz uMgungundlovu District Municipality",
                 "70289_kz uMgungundlovu District Municipality",
                 "18484_kz uMgungundlovu District Municipality") ~ "Aurum Health Research",
    DSPID %in% c("17036_gp City of Tshwane Metropolitan Municipality",
                 "17021_gp City of Tshwane Metropolitan Municipality",
                 "18484_gp City of Tshwane Metropolitan Municipality",
                 "18483_gp City of Tshwane Metropolitan Municipality",
                 "70301_gp City of Tshwane Metropolitan Municipality") ~ "Wits Health Consortium (Pty) Limited",
    DSPID %in% c("17023_ec Alfred Nzo District Municipality",
                 "70289_ec Alfred Nzo District Municipality",
                 "81902_ec Alfred Nzo District Municipality",
                 "17036_ec Buffalo City Metropolitan Municipality",
                 "70288_ec Buffalo City Metropolitan Municipality",
                 "81902_ec Buffalo City Metropolitan Municipality",
                 "17023_kz Harry Gwala District Municipality",
                 "70289_kz Harry Gwala District Municipality",
                 "81902_kz Harry Gwala District Municipality") ~ "Maternal, Adolscent and Child Health (MatCH)",
    DSPID %in% c("18481_ec Oliver Tambo District Municipality",
                 "18482_ec Oliver Tambo District Municipality",
                 "18481_ec Chris Hani District Municipality",
                 "18482_ec Chris Hani District Municipality") ~ "TB/HIV Care",
    DSPID=="70289_kz eThekwini Metropolitan Municipality" &  fy19q1_sitetransition=="USAIDtoCDC" ~ "Health Systems Trust",
    TRUE ~ Partner),
    mech_id_lookback=case_when(
      DSPID %in% c("17020_gp City of Johannesburg Metropolitan Municipality",
                   "17037_gp City of Johannesburg Metropolitan Municipality",
                   "70310_gp City of Johannesburg Metropolitan Municipality",
                   "17023_gp Sedibeng District Municipality",
                   "70310_gp Sedibeng District Municipality",
                   "17036_lp Capricorn District Municipality",
                   "70310_lp Capricorn District Municipality",
                   "17046_wc City of Cape Town Metropolitan Municipality",
                   "70310_wc City of Cape Town Metropolitan Municipality",
                   "70288_wc City of Cape Town Metropolitan Municipality")~ "70310",
      DSPID %in% c("17036_mp Nkangala District Municipality",
                   "70287_mp Nkangala District Municipality") ~ "70287",
      DSPID %in% c("18481_fs Lejweleputswa District Municipality",
                   "70301_fs Lejweleputswa District Municipality") ~ "70301",
      DSPID %in% c("17037_nw Dr Kenneth Kaunda District Municipality",
                   "18484_nw Dr Kenneth Kaunda District Municipality",
                   "17046_kz uMgungundlovu District Municipality",
                   "70289_kz uMgungundlovu District Municipality",
                   "18484_kz uMgungundlovu District Municipality") ~ "18484",
      DSPID %in% c("17036_gp City of Tshwane Metropolitan Municipality",
                   "17021_gp City of Tshwane Metropolitan Municipality",
                   "18484_gp City of Tshwane Metropolitan Municipality",
                   "18483_gp City of Tshwane Metropolitan Municipality",
                   "70301_gp City of Tshwane Metropolitan Municipality") ~ "18483",
      DSPID %in% c("17023_ec Alfred Nzo District Municipality",
                   "70289_ec Alfred Nzo District Municipality",
                   "81902_ec Alfred Nzo District Municipality",
                   "17036_ec Buffalo City Metropolitan Municipality",
                   "70288_ec Buffalo City Metropolitan Municipality",
                   "81902_ec Buffalo City Metropolitan Municipality",
                   "17023_kz Harry Gwala District Municipality",
                   "70289_kz Harry Gwala District Municipality",
                   "81902_kz Harry Gwala District Municipality") ~ "81902",
      DSPID %in% c("18481_ec Oliver Tambo District Municipality",
                   "18482_ec Oliver Tambo District Municipality",
                   "18481_ec Chris Hani District Municipality",
                   "18482_ec Chris Hani District Municipality") ~ "18482",
      DSPID=="70289_kz eThekwini Metropolitan Municipality" &  fy19q1_sitetransition=="USAIDtoCDC" ~ "18481",
      TRUE ~ mech_code),
   agency_lookback=case_when(
      DSPID %in% c("17020_gp City of Johannesburg Metropolitan Municipality",
                   "17037_gp City of Johannesburg Metropolitan Municipality",
                   "70310_gp City of Johannesburg Metropolitan Municipality",
                   "17023_gp Sedibeng District Municipality",
                   "70310_gp Sedibeng District Municipality",
                   "17036_lp Capricorn District Municipality",
                   "70310_lp Capricorn District Municipality",
                   "17046_wc City of Cape Town Metropolitan Municipality",
                   "70310_wc City of Cape Town Metropolitan Municipality",
                   "70288_wc City of Cape Town Metropolitan Municipality")~ "USAID",
      DSPID %in% c("17036_mp Nkangala District Municipality",
                   "70287_mp Nkangala District Municipality") ~ "USAID",
      DSPID %in% c("18481_fs Lejweleputswa District Municipality",
                   "70301_fs Lejweleputswa District Municipality") ~ "USAID",
      DSPID %in% c("17037_nw Dr Kenneth Kaunda District Municipality",
                   "18484_nw Dr Kenneth Kaunda District Municipality",
                   "17046_kz uMgungundlovu District Municipality",
                   "70289_kz uMgungundlovu District Municipality",
                   "18484_kz uMgungundlovu District Municipality") ~ "HHS/CDC",
      DSPID %in% c("17036_gp City of Tshwane Metropolitan Municipality",
                   "17021_gp City of Tshwane Metropolitan Municipality",
                   "18484_gp City of Tshwane Metropolitan Municipality",
                   "18483_gp City of Tshwane Metropolitan Municipality",
                   "70301_gp City of Tshwane Metropolitan Municipality") ~ "HHS/CDC",
      DSPID %in% c("17023_ec Alfred Nzo District Municipality",
                   "70289_ec Alfred Nzo District Municipality",
                   "81902_ec Alfred Nzo District Municipality",
                   "17036_ec Buffalo City Metropolitan Municipality",
                   "70288_ec Buffalo City Metropolitan Municipality",
                   "81902_ec Buffalo City Metropolitan Municipality",
                   "17023_kz Harry Gwala District Municipality",
                   "70289_kz Harry Gwala District Municipality",
                   "81902_kz Harry Gwala District Municipality") ~ "USAID",
      DSPID %in% c("18481_ec Oliver Tambo District Municipality",
                   "18482_ec Oliver Tambo District Municipality",
                   "18481_ec Chris Hani District Municipality",
                   "18482_ec Chris Hani District Municipality") ~ "HHS/CDC",
      DSPID=="70289_kz eThekwini Metropolitan Municipality" &  fy19q1_sitetransition=="USAIDtoCDC" ~ "HHS/CDC",
      TRUE ~ fundingagency
    ),
   shortname=(str_remove_all(psnu," District | Metropolitan |Municipality")))




#Write file#
filename<-paste("msd_fy17to20", Sys.Date(), "attributes.txt",sep="_")

write_tsv(final_test, file.path(here("Processed_Files/MSD_genie"),filename,na=""))


