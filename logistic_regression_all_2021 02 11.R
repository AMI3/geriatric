#install.packages("effects")

library(broom)
library(readxl)
library(dplyr)
library(effects)
#vax_s<-read.csv("Vax_Select.csv")
vax_s <- read_excel("dataForAmi10022020 - 10+ filter (1).xlsx",sheet="Sheet1",na=c("NULL","NO DAY", "NO INST","NO VAC"))

vax_s1<-vax_s%>%
  mutate(outcome=ifelse(is.na(first_pos_swab), 0,1),
         symptoms=ifelse(is.na(symptom_breath_and_fever),0,1),
         vax=ifelse(is.na(Vac2_DateTime),0,1),
         vac2_to_swab=as.Date(first_pos_swab)-as.Date(Vac2_DateTime),
         above_7_from_vax=as.Date(data_update_date)-as.Date(inst_vac2_day),
         dose_1_only=ifelse(is.na(vac2_date)& !is.na(vac1_date),0,1),
         vax_on_inst_vax_day_Plus_minus_day=ifelse(is.na(vac2_date)|`vac_in_instVacDAy+-1`,1,0),
         sector_combined=ifelse(sector=="arab_agas" |sector =="general_agas", "1. other","2. haredi_agas"),
         sector_new=ifelse(sector =="general_agas", "1. general_agas",ifelse (sector=="orthodox_agas", "2. orthodox_agas","3. arab_agas")),
         age_group_combined=ifelse(age_group==("51_65"), "51_65","21_50")
         )%>%
  filter(gendername %in%c("זכר","נקבה"),
         vac2_to_swab>7|is.na(vac2_to_swab),
         above_7_from_vax>7,
         !is.na(vac2_week_num), 
         dose_1_only==1,
         vax_on_inst_vax_day_Plus_minus_day==1
        )
#%>%   select (sector, sector_new)


#log_geriatric1<-glm(formula = outcome ~ factor(vax) + factor(institute_type_name) +  age +  factor(gendername) + factor(sector) + neighborhood_pos_rate + factor(symptoms), data=vax_s, family=binomial)
week_number<-factor(vax_s1$vac2_week_num)
v=factor(vax_s1$vax)
log_geriatric_all<-glm(formula = outcome ~ (v) + (gendername) +  neighborhood_pos_rate + (sector_new)  +  (week_number)+  (age_group), data=vax_s1, family=binomial)

log_geriatric1_v<-glm(formula = outcome ~ (v), data=vax_s1, family=binomial)
log_geriatric1_gendername<-glm(formula = outcome ~ gendername , data=vax_s1, family=binomial)
log_geriatric1_neighborhood_pos_rate <-glm(formula = outcome ~  neighborhood_pos_rate , data=vax_s1, family=binomial)
log_geriatric1_sector<-glm(formula = outcome ~  (sector_new)  , data=vax_s1, family=binomial)
log_geriatric1_week_number <-glm(formula = outcome ~   (week_number), data=vax_s1, family=binomial)
log_geriatric1_age_group <-glm(formula = outcome ~   age_group, data=vax_s1, family=binomial)

log_geriatric2<-glm(formula = outcome ~ (v) + (gendername) + (sector)+  (week_number)+ age_group, data=vax_s1, family=binomial)
log_geriatric3<-glm(formula = outcome ~ (v) , data=vax_s1, family=binomial)
log_geriatric_combined<-glm(formula = outcome ~ (v) + (gendername) + (sector_combined)+ age_group_combined, data=vax_s1, family=binomial)

#summary(log_geriatric1)
#log_g_stats <- exp(cbind(coef(log_geriatric1), confint(log_geriatric1)))
#round(log_g_stats, digits=4)
log_geriatric_all_tidy<-tidy(log_geriatric_all, conf.int = TRUE, conf.level 
                          = 0.95, exponentiate = TRUE)
log_geriatric2_tidy<-tidy(log_geriatric2, conf.int = TRUE, conf.level 
                          = 0.95, exponentiate = TRUE)
log_geriatric_combined_tidy<-tidy(log_geriatric_combined, conf.int = TRUE, conf.level 
                          = 0.95, exponentiate = TRUE)
log_geriatric3_tidy<-tidy(log_geriatric3, conf.int = TRUE, conf.level 
                          = 0.95, exponentiate = TRUE)
log_geriatric1_v_tidy<-tidy(log_geriatric1_v, conf.int = TRUE, conf.level 
                             = 0.95, exponentiate = TRUE)
log_geriatric1_age_group_tidy<-tidy(log_geriatric1_age_group, conf.int = TRUE, conf.level 
                             = 0.95, exponentiate = TRUE)
log_geriatric1_sector_tidy<-tidy(log_geriatric1_sector, conf.int = TRUE, conf.level 
                             = 0.95, exponentiate = TRUE)
log_geriatric1_neighborhood_pos_rate_tidy<-tidy(log_geriatric1_neighborhood_pos_rate, conf.int = TRUE, conf.level 
                                                = 0.95, exponentiate = TRUE)
log_geriatric1_week_number_tidy<-tidy(log_geriatric1_week_number, conf.int = TRUE, conf.level 
                                                = 0.95, exponentiate = TRUE)
log_geriatric1_gendername_tidy<-tidy(log_geriatric1_gendername, conf.int = TRUE, conf.level 
                             = 0.95, exponentiate = TRUE)
write.csv(log_geriatric1_week_number_tidy,file="log_geriatric1_week_number_tidy.csv")
write.csv(log_geriatric1_v_tidy,file="log_geriatric1_v_tidy.csv")
write.csv(log_geriatric1_neighborhood_pos_rate_tidy,file="log_geriatric1_neighborhood_pos_rate_tidy.csv")
write.csv(log_geriatric1_age_group_tidy,file="log_geriatric1_age_group_tidy.csv")
write.csv(log_geriatric1_sector_tidy,file="log_geriatric1_sector_tidy.csv")
write.csv(log_geriatric1_gendername_tidy,file="log_geriatric1_gendername_tidy.csv")
write.csv(log_geriatric_all_tidy,file="log_geriatric_all_tidy.csv")
write.csv(log_geriatric2_tidy,file="log_geriatric2_tidy.csv")
write.csv(log_geriatric3_tidy,file="log_geriatric3_tidy.csv")
write.csv(log_geriatric_combined_tidy,file="log_geriatric_combined_tidy.csv")
plot(allEffects(log_geriatric_all))
plot(allEffects(log_geriatric2))
plot(allEffects(log_geriatric3))
plot(allEffects(log_geriatric_combined))
