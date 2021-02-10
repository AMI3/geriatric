library(broom)
library(readxl)
library(dplyr)

#vax_s<-read.csv("Vax_Select.csv")
vax_s <- read_excel("dataForAmi10022020 - 10+ filter.xlsx",sheet="Sheet1", na="NULL")

vax_s1<-vax_s%>%
  mutate(outcome=ifelse(is.na(first_pos_swab), 0,1),
         symptoms=ifelse(is.na(symptom_breath_and_fever),0,1),
         vax=ifelse(is.na(Vac2_DateTime),0,1),
         vax1=as.Date(first_pos_swab)-as.Date(Vac2_DateTime),
         vax2=as.Date(max(first_pos_swab, na.rm=T))-as.Date(inst_vac_day)
  )%>%
  filter(gendername %in%c("זכר","נקבה"),
         vax1>6|is.na(vax1),
         vax2>28
  )
# select(vax1,first_pos_swab,Vac2_DateTime,inst_vac_day, vax2)



#log_geriatric1<-glm(formula = outcome ~ factor(vax) + factor(institute_type_name) +  age +  factor(gendername) + factor(sector) + neighborhood_pos_rate + factor(symptoms), data=vax_s, family=binomial)
log_geriatric1<-glm(formula = outcome ~ factor(vax) +     factor(gendername) +  neighborhood_pos_rate + factor(sector)+  age  , data=vax_s1, family=binomial)
log_geriatric2<-glm(formula = outcome ~ factor(vax) + factor(gendername) + factor(sector)+  age, data=vax_s1, family=binomial)
log_geriatric3<-glm(formula = outcome ~ factor(vax) , data=vax_s1, family=binomial)

#summary(log_geriatric1)
#log_g_stats <- exp(cbind(coef(log_geriatric1), confint(log_geriatric1)))
#round(log_g_stats, digits=4)
log_geriatric1_tidy<-tidy(log_geriatric1, conf.int = TRUE, conf.level 
                          = 0.95, exponentiate = TRUE)
log_geriatric2_tidy<-tidy(log_geriatric2, conf.int = TRUE, conf.level 
                          = 0.95, exponentiate = TRUE)
log_geriatric3_tidy<-tidy(log_geriatric3, conf.int = TRUE, conf.level 
                          = 0.95, exponentiate = TRUE)
