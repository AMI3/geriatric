library(broom)
vax_s<-read.csv("Vax_Select.csv")

log_geriatric<-glm(formula = Outcome ~ factor(Vax) + factor(institute_type_name) +  age +  factor(gendername) + factor(sector) + Neighborhood_Positive_Rate, data=vax_s, family=binomial)
log_geriatric1<-glm(formula = Outcome ~ factor(Vax) +  Neighborhood_Positive_Rate, data=vax_s, family=binomial)
log_geriatric2<-tidy(log_geriatric)

#log_geriatric<-glm(formula = Outcome ~ factor(Vax) + age , data=vax_s, family=binomial)
summary(log_geriatric)
log_g_stats <- exp(cbind(coef(log_geriatric), confint(log_geriatric)))
round(log_g_stats, digits=4)
log_geriatric1<-tidy(log_geriatric, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)
