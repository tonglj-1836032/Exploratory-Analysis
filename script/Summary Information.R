covid <- read.csv("data/COVID-19_Case_Surveillance_Public_Use_Data.csv")

# Organizing data
covid_hosp <- covid %>%
  filter(hosp_yn != "Unknown") %>%
  filter(hosp_yn != "Missing") %>%
  select(Race.and.ethnicity..combined., hosp_yn)

hosp_by_race_y <- covid_hosp %>%
  filter(Race.and.ethnicity..combined. != "Unknown") %>%
  group_by(Race.and.ethnicity..combined.) %>%
  filter(hosp_yn == "Yes") %>%
  summarize(sum_y = n())

hosp_by_race_sum <- covid_hosp %>%
  filter(Race.and.ethnicity..combined. != "Unknown") %>%
  group_by(Race.and.ethnicity..combined.) %>%
  summarize(sum = n())

hosp_race_rate <- left_join(hosp_by_race_y, hosp_by_race_sum,
  by = "Race.and.ethnicity..combined."
) %>%
  mutate(y_rate = sum_y / sum) %>%
  select(Race.and.ethnicity..combined., y_rate)

# Summary List
summary_info <- list()

#The highest hospital treatment rate among different race & ethnicity groups
summary_info$hosp_y_max <- hosp_race_rate %>%
  filter(y_rate == max(y_rate)) %>%
  pull(y_rate)

##The lowest hospital treatment rate among different race & ethnicity groups
summary_info$hosp_y_min <- hosp_race_rate %>%
  filter(y_rate == min(y_rate)) %>%
  pull(y_rate)

##The overall hospital treatment rate among all patients
summary_info$hosp_y <- covid_hosp %>%
  filter(hosp_yn == "Yes") %>%
  nrow() / nrow(covid_hosp)

####chart_3


covid_death <- COVID %>%
  filter(death_yn != "Unknown" & death_yn != "Missing")
  
covid_death<-covid_death[,c("cdc_report_dt", "sex" ,"age_group","death_yn")]

covid_death <- covid_death %>%
  filter(sex != "Unknown" & sex != "Missing" )
covid_death <- covid_death %>%
  filter(age_group != "Unknown" & age_group != "Missing")
 
age_group=unique(as.character(covid_death$age_group)) 
sex=unique(as.character(covid_death$sex)) 
n=length(age_group)
res=c()
for(i in 1:n){
ss=which(as.character(covid_death$age_group)==age_group[i])
dd=covid_death[ss,]
m=length(sex)
rr=c()
for(j in 1:m)
{
dm=length(which(as.character(dd$sex)==sex[j] & as.character(dd$death_yn)=="Yes"))*100/length(ss)
re=c(sex[j],round(dm,2))
rr=rbind(rr,re)
}
re=cbind(age_group[i],rr)
res=rbind(res,re)
}
colnames(res)=c("Age_group","Sex","Death_yn")
res=data.frame(res)


#The highest mortality rate for male

summary_info$max_M<- res %>%
  filter(Sex == "Male") %>%
  filter(Death_yn == max(Death_yn))  %>%
  pull(Death_yn)
#The age group with the highest mortality rate of male
summary_info$max_M_A<- res %>%
  filter(Sex == "Male") %>%
  filter(Death_yn == max(Death_yn))  %>%
  pull(age_group)

#The highest mortality rate for female
summary_info$max_F<- res %>%
  filter(Sex == "Fmale") %>%
  filter(Death_yn == max(Death_yn))  %>%
  pull(Death_yn)


#The age group with the highest mortality rate of female
summary_info$max_F_A<- res %>%
  filter(Sex == "Fmale") %>%
  filter(Death_yn == max(Death_yn))  %>%
  pull(age_group)

# Style check
lint("Summary Information.R")
style_file("Summary Information.R")
