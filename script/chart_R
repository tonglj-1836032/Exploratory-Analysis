# Chart Script(s)
# In your other .R files you create, you should create a visualization of your data. Create a separate .R file for each chart.

# Each chart must return a different type of visualization (e.g., you can only create one Scatter Plot,one map, one bar chart, etc.).  Again, in your report you must describe why you included the chart (e.g., what it attempts to seeks to express), and what information it reveals. For each chart, we expect the following:

# Chart types are intentionally selected to reveal particular patterns in the dataset
# Optimal graphical encodings are selected to present the data in the most interpretable way
# For two dimensional plots, X, Y axis labels are set with clear human readable titles
# When appropriate, the chart has a title
# A legend is present for any color encodings
# If a legend is present, the legend label has been set to be easily readable
# #If a legend is present, the legend doesn't run off the page (this means you should not be using more than
# ~10 colors)
# When you display each chart in your index.Rmd file, you must:

# Describe the purpose of including the chart
# Describe any notable observations and insights from the chart
library(ggplot2)
library(tidyverse)
library(dplyr)
setwd("C:/Users/rong/Desktop/xuewenxi")

COVID <- read.csv("COVID-19_Case_Surveillance_Public_Use_Data.csv",header=T)
feature=colnames(COVID)

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
res$Death_yn=as.numeric(res$Death_yn)
res$Sex=factor(res$Sex,levels =sex)

#ggplot(data = res, aes(x = Age_group, y = Death_yn, fill =Sex,group=Sex)) + geom_area()
sex=as.character(res$Sex)
M=which(sex=="Male")
res_M=res[M,]
max_M=order(as.numeric(res_M$Death_yn), decreasing =T)[1]
max_res=res_M[max_M,]
max_M_A=max_res$age_group
max_M=max_res$Death_yn
F=which(sex=="Fmale")
res_F=res[F,]
max_F=order(as.numeric(res_F$Death_yn), decreasing =T)[1]
max_res=res_F[max_F,]
max_F_A=max_res$age_group
max_F=max_res$Death_yn


plot_line<-ggplot(data = res, aes(x = Age_group, y = Death_yn, group=Sex,color =Sex)) + geom_line()+ ggtitle("COVID-19 cases mortality rates for every sex in each age group.") +
  labs(
    y = "Mortality rates (%)", x = " Age group"
  )





