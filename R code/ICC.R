#title: "Dalia Data Analysis - Multilevel Regression"
#author: "Xuenan Ni"
#date: "June 5, 2018"
#last modified: April 12, 2020
library(dplyr)
devtools::install_github("strengejacke/sjlabelled")
library(sjlabelled)
devtools::install_github("strengejacke/sjmisc")
library(sjmisc)
devtools::install_github("strengejacke/sjstats")
library(sjstats)
devtools::install_github("strengejacke/ggeffects")
library(ggeffects) 
library(Matrix)
library(lme4) #for multilevel modeling
library(lmerTest)
library(arm)
library(car)  #for recode function
library(ggplot2)
library(lattice)
 
#Data preparation # I change indicators to policies 
#Load complete raw dataset: 43034 observations (in 52 countries) of 120 variables
Data_raw <- read.csv("~/Desktop/Dalia_research/IRT_new/CarDependIntentPride_IRT.csv",header=T)

#Basic data cleaning - remove observations that agreed to every single item (inattention) 
Data_raw$SumScore <- rowSums(subset(Data_raw, select = c(q04A:q04K)))

#People chose 0 items should be deleted
length(which(Data_raw$SumScore ==0)) 
Data_clean = Data_raw[!Data_raw$SumScore %in% c(0), ]

#double check if the row sums are zero
Data_clean$SumScore <- rowSums(subset(Data_clean, select = c(q04A:q04K)))
length(which(Data_clean$SumScore==0)) #0

#Data_clean$age <- 2016 -Data_clean$yob
Country_Lookup <- read.csv("~/Dropbox (MIT)/MITEI-M/03.Dalia/Country_Data/Country_ISO_Lookup_sample2.csv")
Data_clean <- merge(Data_clean , Country_Lookup, by = "iso")
Data_clean$age <- 2016 -Data_clean$yob
#For ICC, use this line
clean <- merge(Data_clean , Country_Lookup, by = "iso")


#ICC to detect nested structure -- 
#################################
# Multi-Level Modeling #Use glmer to acount for logistics regression
 
##q04A 
g = glmer(q04A~1+(1|iso), data=clean, family=binomial)
icc(g)
##q04B
g = glmer(q04B~1+(1|iso), data=clean, family=binomial)
icc(g)
##q04C
g = glmer(q04C~1+(1|iso), data=clean, family=binomial)
icc(g)
##q04D
g = glmer(q04D~1+(1|iso), data=clean, family=binomial)
icc(g)
##q04E
g = glmer(q04E~1+(1|iso), data=clean, family=binomial)
icc(g)
##q04F
g = glmer(q04F~1+(1|iso), data=clean, family=binomial)
icc(g)
##q04G
g = glmer(q04G~1+(1|iso), data=clean, family=binomial)
icc(g)
##q04H
g = glmer(q04H~1+(1|iso), data=clean, family=binomial)
icc(g)
##q04I
g = glmer(q04I~1+(1|iso), data=clean, family=binomial)
icc(g)
##q04J
g = glmer(q04J~1+(1|iso), data=clean, family=binomial)
icc(g)
##q04K
g = glmer(q04K~1+(1|iso), data=clean, family=binomial)
icc(g)
 
