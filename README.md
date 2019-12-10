---
title: "TLC Connect 2019"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
##################
Data cleaning TLC
###############

```{r}
library(prettyR)
library(rstanarm)
setwd("P:/Evaluation/TN Lives Count_Connect/Databases")
tlc_data = read.csv("TLCConnect_10_1_2019.csv", header = TRUE, na.strings = c(-6,-7,-8,-9))
head(tlc_data)
## Change youth id 
colnames(tlc_data)[1] = "YouthID"
head(tlc_data)


describe.factor(tlc_data$CSSRS1)
describe.factor(tlc_data$CSSRS4)
describe.factor(tlc_data$AttemptSuicide)
head(tlc_data)
tlc_data_analysis = tlc_data[,c(1,2,5:9, 11, 13:56, 69:112,118,124)]
tlc_data_analysis = data.frame(tlc_data_analysis, HoursPsychotherapy = tlc_data$HoursPsychotherapy, CurrentlyEngaged  =  tlc_data$CurrentlyEngaged, ReferralsEngaged = tlc_data$ReferralsEngaged, Attend75Referrals = tlc_data$Attend75Referrals, ReferralsProvided = tlc_data$ReferralsProvided, CrisisPlan80Time = tlc_data$CrisisPlan80Time)
head(tlc_data_analysis)
#Check all variables are within the ranges
apply(tlc_data_analysis, 2, function(x){describe.factor(x)})
head(tlc_data_analysis)

### Generate average scores
head(tlc_data_analysis)

#f = 6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RAS_b_average = tlc_data_analysis[,9:28]

RAS_b_1_average = RAS_b_average[,c(6:13,20)]
RAS_b_1_average = apply(RAS_b_1_average, 1, mean, na.rm = TRUE)

#q=17,r=18,s=19
RAS_b_2_average = RAS_b_average[,17:19]
RAS_b_2_average = apply(RAS_b_2_average, 1, mean, na.rm = TRUE)

#a,b,c,d,e
RAS_b_3_average = RAS_b_average[,1:5]
RAS_b_3_average = apply(RAS_b_3_average, 1, mean, na.rm = TRUE)
#n=14,o=15,p=16
RAS_b_5_average = RAS_b_average[,14:16]
RAS_b_5_average = apply(RAS_b_5_average, 1, mean, na.rm = TRUE)

INQ_b_1_average = tlc_data_analysis[,29:34]
INQ_b_1_average = apply(INQ_b_1_average, 1, mean, na.rm = TRUE)

INQ_b_2_average = tlc_data_analysis[,35:40]

#https://www.marsja.se/reverse-scoring-using-r/
INQ_b_2_average = 8-INQ_b_2_average
INQ_b_2_average = apply(INQ_b_2_average,1, mean, na.rm = TRUE)

SSMI_b_average = tlc_data_analysis[,41:45]
SSMI_b_average =  apply(SSMI_b_average, 1, mean, na.rm = TRUE)



SIS_b_average = tlc_data_analysis[,46:52]

#a,b,c,d
SIS_b_1_average = SIS_b_average[,1:4]
SIS_b_1_average = apply(SIS_b_1_average, 1, mean, na.rm =TRUE)

#e,f,g
SIS_b_2_average = SIS_b_average[,5:7]
SIS_b_2_average = apply(SIS_b_2_average, 1, mean, na.rm =TRUE)



#### Discharge
#f = 6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RAS_d_average = tlc_data_analysis[53:72]

RAS_d_1_average = RAS_d_average[,c(6:13,20)]
RAS_d_1_average = apply(RAS_d_1_average, 1, mean, na.rm = TRUE)

#q=17,r=18,s=19
RAS_d_2_average = RAS_d_average[,17:19]
RAS_d_2_average = apply(RAS_d_2_average, 1, mean, na.rm = TRUE)

#a,b,c,d,e
RAS_d_3_average = RAS_d_average[,1:5]
RAS_d_3_average = apply(RAS_d_3_average, 1, mean, na.rm = TRUE)
#n=14,o=15,p=16
RAS_d_5_average = RAS_d_average[,14:16]
RAS_d_5_average = apply(RAS_d_5_average, 1, mean, na.rm = TRUE)

INQ_d_1_average = tlc_data_analysis[,73:78]
INQ_d_1_average = apply(INQ_d_1_average, 1, mean, na.rm = TRUE)

INQ_d_2_average = tlc_data_analysis[,79:84]
INQ_d_2_average = 8-INQ_d_2_average
INQ_d_2_average = apply(INQ_d_2_average, 1, mean, na.rm = TRUE)

SSMI_d_average = tlc_data_analysis[,85:89]
SSMI_d_average =  apply(SSMI_d_average, 1, mean, na.rm = TRUE)


SIS_d_average = tlc_data_analysis[,90:96]

#a,b,c,d
SIS_d_1_average = SIS_d_average[,1:4]
SIS_d_1_average = apply(SIS_d_1_average, 1, mean, na.rm =TRUE)

#e,f,g
SIS_d_2_average = SIS_d_average[,5:7]
SIS_d_2_average = apply(SIS_d_2_average, 1, mean, na.rm =TRUE)

### Create difference scores
RAS_1_diff = RAS_d_1_average - RAS_b_1_average
RAS_2_diff = RAS_d_2_average - RAS_b_2_average
RAS_3_diff = RAS_d_3_average - RAS_b_3_average
RAS_5_diff = RAS_d_5_average - RAS_b_5_average



INQ_1_diff = INQ_d_1_average - INQ_b_1_average
INQ_2_diff = INQ_d_2_average - INQ_b_2_average
SSMI_diff = SSMI_d_average-SSMI_b_average

SIS_1_diff = SIS_d_1_average-SIS_b_1_average
SIS_2_diff = SIS_d_2_average-SIS_b_2_average

PHQ9_b = tlc_data_analysis$PHQ9_1
PHQ9_d = tlc_data_analysis$PHQ9_4
PHQ9_diff = tlc_data_analysis$PHQ9_4 - tlc_data_analysis$PHQ9_1

#### Create new data with average scores
#apply(tlc_data_analysis, 2, function(x){describe.factor(x)})
tlc_data_analysis_average = data.frame(tlc_data_analysis[,c(1,2,4:8, 99:104)], RAS_b_1_average, RAS_b_2_average, RAS_b_3_average, RAS_b_5_average, INQ_b_1_average, INQ_b_2_average, SSMI_b_average, SIS_b_1_average, SIS_b_2_average, PHQ9_b = tlc_data_analysis$PHQ9_1,RAS_d_1_average, RAS_d_2_average, RAS_d_3_average, RAS_d_5_average, INQ_d_1_average, INQ_d_2_average, SSMI_d_average, SIS_d_1_average, SIS_d_2_average,PHQ9_d = tlc_data_analysis$PHQ9_4)


### Get rid of missing treatments
tlc_data_analysis_average$treat_missing = is.na(tlc_data_analysis_average$TXPackageAssigned)
tlc_data_analysis_average = subset(tlc_data_analysis_average, treat_missing == FALSE)
dim(tlc_data_analysis_average)
tlc_data_analysis_average$treat_missing = NULL

### Get rid of EHR vars and PHQ
tlc_data_analysis_average[,c(8:13, 23,33)] = NULL

```
######################
Data cleaning Target
######################
```{r, include=FALSE}
setwd("P:/Evaluation/TN Lives Count_Writing/4_Target1_EnhancedCrisisFollow-up/3_Data & Data Analyses")
datPreAdult = read.csv("Target1EnhancedBaseAdult.csv", header = TRUE)
datPostAdult = read.csv("Target1EnhancedPostAdult.csv", header = TRUE)
datAdultTreat = read.csv("AdultTreatments.csv", header = TRUE)

library(prettyR)


head(datPreAdult)
# subset the variables that you want
datPreAdult =datPreAdult[c(1, 3:4, 6,8,10, 12:13, 15:34, 36:45, 47:51, 53:59)]
dim(datPreAdult)
head(datPostAdult)
datPostAdult = datPostAdult[c(1, 3:22,24:33, 35:39, 41:47)]
head(datPostAdult)
dim(datPostAdult)
# Rename added variables otherwise everything else should be the same
colnames(datPreAdult)[colnames(datPreAdult) == "Added.V2..Thinking.of.Ways.to.Kill.Self"] = "Added"   

### Need to deal with ID problems in here first
describe.factor(datPreAdult$Adult.ID)


dim(datPreAdult)
### Now merge all data with baseline to assess missingness
datAdult = merge(datPreAdult, datPostAdult, all.x = TRUE, by = "Adult.ID")
dim(datAdult)
dim(datPreAdult)
### merge with treatment
datAdult = merge(datAdult, datAdultTreat, all.x = TRUE, by = "Adult.ID")
dim(datAdult)
describe.factor(datAdult$Treatment)

#### Three double ids need to get rid of 1272, 1280, 1131 
datAdult = datAdult[-c(259, 262,208),] 
describe.factor(datAdult$Adult.ID)
head(datAdult[c(259, 262,208),])

#### Get rid of .1's without treatments they are repeats
dim(datAdult)
datAdult$treatment_missing = is.na(datAdult$Treatment)
datAdult = subset(datAdult, treatment_missing == FALSE)
dim(datAdult)
##########


colnames(datAdult) = c("ID", "Age", "Gender", "Race", "SexualOrientation", "RelationshipStatus", "Edu", "Employment", "RAS1_b", "RAS2_b", "RAS3_b", "RAS4_b", "RAS5_b", "RAS6_b", "RAS7_b", "RAS8_b", "RAS9_b", "RAS10_b", "RAS11_b", "RAS12_b", "RAS13_b", "RAS14_b", "RAS15_b", "RAS16_b", "RAS17_b", "RAS18_b", "RAS19_b", "RAS20_b", "INQ1_b", "INQ2_b", "INQ3_b", "INQ4_b", "INQ5_b", "INQ6_b", "INQ7_b", "INQ8_b", "INQ9_b", "INQ10_b", "SSMI1_b", "SSMI2_b", "SSMI3_b", "SSMI4_b", "SSMI5_b", "SIS1_b", "SIS2_b", "SIS3_b", "SIS4_b", "SIS5_b", "SIS6_b", "SIS7_b", "RAS1_d", "RAS2_d", "RAS3_d", "RAS4_d", "RAS5_d", "RAS6_d", "RAS7_d", "RAS8_d", "RAS9_d", "RAS10_d", "RAS11_d", "RAS12_d", "RAS13_d", "RAS14_d", "RAS15_d", "RAS16_d", "RAS17_d", "RAS18_d", "RAS19_d", "RAS20_d", "INQ1_d", "INQ2_d", "INQ3_d", "INQ4_d", "INQ5_d", "INQ6_d", "INQ7_d", "INQ8_d", "INQ9_d", "INQ10_d", "SSMI1_d", "SSMI2_d", "SSMI3_d", "SSMI4_d", "SSMI5_d", "SIS1_d", "SIS2_d", "SIS3_d", "SIS4_d", "SIS5_d", "SIS6_d", "SIS7_d", "Treatment")
describe.factor(datAdult$Treatment)

# Two treatments have B with space first so try and recode those as just B's
datAdult$Treatment = ifelse(datAdult$Treatment == "A", 1, ifelse(datAdult$Treatment =="B", 2, ifelse(datAdult$Treatment == " B", 2, ifelse(datAdult$Treatment == "C", 3, datAdult$Treatment)))) 
describe.factor(datAdult$Treatment)

### Werid B changed to 6 so changing it back
datAdult$Treatment = ifelse(datAdult$Treatment == 5 , 2, datAdult$Treatment)
describe.factor(datAdult$Treatment)
# Three items are reversed scored: f = 6, g = 7, j = 10
datAdult$INQ6_b = 8-datAdult$INQ6_b
datAdult$INQ7_b = 8-datAdult$INQ7_b
datAdult$INQ10_b = 8-datAdult$INQ10_b

datAdult$INQ6_d = 8-datAdult$INQ6_d
datAdult$INQ7_d = 8-datAdult$INQ7_d
datAdult$INQ10_d = 8-datAdult$INQ10_d

#### Check for goofy answers looks good
apply(datAdult, 2, function(x){describe.factor(x)})

### Now get the means
head(datAdult)
# Subscale one: f =6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RAS_b_1_average = datAdult[,c(14:21, 28)]
RAS_b_1_average = apply(RAS_b_1_average,1,mean, na.rm = TRUE)

# Subscale two q = 17, r= 18, s= 19
RAS_b_2_average = datAdult[,c(25:27)]
RAS_b_2_average = apply(RAS_b_2_average, 1, mean, na.rm = TRUE)

# Subscale three: a = 1, b = 2, c = 3, d= 4, e = 5
RAS_b_3_average = datAdult[,9:13]
RAS_b_3_average = apply(RAS_b_3_average, 1, mean, na.rm = TRUE)

# Subscale five: n = 14, o = 15, p = 16
RAS_b_5_average = datAdult[,22:24]
RAS_b_5_average = apply(RAS_b_5_average, 1, mean, na.rm = TRUE)

#Subscale 1 for INQ: a = 1, b = 2, c = 3, d = 4, e = 5
INQ_b_1_average = datAdult[,29:33]
INQ_b_1_average = apply(INQ_b_1_average, 1, mean, na.rm = TRUE)

#Subscale 2 for INQ: f-j: 6-10
INQ_b_2_average = datAdult[,35:38]
INQ_b_2_average = apply(INQ_b_2_average, 1, mean, na.rm = TRUE)

#Subscale 1 for SIS: a-d: 1:4
SIS_b_1_average  = datAdult[,44:47]
SIS_b_1_average  = apply(SIS_b_1_average , 1, mean, na.rm = TRUE)


### SSMI
SSMI_b_average = datAdult[,39:43]
SSMI_b_average = apply(SSMI_b_average, 1, mean, na.rm = TRUE)
########
# Now discharge
#########
# Subscale one: f =6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RAS_d_1_average = datAdult[,c(57:63, 70)]
RAS_d_1_average = apply(RAS_d_1_average,1,mean, na.rm = TRUE)

# Subscale two q = 17, r= 18, s= 19
RAS_d_2_average = datAdult[,c(67:69)]
RAS_d_2_average = apply(RAS_d_2_average, 1, mean, na.rm = TRUE)

# Subscale three: a = 1, b = 2, c = 3, d= 4, e = 5
RAS_d_3_average = datAdult[,51:55]
RAS_d_3_average = apply(RAS_d_3_average, 1, mean, na.rm = TRUE)

# Subscale five: n = 14, o = 15, p = 16
RAS_d_5_average = datAdult[,64:66]
RAS_d_5_average = apply(RAS_d_5_average, 1, mean, na.rm = TRUE)

#Subscale 1 for INQ: a = 1, b = 2, c = 3, d = 4, e = 5
INQ_d_1_average = datAdult[,71:75]
INQ_d_1_average = apply(INQ_d_1_average, 1, mean, na.rm = TRUE)

#Subscale 2 for INQ: f-j: 6-10
INQ_d_2_average = datAdult[,76:80]
INQ_d_2_average = apply(INQ_d_2_average, 1, mean, na.rm = TRUE)

#Subscale 1 for SIS: a-d: 1:4
SIS_d_1_average  = datAdult[,86:89]
SIS_d_1_average  = apply(SIS_d_1_average , 1, mean, na.rm = TRUE)

### SSMI
SSMI_d_average =datAdult[,81:85]
SSMI_d_average = apply(SSMI_d_average, 1, mean, na.rm = TRUE)
#################


#################
# Clean up demographics
datAdult
## Gender female = 1 versus male = 0
describe.factor(datAdult$Gender)
female = ifelse(datAdult$Gender == 1, 0,1)
## Race non-white = 1 versus white = 0
describe.factor(datAdult$Race)
non_white = ifelse(datAdult$Race == 7,0,1)
### Single =1 versus non-single 0
describe.factor(datAdult$RelationshipStatus)
single = ifelse(datAdult$RelationshipStatus == 1, 1, 0)
### Sexual orientation
### sexual_minority != 3, hetero = 3
describe.factor(datAdult$SexualOrientation)
sexual_minority = ifelse(datAdult$SexualOrientation != 3, 1, 0)

### edu high school or greater 2 or above
describe.factor(datAdult$Edu)
high_school_greater = ifelse(datAdult$Edu > 1, 1, 0)

#### employement 2,3 employed and all else not employed
describe.factor(datAdult$Employment)
employed = ifelse(datAdult$Employment == 2, 1, ifelse(datAdult$Employment == 3, 1, 0))


### treatment
treatment =  datAdult$Treatment
describe.factor(treatment)
########## 
# Put together Target dat set
#################
target_dat = data.frame(ID = datAdult$ID, treatment, age = datAdult$Age, female, non_white, single, sexual_minority, high_school_greater, employed, RAS_b_1_average, RAS_b_2_average, RAS_b_3_average, RAS_b_5_average, INQ_b_1_average, INQ_b_2_average, SIS_b_1_average,SSMI_b_average, RAS_d_1_average, RAS_d_2_average, RAS_d_3_average, RAS_d_5_average, INQ_d_1_average, INQ_d_2_average, SIS_d_1_average,SSMI_d_average)


### treatment
treatment =  datAdult$Treatment
describe.factor(treatment)

```
#############
Combine then, then get rid of missing data using 50% rule
```{r}
dim(target_dat)
target_dat_com = target_dat
tlc_dat_com = tlc_data_analysis_average
colnames(tlc_dat_com)[c(1,2,3)] = c("ID", "treatment", "age")
head(tlc_dat_com)
female = ifelse(tlc_dat_com$Gender == 2, 1, 0)
tlc_dat_com$Gender = NULL
non_white = ifelse(tlc_dat_com$RaceEthnicity == 3,0,1)
tlc_dat_com$RaceEthnicity = NULL
sexual_minority = ifelse(tlc_dat_com$SexualOrientation == 5,0,1)
tlc_dat_com$SexualOrientation = NULL
tlc_dat_com[,4:10] = NULL
head(tlc_dat_com)
dim(tlc_dat_com)
tlc_dat_com = data.frame(tlc_dat_com[,1:3], female, non_white, sexual_minority, tlc_dat_com[,4:23])
tlc_dat_com

### Get SSMI after SIS and drop SIS 2 and PHQ-9
tlc_dat_com$PHQ9_b = NULL
tlc_dat_com$PHQ9_d = NULL
tlc_dat_com$SIS_b_2_average = NULL
tlc_dat_com$SIS_d_2_average = NULL
head(tlc_dat_com)
dim(tlc_dat_com)
tlc_dat_com
############### Now target need to get rid of single, high school, and employed
target_dat_com$single = NULL
target_dat_com$high_school_greater = NULL
target_dat_com$employed = NULL

dim(target_dat_com)
target_dat_com$target = rep(1,dim(target_dat_com)[1])
dim(tlc_dat_com)
tlc_dat_com$target = rep(0,dim(tlc_dat_com)[1])

tlc_target_dat = rbind(target_dat_com, tlc_dat_com)
tlc_target_dat

```
################################
Now get quasi imputed data set
################################
```{r}
############ Review missing data
miss_var_summary(tlc_target_dat)


quasi_itt =  apply(tlc_target_dat, 1, function(x)(sum(is.na(x))))
quasi_itt_dat = data.frame(tlc_target_dat,quasi_itt)
describe.factor(quasi_itt_dat$quasi_itt)
### Ten variables and threshold is less than 50% 
dim(quasi_itt_dat)[2]/2
quasi_itt_dat = subset(quasi_itt_dat, quasi_itt < dim(quasi_itt_dat)[2]/2)
dim(tlc_target_dat)
dim(quasi_itt_dat)
quasi_itt_dat$quasi_itt = NULL
tlc_target_dat

```


#######################
Now get imputted data
#######################
```{r}
library(Amelia)
head(quasi_itt_dat)
sum(is.na(quasi_itt_dat))
prop_miss_case(quasi_itt_dat)
a.out = amelia(x=quasi_itt_dat, m = 5, noms = c("treatment", "female", "non_white", "sexual_minority"))
summary(a.out)
impute_dat_loop = a.out$imputations


```
##############
Within between TLC and Target
So regression with stand diff score and target at indicator
##############################
```{r}
### Create difference scores
out_diff_dat = list()
impute_dat_loop[[1]][7:14]
for(i in 1:length(impute_dat_loop)){
  out_diff_dat[[i]] = impute_dat_loop[[i]][15:22]-impute_dat_loop[[i]][7:14]
  colnames(out_diff_dat[[i]]) = c("RAS_1_diff", "RAS_2_diff", "RAS_3_diff", "RAS_5_diff", "INQ_1_diff", "INQ_2_diff", "SIS_1_diff", "SSMI_diff")
  out_diff_dat[[i]] = scale(out_diff_dat[[i]])
  out_diff_dat[[i]] =cbind(impute_dat_loop[[i]], out_diff_dat[[i]])
}
out_diff_dat[[1]]


```
####################
Within between target
#####################
```{r}

impute_dat_loop_t1= list()
impute_dat_loop_t2= list()
impute_dat_loop_t3= list()

for(i in 1:length(out_diff_dat)){
  impute_dat_loop_t1[[i]] = subset(out_diff_dat[[i]], treatment == 1)
  impute_dat_loop_t2[[i]] = subset(out_diff_dat[[i]], treatment == 2)
  impute_dat_loop_t3[[i]] = subset(out_diff_dat[[i]], treatment == 3)
}
dim(impute_dat_loop_t1[[1]])

impute_tlc_target_within_results_t1 = list()
for(i in 1:length(impute_dat_loop_t1)){
  impute_tlc_target_within_results_t1[[i]]=lm(cbind(RAS_1_diff, RAS_2_diff,RAS_3_diff, RAS_5_diff, INQ_1_diff, INQ_2_diff, SIS_1_diff, SSMI_diff) ~ target, data = impute_dat_loop_t1[[i]])
}

impute_tlc_target_within_results_t1_d1 = summary(impute_tlc_target_within_results_t1[[1]])
impute_tlc_target_within_results_t1_d2 = summary(impute_tlc_target_within_results_t1[[2]])
impute_tlc_target_within_results_t1_d3 = summary(impute_tlc_target_within_results_t1[[3]])
impute_tlc_target_within_results_t1_d4 = summary(impute_tlc_target_within_results_t1[[4]])
impute_tlc_target_within_results_t1_d5 = summary(impute_tlc_target_within_results_t1[[5]])

impute_tlc_target_within_results_t1_d1

coefs_1_d1 = list()
ses_1_d1 = list()
for(i in 1:length(impute_tlc_target_within_results_t1_d1)){
  coefs_1_d1[[i]] = impute_tlc_target_within_results_t1_d1[[i]]$coefficients[2,1]
  ses_1_d1[[i]] = impute_tlc_target_within_results_t1_d1[[i]]$coefficients[2,2]
}
coefs_1_d1
coefs_1_d1 = unlist(coefs_1_d1)
coefs_1_d1 = matrix(coefs_1_d1, ncol = 8)
coefs_1_d1

ses_1_d1
ses_1_d1 = unlist(ses_1_d1)
ses_1_d1 = matrix(ses_1_d1, ncol = 8)
ses_1_d1

coefs_1_d2 = list()
ses_1_d2 = list()
for(i in 1:length(impute_tlc_target_within_results_t1_d2)){
  coefs_1_d2[[i]] = impute_tlc_target_within_results_t1_d2[[i]]$coefficients[2,1]
  ses_1_d2[[i]] = impute_tlc_target_within_results_t1_d2[[i]]$coefficients[2,2]
}
coefs_1_d2
coefs_1_d2 = unlist(coefs_1_d2)
coefs_1_d2 = matrix(coefs_1_d2, ncol = 8)
coefs_1_d2

ses_1_d2
ses_1_d2 = unlist(ses_1_d2)
ses_1_d2 = matrix(ses_1_d2, ncol = 8)
ses_1_d2


coefs_1_d3 = list()
ses_1_d3 = list()
for(i in 1:length(impute_tlc_target_within_results_t1_d3)){
  coefs_1_d3[[i]] = impute_tlc_target_within_results_t1_d3[[i]]$coefficients[2,1]
  ses_1_d3[[i]] = impute_tlc_target_within_results_t1_d3[[i]]$coefficients[2,2]
}
coefs_1_d3
coefs_1_d3 = unlist(coefs_1_d3)
coefs_1_d3 = matrix(coefs_1_d3, ncol = 8)
coefs_1_d3

ses_1_d3
ses_1_d3 = unlist(ses_1_d3)
ses_1_d3 = matrix(ses_1_d3, ncol = 8)
ses_1_d3

coefs_1_d4 = list()
ses_1_d4 = list()
for(i in 1:length(impute_tlc_target_within_results_t1_d4)){
  coefs_1_d4[[i]] = impute_tlc_target_within_results_t1_d4[[i]]$coefficients[2,1]
  ses_1_d4[[i]] = impute_tlc_target_within_results_t1_d4[[i]]$coefficients[2,2]
}
coefs_1_d4
coefs_1_d4 = unlist(coefs_1_d4)
coefs_1_d4 = matrix(coefs_1_d4, ncol = 8)
coefs_1_d4

ses_1_d4
ses_1_d4 = unlist(ses_1_d4)
ses_1_d4 = matrix(ses_1_d4, ncol = 8)
ses_1_d4


coefs_1_d5 = list()
ses_1_d5 = list()
for(i in 1:length(impute_tlc_target_within_results_t1_d5)){
  coefs_1_d5[[i]] = impute_tlc_target_within_results_t1_d5[[i]]$coefficients[2,1]
  ses_1_d5[[i]] = impute_tlc_target_within_results_t1_d5[[i]]$coefficients[2,2]
}
coefs_1_d5
coefs_1_d5 = unlist(coefs_1_d5)
coefs_1_d5 = matrix(coefs_1_d5, ncol = 8)
coefs_1_d5

ses_1_d5
ses_1_d5 = unlist(ses_1_d5)
ses_1_d5 = matrix(ses_1_d5, ncol = 8)
ses_1_d5


########## Combine all data
coefs_1_all = rbind(coefs_1_d1, coefs_1_d2, coefs_1_d3, coefs_1_d4, coefs_1_d5)
ses_1_all = rbind(ses_1_d1, ses_1_d2, ses_1_d3, ses_1_d4, ses_1_d5)

coefs_1_ses =  mi.meld(coefs_1_all,ses_1_all)
t_stats_1 = coefs_1_ses$q.mi / coefs_1_ses$se.mi
t_stats_1
#p-value
p_values_1 = round(2*pt(-abs(t_stats_1), df = dim(impute_dat_loop_t1[[1]])[1]-3),3)
p_values_1
#Critical t
critical_ts_1= abs(qt(0.017/2, dim(impute_dat_loop_t1[[1]])[1]-3))
critical_ts_1

#95 CI's
upper_1 = round(coefs_1_ses$q.mi+(critical_ts_1*coefs_1_ses$se.mi),3)
lower_1 = round(coefs_1_ses$q.mi-(critical_ts_1*coefs_1_ses$se.mi),3)
ci_95_1 = paste0(lower_1, sep=",", upper_1)

tlc_target_within_results_t1 = data.frame(t(coefs_1_ses$q.mi), t(coefs_1_ses$se.mi), t(p_values_1), ci_95_1)
colnames(tlc_target_within_results_t1) = c("par_estimate", "se", "p_value", "ci_95")
tlc_target_within_results_t1[,1:2] = round(tlc_target_within_results_t1[,1:2], 3)
tlc_target_within_results_t1$par_estimate = ifelse(tlc_target_within_results_t1$p_value < .017, paste0(tlc_target_within_results_t1$par_estimate, "*"), tlc_target_within_results_t1$par_estimate)

tlc_target_within_results_t1$p_value = ifelse(tlc_target_within_results_t1$p_value < .0009, "<.001", tlc_target_within_results_t1$p_value)
tlc_target_within_results_t1


####### T2 within
impute_tlc_target_within_results_t2 = list()
for(i in 1:length(impute_dat_loop_t2)){
  impute_tlc_target_within_results_t2[[i]]=lm(cbind(RAS_1_diff, RAS_2_diff,RAS_3_diff, RAS_5_diff, INQ_1_diff, INQ_2_diff, SIS_1_diff, SSMI_diff) ~ target, data = impute_dat_loop_t2[[i]])
}

impute_tlc_target_within_results_t2_d1 = summary(impute_tlc_target_within_results_t2[[1]])
impute_tlc_target_within_results_t2_d2 = summary(impute_tlc_target_within_results_t2[[2]])
impute_tlc_target_within_results_t2_d3 = summary(impute_tlc_target_within_results_t2[[3]])
impute_tlc_target_within_results_t2_d4 = summary(impute_tlc_target_within_results_t2[[4]])
impute_tlc_target_within_results_t2_d5 = summary(impute_tlc_target_within_results_t2[[5]])

impute_tlc_target_within_results_t2_d1

coefs_2_d1 = list()
ses_2_d1 = list()


for(i in 1:length(impute_tlc_target_within_results_t2_d1)){
  coefs_2_d1[[i]] = impute_tlc_target_within_results_t2_d1[[i]]$coefficients[2,1]
  ses_2_d1[[i]] = impute_tlc_target_within_results_t2_d1[[i]]$coefficients[2,2]
}
coefs_2_d1
coefs_2_d1 = unlist(coefs_2_d1)
coefs_2_d1 = matrix(coefs_2_d1, ncol = 8)
coefs_2_d1

ses_2_d1
ses_2_d1 = unlist(ses_2_d1)
ses_2_d1 = matrix(ses_2_d1, ncol = 8)
ses_2_d1

coefs_2_d2 = list()
ses_2_d2 = list()
for(i in 1:length(impute_tlc_target_within_results_t2_d2)){
  coefs_2_d2[[i]] = impute_tlc_target_within_results_t2_d2[[i]]$coefficients[2,1]
  ses_2_d2[[i]] = impute_tlc_target_within_results_t2_d2[[i]]$coefficients[2,2]
}
coefs_2_d2
coefs_2_d2 = unlist(coefs_2_d2)
coefs_2_d2 = matrix(coefs_2_d2, ncol = 8)
coefs_2_d2

ses_2_d2
ses_2_d2 = unlist(ses_2_d2)
ses_2_d2 = matrix(ses_2_d2, ncol = 8)
ses_2_d2


coefs_2_d3 = list()
ses_2_d3 = list()
for(i in 1:length(impute_tlc_target_within_results_t2_d3)){
  coefs_2_d3[[i]] = impute_tlc_target_within_results_t2_d3[[i]]$coefficients[2,1]
  ses_2_d3[[i]] = impute_tlc_target_within_results_t2_d3[[i]]$coefficients[2,2]
}
coefs_2_d3
coefs_2_d3 = unlist(coefs_2_d3)
coefs_2_d3 = matrix(coefs_2_d3, ncol = 8)
coefs_2_d3

ses_2_d3
ses_2_d3 = unlist(ses_2_d3)
ses_2_d3 = matrix(ses_2_d3, ncol = 8)
ses_2_d3

coefs_2_d4 = list()
ses_2_d4 = list()
for(i in 1:length(impute_tlc_target_within_results_t2_d4)){
  coefs_2_d4[[i]] = impute_tlc_target_within_results_t2_d4[[i]]$coefficients[2,1]
  ses_2_d4[[i]] = impute_tlc_target_within_results_t2_d4[[i]]$coefficients[2,2]
}
coefs_2_d4
coefs_2_d4 = unlist(coefs_2_d4)
coefs_2_d4 = matrix(coefs_2_d4, ncol = 8)
coefs_2_d4

ses_2_d4
ses_2_d4 = unlist(ses_2_d4)
ses_2_d4 = matrix(ses_2_d4, ncol = 8)
ses_2_d4


coefs_2_d5 = list()
ses_2_d5 = list()
for(i in 1:length(impute_tlc_target_within_results_t2_d5)){
  coefs_2_d5[[i]] = impute_tlc_target_within_results_t2_d5[[i]]$coefficients[2,1]
  ses_2_d5[[i]] = impute_tlc_target_within_results_t2_d5[[i]]$coefficients[2,2]
}
coefs_2_d5
coefs_2_d5 = unlist(coefs_2_d5)
coefs_2_d5 = matrix(coefs_2_d5, ncol = 8)
coefs_2_d5

ses_2_d5
ses_2_d5 = unlist(ses_2_d5)
ses_2_d5 = matrix(ses_2_d5, ncol = 8)
ses_2_d5


########## Combine all data
coefs_2_all = rbind(coefs_2_d1, coefs_2_d2, coefs_2_d3, coefs_2_d4, coefs_2_d5)
ses_2_all = rbind(ses_2_d1, ses_2_d2, ses_2_d3, ses_2_d4, ses_2_d5)

coefs_2_ses =  mi.meld(coefs_2_all,ses_2_all)
t_stats_2 = coefs_2_ses$q.mi / coefs_2_ses$se.mi
t_stats_2
#p-value
p_values_2 = round(2*pt(-abs(t_stats_2), df = dim(impute_dat_loop_t2[[1]])[1]-3),3)
p_values_2
#Critical t
critical_ts_2= abs(qt(0.017/2, dim(impute_dat_loop_t2[[1]])[1]-3))
critical_ts_2

#95 CI's
upper_2 = round(coefs_2_ses$q.mi+(critical_ts_2*coefs_2_ses$se.mi),3)
lower_2 = round(coefs_2_ses$q.mi-(critical_ts_2*coefs_2_ses$se.mi),3)
ci_95_2 = paste0(lower_2, sep=",", upper_2)

tlc_target_within_results_t2 = data.frame(t(coefs_2_ses$q.mi), t(coefs_2_ses$se.mi), t(p_values_2), ci_95_2)
colnames(tlc_target_within_results_t2) = c("par_estimate", "se", "p_value", "ci_95")
tlc_target_within_results_t2[,1:2] = round(tlc_target_within_results_t2[,1:2], 3)
tlc_target_within_results_t2$par_estimate = ifelse(tlc_target_within_results_t2$p_value < .017, paste0(tlc_target_within_results_t2$par_estimate, "*"), tlc_target_within_results_t2$par_estimate)

tlc_target_within_results_t2$p_value = ifelse(tlc_target_within_results_t2$p_value < .0009, "<.001", tlc_target_within_results_t2$p_value)
tlc_target_within_results_t2

########## T3 within
impute_tlc_target_within_results_t3 = list()
for(i in 1:length(impute_dat_loop_t3)){
  impute_tlc_target_within_results_t3[[i]]=lm(cbind(RAS_1_diff, RAS_3_diff,RAS_3_diff, RAS_5_diff, INQ_1_diff, INQ_2_diff, SIS_1_diff, SSMI_diff) ~ target, data = impute_dat_loop_t3[[i]])
}

impute_tlc_target_within_results_t3_d1 = summary(impute_tlc_target_within_results_t3[[1]])
impute_tlc_target_within_results_t3_d2 = summary(impute_tlc_target_within_results_t3[[2]])
impute_tlc_target_within_results_t3_d3 = summary(impute_tlc_target_within_results_t3[[3]])
impute_tlc_target_within_results_t3_d4 = summary(impute_tlc_target_within_results_t3[[4]])
impute_tlc_target_within_results_t3_d5 = summary(impute_tlc_target_within_results_t3[[5]])

impute_tlc_target_within_results_t3_d1

coefs_3_d1 = list()
ses_3_d1 = list()


for(i in 1:length(impute_tlc_target_within_results_t3_d1)){
  coefs_3_d1[[i]] = impute_tlc_target_within_results_t3_d1[[i]]$coefficients[2,1]
  ses_3_d1[[i]] = impute_tlc_target_within_results_t3_d1[[i]]$coefficients[2,2]
}
coefs_3_d1
coefs_3_d1 = unlist(coefs_3_d1)
coefs_3_d1 = matrix(coefs_3_d1, ncol = 8)
coefs_3_d1

ses_3_d1
ses_3_d1 = unlist(ses_3_d1)
ses_3_d1 = matrix(ses_3_d1, ncol = 8)
ses_3_d1

coefs_3_d2 = list()
ses_3_d2 = list()
for(i in 1:length(impute_tlc_target_within_results_t3_d2)){
  coefs_3_d2[[i]] = impute_tlc_target_within_results_t3_d2[[i]]$coefficients[2,1]
  ses_3_d2[[i]] = impute_tlc_target_within_results_t3_d2[[i]]$coefficients[2,2]
}
coefs_3_d2
coefs_3_d2 = unlist(coefs_3_d2)
coefs_3_d2 = matrix(coefs_3_d2, ncol = 8)
coefs_3_d2

ses_3_d2
ses_3_d2 = unlist(ses_3_d2)
ses_3_d2 = matrix(ses_3_d2, ncol = 8)
ses_3_d2


coefs_3_d3 = list()
ses_3_d3 = list()
for(i in 1:length(impute_tlc_target_within_results_t3_d3)){
  coefs_3_d3[[i]] = impute_tlc_target_within_results_t3_d3[[i]]$coefficients[2,1]
  ses_3_d3[[i]] = impute_tlc_target_within_results_t3_d3[[i]]$coefficients[2,2]
}
coefs_3_d3
coefs_3_d3 = unlist(coefs_3_d3)
coefs_3_d3 = matrix(coefs_3_d3, ncol = 8)
coefs_3_d3

ses_3_d3
ses_3_d3 = unlist(ses_3_d3)
ses_3_d3 = matrix(ses_3_d3, ncol = 8)
ses_3_d3

coefs_3_d4 = list()
ses_3_d4 = list()
for(i in 1:length(impute_tlc_target_within_results_t3_d4)){
  coefs_3_d4[[i]] = impute_tlc_target_within_results_t3_d4[[i]]$coefficients[2,1]
  ses_3_d4[[i]] = impute_tlc_target_within_results_t3_d4[[i]]$coefficients[2,2]
}
coefs_3_d4
coefs_3_d4 = unlist(coefs_3_d4)
coefs_3_d4 = matrix(coefs_3_d4, ncol = 8)
coefs_3_d4

ses_3_d4
ses_3_d4 = unlist(ses_3_d4)
ses_3_d4 = matrix(ses_3_d4, ncol = 8)
ses_3_d4


coefs_3_d5 = list()
ses_3_d5 = list()
for(i in 1:length(impute_tlc_target_within_results_t3_d5)){
  coefs_3_d5[[i]] = impute_tlc_target_within_results_t3_d5[[i]]$coefficients[2,1]
  ses_3_d5[[i]] = impute_tlc_target_within_results_t3_d5[[i]]$coefficients[2,2]
}
coefs_3_d5
coefs_3_d5 = unlist(coefs_3_d5)
coefs_3_d5 = matrix(coefs_3_d5, ncol = 8)
coefs_3_d5

ses_3_d5
ses_3_d5 = unlist(ses_3_d5)
ses_3_d5 = matrix(ses_3_d5, ncol = 8)
ses_3_d5


########## Combine all data
coefs_3_all = rbind(coefs_3_d1, coefs_3_d2, coefs_3_d3, coefs_3_d4, coefs_3_d5)
ses_3_all = rbind(ses_3_d1, ses_3_d2, ses_3_d3, ses_3_d4, ses_3_d5)

coefs_3_ses =  mi.meld(coefs_3_all,ses_3_all)
t_stats_3 = coefs_3_ses$q.mi / coefs_3_ses$se.mi
t_stats_3
#p-value
p_values_3 = round(2*pt(-abs(t_stats_3), df = dim(impute_dat_loop_t3[[1]])[1]-3),3)
p_values_3
#Critical t
critical_ts_3= abs(qt(0.017/2, dim(impute_dat_loop_t3[[1]])[1]-3))
critical_ts_3

#95 CI's
upper_3 = round(coefs_3_ses$q.mi+(critical_ts_3*coefs_3_ses$se.mi),3)
lower_3 = round(coefs_3_ses$q.mi-(critical_ts_3*coefs_3_ses$se.mi),3)
ci_95_3 = paste0(lower_3, sep=",", upper_3)

tlc_target_within_results_t3 = data.frame(t(coefs_3_ses$q.mi), t(coefs_3_ses$se.mi), t(p_values_3), ci_95_3)
colnames(tlc_target_within_results_t3) = c("par_estimate", "se", "p_value", "ci_95")
tlc_target_within_results_t3[,1:2] = round(tlc_target_within_results_t3[,1:2], 3)
tlc_target_within_results_t3$par_estimate = ifelse(tlc_target_within_results_t3$p_value < .017, paste0(tlc_target_within_results_t3$par_estimate, "*"), tlc_target_within_results_t3$par_estimate)

tlc_target_within_results_t3$p_value = ifelse(tlc_target_within_results_t3$p_value < .0009, "<.001", tlc_target_within_results_t3$p_value)
tlc_target_within_results_t3

### Combine all results
tlc_target_within_results_all = rbind(tlc_target_within_results_t1, tlc_target_within_results_t2, tlc_target_within_results_t3)
tlc_target_within_results_all
```
###########################
Between tlc target analysis
###########################
```{r}
impute_tlc_target_between_results = list()
for(i in 1:length(out_diff_dat)){
  impute_tlc_target_between_results[[i]]=lm(cbind(RAS_1_diff, RAS_2_diff,RAS_3_diff, RAS_5_diff, INQ_1_diff, INQ_2_diff, SIS_1_diff, SSMI_diff) ~ target*factor(treatment), data = out_diff_dat[[i]])
}
impute_tlc_target_between_results[[1]]
impute_tlc_target_between_results_d1 = summary(impute_tlc_target_between_results[[1]])
impute_tlc_target_between_results_d2 = summary(impute_tlc_target_between_results[[2]])
impute_tlc_target_between_results_d3 = summary(impute_tlc_target_between_results[[3]])
impute_tlc_target_between_results_d4 = summary(impute_tlc_target_between_results[[4]])
impute_tlc_target_between_results_d5 = summary(impute_tlc_target_between_results[[5]])
impute_tlc_target_between_results_d1

impute_tlc_target_between_results_d1[[1]]$coefficients[5:6,1]

coefs_d1 = list()
ses_d1 = list()
for(i in 1:length(impute_tlc_target_between_results_d1)){
  coefs_d1[[i]] = impute_tlc_target_between_results_d1[[i]]$coefficients[5:6,1]
  ses_d1[[i]] = impute_tlc_target_between_results_d1[[i]]$coefficients[5:6,2]
}

coefs_d1 = unlist(coefs_d1)
coefs_d1 = matrix(coefs_d1, ncol = 2, byrow =TRUE)  
coefs_d1_t1vt2 = coefs_d1[,1]
coefs_d1_t1vt3 = coefs_d1[,2]
t(coefs_d1_t1vt3)
ses_d1 = unlist(ses_d1)
ses_d1 = matrix(ses_d1, ncol = 2, byrow =TRUE)  
ses_d1_t1vt2 = ses_d1[,1]
ses_d1_t1vt3 = ses_d1[,1]

### D2
coefs_d2 = list()
ses_d2 = list()
for(i in 1:length(impute_tlc_target_between_results_d2)){
  coefs_d2[[i]] = impute_tlc_target_between_results_d2[[i]]$coefficients[5:6,1]
  ses_d2[[i]] = impute_tlc_target_between_results_d2[[i]]$coefficients[5:6,2]
}


coefs_d2 = unlist(coefs_d2)
coefs_d2 = matrix(coefs_d2, ncol = 2, byrow =TRUE)  
coefs_d2_t1vt2 = coefs_d2[,1]
coefs_d2_t1vt3 = coefs_d2[,2]
t(coefs_d2_t1vt3)
ses_d2 = unlist(ses_d2)
ses_d2 = matrix(ses_d2, ncol = 2, byrow =TRUE)  
ses_d2_t1vt2 = ses_d2[,1]
ses_d2_t1vt3 = ses_d2[,1]


### D3
coefs_d3 = list()
ses_d3 = list()
for(i in 1:length(impute_tlc_target_between_results_d3)){
  coefs_d3[[i]] = impute_tlc_target_between_results_d3[[i]]$coefficients[5:6,1]
  ses_d3[[i]] = impute_tlc_target_between_results_d3[[i]]$coefficients[5:6,2]
}


coefs_d3 = unlist(coefs_d3)
coefs_d3 = matrix(coefs_d3, ncol = 2, byrow =TRUE)  
coefs_d3_t1vt2 = coefs_d3[,1]
coefs_d3_t1vt3 = coefs_d3[,2]
t(coefs_d3_t1vt3)
ses_d3 = unlist(ses_d3)
ses_d3 = matrix(ses_d3, ncol = 2, byrow =TRUE)  
ses_d3_t1vt2 = ses_d3[,1]
ses_d3_t1vt3 = ses_d3[,1]



coefs_d4 = list()
ses_d4 = list()
for(i in 1:length(impute_tlc_target_between_results_d4)){
  coefs_d4[[i]] = impute_tlc_target_between_results_d4[[i]]$coefficients[5:6,1]
  ses_d4[[i]] = impute_tlc_target_between_results_d4[[i]]$coefficients[5:6,2]
}


coefs_d4 = unlist(coefs_d4)
coefs_d4 = matrix(coefs_d4, ncol = 2, byrow =TRUE)  
coefs_d4_t1vt2 = coefs_d4[,1]
coefs_d4_t1vt3 = coefs_d4[,2]
t(coefs_d4_t1vt3)
ses_d4 = unlist(ses_d4)
ses_d4 = matrix(ses_d4, ncol = 2, byrow =TRUE)  
ses_d4_t1vt2 = ses_d4[,1]
ses_d4_t1vt3 = ses_d4[,1]

coefs_d5 = list()
ses_d5 = list()
for(i in 1:length(impute_tlc_target_between_results_d5)){
  coefs_d5[[i]] = impute_tlc_target_between_results_d5[[i]]$coefficients[5:6,1]
  ses_d5[[i]] = impute_tlc_target_between_results_d5[[i]]$coefficients[5:6,2]
}
impute_tlc_target_between_results_d1
coefs_d5 = unlist(coefs_d5)
coefs_d5 = matrix(coefs_d5, ncol = 2, byrow =TRUE)  
coefs_d5_t1vt2 = coefs_d5[,1]
coefs_d5_t1vt3 = coefs_d5[,2]
t(coefs_d5_t1vt3)
ses_d5 = unlist(ses_d5)
ses_d5 = matrix(ses_d5, ncol = 2, byrow =TRUE)  
ses_d5_t1vt2 = ses_d5[,1]
ses_d5_t1vt3 = ses_d5[,1]


coefs_all_t1vt2 = rbind(t(coefs_d1_t1vt2), t(coefs_d2_t1vt2), t(coefs_d3_t1vt2), t(coefs_d4_t1vt2), t(coefs_d5_t1vt2))
coefs_all_t1vt3 = rbind(t(coefs_d1_t1vt3), t(coefs_d2_t1vt3), t(coefs_d3_t1vt3), t(coefs_d4_t1vt3), t(coefs_d5_t1vt3))
coefs_all_t1vt2

ses_all_t1vt2 = rbind(t(ses_d1_t1vt2), t(ses_d2_t1vt2), t(ses_d3_t1vt2), t(ses_d4_t1vt2), t(ses_d5_t1vt2))
ses_all_t1vt3 = rbind(t(ses_d1_t1vt3), t(ses_d2_t1vt3), t(ses_d3_t1vt3), t(ses_d4_t1vt3), t(ses_d5_t1vt3))


coefs_ses_t1vt2 =  mi.meld(coefs_all_t1vt2,ses_all_t1vt2)
coefs_ses_t1vt3 =  mi.meld(coefs_all_t1vt3,ses_all_t1vt3)

coefs_ses_t1vt2 = unlist(coefs_ses_t1vt2)
coefs_ses_t1vt2 = matrix(coefs_ses_t1vt2, ncol = 2)
coefs_ses_t1vt2

coefs_ses_t1vt3 = unlist(coefs_ses_t1vt3)
coefs_ses_t1vt3 = matrix(coefs_ses_t1vt3, ncol = 2)
coefs_ses_t1vt3

coefs_ses = rbind(coefs_ses_t1vt2, coefs_ses_t1vt3)
colnames(coefs_ses) = c("q.mi", "se.mi") 
coefs_ses = data.frame(coefs_ses)
coefs_ses
t_stats = coefs_ses$q.mi / coefs_ses$se.mi
# n - minus 5 for parameters
p_values = round(2*pt(-abs(t_stats), df = dim(out_diff_dat[[1]])[1]-5),3)
#Critical t
critical_ts= abs(qt(0.017/2, df = dim(out_diff_dat[[1]])[1]-5)) 
critical_ts
upper = round(coefs_ses$q.mi+(critical_ts*coefs_ses$se.mi),3)
lower = round(coefs_ses$q.mi-(critical_ts*coefs_ses$se.mi),3)
ci_95 = paste0(lower, sep=",", upper)

tlc_target_between_impute_results = data.frame( par_estimate = coefs_ses$q.mi, se = coefs_ses$se.mi, p_values, ci_95)
tlc_target_between_impute_results[,1:2] = round(tlc_target_between_impute_results[,1:2], 3)
tlc_target_between_impute_results

tlc_target_between_impute_results$par_estimate = ifelse(tlc_target_between_impute_results$p_value < .017, paste0(tlc_target_between_impute_results$par_estimate, "*"), tlc_target_between_impute_results$par_estimate)
tlc_target_between_impute_results

```
########## 
Get contrasts
###########
```{r}
se_con_between_d1 = list()
mean_con_bewteen_d1 = list()
for(i in 1:length(impute_tlc_target_between_results_d1)){
  se_con_between_d1[[i]] = vcov(impute_tlc_target_between_results_d1[[i]])
  se_con_between_d1[[i]] = sqrt((se_con_between_d1[[i]][5,5]+se_con_between_d1[[i]][6,6])-2*se_con_between_d1[[i]][6,5])
  ## Now get difference
  mean_con_bewteen_d1[[i]] = impute_tlc_target_between_results_d1[[i]]$coefficients[5]-impute_tlc_target_between_results_d1[[i]]$coefficients[6]
}
mean_con_bewteen_d1 = unlist(mean_con_bewteen_d1)
mean_con_bewteen_d1 = t(mean_con_bewteen_d1)
mean_con_bewteen_d1

se_con_between_d1 = unlist(se_con_between_d1)
se_con_between_d1 = t(se_con_between_d1)
se_con_between_d1

#### Test that se is right
test_tlc_target_con_dat = out_diff_dat[[1]]
test_tlc_target_con_model = lm(RAS_1_diff ~ factor(treatment)*target, data = test_tlc_target_con_dat)
test_tlc_target_con_model
K = matrix(c(0, 0,0,0,1,-1), ncol = 6, nrow = 1, byrow = TRUE)
t= glht(test_tlc_target_con_model, linfct = K)
summary(t)

se_con_between_d2 = list()
mean_con_bewteen_d2 = list()
for(i in 1:length(impute_tlc_target_between_results_d2)){
  se_con_between_d2[[i]] = vcov(impute_tlc_target_between_results_d2[[i]])
  se_con_between_d2[[i]] = sqrt((se_con_between_d2[[i]][5,5]+se_con_between_d2[[i]][6,6])-2*se_con_between_d2[[i]][6,5])
  ## Now get difference
  mean_con_bewteen_d2[[i]] = impute_tlc_target_between_results_d2[[i]]$coefficients[5]-impute_tlc_target_between_results_d2[[i]]$coefficients[6]
}
mean_con_bewteen_d2 = unlist(mean_con_bewteen_d2)
mean_con_bewteen_d2 = t(mean_con_bewteen_d2)
mean_con_bewteen_d2

se_con_between_d2 = unlist(se_con_between_d2)
se_con_between_d2 = t(se_con_between_d2)
se_con_between_d2

se_con_between_d2 = list()
mean_con_bewteen_d2 = list()
for(i in 1:length(impute_tlc_target_between_results_d2)){
  se_con_between_d2[[i]] = vcov(impute_tlc_target_between_results_d2[[i]])
  se_con_between_d2[[i]] = sqrt((se_con_between_d2[[i]][5,5]+se_con_between_d2[[i]][6,6])-2*se_con_between_d2[[i]][6,5])
  ## Now get difference
  mean_con_bewteen_d2[[i]] = impute_tlc_target_between_results_d2[[i]]$coefficients[5]-impute_tlc_target_between_results_d2[[i]]$coefficients[6]
}
mean_con_bewteen_d2 = unlist(mean_con_bewteen_d2)
mean_con_bewteen_d2 = t(mean_con_bewteen_d2)
mean_con_bewteen_d2

se_con_between_d2 = unlist(se_con_between_d2)
se_con_between_d2 = t(se_con_between_d2)
se_con_between_d2

se_con_between_d3 = list()
mean_con_bewteen_d3 = list()
for(i in 1:length(impute_tlc_target_between_results_d3)){
  se_con_between_d3[[i]] = vcov(impute_tlc_target_between_results_d3[[i]])
  se_con_between_d3[[i]] = sqrt((se_con_between_d3[[i]][5,5]+se_con_between_d3[[i]][6,6])-2*se_con_between_d3[[i]][6,5])
  ## Now get difference
  mean_con_bewteen_d3[[i]] = impute_tlc_target_between_results_d3[[i]]$coefficients[5]-impute_tlc_target_between_results_d3[[i]]$coefficients[6]
}
mean_con_bewteen_d3 = unlist(mean_con_bewteen_d3)
mean_con_bewteen_d3 = t(mean_con_bewteen_d3)
mean_con_bewteen_d3

se_con_between_d3 = unlist(se_con_between_d3)
se_con_between_d3 = t(se_con_between_d3)
se_con_between_d3

se_con_between_d4 = list()
mean_con_bewteen_d4 = list()
for(i in 1:length(impute_tlc_target_between_results_d4)){
  se_con_between_d4[[i]] = vcov(impute_tlc_target_between_results_d4[[i]])
  se_con_between_d4[[i]] = sqrt((se_con_between_d4[[i]][5,5]+se_con_between_d4[[i]][6,6])-2*se_con_between_d4[[i]][6,5])
  ## Now get difference
  mean_con_bewteen_d4[[i]] = impute_tlc_target_between_results_d4[[i]]$coefficients[5]-impute_tlc_target_between_results_d4[[i]]$coefficients[6]
}
mean_con_bewteen_d4 = unlist(mean_con_bewteen_d4)
mean_con_bewteen_d4 = t(mean_con_bewteen_d4)
mean_con_bewteen_d4

se_con_between_d4 = unlist(se_con_between_d4)
se_con_between_d4 = t(se_con_between_d4)
se_con_between_d4

se_con_between_d5 = list()
mean_con_bewteen_d5 = list()
for(i in 1:length(impute_tlc_target_between_results_d5)){
  se_con_between_d5[[i]] = vcov(impute_tlc_target_between_results_d5[[i]])
  se_con_between_d5[[i]] = sqrt((se_con_between_d5[[i]][5,5]+se_con_between_d5[[i]][6,6])-2*se_con_between_d5[[i]][6,5])
  ## Now get difference
  mean_con_bewteen_d5[[i]] = impute_tlc_target_between_results_d5[[i]]$coefficients[5]-impute_tlc_target_between_results_d5[[i]]$coefficients[6]
}
mean_con_bewteen_d5 = unlist(mean_con_bewteen_d5)
mean_con_bewteen_d5 = t(mean_con_bewteen_d5)
mean_con_bewteen_d5

se_con_between_d5 = unlist(se_con_between_d5)
se_con_between_d5 = t(se_con_between_d5)
se_con_between_d5

est_con = rbind(mean_con_bewteen_d1, mean_con_bewteen_d2, mean_con_bewteen_d3, mean_con_bewteen_d4, mean_con_bewteen_d5)

se_con = rbind(se_con_between_d1, se_con_between_d2, se_con_between_d3, se_con_between_d4, se_con_between_d5)

con_between = mi.meld(est_con, se_con)
con_between

critical_t = abs(qt(0.017/2, dim(out_diff_dat[[1]])[[1]]-5))
est_con = data.frame(est_con  = con_between$q.mi)
se_con = data.frame(se_con = con_between$se.mi)
est_se_con = data.frame(est_con = t(est_con), se_con = t(se_con))
t_stats = est_se_con$est_con / est_se_con$se_con
est_se_con$p_values = round(2*pt(-abs(t_stats), df = dim(impute_dat_loop[[1]])[[1]]-5),3)
est_se_con
est_se_con = round(est_se_con,3)
est_se_con
#### 95% ci's
upper = round(est_se_con$est_con +(critical_t*est_se_con$se_con),3)
upper
lower = round(est_se_con$est_con -(critical_t*est_se_con$se_con),3)
lower
ci_95 = paste0(lower, sep =",", upper)
ci_95
est_se_con$ci_95 = ci_95
est_se_con
est_se_con$est_con = ifelse(est_se_con$p_values < .017, paste0(est_se_con$est_con, "*"), est_se_con$est_con)
est_se_con$est_con
est_se_con

```


