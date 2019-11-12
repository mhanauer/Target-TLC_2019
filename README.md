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

head(tlc_data_analysis_average)

tlc_data_analysis_average

########## Invalid parts
## Excluding .1's those without a TXPackageAssigned package assigned
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
whole_number =  is.wholenumber(tlc_data_analysis_average$YouthID)
tlc_data_analysis_average$whole_number = whole_number
tlc_data_analysis_average = subset(tlc_data_analysis_average, whole_number == TRUE)
dim(tlc_data_analysis_average)
tlc_data_analysis_average$whole_number = NULL
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

#### Get the missing ids for Rachel
target_id_treat = data.frame(id = datAdult$Adult.ID, treat = datAdult$Treatment)
target_id_treat
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
whole_number =  is.wholenumber(target_id_treat$id)
target_id_treat$whole_number = whole_number
dim(target_id_treat)
target_id_treat = subset(target_id_treat, whole_number == TRUE)
dim(target_id_treat)
target_id_treat$whole_number = NULL
target_id_treat$na_true = is.na(target_id_treat$treat)
target_id_treat = subset(target_id_treat, na_true == TRUE)
dim(target_id_treat)
write.csv(target_id_treat, "target_id_treat.csv", row.names = FALSE)



##########
#### Three double ids need to get rid of 1272, 1280, 1131 
datAdult = datAdult[-c(259, 262,208),] 
describe.factor(datAdult$Adult.ID)

### Now get rid of .1's
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
whole_number =  is.wholenumber(datAdult$Adult.ID)
datAdult$whole_number = whole_number
dim(datAdult)
datAdult = subset(datAdult, whole_number == TRUE)
dim(datAdult)
datAdult$whole_number = NULL

colnames(datAdult) = c("ID", "Age", "Gender", "Race", "SexualOrientation", "RelationshipStatus", "Edu", "Employment", "RAS1_b", "RAS2_b", "RAS3_b", "RAS4_b", "RAS5_b", "RAS6_b", "RAS7_b", "RAS8_b", "RAS9_b", "RAS10_b", "RAS11_b", "RAS12_b", "RAS13_b", "RAS14_b", "RAS15_b", "RAS16_b", "RAS17_b", "RAS18_b", "RAS19_b", "RAS20_b", "INQ1_b", "INQ2_b", "INQ3_b", "INQ4_b", "INQ5_b", "INQ6_b", "INQ7_b", "INQ8_b", "INQ9_b", "INQ10_b", "SSMI1_b", "SSMI2_b", "SSMI3_b", "SSMI4_b", "SSMI5_b", "SIS1_b", "SIS2_b", "SIS3_b", "SIS4_b", "SIS5_b", "SIS6_b", "SIS7_b", "RAS1_d", "RAS2_d", "RAS3_d", "RAS4_d", "RAS5_d", "RAS6_d", "RAS7_d", "RAS8_d", "RAS9_d", "RAS10_d", "RAS11_d", "RAS12_d", "RAS13_d", "RAS14_d", "RAS15_d", "RAS16_d", "RAS17_d", "RAS18_d", "RAS19_d", "RAS20_d", "INQ1_d", "INQ2_d", "INQ3_d", "INQ4_d", "INQ5_d", "INQ6_d", "INQ7_d", "INQ8_d", "INQ9_d", "INQ10_d", "SSMI1_d", "SSMI2_d", "SSMI3_d", "SSMI4_d", "SSMI5_d", "SIS1_d", "SIS2_d", "SIS3_d", "SIS4_d", "SIS5_d", "SIS6_d", "SIS7_d", "Treatment")
describe.factor(datAdult$Treatment)

# Two treatments have B with space first so try and recode those as just B's
datAdult$Treatment = ifelse(datAdult$Treatment == "A", 1, ifelse(datAdult$Treatment =="B", 2, ifelse(datAdult$Treatment == " B", 2, ifelse(datAdult$Treatment == "C", 3, datAdult$Treatment)))) 
describe.factor(datAdult$Treatment)

### Werid B changed to 6 so changing it back
datAdult$Treatment = ifelse(datAdult$Treatment == 6 , 2, datAdult$Treatment)
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

target_dat

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
### Get SSMI after SIS and drop SIS 2 and PHQ-9
tlc_dat_com$PHQ9_b = NULL
tlc_dat_com$PHQ9_d = NULL
tlc_dat_com$SIS_b_2_average = NULL
tlc_dat_com$SIS_d_2_average = NULL
head(tlc_dat_com)
dim(tlc_dat_com)
tlc_dat_com[,21:22]
tlc_dat_com = data.frame(tlc_dat_com[,1:12], SIS_b_1_average = tlc_dat_com[,14], SSMI_b_average = tlc_dat_com[,13], tlc_dat_com[15:20], SIS_d_1_average = tlc_dat_com[,22], SSMI_d_average= tlc_dat_com[,21])
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
#######################
Now get imputted data
#######################
```{r}

```



