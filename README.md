# Target-TLC_2019
---
title: "Enhanced Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
#########################
Target Data Cleaning
##########################
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

############
###### Drop these two people they are rentries 394.1, 943.1, 393.1, 406.1, 427.1, 451.1, 564.1, 658.1, 756.1, 1025.1,  1131.1, 1135.1, 1148.1, 1168.1, 1173.1, 1216.1, 1233.1, 1280.1
#datPreAdult = subset(datPreAdult,Adult.ID != 394.1)
#datPreAdult = subset(datPreAdult, Adult.ID != 943.1)
#datPostAdult = subset(datPostAdult, Adult.ID != 394.1)
#datPostAdult = subset(datPostAdult, Adult.ID != 943.1)
#sum(datPreAdult$Adult.ID == 394.1)

### Now merge all data with baseline to assess missingness
datAdult = merge(datPreAdult, datPostAdult, all.x = TRUE, by = "Adult.ID")
dim(datAdult)
dim(datPreAdult)
### merge with treatment
datAdult = merge(datAdult, datAdultTreat, all.x = TRUE, by = "Adult.ID")
dim(datAdult)

##########
### Three double ids need to get rid of 1272, 1280, 1131 
datAdult = datAdult[-c(208,258,263),] 
sum(datAdult$Adult.ID == 1272)
dim(datAdult)

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
sexual_minorty = ifelse(datAdult$SexualOrientation != 3, 1, 0)

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
target_dat = data.frame(ID = datAdult$ID, treatment, age = datAdult$Age, female, non_white, single, sexual_minorty, high_school_greater, employed, RAS_b_1_average, RAS_b_2_average, RAS_b_3_average, RAS_b_5_average, INQ_b_1_average, INQ_b_2_average, SIS_b_1_average,SSMI_b_average, RAS_d_1_average, RAS_d_2_average, RAS_d_3_average, RAS_d_5_average, INQ_d_1_average, INQ_d_2_average, SIS_d_1_average,SSMI_d_average)

target_dat

```
##############
Target
Assess missing data
#####################
```{r}
library(MissMech)
library(naniar)
#TestMCARNormality(dat_pre_post_adult[,10:92])
dim(target_dat)
### percentage of missing per variable
# Get rid of Added.y almost all missing
miss_var_summary(target_dat)
miss_case_summary(target_dat)
prop_miss_case(target_dat)
dim(target_dat_complete)[1]



```



####################
Target Descriptives
###################
```{r}
target_dat_complete[,c(2,4:9)] = apply(target_dat_complete[,c(2,4:9)], 2, function(x){as.factor(x)})
des_target_dat_complete= describe(target_dat_complete)
des_target_dat_complete_num =  data.frame(des_target_dat_complete$Numeric)
des_target_dat_complete_num = des_target_dat_complete_num[c(1,4),]
des_target_dat_complete_num = t(des_target_dat_complete_num)

des_target_dat_complete_num

des_target_dat_complete$Factor$ID = NULL
des_target_dat_complete_fac = data.frame(des_target_dat_complete$Factor)
des_target_dat_complete_fac = round(t(des_target_dat_complete_fac),3)
des_target_dat_complete_fac

############# Range
des_target_range = apply(target_dat_complete[,10:25], 2, range)
des_target_range = data.frame(des_target_range)
des_target_range = t(des_target_range)
des_target_range = round(des_target_range, 3)
des_target_range = data.frame(des_target_range)
des_target_range$range = paste0(des_target_range$X1, sep=",", des_target_range$X2)
des_target_range[,1:2] = NULL
des_target_range

```
#################
Within Study Target
Checking assumptions for t-tests
Not normal 
#########################
```{r}
within_target_norm = target_dat_complete[,10:25]
log_within_target_norm = log(within_target_norm)
results_hist_norm = list()
results_stat_norm = list()
log_hist_norm = list()
apply(log_within_target_norm, 2, hist)
apply(log_within_target_norm, 2, )

for(i in 1:length(within_target_norm)){
  results_hist_norm[[i]] = hist(within_target_norm[[i]])
  log_hist_norm[[i]] = hist(log_within_target_norm[[i]])
  results_stat_norm[[i]] = shapiro.test(within_target_norm[[i]])
}
log_hist_norm
```
###################
Run robust regression with time as covariate  
Check if R^2 changes
```{r}
target_dat_complete_long = reshape(target_dat_complete, varying = list(c("RAS_b_1_average","RAS_d_1_average"),c("RAS_b_2_average", "RAS_d_2_average"), c("RAS_b_3_average", "RAS_d_3_average"), c("RAS_b_5_average", "RAS_d_5_average"), c("INQ_b_1_average", "INQ_d_1_average"), c("INQ_b_2_average", "INQ_d_2_average"), c("SIS_b_1_average" ,"SIS_d_1_average"), c("SSMI_b_average", "SSMI_d_average")), direction = "long", times =c(0,1))

target_dat_complete_long

```
#################################################
Generate regression format for excel pasting
#################################################
```{r}
library(gvlma)
### Create three data sets 
target_dat_complete_long_t1 = subset(target_dat_complete_long, treatment == 1)
target_dat_complete_long_t2 = subset(target_dat_complete_long, treatment == 2)
target_dat_complete_long_t3 = subset(target_dat_complete_long, treatment == 3)
outcomes_within_target_t1 = target_dat_complete_long_t1[,11:18]
results_within_target_t1 = list()
results_within_target_t1_sum = list() 
results_within_target_t1_check = list()
results_within_target_t1_confin = list()
results_within_target_t1_f_2 = list()
results_within_target_t1_sum_pars = list()
library(MASS)

library(forecast)
for(i in 1:length(outcomes_within_target_t1)){
  results_within_target_t1[[i]] = lm(outcomes_within_target_t1[[i]] ~ time, data = target_dat_complete_long_t1)
  results_within_target_t1_sum[[i]] = summary(results_within_target_t1[[i]])
  results_within_target_t1_sum_pars[[i]] =results_within_target_t1_sum[[i]][[4]][2,c(1:2,4)]
  results_within_target_t1_check[[i]] = gvlma(results_within_target_t1[[i]])
  results_within_target_t1_confin[[i]] = confint(results_within_target_t1[[i]]) 
  results_within_target_t1_confin[[i]] = results_within_target_t1_confin[[i]][2,]
  results_within_target_t1_f_2[[i]] = results_within_target_t1_sum[[i]]$adj.r.squared/(1-results_within_target_t1_sum[[i]]$adj.r.squared)
}
summary(results_within_target_t1_check[[1]])

### Figure out how to get them into a format for excel with variable, parameter estimate, se, confint, and f^2
results_within_target_t1_sum_pars = unlist(results_within_target_t1_sum_pars)
results_within_target_t1_sum_pars = matrix(results_within_target_t1_sum_pars, ncol= 3, byrow = TRUE)
colnames(results_within_target_t1_sum_pars) = c("par_est", "se", "p_value")
results_within_target_t1_sum_pars = round(results_within_target_t1_sum_pars, 3)
results_within_target_t1_sum_pars = data.frame(results_within_target_t1_sum_pars)
results_within_target_t1_sum_pars

###### Clean up the confin 
results_within_target_t1_confin= unlist(results_within_target_t1_confin)
results_within_target_t1_confin = matrix(results_within_target_t1_confin, ncol = 2, byrow = TRUE)
results_within_target_t1_confin = data.frame(results_within_target_t1_confin)
results_within_target_t1_confin = round(results_within_target_t1_confin, 3)
results_within_target_t1_confin$ci = paste0(results_within_target_t1_confin$X1, sep=",", results_within_target_t1_confin$X2)
results_within_target_t1_confin = results_within_target_t1_confin[,3]
results_within_target_t1_confin = data.frame(ci = results_within_target_t1_confin)
results_within_target_t1_confin


### Add f^2
results_within_target_t1_f_2 = unlist(results_within_target_t1_f_2)
results_within_target_t1_f_2 = matrix(results_within_target_t1_f_2, ncol = 1, byrow = TRUE)
results_within_target_t1_f_2 = round(results_within_target_t1_f_2, 3)
colnames(results_within_target_t1_f_2) = "f_2"
results_within_target_t1_f_2 = data.frame(results_within_target_t1_f_2)
results_within_target_t1_f_2


#### All data
all_results_within_target_1 = data.frame(results_within_target_t1_sum_pars, ci = results_within_target_t1_confin$ci, f_2 = results_within_target_t1_f_2$f_2)
all_results_within_target_1

```
Within target 1 Standardized version
Between target 1 regular
Between target 1 standard
SIS variance explained version

Now try compare all treatments
```{r}
outcomes_within_target = target_dat_complete_long[,11:18]

results_within_target = list()
results_within_target_sum = list() 
results_within_target_t1_check = list()
results_within_target_t1_confin = list()
results_within_target_t1_f_2 = list()
results_within_target_t1_sum_pars = list()

for(i in 1:length(outcomes_within_target_t1)){
  results_within_target[[i]] = lm(outcomes_within_target[[i]]~ treatment*time, data = target_dat_complete_long)
  results_within_target_sum[[i]] = summary(results_within_target[[i]])
  results_within_target_sum[[i]] = results_within_target_sum[[i]]$coefficients[c(5:6), c(1,2,4)]
}
results_within_target_sum
```


