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



## Now merge everything
datAdult = merge(datPreAdult, datPostAdult, by = "Adult.ID", all.y = TRUE, sort = TRUE)
head(datAdult)
dim(datAdult)

### Seemed to solve the problem, but check again
describe.factor(datAdult$Adult.ID)


head(datAdultTreat)

## If you don't have a treatment you cannot be included
datAdult = merge(datAdult, datAdultTreat, by = "Adult.ID", sort = TRUE)
head(datAdult)
dim(datAdult)

### This is the actual sample size, because you cannot be included in the study if you do not have a treatment
dim(datAdult)
head(datAdult)

colnames(datAdult) = c("ID", "Age", "Gender", "Race", "SexualOrientation", "RelationshipStatus", "Edu", "Employment", "RAS1_b", "RAS2_b", "RAS3_b", "RAS4_b", "RAS5_b", "RAS6_b", "RAS7_b", "RAS8_b", "RAS9_b", "RAS10_b", "RAS11_b", "RAS12_b", "RAS13_b", "RAS14_b", "RAS15_b", "RAS16_b", "RAS17_b", "RAS18_b", "RAS19_b", "RAS20_b", "INQ1_b", "INQ2_b", "INQ3_b", "INQ4_b", "INQ5_b", "INQ6_b", "INQ7_b", "INQ8_b", "INQ9_b", "INQ10_b", "SSMI1_b", "SSMI2_b", "SSMI3_b", "SSMI4_b", "SSMI5_b", "SIS1_b", "SIS2_b", "SIS3_b", "SIS4_b", "SIS5_b", "SIS6_b", "SIS7_b", "RAS1_d", "RAS2_d", "RAS3_d", "RAS4_d", "RAS5_d", "RAS6_d", "RAS7_d", "RAS8_d", "RAS9_d", "RAS10_d", "RAS11_d", "RAS12_d", "RAS13_d", "RAS14_d", "RAS15_d", "RAS16_d", "RAS17_d", "RAS18_d", "RAS19_d", "RAS20_d", "INQ1_d", "INQ2_d", "INQ3_d", "INQ4_d", "INQ5_d", "INQ6_d", "INQ7_d", "INQ8_d", "INQ9_d", "INQ10_d", "SSMI1_d", "SSMI2_d", "SSMI3_d", "SSMI4_d", "SSMI5_d", "SIS1_d", "SIS2_d", "SIS3_d", "SIS4_d", "SIS5_d", "SIS6_d", "SIS7_d", "Treatment")

# Two treatments have B with space first so try and recode those as just B's
datAdult$Treatment = ifelse(datAdult$Treatment == "A", 1, ifelse(datAdult$Treatment =="B", 2, ifelse(datAdult$Treatment == " B", 2, ifelse(datAdult$Treatment == "C", 3, datAdult$Treatment)))) 
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

#Subscale 2 for SIS: e-g: 5:7
SIS_b_2_average =  datAdult[,48:50]
SIS_b_2_average = apply(SIS_b_2_average, 1, mean, na.rm = TRUE)

### SSMI
SSMI_b_average = datAdult[,39:43]

########
# Now discharge
#########
# Subscale one: f =6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RAS_d_1_average = datAdult[,c(57:63, 70)]
RAS_d_1_average = apply(RAS_d_1_average,1,mean, na.rm = TRUE)

# Subscale two q = 17, r= 18, s= 19
RAS_d_2_average = datAdult[,c(25:27)]
RAS_d_2_average = apply(RAS_d_2_average, 1, mean, na.rm = TRUE)

# Subscale three: a = 1, b = 2, c = 3, d= 4, e = 5
RAS_d_3_average = datAdult[,9:13]
RAS_d_3_average = apply(RAS_d_3_average, 1, mean, na.rm = TRUE)

# Subscale five: n = 14, o = 15, p = 16
RAS_d_5_average = datAdult[,22:24]
RAS_d_5_average = apply(RAS_d_5_average, 1, mean, na.rm = TRUE)

#Subscale 1 for INQ: a = 1, b = 2, c = 3, d = 4, e = 5
INQ_d_1_average = datAdult[,29:33]
INQ_d_1_average = apply(INQ_d_1_average, 1, mean, na.rm = TRUE)

#Subscale 2 for INQ: f-j: 6-10
INQ_d_2_average = datAdult[,35:38]
INQ_d_2_average = apply(INQ_d_2_average, 1, mean, na.rm = TRUE)

#Subscale 1 for SIS: a-d: 1:4
SIS_d_1_average  = datAdult[,44:47]
SIS_d_1_average  = apply(SIS_d_1_average , 1, mean, na.rm = TRUE)

#Subscale 2 for SIS: e-g: 5:7
SIS_d_2_average =  datAdult[,48:50]
SIS_d_2_average = apply(SIS_d_2_average, 1, mean, na.rm = TRUE)

### SSMI
SSMI_d_average = datAdult[,39:43]


#####################################################################################################
# Data cleaning for TLC Connect Youth
#####################################################################################################
datAdult

describe.factor(datAdult$Treatment)



describe.factor(datAdult$ID)
write.csv(datAdult, "datAdult.csv", row.names = FALSE)

```
Target data cleaning
```{r}




datAdult$INQ6 = ifelse(datAdult$INQ6 == 1, 5, ifelse(datAdult$INQ6 == 2,4, ifelse(datAdult$INQ6  == 3,3, ifelse(datAdult$INQ6  == 4,2, ifelse(datAdult$INQ6  == 5,1,datAdult$INQ6)))))

datAdult$INQ7= ifelse(datAdult$INQ7== 1, 5, ifelse(datAdult$INQ7== 2,4, ifelse(datAdult$INQ7 == 3,3, ifelse(datAdult$INQ7 == 4,2, ifelse(datAdult$INQ7 == 5,1,datAdult$INQ7)))))

datAdult$INQ10= ifelse(datAdult$INQ10== 1, 5, ifelse(datAdult$INQ10== 2,4, ifelse(datAdult$INQ10 == 3,3, ifelse(datAdult$INQ10 == 4,2, ifelse(datAdult$INQ10 == 5,1,datAdult$INQ10)))))


head(datAdult)

#Checking for issues with adult data set
## In the paper start with 115, because we have three less people later
## 763.0 likely double entry 1131.0, 1272
datAdult[c(187:189, 227:229), c(1:5)]

datAdult = datAdult[-c(187,189, 227,229),]

## Two people dropped, because of two dups
dim(datAdult)
describe.factor(datAdult$Time)
### SO 116 is the analytical starting place#######

datDemo = datAdult[,c(1:10)]


# Create pre data sets for psychometrics
head(datAdult)
RAS = datAdult[c(11:30)]
head(RAS)

head(datAdult)
INQ = datAdult[c(31:40)]
head(INQ)

SSMI = datAdult[c(41:45)]
head(SSMI)

SIS = datAdult[c(46:52)]
SIS

# Subscale one: f =6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RASSub1 = RAS[c(6:13, 20)]

# Subscale two q = 17, r= 18, s= 19
head(RAS)
RASSub2 = RAS[c(17:19)]
# Subscale three: a = 1, b = 2, c = 3, d= 4, e = 5
head(RAS)
RASSub3 = RAS[c(1:5)]

# Subscale five: n = 14, o = 15, p = 16
head(RAS)
RASSub5 = RAS[c(14:16)]


#Subscale 1 for INQ: a = 1, b = 2, c = 3, d = 4, e = 5
INQSub1 = INQ[c(1:5)]

#Subscale 2 for INQ: f-j: 6-10
INQSub2 = INQ[c(6:10)]

#Subscale 1 for SIS: a-d: 1:4
SISSub1 = SIS[c(1:4)]

#Subscale 2 for SIS: e-g: 5:7
SISSub2 = SIS[c(5:7)]

SSMIPrePost = datAdult[c(41:45)]
# Creating sum scores for the data analysis that contains all data not just pre data
datAdultDemos = datAdult[c(1:10)]


RASTotalScoreF1 = rowMeans(RASSub1, na.rm = TRUE)
RASTotalScoreF2 = rowMeans(RASSub2, na.rm = TRUE)
RASTotalScoreF3 = rowMeans(RASSub3, na.rm = TRUE)
RASTotalScoreF5 = rowMeans(RASSub5, na.rm = TRUE)
INQTotalScoreF1 = rowMeans(INQSub1, na.rm = TRUE)
INQTotalScoreF2 = rowMeans(INQSub2, na.rm = TRUE)
SISTotalScoreF1 = rowMeans(SISSub1, na.rm = TRUE)
SISTotalScoreF2 = rowMeans(SISSub2, na.rm = TRUE)
SSMITotalScore = rowMeans(SSMIPrePost, na.rm = TRUE)



datAdultAnalysis = data.frame(datDemo, RASTotalScoreF1, RASTotalScoreF2, RASTotalScoreF3, RASTotalScoreF5, INQTotalScoreF1, INQTotalScoreF2, SISTotalScoreF1, SISTotalScoreF2, SSMITotalScore)
dim(datAdultAnalysis)

#Need code gender, race, sexual orientation, edu, employment, RelationshipStatus as binary
#Gender: 2 = 1, 1 = 0 female
#Race: 7 = 0, all else 1 non-white
#Sex Orien: 3 = 0, all else 1 sexual minrotiry
#Edu: 2 = 1, all else = 0; high school over lower for one
#Employment 1 = 1 else = 0; unemployed versus everyone else
#Relationship Status: 1,2,3,4 = 1 else = 0 single


datAdultAnalysis$Gender = ifelse(datAdultAnalysis$Gender == 2,1, 0)
datAdultAnalysis$Race = ifelse(datAdultAnalysis$Race == 7,0, 1)
datAdultAnalysis$SexualOrientation = ifelse(datAdultAnalysis$SexualOrientation == 3,0, 1)
datAdultAnalysis$Edu = ifelse(datAdultAnalysis$Edu <= 2,1, 0)
datAdultAnalysis$Employment = ifelse(datAdultAnalysis$Employment == 1,1, 0)

datAdultAnalysis$RelationshipStatus = ifelse(datAdultAnalysis$RelationshipStatus <= 4, 1, 0)
describe.factor(datAdultAnalysis$RelationshipStatus)
# For the complete data set I need to drop SIS, because there is a ton of missing data
dim(datAdultAnalysis)
describe.factor(datAdultAnalysis$SISTotalScoreF2)
describe.factor(datAdultAnalysis$SISTotalScoreF1)

datAdultAnalysis$SISTotalScoreF2 = NULL

#### Make the data wide again 
datAdultAnalysis
```


