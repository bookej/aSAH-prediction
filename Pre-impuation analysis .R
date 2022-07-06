# Import data 
library(readxl)
SAH_outcome_prediction <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction.xlsx", 
                                     na = "NA")
View(SAH_outcome_prediction)

# Assign Data 1 

data1 <- SAH_outcome_prediction

# Sex histogram and summary statistics

Sex <- data2$Sex
class(Sex)
summary(as.factor(Sex))


# Age histogram and summary statistics

Age <- data2$Age
class(Age)
summary(Age)
hist(Age, main="Age Distribution", xlab="Age (years)", xlim= c(20, 90), ylab="Number of Patients", col="cornflowerblue")

# Fisher grade histogram 

fisher <- data2$`Fisher grade`
class(fisher)
table(fisher)
fisher <- as.factor(fisher)
count <- table(fisher)
count
barplot(count, main="Fisher Grade", xlab="Fisher Grade", ylab="Number of Patients", ylim= c(0, 250), col= "cornflowerblue")

# CRP @  NS+Day3+Day5 histogram and summary statistics 

CRPns <- data1$`CRP            NS`
class(CRPns)
summary(CRPns)
hist(CRPns, main="CRP at Neurosurgical centre", xlab="CRP", xlim= c(0, 300), ylab="Number of Patients", ylim = c(0, 500), las=1, breaks= 10, col="cornflowerblue")

CRPd3 <- data1$`CRP              3`
class(CRPd3)
summary(CRPd3)
hist(CRPd3, main="CRP Day 3", xlab="CRP", xlim= c(0, 300), ylab="Number of Patients", ylim = c(0, 300), las=1, breaks= 10, col="cornflowerblue")

CRPd5 <- data1$`CRP             5`
class(CRPd5)
summary(CRPd5)
hist(CRPd5, main="CRP Day 5", xlab="CRP", xlim= c(0, 300), ylab="Number of Patients", ylim = c(0, 300), las=1, breaks= 10, col="cornflowerblue")


# WBC @ NS+Day3+Day5

WBCns <- data1$`WBC          NS`
class(WBCns)
summary(WBCns)
hist(WBCns, main="WBC at Neurosurgical centre", xlab="WBC", xlim= c(0, 40), ylab="Number of Patients", ylim = c(0, 300), las=1, breaks= 10, col="cornflowerblue")

WBCd3 <- data1$`WBC            3`
class(WBCd3)
summary(WBCd3)
hist(WBCd3, main="WBC Day 3", xlab="WBC", xlim= c(0, 40), ylab="Number of Patients", ylim = c(0, 300), las=1, breaks= 10, col="cornflowerblue")

WBCd5 <- data1$`WBC        5`
class(WBCd5)
summary(WBCd5)
hist(WBCd5, main="WBC Day 5", xlab="WBC", xlim= c(0, 40), ylab="Number of Patients", ylim = c(0, 300), las=1, breaks= 7, col="cornflowerblue")

# Sex barplot and summary statistics 

Sex <- data1$Sex
class(Sex)
as.factor(Sex)
table(Sex)/651
percent <- table(Sex)/651
barplot(percent, main="Sex Distribution", xlab="Sex", ylab="%", ylim = c(0.0, 0.8), las=1, names.arg=c("Male", "Female"), col= c("cornflowerblue", "plum"))
        
# Location of aneurysm 

Location_aneurysm <- data2$`Classification of aneurysm`
class(Location_aneurysm)
as.factor(Location_aneurysm)
count <- table(Location_aneurysm)
count
barplot(count, main="Location of Aneurysm", ylim = c(0, 300), xlab="Location", ylab="Number of Patients", las=1, names.arg=c("ICA", "ACA", "MCA", "Posterior"), col= c("cornflowerblue", "plum", "red", "tan"))

#Surgical intervention barplot and summary statistics 

treat <- data2$`Surgical intervention`
class(treat)
as.factor(treat)
count <- table(treat)
count
barplot(count, main="Surgical Intervention", xlim= c(0,500), xlab="Number of Patients", cex.names = 0.5, las=1, names.arg=c("Coil", "Clip", "Glue emolisation", "Angioseal", "No Treatment"), col= rainbow(5), horiz=TRUE)

# mRS @ 3 month follow-up 

mrs3 <- data1$`MRS 3m`
class(mrs3)
as.factor(mrs3)
count <- table(mrs3)
count
barplot(count, main="Modified Rankin at 3 month follow-up", xlab="mRS at 3 months", ylab = "Number of Patients", las=1, col= "cornflowerblue")
summary(mrs3)   


# mRS @ 3 months follow-up of patients with missing data

library(dplyr)
GCS_miss <- data1[is.na(data1$`GCS           NS`) | is.na(data1$`GCS            1`) | is.na(data1$`GCS 3`),]
mrs3_miss1 <- GCS_miss$`MRS 3m`
as.factor(mrs3_miss1)
summary(mrs3_miss1)
count <- table(mrs3_miss1)
count
barplot(count, main="Modified Rankin at 3 month follow-up", xlab="mRS at 3 months", ylab = "Number of Patients", las=1, col= "cornflowerblue")

WFNS_miss <- data1[is.na(data1$`WFNS NS`) | is.na(data1$`WFNS 1`) | is.na(data1$`WFNS 3`),]
mrs3_miss2 <- WFNS_miss$`MRS 3m`
as.factor(mrs3_miss2)
summary(mrs3_miss2)
count <- table(mrs3_miss2)
count
barplot(count, main="Modified Rankin at 3 month follow-up", xlab="mRS at 3 months", ylab = "Number of Patients", las=1, col= "cornflowerblue")

WBC_miss <- data1[is.na(data1$`WBC          NS`) | is.na(data1$`WBC            1`) | is.na(data1$`WBC            3`),]
mrs3_miss3 <- WBC_miss$`MRS 3m`
as.factor(mrs3_miss3)
summary(mrs3_miss3)
count <- table(mrs3_miss3)
count
barplot(count, main="Modified Rankin at 3 month follow-up", xlab="mRS at 3 months", ylab = "Number of Patients", las=1, col= "cornflowerblue")

CRP_miss <- data1[is.na(data1$`CRP            NS`) | is.na(data1$`CRP            1`) | is.na(data1$`CRP              3`),]
mrs3_miss4 <- CRP_miss$`MRS 3m`
as.factor(mrs3_miss4)
summary(mrs3_miss4)
count <- table(mrs3_miss4)
count
barplot(count, main="Modified Rankin at 3 month follow-up", xlab="mRS at 3 months", ylab = "Number of Patients", las=1, col= "cornflowerblue")


nrow(data1[is.na(data1$`GCS           NS`) | is.na(data1$`GCS            1`) | is.na(data1$`GCS 3`),])
data1[is.na(data1$`Eye             ED`) | is.na(data1$`Verbal      ED`) | is.na(data1$`Motor         ED`) | is.na(data1$`GCS          ED`),]
nrow(data1)
dataGCS <- data1[!(is.na(data1$`Eye             ED`) | is.na(data1$`Verbal      ED`) | is.na(data1$`Motor         ED`) | is.na(data1$`GCS          ED`)),]
nrow(dataGCS)


# Dichotomisation of mRS into Good = 0-2 and Bad = 3-6 outcome

g_b <- data1$`Good/Bad outcome`
class(g_b)
as.factor(g_b)
count <- table(g_b)
count
barplot(count, main="Good vs Bad Outcome at 3 months", xlab="Outcome", names.arg=c("Good", "Bad"), ylab = "Number of Patients", ylim= c(0, 500), las=1, col= c("cornflowerblue", "red"))

# GCS in ED vs Age vs outcome 

library(ggplot2)
WFNS <- data1$`WFNS ED`
GCS <-  data1$`GCS          ED`
Good_Bad_Outcome <- g_b
table(Good_Bad_Outcome)
ggplot(data1, aes(GCS, Age, color=as.factor(Good_Bad_Outcome))) + 
  geom_point(shape=16, size=1, colour= )    + 
  labs(x = "GCS in ED", y = "Age", title = "Age vs GCS vs Outcome") +
  scale_colour_discrete(name  ="Good or Bad Outcome")


# Location of aneurysm vs Age vs Good/Bad outcome 

library(ggplot2)
GCS <-  data1$`GCS          ED`
Good_Bad_Outcome <- g_b
ggplot(data1, aes(Location_aneurysm, Age, color=as.factor(Good_Bad_Outcome))) + 
  geom_point(shape=16, size=1) + 
  labs(x = "Location of aneurysm", y = "Age", title = "Age vs Location of Aneurysm vs Outcome") + 
  scale_x_discrete(name = "Location of Aneurysm", limits=c("1","2","3", "4"), labels=c("ICA", "ACA", "MCA", "Posterior")) +
  scale_colour_discrete(name  ="Good or Bad Outcome")

# Visualise missing data 

install.packages("Amelia")
library(Amelia)
library(dplyr)
?select
rm(data1$`WFNS database`)
filterd_data <- data1 %>% select(starts_with("WFNS") | starts_with("GCS"))
missmap(filterd_data, main = "Missing values vs observed", rank.order = FALSE)
install.packages("naniar")
library("naniar")
gg_miss_var(filterd_data)
gg_miss_case(filterd_data)
vis_miss(filterd_data)
?vis_miss

filterd_data <- data1 %>% select(starts_with("WBC") | starts_with("CRP"))
vis_miss(filterd_data)

filterd_data <- data1 %>% select(starts_with("mrs"))
vis_miss(filterd_data)

# Logistic regression for good/bad outcome 

str(data1)
data1$Sex <- as.factor(data1$Sex)
data1$`Surgical intervention`<- as.factor(data1$`Surgical intervention`)
data1$`Classification of aneurysm`<- as.factor(data1$`Classification of aneurysm`)
data1$`Fisher grade` <- as.factor(data1$`Fisher grade`)
data1$`Eye score       ictus` <- as.factor(data1$`Eye score       ictus`)
data1$`Verbal ictus` <- as.factor(data1$`Verbal ictus`)
data1$`Motor ictus`<- as.factor(data1$`Motor ictus`)
data1$`GCS    ictus`<- as.factor(data1$`GCS    ictus`)
data1$`WFNS ictus`<- as.factor(data1$`WFNS ictus`)
data1$`Verbal      ED` <-  as.factor(data1$`Verbal      ED`)
data1$`Eye             ED` <- as.factor(data1$`Eye             ED`)
data1$`Motor         ED`<- as.factor(data1$`Motor         ED`)
data1$`GCS          ED`<- as.factor(data1$`GCS          ED`)
data1$`WFNS ED`<- as.factor(data1$`WFNS ED`)
data1$`Eye            NS`<- as.factor(data1$`Eye            NS`)
data1$`Verbal      NS` <-  as.factor(data1$`Verbal      NS`)
data1$`Motor          NS`<- as.factor(data1$`Motor          NS`)
data1$`GCS           NS`<- as.factor(data1$`GCS           NS`)
data1$`WFNS NS`<- as.factor(data1$`WFNS NS`)
data1$`Eye         PR`<- as.factor(data1$`Eye         PR`)
data1$`Verbal       PR` <-  as.factor(data1$`Verbal       PR`)
data1$`Motor        PR`<- as.factor(data1$`Motor        PR`)
data1$`GCS          PR`<- as.factor(data1$`GCS          PR`)
data1$`WFNS PR`<- as.factor(data1$`WFNS PR`)
data1$`Eye            0`<- as.factor(data1$`Eye            0`)
data1$`Verbal           0`<-  as.factor(data1$`Verbal           0`)
data1$`Motor           0`<- as.factor(data1$`Motor           0`)
data1$`WFNS 0`<- as.factor(data1$`WFNS 0`)
data1$`Eye           1`<- as.factor(data1$`Eye           1`)
data1$`Verbal            1`<-  as.factor(data1$`Verbal            1`)
data1$`Motor              1`<- as.factor(data1$`Motor              1`)
data1$`WFNS 1`<- as.factor(data1$`WFNS 1`)
data1$`Eye                  2`<- as.factor(data1$`Eye                  2`)
data1$`Verbal          2`<-  as.factor(data1$`Verbal          2`)
data1$`Motor              2`<- as.factor(data1$`Motor              2`)
data1$`WFNS 2`<- as.factor(data1$`WFNS 2`)
data1$`Eye              3`<- as.factor(data1$`Eye              3`)
data1$`Verbal        3` <-  as.factor(data1$`Verbal        3`)
data1$`Motor              3`<- as.factor(data1$`Motor              3`)
data1$`WFNS 3`<- as.factor(data1$`WFNS 3`)
data1$`Eye             4`<- as.factor(data1$`Eye             4`)
data1$`Verbal              4`<-  as.factor(data1$`Verbal              4`)
data1$`Motor               4` <- as.factor(data1$`Motor               4`)
data1$`WFNS 4`<- as.factor(data1$`WFNS 4`)
data1$`Eye             5`<- as.factor(data1$`Eye             5`)
data1$`Verbal          5` <-  as.factor(data1$`Verbal          5`)
data1$`Motor              5`<- as.factor(data1$`Motor              5`)
data1$`WFNS 5`<- as.factor(data1$`WFNS 5`)
data1$`Eye              6`<- as.factor(data1$`Eye              6`)
data1$`Verbal           6` <-  as.factor(data1$`Verbal           6`)
data1$`Motor                6`<- as.factor(data1$`Motor                6`)
data1$`WFNS 6`<- as.factor(data1$`WFNS 6`)
data1$`Eye                 7`<- as.factor(data1$`Eye                 7`)
data1$`Verbal            7`<-  as.factor(data1$`Verbal            7`)
data1$`Motor  7`<- as.factor(data1$`Motor  7`)
data1$`WFNS 7`<- as.factor(data1$`WFNS 7`)
data1$`GCS 6` <- as.numeric(data1$`GCS 6`)
data1$`CRP       ED` <- as.numeric(data1$`CRP       ED`)
data1$`CRP               0` <- as.numeric(data1$`CRP               0`)
data1$`CRP            7` <- as.numeric(data1$`CRP            7`)
data1$`WBC        2` <- as.numeric(data1$`WBC        2`)
data1$`Good/Bad outcome` <- as.factor(data1$`Good/Bad outcome`)
str(data1)
summary(data1$`Good/Bad outcome`)     

# Logstic regression for GCS 

## GCS @ Ictus 

nrow(data1[is.na(data1$`Eye score       ictus`) | is.na(data1$`Verbal ictus`) | is.na(data1$`Motor ictus`) | is.na(data1$`GCS    ictus`),])
data1[is.na(data1$`Eye score       ictus`) | is.na(data1$`Verbal ictus`) | is.na(data1$`Motor ictus`) | is.na(data1$`GCS    ictus`),]
nrow(data1)
dataGCS <- data1[!(is.na(data1$`Eye score       ictus`) | is.na(data1$`Verbal ictus`) | is.na(data1$`Motor ictus`) | is.na(data1$`GCS    ictus`)),]
nrow(dataGCS)
ei <- data1$`Eye score       ictus`
vi <- data1$`Verbal ictus`
mi <- data1$`Motor ictus`
gcsi <- data1$`GCS    ictus`
xtabs(~ Good_Bad_Outcome + ei, data = dataGCS)
xtabs(~ Good_Bad_Outcome + vi, data = dataGCS)
xtabs(~ Good_Bad_Outcome + mi, data = dataGCS)
xtabs(~ Good_Bad_Outcome + gcsi, data = dataGCS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ ei, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))
logistic <- glm(as.factor(Good_Bad_Outcome) ~ vi, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))
logistic <- glm(as.factor(Good_Bad_Outcome) ~ mi, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))
logistic <- glm(as.factor(Good_Bad_Outcome) ~ gcsi, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

## GCS @ ED  

nrow(data1[is.na(data1$`Eye             ED`) | is.na(data1$`Verbal      ED`) | is.na(data1$`Motor         ED`) | is.na(data1$`GCS          ED`),])
data1[is.na(data1$`Eye             ED`) | is.na(data1$`Verbal      ED`) | is.na(data1$`Motor         ED`) | is.na(data1$`GCS          ED`),]
nrow(data1)
dataGCS <- data1[!(is.na(data1$`Eye             ED`) | is.na(data1$`Verbal      ED`) | is.na(data1$`Motor         ED`) | is.na(data1$`GCS          ED`)),]
nrow(dataGCS)
eed <- data1$`Eye             ED`
ved <- data1$`Verbal      ED`
med <- data1$`Motor         ED`
gcsed <- data1$`GCS          ED`
xtabs(~ Good_Bad_Outcome + eed, data = dataGCS)
xtabs(~ Good_Bad_Outcome + ved, data = dataGCS)
xtabs(~ Good_Bad_Outcome + med, data = dataGCS)
xtabs(~ Good_Bad_Outcome + gcsed, data = dataGCS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ eed, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))
logistic <- glm(as.factor(Good_Bad_Outcome) ~ ved, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))
logistic <- glm(as.factor(Good_Bad_Outcome) ~ med, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))
logistic <- glm(as.factor(Good_Bad_Outcome) ~ gcsed, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

## GCS @ NS

dataGCS <- data1[!(is.na(data1$`Eye            NS`) | is.na(data1$`Verbal      NS`) | is.na(data1$`Motor          NS`) | is.na(data1$`GCS           NS`)),]
nrow(dataGCS)
ens <- data1$`Eye            NS`
vns <- data1$`Verbal      NS`
mns <- data1$`Motor          NS`
gcsns <- data1$`GCS           NS`
xtabs(~ Good_Bad_Outcome + ens, data = dataGCS)
xtabs(~ Good_Bad_Outcome + vns, data = dataGCS)
xtabs(~ Good_Bad_Outcome + mns, data = dataGCS)
xtabs(~ Good_Bad_Outcome + gcsns, data = dataGCS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ ens, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))
logistic <- glm(as.factor(Good_Bad_Outcome) ~ vns, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))
logistic <- glm(as.factor(Good_Bad_Outcome) ~ mns, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))
logistic <- glm(as.factor(Good_Bad_Outcome) ~ gcsns, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))


## GCS @ PR 

dataGCS <- data1[!(is.na(data1$`Eye         PR`) | is.na(data1$`Verbal       PR`) | is.na(data1$`Motor        PR`) | is.na(data1$`GCS          PR`)),]
nrow(dataGCS)
epr <- data1$`Eye         PR`
vpr <- data1$`Verbal       PR`
mpr <- data1$`Motor        PR`
gcspr <- data1$`GCS          PR`
xtabs(~ Good_Bad_Outcome + epr, data = dataGCS)
xtabs(~ Good_Bad_Outcome + vpr, data = dataGCS)
xtabs(~ Good_Bad_Outcome + mpr, data = dataGCS)
xtabs(~ Good_Bad_Outcome + gcspr, data = dataGCS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ epr, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))
logistic <- glm(as.factor(Good_Bad_Outcome) ~ vpr, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))
logistic <- glm(as.factor(Good_Bad_Outcome) ~ mpr, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))
logistic <- glm(as.factor(Good_Bad_Outcome) ~ gcspr, data = dataGCS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

## Missing data fro GCS 

dataGCS <- data1[!(is.na(data1$`Eye            0`) | is.na(data1$`Verbal           0`) | is.na(data1$`Motor           0`) | is.na(data1$`GCS               0`)),]
nrow(dataGCS)
dataGCS <- data1[!(is.na(data1$`Eye           1`) | is.na(data1$`Verbal            1`) | is.na(data1$`Motor              1`) | is.na(data1$`GCS            1`)),]
nrow(dataGCS)
dataGCS <- data1[!(is.na(data1$`Eye                  2`) | is.na(data1$`Verbal          2`) | is.na(data1$`Motor              2`) | is.na(data1$`GCS 2`)),]
nrow(dataGCS)
dataGCS <- data1[!(is.na(data1$`Eye              3`) | is.na(data1$`Verbal        3`) | is.na(data1$`Motor              3`) | is.na(data1$`GCS 3`)),]
nrow(dataGCS)
dataGCS <- data1[!(is.na(data1$`Eye             4`) | is.na(data1$`Verbal              4`) | is.na(data1$`Motor               4`) | is.na(data1$`GCS 4`)),]
nrow(dataGCS)
dataGCS <- data1[!(is.na(data1$`Eye             5`) | is.na(data1$`Verbal          5`) | is.na(data1$`Motor              5`) | is.na(data1$`GCS 5`)),]
nrow(dataGCS)
dataGCS <- data1[!(is.na(data1$`Eye              6`) | is.na(data1$`Verbal           6`) | is.na(data1$`Motor                6`) | is.na(data1$`GCS 6`)),]
nrow(dataGCS)
dataGCS <- data1[!(is.na(data1$`Eye                 7`) | is.na(data1$`Verbal            7`) | is.na(data1$`Motor  7`) | is.na(data1$`GCS 7`)),]
nrow(dataGCS)

#Logistic regression for WFNS

## WFNS @ Ictus 

nrow(data1[is.na(data1$`WFNS ictus`),])
data1[is.na(data1$`WFNS ictus`),]
nrow(data1)
dataWFNS <- data1[!(is.na(data1$`WFNS ictus`)),]
nrow(dataWFNS)
WFNSi <- data1$`WFNS ictus`
xtabs(~ Good_Bad_Outcome + WFNSi, data = dataWFNS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ WFNSi, data = dataWFNS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

## WFNS @ ED 

nrow(data1[is.na(data1$`WFNS ED`),])
data1[is.na(data1$`WFNS ED`),]
nrow(data1)
dataWFNS <- data1[!(is.na(data1$`WFNS ED`)),]
nrow(dataWFNS)
WFNSed <- data1$`WFNS ED`
xtabs(~ Good_Bad_Outcome + WFNSed, data = dataWFNS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ WFNSed, data = dataWFNS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

## WFNS @ NS

nrow(data1[is.na(data1$`WFNS NS`),])
data1[is.na(data1$`WFNS NS`),]
nrow(data1)
dataWFNS <- data1[!(is.na(data1$`WFNS NS`)),]
nrow(dataWFNS)
WFNSns <- data1$`WFNS NS`
xtabs(~ Good_Bad_Outcome + WFNSns, data = dataWFNS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ WFNSns, data = dataWFNS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

## WFNS @ PR

nrow(data1[is.na(data1$`WFNS PR`),])
data1[is.na(data1$`WFNS PR`),]
nrow(data1)
dataWFNS <- data1[!(is.na(data1$`WFNS PR`)),]
nrow(dataWFNS)
WFNSpr <- data1$`WFNS PR`
xtabs(~ Good_Bad_Outcome + WFNSpr, data = dataWFNS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ WFNSpr, data = dataWFNS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))


## WFNS @ Day 0

nrow(data1[is.na(data1$`WFNS 0`),])
data1[is.na(data1$`WFNS 0`),]
nrow(data1)
dataWFNS <- data1[!(is.na(data1$`WFNS 0`)),]
nrow(dataWFNS)
WFNS0 <- data1$`WFNS 0`
xtabs(~ Good_Bad_Outcome + WFNS0, data = dataWFNS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ WFNS0, data = dataWFNS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))


## WFNS @ Day 1

nrow(data1[is.na(data1$`WFNS 1`),])
data1[is.na(data1$`WFNS 1`),]
nrow(data1)
dataWFNS <- data1[!(is.na(data1$`WFNS 1`)),]
nrow(dataWFNS)
WFNS1 <- data1$`WFNS 1`
xtabs(~ Good_Bad_Outcome + WFNS1, data = dataWFNS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ WFNS1, data = dataWFNS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

## WFNS @ Day 2

nrow(data1[is.na(data1$`WFNS 2`),])
data1[is.na(data1$`WFNS 2`),]
nrow(data1)
dataWFNS <- data1[!(is.na(data1$`WFNS 2`)),]
nrow(dataWFNS)
WFNS2 <- data1$`WFNS 2`
xtabs(~ Good_Bad_Outcome + WFNS2, data = dataWFNS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ WFNS2, data = dataWFNS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

## WFNS @ Day 3

nrow(data1[is.na(data1$`WFNS 3`),])
data1[is.na(data1$`WFNS 3`),]
nrow(data1)
dataWFNS <- data1[!(is.na(data1$`WFNS 3`)),]
nrow(dataWFNS)
WFNS3 <- data1$`WFNS 3`
xtabs(~ Good_Bad_Outcome + WFNS3, data = dataWFNS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ WFNS3, data = dataWFNS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

## WFNS @ Day 4 

nrow(data1[is.na(data1$`WFNS 4`),])
data1[is.na(data1$`WFNS 4`),]
nrow(data1)
dataWFNS <- data1[!(is.na(data1$`WFNS 4`)),]
nrow(dataWFNS)
WFNS4 <- data1$`WFNS 4`
xtabs(~ Good_Bad_Outcome + WFNS4, data = dataWFNS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ WFNS4, data = dataWFNS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

## WFNS @ Day 5

nrow(data1[is.na(data1$`WFNS 5`),])
data1[is.na(data1$`WFNS 5`),]
nrow(data1)
dataWFNS <- data1[!(is.na(data1$`WFNS 5`)),]
nrow(dataWFNS)
WFNS5 <- data1$`WFNS 5`
xtabs(~ Good_Bad_Outcome + WFNS5, data = dataWFNS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ WFNS5, data = dataWFNS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

## WFNS @ Day 6

nrow(data1[is.na(data1$`WFNS 6`),])
data1[is.na(data1$`WFNS 6`),]
nrow(data1)
dataWFNS <- data1[!(is.na(data1$`WFNS 6`)),]
nrow(dataWFNS)
WFNS6 <- data1$`WFNS 6`
xtabs(~ Good_Bad_Outcome + WFNS6, data = dataWFNS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ WFNS6, data = dataWFNS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

## WFNS @ Day 7

nrow(data1[is.na(data1$`WFNS 7`),])
data1[is.na(data1$`WFNS 7`),]
nrow(data1)
dataWFNS <- data1[!(is.na(data1$`WFNS 7`)),]
nrow(dataWFNS)
WFNS7 <- data1$`WFNS 7`
xtabs(~ Good_Bad_Outcome + WFNS7, data = dataWFNS)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ WFNS7, data = dataWFNS, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

#Logistic regression for fisher grade 

row(data1[is.na(data1$`Fisher grade`),])
data1[is.na(data1$`Fisher grade`),]
nrow(data1)
datafish <- data1[!(is.na(data1$`Fisher grade`)),]
nrow(datafish)
fish <- data1$`Fisher grade`
xtabs(~ Good_Bad_Outcome + fish, data = datafish)
logistic <- glm(as.factor(Good_Bad_Outcome) ~ fish, data = datafish, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint(logistic)))

## ggplots of pre-imputation data ##

library(readxl)
SAH_outcome_prediction <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction.xlsx", 
                                     sheet = "Sheet1")
View(SAH_outcome_prediction)
ORs <- SAH_outcome_prediction
head(ORs)
class(WFNS)
wfns <- as.factor(ORs$WFNS)
library(ggplot2)
install.packages("RColorBrewer")
library(RColorBrewer)
Time <- as.factor(ORs$Time)
OR <- ORs$OR
WFNS <- ORs$WFNS
class(ORs$Lower)
class(ORs$Upper)
lower <- ORs$Lower
upper <- ORs$Upper
pd <- position_dodge(0.3) 
ggplot(data=ORs, aes(x=Time, y=OR, color=as.factor(WFNS), group = WFNS)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, linetype = "dashed", position = pd)+
  geom_line(position = pd)+
  geom_point(position= pd, size = 0.5) + 
  labs(x = "Time", y = "Odds Ratio", title = "Odds Ratio of Bad Outcome at 3 months", subtitle = "WFNS Grade") + scale_x_discrete(name = "Time", limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), labels=c("Ictus", "ED", "NS", "PR", "Day 0", "Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7")) +
  theme_light() +
  scale_colour_discrete(name  ="WFNS Grade")

# Import data after LOCF imputation 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Data with exluded cases removed", 
                                            na = "NA")
View(SAH_outcome_prediction_impute)

data2 <- SAH_outcome_prediction_impute

## Check for any missing data for WFNS and mGCS 

install.packages("naniar")
library("naniar")
filterd_data <- data2 %>% select(starts_with("WFNS"))
vis_miss(filterd_data)
filterd_data <- data2 %>% select(starts_with("motor"))
vis_miss(filterd_data)
filterd_data <- data2 %>% select(starts_with("mRS"))
vis_miss(filterd_data)
filterd_data <- data2 %>% select(starts_with("Good"))
vis_miss(filterd_data)

# Imputation of missing WFNS

install.packages("mice")
library("mice")  
summary(data2$`MRS 3m`)
data2$`MRS 3m` <- as.factor(data2$`MRS 3m`)
summary(data2$`MRS 3m`)
library("naniar")
filterd_data <- data2 %>% select(starts_with("MRS 3m") | starts_with("Good/Bad outcome"))
vis_miss(filterd_data)

## Remove the variables not included in model for MRS (WFNS and Fisher included only)

library(readxl)
SAH_outcome_prediction_impute2 <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute2.xlsx", 
                                             na = "NA")
View(SAH_outcome_prediction_impute2)

data3 <- SAH_outcome_prediction_impute2

## Preparing dataset 

head(data3)

### Check data for missing variables
sapply(data3, function(x) sum(is.na(x)))

### Transform the variables into factors or numeric
library(dplyr) 
str(data3)
data3$Fishergrade <- as.factor(data3$Fishergrade)
data3$WFNSictus <- as.factor(data3$WFNSictus)
data3$WFNSNS <- as.factor(data3$WFNSNS)
data3$WFNSED <- as.factor(data3$WFNSED)
data3$WFNSPR <-  as.factor(data3$WFNSPR)
data3$WFNS0 <-  as.factor(data3$WFNS0)
data3$WFNS1 <- as.factor(data3$WFNS1)
data3$WFNS2 <- as.factor(data3$WFNS2)
data3$WFNS3 <- as.factor(data3$WFNS3)
data3$WFNS4 <- as.factor(data3$WFNS4)
data3$WFNS5 <- as.factor(data3$WFNS5)
data3$WFNS6 <- as.factor(data3$WFNS6)
data3$WFNS7 <- as.factor(data3$WFNS7)
data3$MRS3m <- as.factor(data3$MRS3m)
data3$Good_Badoutcome <- as.factor(data3$Good_Badoutcome)
str(data3)


## Mice Imputation

library(mice)
methods(mice)
summary(data3)
data3$Good_Badoutcome <- as.factor(data3$Good_Badoutcome)
my_imp <- mice(data3, 
               m = 5,
               maxit = 20, 
               seed = 500)
summary(my_imp)
my_imp$imp$Good_Badoutcome
my_imp$imp$Fishergrade
my_imp$imp$MRS3m
data4 <- complete(my_imp, 4)
View(data4)

## Export data4

install.packages("openxlsx")
library("openxlsx")
library(dplyr)
install.packages("rio")
library(rio)
export(data4, "complete_dataset.xlsx")

# Post-imputation analysis 

## Repeat descriptive stats for imputed variables 

fisher <- data4$Fishergrade
class(fisher)
table(fisher)
fisher <- as.factor(fisher)
count <- table(fisher)
count
barplot(count, main="Fisher Grade", xlab="Fisher Grade", ylab="Number of Patients", ylim= c(0, 350), col= "cornflowerblue")


mrs3 <- data4$MRS3m
class(mrs3)
as.factor(mrs3)
count <- table(mrs3)
count
barplot(count, main="Modified Rankin at 3 month follow-up", xlab="mRS at 3 months", ylab = "Number of Patients", ylim = c(0, 350), las=1, col= "cornflowerblue")
summary(mrs3)   

g_b <- data4$Good_Badoutcome
class(g_b)
as.factor(g_b)
count <- table(g_b)
count
barplot(count, main="Good vs Bad Outcome at 3 months", xlab="Outcome", names.arg=c("Good", "Bad"), ylab = "Number of Patients", ylim= c(0, 500), las=1, col= c("cornflowerblue", "red"))

## Geom_bar for good and bad outcome time series 

library(ggplot2)

### WFNS

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Data excluded cases removed", 
                                            na = "NA")
View(SAH_outcome_prediction_impute)

data2 <- SAH_outcome_prediction_impute

Good_Badoutcome <- as.factor(data2$`Good/Bad outcome`)  

ggplot(data2,
       aes(x=data2$`WFNS ictus`, fill= Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,400))+
  labs(x = "WFNS Ictus", y = "Number of patients", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("2","1"), labels=c("Bad", "Good")) 
 

ggplot(data2,
       aes(x=data2$`WFNS ED`, fill= Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,400))+
  labs(x = "WFNS ED", y = "Number of patients", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("2","1"), labels=c("Bad", "Good")) 

ggplot(data2,
       aes(x=data2$`WFNS NS`, fill= Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,400))+
  labs(x = "WFNS NS", y = "Number of patients", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("2","1"), labels=c("Bad", "Good")) 

ggplot(data2,
       aes(x=data2$`WFNS PR`, fill= Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,400))+
  labs(x = "WFNS PR", y = "Number of patients", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("2","1"), labels=c("Bad", "Good")) 

ggplot(data2,
       aes(x=data2$`WFNS 7`, fill= Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,400))+
  labs(x = "WFNS Day 7", y = "Number of patients", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("2","1"), labels=c("Bad", "Good")) 

ggplot(data4,
       aes(x=WFNS0, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,400))+
  labs(x = "WFNS Day 0", y = "Number of patients", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data4,
       aes(x=WFNS1, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,400))+
  labs(x = "WFNS Day 1", y = "Number of patients", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data4,
       aes(x=WFNS2, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,400))+
  labs(x = "WFNS Day 2", y = "Number of patients", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data4,
       aes(x=WFNS3, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,400))+
  labs(x = "WFNS Day 3", y = "Number of patients", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data4,
       aes(x=WFNS4, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,400))+
  labs(x = "WFNS Day 4", y = "Number of patients", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data4,
       aes(x=WFNS5, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,400))+
  labs(x = "WFNS Day 5", y = "Number of patients", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data4,
       aes(x=WFNS6, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,400))+
  labs(x = "WFNS Day 6", y = "Number of patients", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data4,
       aes(x=WFNS7, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,400))+
  labs(x = "WFNS Day 7", y = "Number of patients", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

### mGCS

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Data excluded cases removed", 
                                            na = "NA")
View(SAH_outcome_prediction_impute)

data2 <- SAH_outcome_prediction_impute

Good_Badoutcome <- as.factor(data2$`Good/Bad outcome`)  

ggplot(data2, aes(x=data2$motor_cat_7, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + 
  theme_light() +
  coord_cartesian(ylim = c(0,450))+
  labs(x = "Motor Score ED", y = "Number of patients", fill = "Outcome") +
  scale_x_discrete(name = "Motor Score Day 7", limits=c("1", "2", "3"), labels=c("None", "Localising", "Obeying")) +
  scale_fill_discrete(name = "Outcome", limits=c("2", "1"), labels=c("Bad", "Good")) 

ggplot(data5, aes(x=motor_cat_ed, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + 
  theme_light() +
  coord_cartesian(ylim = c(0,450))+
  labs(x = "Motor Score ED", y = "Number of patients", fill = "Outcome") +
  scale_x_discrete(name = "Motor Score ED", limits=c("1", "2", "3"), labels=c("None", "Localising", "Obeying")) +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data5, aes(x=motor_cat_pr, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + 
  theme_light() +
  coord_cartesian(ylim = c(0,450))+
  labs(x = "Motor Score PR", y = "Number of patients", fill = "Outcome") +
  scale_x_discrete(name = "Motor Score PR", limits=c("1", "2", "3"), labels=c("None", "Localising", "Obeying")) +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data5, aes(x=motor_cat_ns, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + 
  theme_light() +
  coord_cartesian(ylim = c(0,450))+
  labs(x = "Motor Score NS", y = "Number of patients", fill = "Outcome") +
  scale_x_discrete(name = "Motor Score NS", limits=c("1", "2", "3"), labels=c("None", "Localising", "Obeying")) +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data5, aes(x=motor_cat_0, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + 
  theme_light() +
  coord_cartesian(ylim = c(0,450))+
  labs(x = "Motor Score Day 0", y = "Number of patients", fill = "Outcome") +
  scale_x_discrete(name = "Motor Score Day 0", limits=c("1", "2", "3"), labels=c("None", "Localising", "Obeying")) +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data5, aes(x=motor_cat_1, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + 
  theme_light() +
  coord_cartesian(ylim = c(0,450))+
  labs(x = "Motor Score Day 1", y = "Number of patients", fill = "Outcome") +
  scale_x_discrete(name = "Motor Score Day 1", limits=c("1", "2", "3"), labels=c("None", "Localising", "Obeying")) +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data5, aes(x=motor_cat_2, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + 
  theme_light() +
  coord_cartesian(ylim = c(0,450))+
  labs(x = "Motor Score Day 2", y = "Number of patients", fill = "Outcome") +
  scale_x_discrete(name = "Motor Score Day 2", limits=c("1", "2", "3"), labels=c("None", "Localising", "Obeying")) +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data5, aes(x=motor_cat_3, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + 
  theme_light() +
  coord_cartesian(ylim = c(0,450))+
  labs(x = "Motor Score Day 3", y = "Number of patients", fill = "Outcome") +
  scale_x_discrete(name = "Motor Score Day 3", limits=c("1", "2", "3"), labels=c("None", "Localising", "Obeying")) +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data5, aes(x=motor_cat_4, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + 
  theme_light() +
  coord_cartesian(ylim = c(0,450))+
  labs(x = "Motor Score Day 4", y = "Number of patients", fill = "Outcome") +
  scale_x_discrete(name = "Motor Score Day 4", limits=c("1", "2", "3"), labels=c("None", "Localising", "Obeying")) +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data5, aes(x=motor_cat_5, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + 
  theme_light() +
  coord_cartesian(ylim = c(0,450))+
  labs(x = "Motor Score Day 5", y = "Number of patients", fill = "Outcome") +
  scale_x_discrete(name = "Motor Score Day 5", limits=c("1", "2", "3"), labels=c("None", "Localising", "Obeying")) +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data5, aes(x=motor_cat_6, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + 
  theme_light() +
  coord_cartesian(ylim = c(0,450))+
  labs(x = "Motor Score Day 6", y = "Number of patients", fill = "Outcome") +
  scale_x_discrete(name = "Motor Score Day 6", limits=c("1", "2", "3"), labels=c("None", "Localising", "Obeying")) +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad")) 

ggplot(data5, aes(x=motor_cat_7, fill=Good_Badoutcome)) +
  geom_bar(position = position_dodge()) + 
  theme_light() +
  coord_cartesian(ylim = c(0,450))+
  labs(x = "Motor Score Day 7", y = "Number of patients", fill = "Outcome") +
  scale_x_discrete(name = "Motor Score Day 7", limits=c("1", "2", "3"), labels=c("None", "Localising", "Obeying")) +
  scale_fill_discrete(name = "Outcome", limits=c("1", "2"), labels=c("Good", "Bad"))

## Making compararitive percentage panel for different scores
### WFNS


install.packages("breakDown")
library(breakDown)
install.packages("dplyr")
library(dplyr)
library(tidyr)
library(scales)

count_data2 <- data2 %>% count(data2$`WFNS PR` | data2$`Good/Bad outcome`)

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Study dataset + PAASH", na = "NA")
View(SAH_outcome_prediction_impute)

data2 <- SAH_outcome_prediction_impute

xtabs(~ data2$`Good/Bad outcome` + data2$`PAASH PR`, data = data2)

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Percentage G B outcome ", na = "NA")
View(SAH_outcome_prediction_impute)

data_percentage <- SAH_outcome_prediction_impute

data_percentage <- data2 %>% count(data2$`WFNS PR`) %>% mutate(per = n / nrow(data2)) 

ggplot(data_percentage, aes(x = data_percentage$`data2$`WFNS PR``))
library(ggplot2)
library(dplyr)
library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Percentage G B outcome", na = "NA")
View(SAH_outcome_prediction_impute)

data_percentage <- SAH_outcome_prediction_impute

good_bad_outcome <- as.factor(data_percentage$good_bad_outcome)
WFNS <- as.factor(data_percentage$WFNS)
Per <- as.factor(data_percentage$Per)
motor <- as.factor(data_percentage$`Motor score`)
motorcat <- as.factor(data_percentage$motorcat)
PAASH <- as.factor(data_percentage$PAASH)
mWFNS <- as.factor((data_percentage$mWFNS))

data_percentage <- filter(data_percentage, Time == "Day 7")

ggplot(data_percentage,
       aes(x= as.factor(WFNS), y = Per, fill = as.factor(good_bad_outcome))) +
  geom_bar(stat= "identity", position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,100))+
  geom_text(aes(label=Per), vjust=1.6, color="black", position=position_dodge(0.9), size=3.5)+
  labs(title = "Day 7", x = "WFNS", y = "Percentage", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("2","1"), labels=c("Poor", "Good")) +
  theme(plot.title = element_text(hjust = 0.5))

## Percentage panel for WFNS 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Study dataset + PAASH", na = "NA")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

xtabs(~ data$`Good/Bad outcome` + data$`mWFNS PR`, data = data)


data2 <- SAH_outcome_prediction_impute$Time[, c("Ictus")]

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Percentage G B outcome ")
View(SAH_outcome_prediction_impute)

data_percentage <- SAH_outcome_prediction_impute

data_percentage <- SAH_outcome_prediction_impute[SAH_outcome_prediction_impute$Time=="Day 7",]

good_bad_outcome <- as.factor(data_percentage$good_bad_outcome)
WFNS <- as.factor(data_percentage$WFNS)

library(ggplot2)

ggplot(data_percentage,
       aes(x= as.factor(mWFNS), y = Per, fill = as.factor(good_bad_outcome))) +
  geom_bar(stat= "identity", position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,100))+
  geom_text(aes(label=Per), vjust=1.6, color="black", position=position_dodge(0.9), size=3.5)+
  labs(x = "mWFNS", y = "Percentage", fill = "Outcome") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name = "Outcome", limits=c("2","1"), labels=c("Bad", "Good")) 

## Motor Cat 

xtabs(~ data2$`Good/Bad outcome` + data2$motor_cat_pr, data = data2)

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Percentage G B outcome ", na = "NA")
View(SAH_outcome_prediction_impute)

data_percentage <- SAH_outcome_prediction_impute

ggplot(data_percentage,
       aes(x= as.factor(motor_cat), y = Per, fill = as.factor(good_bad_outcome))) +
  geom_bar(stat= "identity", position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,100))+
  geom_text(aes(label=Per), vjust=1.6, color="black", position=position_dodge(0.9), size=3.5)+
  labs(x = "Motor Category", y = "Percentage", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("2","1"), labels=c("Bad", "Good")) 

## Motor score 

xtabs(~ data2$`Good/Bad outcome` + data2$`Motor        PR`, data = data2)

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Percentage G B outcome", na = "NA")
View(SAH_outcome_prediction_impute)

data_percentage <- SAH_outcome_prediction_impute

ggplot(data_percentage,
       aes(x= as.factor(data_percentage$`Motor score`), y = Per, fill = as.factor(good_bad_outcome))) +
  geom_bar(stat= "identity", position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,100))+
  geom_text(aes(label=Per), vjust=1.6, color="black", position=position_dodge(0.9), size=3.5)+
  labs(x = "GCS Motor Score", y = "Percentage", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("2","1"), labels=c("Bad", "Good")) 

## PAASH

xtabs(~ data$`Good/Bad outcome` + data$`PAASH PR`, data = data)

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "ROC", na = "NA")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Percentage G B outcome ", na = "NA")
View(SAH_outcome_prediction_impute)


data_percentage <- SAH_outcome_prediction_impute

ggplot(data_percentage,
       aes(x= as.factor(data_percentage$PAASH), y = Per, fill = as.factor(good_bad_outcome))) +
  geom_bar(stat= "identity", position = position_dodge()) + theme_light() +
  coord_cartesian(ylim = c(0,100))+
  geom_text(aes(label=Per), vjust=1.6, color="black", position=position_dodge(0.9), size=3.5)+
  labs(x = "PAASH", y = "Percentage", fill = "Outcome") +
  scale_fill_discrete(name = "Outcome", limits=c("2","1"), labels=c("Bad", "Good")) 


# Logistic regression 

library('readr')
library('tibble')
library('dplyr')
library('ggplot2')

## WFNS

WFNS <- as.factor(data2$`WFNS 7`)
logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ WFNS, data = data2, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))

## Motor Cat 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Study dataset + PAASH", na = "NA")
View(SAH_outcome_prediction_impute)

data2 <- SAH_outcome_prediction_impute

motor_cat <- as.factor(data2$motor_cat_7)
logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(motor_cat), data = data2, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))


#### Logistic regression graph 

WFNSin <- as.numeric(WFNSi)
coef(logistic)
head(predict(logistic))
head(predict(logistic, type = "response"))
model_glm_pred = ifelse(predict(logistic, type = "link") > 0, "Yes", "No")
calc_class_err = function(actual, predicted) {mean(actual != predicted)}
calc_class_err(actual = WFNSi, predicted = model_glm_pred)

plot(as.factor(data5$Good_Badoutcome) ~ WFNSin, data = data5, 
     col = "darkorange", pch = "|", ylim = c(-0.2, 1),
     main = "Using Logistic Regression for Classification")
abline(h = 0, lty = 3)
abline(h = 1, lty = 3)
abline(h = 0.5, lty = 2)
curve(predict(logistic, data.frame(balance = x), type = "response"), 
      add = TRUE, lwd = 3, col = "dodgerblue")
abline(v = -coef(logistic)[1] / coef(logistic)[2], lwd = 2)

### Logistic regression WFNS

WFNSed <- as.factor(data5$`WFNS ED`)
class(WFNSed)
xtabs(~ data5$Good_Badoutcome + WFNSed, data = data5)
logistic <- glm(as.factor(data5$Good_Badoutcome) ~ WFNSed, data = data5, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))

### Logistic regression WFNS+mWFNS

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Study dataset + PAASH", na = "NA")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

WFNS <- as.factor(data$`WFNS PR`)
logistic <- glm(as.factor(data$`Good/Bad outcome`) ~ as.factor(WFNS), data = data, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))

mWFNS <- as.factor(data$`mWFNS PR`)
logistic <- glm(as.factor(data$`Good/Bad outcome`) ~ as.factor(mWFNS), data = data, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))

### Logistic regression motor categories

motor_cat <- as.factor(data$motor_cat_pr)
levels(motor_cat)
motor_cat <- relevel(motor_cat, ref = "3")
levels(motor_cat)
logistic <- glm(as.factor(data$`Good/Bad outcome`) ~ as.factor(motor_cat), data = data, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))

### Logistic regression with redefined motor categories (1-3), 4, 5, 6 

xtabs(~ data$`Motor        PR`)
motorPRnewcat <- recode(data$`Motor        PR`, "1" = "1", "2" = "1", "3" = "1", "4" = "2", "5" = "3", "6" = "4")
xtabs(~ motorPRnewcat + data$`Good/Bad outcome`)
motorPRnewcat <- as.factor(motorPRnewcat)
levels(motorPRnewcat)
motorPRnewcat <- relevel(motorPRnewcat, ref = "4")
levels(motorPRnewcat)
logistic <- glm(as.factor(data$`Good/Bad outcome`) ~ as.factor(motorPRnewcat), data = data, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))

### Logistic regresison PAASH 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Study dataset + PAASH", na = "NA")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

paash <- as.factor(data$`PAASH PR`)
logistic <- glm(as.factor(data$`Good/Bad outcome`) ~ as.factor(paash), data = data, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))

## OR Curve 

library(readxl)
SAH_outcome_prediction_impute2 <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute2.xlsx", 
                                             sheet = "Post imputation")
View(SAH_outcome_prediction_impute2)

DataORs <- SAH_outcome_prediction_impute2

head(DataORs)
class(DataORs$Predictor)
predictor <- as.factor(DataORs$Predictor)
motor_cat <- factor(predictor, levels=c("motor_cat_1", "motor_cat_2", "motor_cat_3"))
str(motor_cat)
table(motor_cat)
view(motor_cat)
table(motor_cat)
class(DataORs$Time)
time <- as.factor(DataORs$Time)
class(DataORs$Lower)
library(ggplot2)
install.packages("RColorBrewer")
library(RColorBrewer)
ORs <- DataORs$OR
lower <- DataORs$Lower
upper <- DataORs$Upper
pd <- position_dodge(0.3) 


ggplot(data=DataORs, aes(x=time, y=ORs, color=as.factor(motor_cat), group = as.factor(motor_cat))) +
  geom_line(position = pd)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, linetype = "dashed", position = pd)+
  geom_point(position= pd, size = 0.5) + 
  ylim(0, 3) +
  labs(x = "Time", y = "Odds Ratio", title = "Odds Ratio of Bad Outcome at 3 months", subtitle = "Predictors of outcome") + 
  scale_x_discrete(name = "Time", limits=c("1","2","3","4"), labels=c("Ictus", "ED", "NS", "PR")) +
  theme_light() +
  scale_colour_discrete(name  ="Motor category", limits = c("motor_cat_1", "motor_cat_2", "motor_cat_3"), labels=c("None", "Localising", "Obeying"))

ggplot(data=DataORs, aes(x=time, y=ORs, color=as.factor(motor_cat), group = as.factor(motor_cat))) +
  geom_line(position = pd)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, linetype = "dashed", position = pd)+
  geom_point(position= pd, size = 0.5) + 
  ylim(0, 8) +
  labs(x = "Time", y = "Odds Ratio", title = "Odds Ratio of Bad Outcome at 3 months", subtitle = "Predictors of outcome") + 
  scale_x_discrete(name = "Time", limits=c("5","6","7","8", "9", "10","11","12"), labels=c("Day 0", "Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7")) +
  theme_light() +
  scale_colour_discrete(name  ="Motor category", limits = c("motor_cat_1", "motor_cat_2", "motor_cat_3"), labels=c("None", "Localising", "Obeying"))

?labs

ggplot(data=DataORs, aes(x=time, y=ORs, color= c("blue", "red", "green"), group = as.factor(motor_cat))) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, linetype = "dashed", position = pd)+
  geom_line(position = pd)+
  geom_point(position= pd, size = 0.5) + 
  labs(x = "Time", y = "Odds Ratio", title = "Odds Ratio of Bad Outcome at 3 months", subtitle = "Predictors of Outcome") + scale_x_discrete(name = "Time", limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), labels=c("Ictus", "ED", "NS", "PR", "Day 0", "Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7")) +
  theme_light() +
  scale_colour_discrete(name  ="Motor Category")

# Boxplots 

library(ggplot2)
library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "ORs WFNS", na = "NA")
View(SAH_outcome_prediction_impute)

data3 <- SAH_outcome_prediction_impute
view(data3)


time <- as.factor(data3$Time)
wfns <- as.factor(data3$WFNS)

ggplot(data3, aes(wfns, data3$OR)) + geom_boxplot() +
  labs(x = "WFNS Day 0 - Day 7", y = "Odds Ratio of Bad Outcome") +
  theme_clean()

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "ORs Motor")
View(SAH_outcome_prediction_impute)

data3 <- SAH_outcome_prediction_impute
view(data3)

time <- as.factor(data3$Time)
motor <- as.factor(data3$Motor)

ggplot(data3, aes(motor, data3$OR)) + geom_boxplot() +
  labs(y = "Odds Ratio of Bad Outcome") +
  scale_x_discrete(name = "Motor Score at acute presentation", limits=c("1","2","3"), labels=c("None", "Localising", "Obeying")) +
  theme_clean()

## Binary logistic regression at PR for different scales 

### WFNS

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "ORs WFNS")
View(SAH_outcome_prediction_impute)

DataORs <- SAH_outcome_prediction_impute

wfns <- as.factor(DataORs$WFNS)
lower <- DataORs$Lower
upper <- DataORs$Upper

neworder <- c("Ictus", "ED", "NS", "PR", "Day 0", "Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7")

library(ggplot2)
library(dplyr)
time2 <- arrange(mutate(DataORs, Time=factor(Time, levels= neworder)), Time)
time3 <- time2[time2$Time=="Ictus":"PR",]                 

ggplot(time2, aes(time2$WFNS, OR)) + 
  geom_point() +
  facet_wrap(~time2$Time)+
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  labs(x = "mWFNS", y = "Odds Ratio of Bad Outcome", title = "WFNS Inpatient Timepoints") +
  theme_grey()

p1 <- ggplot(time2, aes(time2$WFNS, OR)) + 
  geom_point() +
  facet_wrap(~time2$Time)+
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  labs(x = "WFNS", y = "Odds Ratio of Bad Outcome", title = "WFNS Panel") +
  theme_grey()

### mWFNS

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "ORs mWFNS", na = "NA")
View(SAH_outcome_prediction_impute)

DataORs <- SAH_outcome_prediction_impute
time2 <- arrange(mutate(DataORs, Time=factor(Time, levels= neworder)), Time)
mWFNS <- as.factor(time2$mWFNS)  

ggplot(time2, aes(time2$mWFNS, OR)) + 
  geom_point() +
  facet_wrap(~time2$Time)+
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  labs(x = "mWFNS", y = "Odds Ratio of Bad Outcome", title = "mWFNS Panel") +
  theme_grey()

p1 <- ggplot(time2, aes(time2$mWFNS, OR)) + 
  geom_point() +
  facet_wrap(~time2$Time)+
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  labs(x = "mWFNS", y = "Odds Ratio of Bad Outcome", title = "mWFNS Panel") +
  theme_grey()


### Motor Cat

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "ORs Motor cat")
View(SAH_outcome_prediction_impute)

DataORs <- SAH_outcome_prediction_impute
time2 <- arrange(mutate(DataORs, Time=factor(Time, levels= neworder)), Time)

ggplot(time2, aes(Motor, OR)) + 
  geom_point() +
  facet_wrap(~Time)+
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  labs(x = "Motor Category", y = "Odds Ratio of Bad Outcome", title = "Motor Category Panel") +
  scale_x_discrete(name = "Motor Category", limits=c("1","2","3"), labels=c("None", "Localising", "Obeying")) +
  theme_grey()

p2 <- ggplot(time2, aes(Motor, OR)) + 
  geom_point() +
  facet_wrap(~Time)+
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  labs(x = "Motor Category", y = "Odds Ratio of Bad Outcome", title = "Motor Category Panel") +
  theme_grey()

### PAASH

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "ORs PAASH", na = "NA")
View(SAH_outcome_prediction_impute)

DataORs <- SAH_outcome_prediction_impute
time2 <- arrange(mutate(DataORs, Time=factor(Time, levels= neworder)), Time)


ggplot(time2, aes(PAASH, OR)) + 
  geom_point() +
  facet_wrap(~Time)+
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  labs(x = "PAASH", y = "Odds Ratio of Bad Outcome", title = "PAASH Panel") +
  theme_grey()

p3 <- ggplot(time2, aes(PAASH, OR)) + 
  geom_point() +
  facet_wrap(~Time)+
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  labs(x = "PAASH", y = "Odds Ratio of Bad Outcome", title = "PAASH Panel") +
  theme_grey()

### Combine panels 

library(ggplot2)
install.packages("ggpubr")
library(ggpubr)

ggarrange(p1, p2, p3, labels = "AUTO")

## ROC 
?pROC
install.packages("pROC") 
install.packages("randomForest")   
library(pROC)
library(randomForest)
set.seed(420)


### Call in data with equal length WFNS and motor_cat

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "ROC", na = "NA")
View(SAH_outcome_prediction_impute)

data2 <- SAH_outcome_prediction_impute

### WFNS admission timepoints 

logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS ictus`), data = data2, family = "binomial")
summary(logistic)

lines(data2$`WFNS ictus`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS ED`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)

logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS NS`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)

logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS PR`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)


legend("bottomright", title = "WFNS", legend = c("Ictus", "ED", "NS", "PR"), col = c("forestgreen", "cornflowerblue", "red", "orange"), lwd = 4)

### WFNS inpatient timepoints 

logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 0`), data = data2, family = "binomial")
lines(data2$`WFNS 0`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 1, print.auc = 35)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 1`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 1, print.auc = TRUE, print.auc.y = 35)
plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE, print.auc.y = 30)

logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 2`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 1, print.auc = TRUE, add = TRUE, print.auc.y = 25)

logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 3`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 1, print.auc = TRUE, add = TRUE, print.auc.y = 20)

logistic5 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 4`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic5$fitted.values, percent = TRUE, col = "purple", lwd = 1, print.auc = TRUE, add = TRUE, print.auc.y = 15)

logistic6 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 5`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic6$fitted.values, percent = TRUE, col = "black", lwd = 1, print.auc = TRUE, add = TRUE, print.auc.y = 10)

logistic7 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 6`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic7$fitted.values, percent = TRUE, col = "brown", lwd = 1, print.auc = TRUE, add = TRUE, print.auc.y = 5)

logistic8 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 7`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic8$fitted.values, percent = TRUE, col = "pink", lwd = 1, print.auc = TRUE, add = TRUE, print.auc.y = 0)

legend("bottomright", title = "WFNS", legend = c("Day 0", "Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7"), col = c("forestgreen", "cornflowerblue", "red", "orange", "purple", "black", "brown", "pink"), lwd = 2)


### Ictus

logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS ictus`), data = data2, family = "binomial")
lines(data2$`WFNS ictus`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`mWFNS ictus`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)


logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`PAASH ictus`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)


logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`Motor ictus`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)
legend("bottomright", title = "Ictus", legend = c("WFNS", "mWFNS", "PAASH", "Motor score"), col = c("forestgreen", "cornflowerblue", "red", "orange"), lwd = 4)



### ED

logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS ED`), data = data2, family = "binomial")
lines(data2$`WFNS ED`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")


roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`mWFNS ED`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)


logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`PAASH ED`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)


logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`Motor         ED`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)
legend("bottomright", title = "ED", legend = c("WFNS", "mWFNS", "PAASH", "Motor score"), col = c("forestgreen", "cornflowerblue", "red", "orange"), lwd = 4)


### NS 

logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS NS`), data = data2, family = "binomial")
lines(data2$`WFNS NS`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")


roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`mWFNS NS`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
 plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)


logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`PAASH NS`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)


logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`Motor          NS`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)

legend("bottomright", title = "NS", legend = c("WFNS", "mWFNS", "PAASH", "Motor Score"), col = c("forestgreen", "cornflowerblue", "red", "orange"), lwd = 4)


### PR 

logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS PR`), data = data2, family = "binomial")
lines(data2$`WFNS PR`, logistic$fitted.values)
par(pty = "s")


roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`mWFNS PR`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)

logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`PAASH PR`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)


logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`Motor        PR`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)

legend("bottomright", title = "PR", legend = c("WFNS", "mWFNS", "PAASH", "Motor Score"), col = c("forestgreen", "cornflowerblue", "red", "orange"), lwd = 4)



### Day 0

logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 0`), data = data2, family = "binomial")
lines(data2$`WFNS 0`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`mWFNS 0`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)


logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`PAASH 0`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)


logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`Motor           0`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)

legend("bottomright", title = "Day 0", legend = c("WFNS", "mWFNS", "PAASH", "Motor Score"), col = c("forestgreen", "cornflowerblue", "red", "orange"), lwd = 4)

### Day 1

logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 1`), data = data2, family = "binomial")
lines(data2$`WFNS 1`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")


roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`mWFNS 1`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)


logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`PAASH 1`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)


logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`Motor              1`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)
legend("bottomright", title = "Day 1", legend = c("WFNS", "mWFNS", "PAASH", "Motor Score"), col = c("forestgreen", "cornflowerblue", "red", "orange"), lwd = 4)

### Day 2 

logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 2`), data = data2, family = "binomial")
lines(data2$`WFNS 2`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")


roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`mWFNS 2`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)

logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`PAASH 2`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)

logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`Motor              2`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)

legend("bottomright", title = "Day 2", legend = c("WFNS", "mWFNS", "PAASH", "Motor Score"), col = c("forestgreen", "cornflowerblue", "red", "orange"), lwd = 4)


### Day 3

logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 3`), data = data2, family = "binomial")
lines(data2$`WFNS 3`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`mWFNS 3`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)

logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`PAASH 3`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)

logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`Motor              3`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)

legend("bottomright", title = "Day 3", legend = c("WFNS", "mWFNS", "PAASH", "Motor Score"), col = c("forestgreen", "cornflowerblue", "red", "orange"), lwd = 4)

### Day 4 


logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 4`), data = data2, family = "binomial")
lines(data2$`WFNS 4`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`mWFNS 4`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)


logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`PAASH 4`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)


logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`Motor               4`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)

legend("bottomright", title = "Day 4", legend = c("WFNS", "mWFNS", "PAASH", "Motor Score"), col = c("forestgreen", "cornflowerblue", "red", "orange"), lwd = 4)


### Day 5

logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 5`), data = data2, family = "binomial")
lines(data2$`WFNS 5`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`mWFNS 5`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)

logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`PAASH 5`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)

logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`Motor              5`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)
legend("bottomright", title = "Day 5", legend = c("WFNS", "mWFNS", "PAASH", "Motor Score"), col = c("forestgreen", "cornflowerblue", "red", "orange"), lwd = 4)

### Day 6 

logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 6`), data = data2, family = "binomial")
lines(data2$`WFNS 6`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`mWFNS 6`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)

logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`PAASH 6`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)


logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`Motor                6`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)
legend("bottomright", title = "Day 6", legend = c("WFNS", "mWFNS", "PAASH", "Motor Score"), col = c("forestgreen", "cornflowerblue", "red", "orange"), lwd = 4)

### Day 7 


logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`WFNS 7`), data = data2, family = "binomial")
lines(data2$`WFNS 7`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`mWFNS 7`), data = data2, family = "binomial")

roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
plot.roc(data2$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)

logistic3 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`PAASH 7`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)

logistic4 <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`Motor  7`), data = data2, family = "binomial")
plot.roc(data2$`Good/Bad outcome`, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)
legend("bottomright", title = "Day 7", legend = c("WFNS", "mWFNS", "PAASH", "Motor Score"), col = c("forestgreen", "cornflowerblue", "red", "orange"), lwd = 4)


#Confidence intervals 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "ROC")
View(SAH_outcome_prediction_impute)

data2 <- SAH_outcome_prediction_impute

logistic <- glm(as.factor(data2$`Good/Bad outcome`) ~ as.factor(data2$`Motor  7`), data = data2, family = "binomial")
lines(data2$`Motor  7`, logistic$fitted.values)
roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")


roc <- roc(data2$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
           xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

ci(roc)

# Sensitivity analyses 

## Filter for nadir value 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "ROC")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

library(dplyr)


data_nadir <- transform(data, min = pmin(data$`Motor ictus`, data$`Motor         ED`, data$`Motor          NS`, data$`Motor        PR`))

nadir <- data_nadir$min

logistic <- glm(as.factor(data_nadir$Good.Bad.outcome) ~ as.factor(nadir), data = data_nadir, family = "binomial")
summary(logistic)
lines(nadir, logistic$fitted.values)
roc(data_nadir$Good.Bad.outcome, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc <- roc(data_nadir$Good.Bad.outcome, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

ci(roc)

# Filter for Best WFNS 

data_max <- transform(data, max = pmax(data$`Motor ictus`, data$`Motor         ED`, data$`Motor          NS`, data$`Motor        PR`))

best <- data_max$max

logistic <- glm(as.factor(data_max$Good.Bad.outcome) ~ as.factor(best), data = data_max, family = "binomial")
summary(logistic)
lines(best, logistic$fitted.values)
roc(data_max$Good.Bad.outcome, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc <- roc(data_max$Good.Bad.outcome, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
           xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

ci(roc)


## Poor grade only 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Poor grade WFNS", na = "NA")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

data_nadir <- transform(data, max = pmax(data$`WFNS ictus`, data$`WFNS ED`, data$`WFNS NS`, data$`WFNS PR`))

nadir <- data_nadir$max

logistic <- glm(as.factor(data_nadir$Good.Bad.outcome) ~ as.factor(nadir), data = data_nadir, family = "binomial")
lines(nadir, logistic$fitted.values)
roc(data_nadir$Good.Bad.outcome, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc(data_nadir$Good.Bad.outcome, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = 60)

logistic2 <- glm(as.factor(data_nadir$Good.Bad.outcome) ~ as.factor(data_nadir$WFNS.ictus), data = data_nadir, family = "binomial")

roc(data_nadir$Good.Bad.outcome, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 2, print.auc = TRUE, print.auc.y = 60)
plot.roc(data_nadir$Good.Bad.outcome, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 2, add = TRUE, print.auc = TRUE)

logistic3 <- glm(as.factor(data_nadir$Good.Bad.outcome) ~ as.factor(data_nadir$WFNS.ED), data = data_nadir, family = "binomial")
plot.roc(data_nadir$Good.Bad.outcome, logistic3$fitted.values, percent = TRUE, col = "red", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 40)


logistic4 <- glm(as.factor(data_nadir$Good.Bad.outcome) ~ as.factor(data_nadir$WFNS.NS), data = data_nadir, family = "binomial")
plot.roc(data_nadir$Good.Bad.outcome, logistic4$fitted.values, percent = TRUE, col = "orange", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 30)

logistic5 <- glm(as.factor(data_nadir$Good.Bad.outcome) ~ as.factor(data_nadir$WFNS.PR), data = data_nadir, family = "binomial")
plot.roc(data_nadir$Good.Bad.outcome, logistic5$fitted.values, percent = TRUE, col = "pink", lwd = 2, print.auc = TRUE, add = TRUE, print.auc.y = 20)

legend("bottomright", title = "Poor grade WFNS", legend = c("Nadir", "WFNS ictus", "WFNS ED", "WFNS NS", "WFNS PR"), col = c("forestgreen", "cornflowerblue", "red", "orange", "pink"), lwd = 4)


## Best and worst WFNS 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Data excluded cases removed", 
                                            na = "NA")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute
View(data)

### Logistic regression 

bestpre <- as.factor(data$best_WFNS_pretreatment)
class(bestpre)
xtabs(~ data$`Good/Bad outcome` + bestpre, data = data)
logistic <- glm(as.factor(data$`Good/Bad outcome`) ~ bestpre, data = data, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))

bestpost <- as.factor(data$best_WFNS_postreatment)
class(bestpost)
xtabs(~ data$`Good/Bad outcome` + bestpost, data = data)
logistic <- glm(as.factor(data$`Good/Bad outcome`) ~ bestpost, data = data, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))

worstpre <- as.factor(data$worst_WFNS_pretreatment)
class(worstpre)
xtabs(~ data$`Good/Bad outcome` + worstpre, data = data)
logistic <- glm(as.factor(data$`Good/Bad outcome`) ~ worstpre, data = data, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))

worstpost <- as.factor(data$worst_WFNS_posttreatment)
class(worstpost)
xtabs(~ data$`Good/Bad outcome` + worstpost, data = data)
logistic <- glm(as.factor(data$`Good/Bad outcome`) ~ worstpost, data = data, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))

### Geompoint 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Sensitivity analyses", na = "NA")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

time <- as.factor(data$Time)
wfns <- as.factor(data$WFNS)
lower <- data$Lower
upper <- data$Upper

ggplot(data, aes(wfns, data$OR)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  labs(x = "WFNS Day 0 - Day 7", y = "Odds Ratio of Bad Outcome", title = "Worst WFNS post-treatment") +
  theme_grey()

### AUC

## ROC 
?pROC
install.packages("pROC") 
install.packages("randomForest")   
library(pROC)
library(randomForest)
set.seed(420)

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Post", na = "NA")
View(SAH_outcome_prediction_impute)

DataROC <- SAH_outcome_prediction_impute

### Best WFNS  

logistic <- glm(as.factor(DataROC$`Good/Bad outcome`) ~ as.factor(DataROC$best_WFNS_pretreatment), family = "binomial")
lines(DataROC$best_WFNS_pretreatment, logistic$fitted.values)
roc(DataROC$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc(DataROC$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 4, print.auc = TRUE)

logistic2 <- glm(as.factor(DataROC$`Good/Bad outcome`) ~ as.factor(DataROC$best_WFNS_postreatment), data = DataROC, family = "binomial")

roc(DataROC$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 4, print.auc = TRUE)
plot.roc(DataROC$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 4, print.auc = TRUE, add = TRUE, print.auc.y = 40)
legend("bottomright", title = "Best WNFS score", legend = c("Pre-treatment", "Post-treatment"), col = c("forestgreen", "cornflowerblue"), lwd = 4)

### Worst WFNS

logistic <- glm(as.factor(DataROC$`Good/Bad outcome`) ~ as.factor(DataROC$worst_WFNS_pretreatment), family = "binomial")
lines(DataROC$worst_WFNS_pretreatment, logistic$fitted.values)
roc(DataROC$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc(DataROC$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 4, print.auc = TRUE)

logistic2 <- glm(as.factor(DataROC$`Good/Bad outcome`) ~ as.factor(DataROC$worst_WFNS_posttreatment), data = DataROC, family = "binomial")

roc(DataROC$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 4, print.auc = TRUE)
plot.roc(DataROC$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 4, print.auc = TRUE, add = TRUE, print.auc.y = 40)
legend("bottomright", title = "Best WNFS score", legend = c("Pre-treatment", "Post-treatment"), col = c("forestgreen", "cornflowerblue"), lwd = 4)

## Only patients with bad WFNS 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Data excluded cases removed", 
                                            na = "NA")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

worstpre <- data$worst_WFNS_pretreatment
worstpost <- data$worst_WFNS_posttreatment

library(dplyr)

?filter

datapor <- filter(data, worstpre >= 4)
datapoor <- filter(datapor, datapor$worst_WFNS_posttreatment >= 4)       

View(datapoor)

### Logistic regression 

WFNS7 <- as.factor(datapoor$`WFNS 7`)
class(WFNSictus)
xtabs(~ datapoor$`Good/Bad outcome` + WFNS7, data = datapoor)
logistic <- glm(as.factor(datapoor$`Good/Bad outcome`) ~ WFNS7, data = datapoor, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))

### Boxplot

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Boxplot")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

time <- as.factor(data$Time)
wfns <- as.factor(data$WFNS)

ggplot(data, aes(wfns, data$OR)) + 
  geom_boxplot() +
  labs(x = "WFNS Post-treatment", y = "Odds Ratio of Bad Outcome") 

### ROC 

?pROC
install.packages("pROC") 
install.packages("randomForest")   
library(pROC)
library(randomForest)
set.seed(420)
View(datapoor)

logistic <- glm(as.factor(datapoor$`Good/Bad outcome`) ~ as.factor(datapoor$`WFNS PR`), family = "binomial")
lines(datapoor$`WFNS PR`, logistic$fitted.values)
roc(datapoor$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE)
par(pty = "s")

roc(datapoor$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 4, print.auc = TRUE)

logistic2 <- glm(as.factor(datapoor$`Good/Bad outcome`) ~ as.factor(datapoor$`WFNS 5`), data = datapoor, family = "binomial")

roc(datapoor$`Good/Bad outcome`, logistic$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "forestgreen", lwd = 4, print.auc = TRUE)
plot.roc(datapoor$`Good/Bad outcome`, logistic2$fitted.values, percent = TRUE, col = "cornflowerblue", lwd = 4, print.auc = TRUE, add = TRUE, print.auc.y = 40)
legend("bottomright", title = "Best WNFS score", legend = c("Post-resusciation", "Day 5"), col = c("forestgreen", "cornflowerblue"), lwd = 4)

# Ordinal logistic regression 

## Is it possible? 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Study dataset + PAASH", na = "NA")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

xtabs(~ data$`MRS 3m` + data$`WFNS PR`, data = data)

## It is not possible ##

## Analyse only to day 5 and collapse WFNS 2 into 3 

library(dplyr)
data_mutate <- data
data_mutate$`WFNS ictus` <- recode(data_mutate$`WFNS ictus`, "1"="1", "2" = "2", "3"="2", "4"="4", "5"="5")
data_mutate$`WFNS ED` <- recode(data_mutate$`WFNS ED`, "1"="1", "2" = "2", "3"="2", "4"="4", "5"="5")
data_mutate$`WFNS NS` <- recode(data_mutate$`WFNS NS`, "1"="1", "2" = "2", "3"="2", "4"="4", "5"="5")
data_mutate$`WFNS PR` <- recode(data_mutate$`WFNS PR`, "1"="1", "2" = "2", "3"="2", "4"="4", "5"="5")
data_mutate$`WFNS 0` <- recode(data_mutate$`WFNS 0`, "1"="1", "2" = "2", "3"="2", "4"="4", "5"="5")
data_mutate$`WFNS 1` <- recode(data_mutate$`WFNS 1`, "1"="1", "2" = "2", "3"="2", "4"="4", "5"="5")
data_mutate$`WFNS 2` <- recode(data_mutate$`WFNS 2`, "1"="1", "2" = "2", "3"="2", "4"="4", "5"="5")
data_mutate$`WFNS 3` <- recode(data_mutate$`WFNS 3`, "1"="1", "2" = "2", "3"="2", "4"="4", "5"="5")
data_mutate$`WFNS 4` <- recode(data_mutate$`WFNS 4`, "1"="1", "2" = "2", "3"="2", "4"="4", "5"="5")
data_mutate$`WFNS 5` <- recode(data_mutate$`WFNS 5`, "1"="1", "2" = "2", "3"="2", "4"="4", "5"="5")
data_mutate$`WFNS 6` <- recode(data_mutate$`WFNS 6`, "1"="1", "2" = "2", "3"="2", "4"="4", "5"="5")
data_mutate$`WFNS 7` <- recode(data_mutate$`WFNS 7`, "1"="1", "2" = "2", "3"="2", "4"="4", "5"="5")


install.packages("foreign")
install.packages("MASS")
install.packages("Hmisc")
install.packages("reshape2")

data_mutate <- data_mutate[!is.na(data_mutate$`WFNS 0`),]
data_mutate <- data_mutate[!is.na(data_mutate$`WFNS 7`),]

library(ggplot2)

ggplot(data_mutate, aes(x = data_mutate$`WFNS 0`, y = data_mutate$`MRS 3m`)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Description of data", x = "WNFS 0", y= "Modified Rankin Score at 3 months")



library(MASS)
xtabs(~ data_mutate$`WFNS 7`)
m <- polr(as.factor(data_mutate$`MRS 3m`) ~ as.factor(data_mutate$`WFNS 7`), data = data_mutate, Hess=TRUE)
summary(m)
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m))

# Odds ratios and confidence intervals
exp(cbind(OR = coef(m), ci))



## Demographic table 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Study dataset + PAASH")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

mean(data$Age)
sd(data$Age)
?xtabs
xtabs(~ data$`Fisher grade`)
x <- c(14, 39, 147, 213, 230)
x/645


## Probability graphs 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Study dataset + PAASH", na = "NA")
View(SAH_outcome_prediction_impute)

dat <- SAH_outcome_prediction_impute

xtabs(~ dat$`Good/Bad outcome` + dat$`WFNS PR`, data = dat)
logistic <- glm(as.factor(dat$`Good/Bad outcome`) ~ as.factor(dat$`WFNS PR`), data = dat, family = "binomial")
summary(logistic)
exp(cbind(OR = coef(logistic), confint.default(logistic)))

### Chi squared test 
install.packages("aod")
library(aod)
wald.test(b = coef(logistic), Sigma = vcov(logistic), Terms = 1:5)

### Predicted probability 

newdata1 <- with(dat, data.frame(dat$`WFNS PR` <- factor(1:5)))

#unclear if this model should contain multiple scores or should contain multiple timepoints #

#Multilevel modelling 

## Research question is to (1) determine how neurological function varies at different timepoints during admission after aSAH.
## (2) determine which marker of neurolgical function is best at predicting long-term recovery after aSAH according to mRS. 

## Transfer the data to longway 

install.packages("dyplr")

library(readxl)
SAH_outcome_prediction_impute <- read_excel("Desktop/James Docs/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Longform", na = "NA")
View(SAH_outcome_prediction_impute)

dat = SAH_outcome_prediction_impute

install.packages("tidyr")
library("tidyr")

install.packages("dplyr")
library("dplyr")
?pivot_longer

longform <- pivot_longer(data = dat, cols = starts_with("WFNS"), names_to = "Time", values_to = "WFNS")

write.table(longform, file = "longform.csv", sep = ",")

longform <- pivot_longer(data = dat, cols = starts_with("mWFNS"), names_to = "Time_mWFNS", values_to = "mWFNS")

write.table(longform, file = "longform_mWFNS.csv", sep = ",")

longform <- pivot_longer(data = dat, cols = starts_with("GCS"), names_to = "Time_GCS", values_to = "GCS")
write.table(longform, file = "longform_GCS.csv", sep = ",")

longform <- pivot_longer(data = dat, cols = starts_with("PAASH"), names_to = "Time_PAASH", values_to = "PAASH")
write.table(longform, file = "longform_PAASH.csv", sep = ",")

longform <- pivot_longer(data = dat, cols = starts_with("motor_cat"), names_to = "Time_motor", values_to = "Motor")
write.table(longform, file = "longform_motor.csv", sep = ",")

head
head(dat, n= 94)
colnames(dat)
longform <- pivot_longer(data = dat, cols = c("Motor ictus", "Motor         ED", "Motor          NS", "Motor        PR", "Motor           0", "Motor              1", "Motor              2", "Motor              3", "Motor               4", "Motor              5", "Motor                6", "Motor  7"), names_to = "Time_motor", values_to = "Motor")
write.table(longform, file = "longform_motor_score.csv", sep = ",")

longform = dat[, ~datstartswith('motor_cat')]
longform <- pivot_longer(data = dat, cols = starts_with("motor_cat"), names_to = "Time_motor", values_to = "Motor")
write.table(longform, file = "longform_motor.csv", sep = ",")

write.table()

## Upload longform dataset 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Longform", na = "NA")
View(SAH_outcome_prediction_impute)

dat <- SAH_outcome_prediction_impute

## Upload multilevel stats package 

install.packages("lme4")
library(lme4)

library(nlme)

## Examine headers 

colnames(dat)

## run unconditional means model with summary - Using Patient number as the random effect 


?lme
?na.omit
mrs3 <- dat$`MRS 3m`
ID <- dat$Patient
neurolgical_function <- dat$WFNS
View(dat)
?nlme
?lme

## Baseline intercept Mod0

mod0 <- gls(neurolgical_function~1, data = dat, method = "ML", na.action=na.omit )
summary(mod0)

### Random intercept only Mod1 = Unconditional Means Model 

mod1 <- lme(neurolgical_function~1, random = ~1|ID, data = dat, method = "ML", na.action=na.omit )
summary(mod1)

### Intercept 2.30, p = 0, number of groups 645/645 
### Therefore I can reject the null model as intercept significantly varies from 0 

anova(mod0, mod1)

## MLM indicated 

intervals(mod1)

### lower interval 2.21, upper interval = 2.40

## Intra-class correlation coefficient for the unconditional means model 
(1.260272^2)/ ((1.260272^2) + (0.8539285^2))
### = 0.69 therefore a lot of clustering is occurring and multilevel modelling is indicated

## Is time after SAH best represented as a fixed or a random slope 

install.packages("lattice")
library(lattice)
time_in_hosp <- dat$Time

library(dplyr)

dat_subset <- filter(dat, ID >= "1", ID <= "15")

xyplot(dat_subset$WFNS ~ dat_subset$Time | dat_subset$Patient, data = dat_subset, type = c("p", "r"))

### In general there is a fixed negative effect of time on WFNS

## Run the unconditional growth models 
## Unconditional growth model (mod2) - Time as a fixed slope 

mod2 <- lme(neurolgical_function ~ time_in_hosp, random = ~1|ID, data = dat, method = "ML", na.action=na.omit)
summary(mod2)
intervals(mod2)

anova(mod0, mod1, mod2)

### time value = -0.01, p value very significant 1e-04 showing model (time) has fixed slope also improvement in logLik indicating better fit 

intervals(mod2)

## Run the unconditional growth models 
## Unconditional growth model (mod2) - Time as a random slope 

mod3 <- lme(neurolgical_function ~ 1, random = ~ time_in_hosp|ID, data = dat, method = "ML", na.action=na.omit)
summary(mod3)

intervals(mod3)

anova(mod2, mod3)

### Improvement in LogLik when comparing time as fixed to time as random variable
###  -10695.96 vs -10260.75

## Run deviance statistics comparing mod0 with mod1 

(results <- anova(mod0, mod1))
### p = 0.001

## Run deviance statistics comparing mod0 with mod2 

(results <- anova(mod0, mod2))
### p < 0.0001

## Run deviance statistics comparing mod1 with mod2 

(results <- anova(mod1, mod2))
### p < 0.0001
### Therefore improved fit with time as RANDOM effect 

## ICC for 3 models
## Mod 0 = 0.69
(1.260272^2)/ ((1.260272^2) + (0.8539285^2))
## Mod 1 = 0.69
(1.259185^2)/ ((1.259185^2) + (0.8530181^2))
## Mod 2 = 0.77 - accounting for more clustering 
(1.3601727^2)/ ((1.3601727^2) + (0.7508311^2))

## Conditional Growth Model 
### Does Age, sex surgical intervention, classification of aneurysm and fisher grade improve model when incorporated as predictors of WFNS?

age <- dat$Age
surg <- as.factor(dat$`Surgical intervention`)
anur_type <- as.factor(dat$`Classification of aneurysm`)
fisher <- as.factor(dat$`Fisher grade`)
rm(mod4)

mod4 <- lme(neurolgical_function ~ age + surg + fisher + anur_type, random = ~ time_in_hosp|ID, data = dat, method = "ML", na.action=na.omit)

summary(mod4)
intervals(mod4)

anova(mod3, mod4)

### Model fitting improved according to logLik, time_in_hosp + sex has negative interaction and surgery type has a positive interaction 

interaction.plot(dat$Time, dat$Sex, dat$WFNS)

## Deviance statistics 

(results <- anova(mod2, mod3))
# Mod3 is significantly better fit p = 0.0372

## Compare ICC of mod 3 - 0.77
(1.3616254^2)/ ((1.3616254^2) + (0.7508419^2))

## Unexplaiend variance is captured in the full model 
(as.numeric(VarCorr(mod2)[1,1])-as.numeric(VarCorr(mod3)[1,1]))/as.numeric(VarCorr(mod2)[1,1])


## Research question is to (1) determine how neurological function varies at different timepoints during admission after aSAH.
## Answer: neurological function improves at subsequent timepoints and the relationship is fixed. 

## (2) determine which marker of neurological function is best at predicting long-term recovery after aSAH according to mRS. 

library(lme4)

mod3 <- lmer(mrs3~1+(1|ID), data = dat, REML = FALSE)
summary(mod3)

library(jtools)

summ(mod3)

### mod4 add in predictors 

WFNS <- dat$WFNS
mWFNS <- dat$mWFNS
PAASH <- dat$PAASH
Motor <- dat$Motor
Motor_cat <- dat$motor_cat

mod4 <- lmer(mrs3~1 + WFNS + mWFNS + PAASH + Motor + Motor_cat + (1+WFNS+ mWFNS + PAASH + Motor + Motor_cat|ID), data = dat, REML = FALSE)
summary(mod4)
summ(mod4)

## Levels of the multilevel model 
## Level 1 is the timepoint with WFNS
## Level 2 is patients 
## LEvel 3 is outcome good/bad

#Check mRS 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Study dataset + PAASH", na = "NA")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

xtabs(~ data$`Good/Bad outcome` + data$`Motor  7`, data = data)


# NparaLD test 

## mRS as the independent whole-plot factor 
## Time (WFNS) as the dependent sub-plot factor with different treatment groups (WFNS) at each timepoint 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Longform")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

install.packages("nparLD")
library(nparLD)


boxplot(data$`MRS 3m` ~ data$WFNS * data$Time, data = data, names = FALSE)

# Line graph of AUCs 

library(readxl)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "AUC ")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

library(ggplot2)
install.packages("RColorBrewer")
library(RColorBrewer)
Time <- as.factor(data$Time)
AUC <- data$WFNS_AUC
lower <- data$WFNS_lower
upper <- data$WFNS_upper
pd <- position_dodge(0.3) 

ggplot(data = data, aes(x= Time, y= data$WFNS_AUC))+
  geom_point(position= pd, size = 2) + 
  geom_errorbar(aes(ymin= lower, ymax= upper), width=0.8, position = pd)+
  labs(x = "Time", y = "Area Under Curve", title = "Prediction of outcome with WFNS") + scale_x_discrete(name = "Time", limits=c("1","2","3", "4","5", "6", "7", "8", "9", "10", "11", "12"), labels=c("Ictus", "ED", "NS", "PR","Day 0", "Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7")) +
  theme_light()

Admission_data <- filter(data, Time == c(1:4))

ggplot(data = Admission_data, aes(x= Time, y= Admission_data$WFNS_AUC))+
  geom_point(position= pd, size = 2) + 
  geom_errorbar(aes(ymin= Admission_data$WFNS_lower, ymax= Admission_data$WFNS_upper), width=0.8, position = pd)+
  labs(x = "Time", y = "Area Under Curve", title = "Prediction of outcome with WFNS") + scale_x_discrete(name = "Time", limits=c("1","2","3", "4"), labels=c("Ictus", "ED", "NS", "PR")) +
  theme_light()

Inpatient_data <- filter(data, data$Time >= 5)
View(Inpatient_data)

ggplot(data = Inpatient_data, aes(x= Inpatient_data$Time, y= Inpatient_data$WFNS_AUC))+
  geom_errorbar(aes(ymin=Inpatient_data$WFNS_lower, ymax=Inpatient_data$WFNS_upper), width=0.8, position = pd)+
  geom_point(position= pd, size = 2) + 
  labs(x = "Time", y = "Area Under Curve", title = "Prediction of outcome with WFNS") + scale_x_discrete(name = "Time", limits=c(5, 6, 7, 8, 9, 10, 11, 12), labels=c( "Day 0", "Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7")) +
  theme_light()


# Updated patient demographics 
install.packages("gtsummary")
remotes::install_github("ddsjoberg/gtsummary")
library(dplyr)
library(readxl)
library(gtsummary)
SAH_outcome_prediction_impute <- read_excel("~/Documents/Job/Research /Bulters research/SAH when best to predict /Data/SAH_outcome_prediction_impute.xlsx", 
                                            sheet = "Study dataset + PAASH", na = "NA")
View(SAH_outcome_prediction_impute)

data <- SAH_outcome_prediction_impute

dataclean <- data[complete.cases(data$`MRS 3m`),]

xtabs(~ dataclean$`MRS 3m`)
sd(dataclean$Age)
