---
title: "DD Var Select"
author: "PJH"
date: '2022-11-15'
output: html_document
---

```{r}
rm(list=ls())

data$sex_0y<-as.factor(data$sex_0y)
data$married_0y<-as.factor(data$married_0y)
data$parent_identity_0y<-as.factor(data$parent_identity_0y)
data$race_ethnicity_0y<-as.factor(data$race_ethnicity_0y)


library(fastDummies)

data<-dummy_cols(data, select_columns = c('sex_0y', 'married_0y', 'race_g', 'parent_identity_0y', 'race_ethnicity_0y'))

```



```{r}
####GPS Variable Selection using Random Forests (Boruta)####

library(Boruta)

gps_list<-c('cpeur2', 'eaeur1', 'mddeur6', 'depmulti', 'bmimulti', 'iqeur2', 'insomniaeur6', 'snoringeur1',
       'happieur4', 'ghappieur2', 'ghappimeaneur1', 'ghappihealth6', 'alcdep_eurauto',
       'asdauto', 'aspauto', 'bipauto', 'cannabisauto', 'crossauto', 'drinkauto', 'edauto', 'neuroticismauto', 'ocdauto',
       'risk4pcauto', 'risktolauto', 'scz_eurauto', 'smokerauto', 'worryauto', 'anxietyauto',
       'ptsdeur4', 'adhdeur6')
GPS<-data[gps_list]
GPS<-cbind(GPS, data['subjectkey'])
covariates<-c('subjectkey', 'age_0y', 'bmi_0y', 'income_0y', 'high_educ_0y', 'parent_age_0y', 'history_ratio_0y', 'vol',
              'sex_0y_F', 'race_ethnicity_0y_2', 'race_ethnicity_0y_3', 'race_ethnicity_0y_4', 'race_ethnicity_0y_5',
              'married_0y_2', 'married_0y_3', 'married_0y_4', 'married_0y_5', 'married_0y_6',
              'parent_identity_0y_2', 'parent_identity_0y_3', 'parent_identity_0y_4', 'parent_identity_0y_5')

COV<-as.data.frame(data[, covariates])
GPSCOV<-merge(GPS, COV, by='subjectkey')

X<-subset(data.frame(GPSCOV), select = -c(subjectkey))
y<-as.vector(data[, 'discount_rate_1y'])

outcome<-as.data.frame(data[, 'discount_rate_1y'])
dataBoruta<-cbind(GPSCOV, outcome)


set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
res<-Boruta(x=X, y=y, pValue = 0.05, mcAdj = TRUE, maxRuns = 5000)
print(res)

result<-as.data.frame(attStats(res))
attStats(res)
openxlsx::write.xlsx(result, rowNames=TRUE, "ABCD DD Boruta_GPS_results2.xlsx")

```



```{r}
####Structure MRI Variable Selection using Random Forests (Boruta)####

library(Boruta)

smri<-as.data.frame(data[, c(61:1244)])
smri<-cbind(smri, data['subjectkey'])
covariates<-c('subjectkey', 'age_0y', 'bmi_0y', 'income_0y', 'high_educ_0y', 'parent_age_0y', 'history_ratio_0y', 'vol',
              'sex_0y_F', 'race_ethnicity_0y_2', 'race_ethnicity_0y_3', 'race_ethnicity_0y_4', 'race_ethnicity_0y_5',
              'married_0y_2', 'married_0y_3', 'married_0y_4', 'married_0y_5', 'married_0y_6',
              'parent_identity_0y_2', 'parent_identity_0y_3', 'parent_identity_0y_4', 'parent_identity_0y_5')

COV<-as.data.frame(data[, covariates])
smriCOV<-merge(smri, COV, by='subjectkey')

X<-subset(data.frame(smriCOV), select = -c(subjectkey))
y<-as.vector(data[, 'discount_rate_1y'])

outcome<-as.data.frame(data[, 'discount_rate_1y'])
dataBoruta<-cbind(smriCOV, outcome)


set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
res<-Boruta(x=X, y=y, pValue = 0.05, mcAdj = TRUE, maxRuns = 5000)
print(res)

result<-as.data.frame(attStats(res))

openxlsx::write.xlsx(result, rowNames=TRUE, "ABCD DD Boruta_sMRI_results2.xlsx")

```



```{r}
####MID task fMRI Variable Selection using Random Forests (Boruta)####

library(Boruta)

fmri<-as.data.frame(data[, c(1246:2224)])
fmri<-cbind(fmri, data['subjectkey'])
covariates<-c('subjectkey', 'age_0y', 'bmi_0y', 'income_0y', 'high_educ_0y', 'parent_age_0y', 'history_ratio_0y', 'vol',
              'sex_0y_F', 'race_ethnicity_0y_2', 'race_ethnicity_0y_3', 'race_ethnicity_0y_4', 'race_ethnicity_0y_5',
              'married_0y_2', 'married_0y_3', 'married_0y_4', 'married_0y_5', 'married_0y_6',
              'parent_identity_0y_2', 'parent_identity_0y_3', 'parent_identity_0y_4', 'parent_identity_0y_5')

COV<-as.data.frame(data[, covariates])
fmriCOV<-merge(fmri, COV, by='subjectkey')

X<-subset(data.frame(fmriCOV), select = -c(subjectkey))
y<-as.vector(data[, 'discount_rate_1y'])

outcome<-as.data.frame(data[, 'discount_rate_1y'])
dataBoruta<-cbind(fmriCOV, outcome)

set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
res<-Boruta(x=X, y=y, pValue = 0.05, mcAdj = TRUE, maxRuns = 5000)
print(res)

result<-as.data.frame(attStats(res))

openxlsx::write.xlsx(result, rowNames=TRUE, "ABCD DD Boruta_MID task fMRI_results2.xlsx")

```

