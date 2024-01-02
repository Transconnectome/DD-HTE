rm(list=ls())
dd_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain discounting\\Data\\ABCD Delay discounting merged_wide_kNN imputed dataset.csv", header=TRUE)

dd<-c('subjectkey', 'sex_0y', 'married_0y', 'income_0y', 'age_0y', 'race_g', 'race_ethnicity_0y', 'high_educ_0y', 'bmi_0y',
      'history_ratio_0y', 'abcd_site_0y', 'discount_rate_1y',
      'totalscore_pps_1y', 'totalscore_pps_2y', 'distress_score_pps_1y', 'distress_score_pps_2y',
      'section8_0y', 'rh_adi_perc1_0y', 'parent_age_0y', 'parent_identity_0y', 
      'JB_val_total_1y', 'nihtbx_totalcomp_uncorrected_0y',
      'cpeur2', 'eaeur1', 'depeur4', 'mddeur6', 'depmulti', 'bmieur4', 'bmimulti', 'iqeur2', 'insomniaeur6', 'snoringeur1',
      'happieur4', 'ghappieur2', 'ghappimeaneur1', 'ghappihealth6', 'alcdep_eurauto', 'alcdep_afrauto', 'alcdep_metaauto',
      'asdauto', 'aspauto', 'bipauto', 'cannabisauto', 'crossauto', 'drinkauto', 'edauto', 'neuroticismauto', 'ocdauto',
      'risk4pcauto', 'risktolauto', 'scz_eurauto', 'scz_easauto', 'scz_metaauto', 'smokerauto', 'worryauto', 'anxietyauto',
      'ptsdeur4', 'ptsdmeta6', 'adhdeur6', 'vol')


smri_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\mor.some.qc.desikan.csv", header=TRUE)

MID_fMRI_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\ABCD MID task fMRI Desikan_all_baseline.csv", header=TRUE)

MID_fMRI_data<-subset(data.frame(MID_fMRI_data), select = -c(eventname, interview_date, 
                                                             interview_age, sex, visit,
                                                             imgincl_t1w_include, imgincl_t2w_include, imgincl_dmri_include,
                                                             imgincl_rsfmri_include, imgincl_mid_include, imgincl_nback_include,
                                                             imgincl_sst_include, tfmri_mid_all_b_tr, tfmri_mid_all_b_numtrs,
                                                             tfmri_mid_all_b_dof, tfmri_mid_all_b_nvols, tfmri_mid_all_b_subthreshnvols, 
                                                             tfmri_mid_all_b_meanmotion, tfmri_mid_all_b_maxmotion, 
                                                             tfmri_mid_all_b_meantrans, tfmri_mid_all_b_maxtrans, tfmri_mid_all_b_meanrot,
                                                             tfmri_mid_all_b_maxrot))

dd_s<-dd_data[,dd]
MERGED1<-merge(dd_s, smri_data, by='subjectkey')
MERGED2<-merge(MERGED1, MID_fMRI_data, by='subjectkey')
data<-subset(MERGED2, JB_val_total_1y==1)
data<-na.omit(data)
print(paste0("Sample Size = ", nrow(data)))

#########################Scale Continuous Variables#############################
#Structure MRI
# grep('lh_bankssts_area._.1', colnames(data))
# grep('CerebralWhiteMatterVol._.18', colnames(data))
data[, c(65:463)]<-scale(data.frame(data[, c(65:463)]))

#MID task fMRI
# grep("tfmri_ma_arvn_b_cds_bkslh", colnames(data))
# grep('tfmri_ma_alvsl_b_scs_vtdcrh', colnames(data))
data[, c(464:1443)]<-scale(data.frame(data[, c(464:1443)]))


#Other
contvar<-c("age_0y", "bmi_0y", "history_ratio_0y", 
       "income_0y", "high_educ_0y", "parent_age_0y", "vol", "rh_adi_perc1_0y",
       "discount_rate_1y", "totalscore_pps_1y", "totalscore_pps_2y", 
       "distress_score_pps_1y", "distress_score_pps_2y", "nihtbx_totalcomp_uncorrected_0y", 
       'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y')

data[contvar]<-scale(data.frame(data[contvar]))


#################################Binary Treatment & Covariates######################################

data$rh_adi_bi_0y = ifelse(data$rh_adi_perc1_0y >= mean(data$rh_adi_perc1_0y), 1, 0)

# data$sex_0y<-as.factor(data$sex_0y)
# data$married_0y<-as.factor(data$married_0y)
# data$parent_identity_0y<-as.factor(data$parent_identity_0y)
# data$race_ethnicity_0y<-as.factor(data$race_ethnicity_0y)


library(fastDummies)

data<-dummy_cols(data, select_columns = c('sex_0y', 'married_0y', 'race_g', 'parent_identity_0y', 'race_ethnicity_0y'))


library(learnr)
library(DiagrammeR)
library(grf)
library(tidyverse)
library(broom)
library(fastDummies)
library(lmtest)
library(rpart)
library(glmnet)
library(splines)
library(MASS)
library(sandwich)
library(ggplot2)
library(data.table)

covariates<-c('age_0y', 'bmi_0y', 'income_0y', 
                 'high_educ_0y', 'sex_0y_F', 
                 'married_0y_2', 'married_0y_3', 'married_0y_4', 'married_0y_5', 'married_0y_6',
                 'parent_identity_0y_2','parent_identity_0y_3', 
                 'parent_identity_0y_4', 'parent_identity_0y_5',
                 'race_ethnicity_0y_2', 'race_ethnicity_0y_3', 
                 'race_ethnicity_0y_4', 'race_ethnicity_0y_5',
                 'parent_age_0y', 
                 'nihtbx_totalcomp_uncorrected_0y', 'history_ratio_0y')

W = data$rh_adi_bi_0y
X = data[, covariates]
Z = data$section8_0y
n = nrow(data)


####################### ATE: Total Score PLEs 1 year ###################################

y<-'totalscore_pps_1y'
  
Y <- data[, y]
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
cforest <- instrumental_forest(X, Y, W, Z,
                               num.trees = 40000,
                                 tune.parameters = c('sample.fraction', 'mtry', 'min.node.size', 'alpha', 'imbalance.penalty'),
                                 tune.num.trees = 3000,
                                 tune.num.reps = 200,
                                 tune.num.draws = 2000)
  
  
  
  ############### AIPW Average Treatment Effect ##################
  aipw.scores<-get_scores(cforest, num.trees.for.weights = 2000)
  ate.aipw.est <- mean(aipw.scores)
  ate.aipw.se <- sd(aipw.scores) / sqrt(n)
  ate.aipw.tstat <- ate.aipw.est / ate.aipw.se
  ate.aipw.pvalue <- 2*(1-pnorm(abs(ate.aipw.tstat)))
  lowci<- ate.aipw.est + -1 * qnorm(0.975) * ate.aipw.se 
  highci<- ate.aipw.est + 1 * qnorm(0.975) * ate.aipw.se 
  ate.aipw.results <- as.data.frame(cbind(ate.aipw.est, ate.aipw.se, ate.aipw.tstat, ate.aipw.pvalue, lowci, highci))
  rownames(ate.aipw.results)<-y
  colnames(ate.aipw.results)<-c("Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")
  print(ate.aipw.results)
  
    ################### Variable Importance of Heterogeneity #######################
  
  var_imp <- round(c(variable_importance(cforest)), 10)
  names(var_imp) <- covariates
  sorted_var_imp <- sort(var_imp, decreasing = TRUE)
  sorted_var_imp<-as.data.frame(sorted_var_imp)
  setDT(sorted_var_imp, keep.rownames=TRUE)[]
  print(sorted_var_imp)


ate.aipw.results$Outcome<-y
ate.aipw.results<-ate.aipw.results[, c("Outcome", "Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")]
colnames(sorted_var_imp)<-c("Variables", paste0("Importance_", y))
list_res<-list(ATE=ate.aipw.results, VarImp=sorted_var_imp)
openxlsx::write.xlsx(list_res, rowNames=FALSE, paste0("C:\\Users\\socra\\Desktop\\Brain discounting\\GRF Results_New_ATE\\", y, "_baseline.xlsx"))

```


```{r ATE No DD, No GPS, No Brain: Distress 1year}
rm(list=ls())
dd_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain discounting\\Data\\ABCD Delay discounting merged_wide_kNN imputed dataset.csv", header=TRUE)

dd<-c('subjectkey', 'sex_0y', 'married_0y', 'income_0y', 'age_0y', 'race_g', 'race_ethnicity_0y', 'high_educ_0y', 'bmi_0y',
      'history_ratio_0y', 'abcd_site_0y', 'discount_rate_1y',
      'totalscore_pps_1y', 'totalscore_pps_2y', 'distress_score_pps_1y', 'distress_score_pps_2y',
      'section8_0y', 'rh_adi_perc1_0y', 'parent_age_0y', 'parent_identity_0y', 
      'JB_val_total_1y', 'nihtbx_totalcomp_uncorrected_0y',
      'cpeur2', 'eaeur1', 'depeur4', 'mddeur6', 'depmulti', 'bmieur4', 'bmimulti', 'iqeur2', 'insomniaeur6', 'snoringeur1',
      'happieur4', 'ghappieur2', 'ghappimeaneur1', 'ghappihealth6', 'alcdep_eurauto', 'alcdep_afrauto', 'alcdep_metaauto',
      'asdauto', 'aspauto', 'bipauto', 'cannabisauto', 'crossauto', 'drinkauto', 'edauto', 'neuroticismauto', 'ocdauto',
      'risk4pcauto', 'risktolauto', 'scz_eurauto', 'scz_easauto', 'scz_metaauto', 'smokerauto', 'worryauto', 'anxietyauto',
      'ptsdeur4', 'ptsdmeta6', 'adhdeur6', 'vol')


smri_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\mor.some.qc.desikan.csv", header=TRUE)

MID_fMRI_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\ABCD MID task fMRI Desikan_all_baseline.csv", header=TRUE)

MID_fMRI_data<-subset(data.frame(MID_fMRI_data), select = -c(eventname, interview_date, 
                                                             interview_age, sex, visit,
                                                             imgincl_t1w_include, imgincl_t2w_include, imgincl_dmri_include,
                                                             imgincl_rsfmri_include, imgincl_mid_include, imgincl_nback_include,
                                                             imgincl_sst_include, tfmri_mid_all_b_tr, tfmri_mid_all_b_numtrs,
                                                             tfmri_mid_all_b_dof, tfmri_mid_all_b_nvols, tfmri_mid_all_b_subthreshnvols, 
                                                             tfmri_mid_all_b_meanmotion, tfmri_mid_all_b_maxmotion, 
                                                             tfmri_mid_all_b_meantrans, tfmri_mid_all_b_maxtrans, tfmri_mid_all_b_meanrot,
                                                             tfmri_mid_all_b_maxrot))

dd_s<-dd_data[,dd]
MERGED1<-merge(dd_s, smri_data, by='subjectkey')
MERGED2<-merge(MERGED1, MID_fMRI_data, by='subjectkey')
data<-subset(MERGED2, JB_val_total_1y==1)
data<-na.omit(data)
print(paste0("Sample Size = ", nrow(data)))

#########################Scale Continuous Variables#############################
#Structure MRI
# grep('lh_bankssts_area._.1', colnames(data))
# grep('CerebralWhiteMatterVol._.18', colnames(data))
data[, c(65:463)]<-scale(data.frame(data[, c(65:463)]))

#MID task fMRI
# grep("tfmri_ma_arvn_b_cds_bkslh", colnames(data))
# grep('tfmri_ma_alvsl_b_scs_vtdcrh', colnames(data))
data[, c(464:1443)]<-scale(data.frame(data[, c(464:1443)]))

#Other
contvar<-c("age_0y", "bmi_0y", "history_ratio_0y", 
       "income_0y", "high_educ_0y", "parent_age_0y", "vol", "rh_adi_perc1_0y",
       "discount_rate_1y", "totalscore_pps_1y", "totalscore_pps_2y", 
       "distress_score_pps_1y", "distress_score_pps_2y", "nihtbx_totalcomp_uncorrected_0y",
       'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y')

data[contvar]<-scale(data.frame(data[contvar]))


#################################Binary Treatment & Covariates######################################

data$rh_adi_bi_0y = ifelse(data$rh_adi_perc1_0y >= mean(data$rh_adi_perc1_0y), 1, 0)

# data$sex_0y<-as.factor(data$sex_0y)
# data$married_0y<-as.factor(data$married_0y)
# data$parent_identity_0y<-as.factor(data$parent_identity_0y)
# data$race_ethnicity_0y<-as.factor(data$race_ethnicity_0y)


library(fastDummies)

data<-dummy_cols(data, select_columns = c('sex_0y', 'married_0y', 'race_g', 'parent_identity_0y', 'race_ethnicity_0y'))


library(learnr)
library(DiagrammeR)
library(grf)
library(tidyverse)
library(broom)
library(fastDummies)
library(lmtest)
library(rpart)
library(glmnet)
library(splines)
library(MASS)
library(sandwich)
library(ggplot2)
library(data.table)

covariates<-c('age_0y', 'bmi_0y', 'income_0y', 
                 'high_educ_0y', 'sex_0y_F', 
                 'married_0y_2', 'married_0y_3', 'married_0y_4', 'married_0y_5', 'married_0y_6',
                 'parent_identity_0y_2','parent_identity_0y_3', 
                 'parent_identity_0y_4', 'parent_identity_0y_5',
                 'race_ethnicity_0y_2', 'race_ethnicity_0y_3', 
                 'race_ethnicity_0y_4', 'race_ethnicity_0y_5',
                 'parent_age_0y', 
                 'nihtbx_totalcomp_uncorrected_0y', 'history_ratio_0y')

W = data$rh_adi_bi_0y
X = data[, covariates]
Z = data$section8_0y
n = nrow(data)


####################### ATE: Distress Score PLEs 1 year ###################################

y<-'distress_score_pps_1y'
  
Y <- data[, y]
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
cforest <- instrumental_forest(X, Y, W, Z,
                               num.trees = 40000,
                                 tune.parameters = c('sample.fraction', 'mtry', 'min.node.size', 'alpha', 'imbalance.penalty'),
                                 tune.num.trees = 3000,
                                 tune.num.reps = 200,
                                 tune.num.draws = 2000)
  
  
  
  ############### AIPW Average Treatment Effect ##################
  aipw.scores<-get_scores(cforest, num.trees.for.weights = 5000)
  ate.aipw.est <- mean(aipw.scores)
  ate.aipw.se <- sd(aipw.scores) / sqrt(n)
  ate.aipw.tstat <- ate.aipw.est / ate.aipw.se
  ate.aipw.pvalue <- 2*(1-pnorm(abs(ate.aipw.tstat)))
  lowci<- ate.aipw.est + -1 * qnorm(0.975) * ate.aipw.se 
  highci<- ate.aipw.est + 1 * qnorm(0.975) * ate.aipw.se 
  ate.aipw.results <- as.data.frame(cbind(ate.aipw.est, ate.aipw.se, ate.aipw.tstat, ate.aipw.pvalue, lowci, highci))
  rownames(ate.aipw.results)<-y
  colnames(ate.aipw.results)<-c("Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")
  print(ate.aipw.results)
  
    ################### Variable Importance of Heterogeneity #######################
  
  var_imp <- round(c(variable_importance(cforest)), 10)
  names(var_imp) <- covariates
  sorted_var_imp <- sort(var_imp, decreasing = TRUE)
  sorted_var_imp<-as.data.frame(sorted_var_imp)
  setDT(sorted_var_imp, keep.rownames=TRUE)[]
  print(sorted_var_imp)


ate.aipw.results$Outcome<-y
ate.aipw.results<-ate.aipw.results[, c("Outcome", "Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")]
colnames(sorted_var_imp)<-c("Variables", paste0("Importance_", y))
list_res<-list(ATE=ate.aipw.results, VarImp=sorted_var_imp)
openxlsx::write.xlsx(list_res, rowNames=FALSE, paste0("C:\\Users\\socra\\Desktop\\Brain discounting\\GRF Results_New_ATE\\", y, "_baseline.xlsx"))

```


```{r ATE No DD, No GPS, No Brain: Total 2year}
rm(list=ls())
dd_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain discounting\\Data\\ABCD Delay discounting merged_wide_kNN imputed dataset.csv", header=TRUE)

dd<-c('subjectkey', 'sex_0y', 'married_0y', 'income_0y', 'age_0y', 'race_g', 'race_ethnicity_0y', 'high_educ_0y', 'bmi_0y',
      'history_ratio_0y', 'abcd_site_0y', 'discount_rate_1y',
      'totalscore_pps_1y', 'totalscore_pps_2y', 'distress_score_pps_1y', 'distress_score_pps_2y',
      'section8_0y', 'rh_adi_perc1_0y', 'parent_age_0y', 'parent_identity_0y', 
      'JB_val_total_1y', 'nihtbx_totalcomp_uncorrected_0y',
      'cpeur2', 'eaeur1', 'depeur4', 'mddeur6', 'depmulti', 'bmieur4', 'bmimulti', 'iqeur2', 'insomniaeur6', 'snoringeur1',
      'happieur4', 'ghappieur2', 'ghappimeaneur1', 'ghappihealth6', 'alcdep_eurauto', 'alcdep_afrauto', 'alcdep_metaauto',
      'asdauto', 'aspauto', 'bipauto', 'cannabisauto', 'crossauto', 'drinkauto', 'edauto', 'neuroticismauto', 'ocdauto',
      'risk4pcauto', 'risktolauto', 'scz_eurauto', 'scz_easauto', 'scz_metaauto', 'smokerauto', 'worryauto', 'anxietyauto',
      'ptsdeur4', 'ptsdmeta6', 'adhdeur6', 'vol')


smri_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\mor.some.qc.desikan.csv", header=TRUE)

MID_fMRI_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\ABCD MID task fMRI Desikan_all_baseline.csv", header=TRUE)

MID_fMRI_data<-subset(data.frame(MID_fMRI_data), select = -c(eventname, interview_date, 
                                                             interview_age, sex, visit,
                                                             imgincl_t1w_include, imgincl_t2w_include, imgincl_dmri_include,
                                                             imgincl_rsfmri_include, imgincl_mid_include, imgincl_nback_include,
                                                             imgincl_sst_include, tfmri_mid_all_b_tr, tfmri_mid_all_b_numtrs,
                                                             tfmri_mid_all_b_dof, tfmri_mid_all_b_nvols, tfmri_mid_all_b_subthreshnvols, 
                                                             tfmri_mid_all_b_meanmotion, tfmri_mid_all_b_maxmotion, 
                                                             tfmri_mid_all_b_meantrans, tfmri_mid_all_b_maxtrans, tfmri_mid_all_b_meanrot,
                                                             tfmri_mid_all_b_maxrot))

dd_s<-dd_data[,dd]
MERGED1<-merge(dd_s, smri_data, by='subjectkey')
MERGED2<-merge(MERGED1, MID_fMRI_data, by='subjectkey')
data<-subset(MERGED2, JB_val_total_1y==1)
data<-na.omit(data)
print(paste0("Sample Size = ", nrow(data)))

#########################Scale Continuous Variables#############################
#Structure MRI
# grep('lh_bankssts_area._.1', colnames(data))
# grep('CerebralWhiteMatterVol._.18', colnames(data))
data[, c(65:463)]<-scale(data.frame(data[, c(65:463)]))

#MID task fMRI
# grep("tfmri_ma_arvn_b_cds_bkslh", colnames(data))
# grep('tfmri_ma_alvsl_b_scs_vtdcrh', colnames(data))
data[, c(464:1443)]<-scale(data.frame(data[, c(464:1443)]))

#Other
contvar<-c("age_0y", "bmi_0y", "history_ratio_0y", 
       "income_0y", "high_educ_0y", "parent_age_0y", "vol", "rh_adi_perc1_0y",
       "discount_rate_1y", "totalscore_pps_1y", "totalscore_pps_2y", 
       "distress_score_pps_1y", "distress_score_pps_2y", "nihtbx_totalcomp_uncorrected_0y",
       'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y')

data[contvar]<-scale(data.frame(data[contvar]))


#################################Binary Treatment & Covariates######################################

data$rh_adi_bi_0y = ifelse(data$rh_adi_perc1_0y >= mean(data$rh_adi_perc1_0y), 1, 0)

# data$sex_0y<-as.factor(data$sex_0y)
# data$married_0y<-as.factor(data$married_0y)
# data$parent_identity_0y<-as.factor(data$parent_identity_0y)
# data$race_ethnicity_0y<-as.factor(data$race_ethnicity_0y)


library(fastDummies)

data<-dummy_cols(data, select_columns = c('sex_0y', 'married_0y', 'race_g', 'parent_identity_0y', 'race_ethnicity_0y'))


library(learnr)
library(DiagrammeR)
library(grf)
library(tidyverse)
library(broom)
library(fastDummies)
library(lmtest)
library(rpart)
library(glmnet)
library(splines)
library(MASS)
library(sandwich)
library(ggplot2)
library(data.table)

covariates<-c('age_0y', 'bmi_0y', 'income_0y', 
                 'high_educ_0y', 'sex_0y_F', 
                 'married_0y_2', 'married_0y_3', 'married_0y_4', 'married_0y_5', 'married_0y_6',
                 'parent_identity_0y_2','parent_identity_0y_3', 
                 'parent_identity_0y_4', 'parent_identity_0y_5',
                 'race_ethnicity_0y_2', 'race_ethnicity_0y_3', 
                 'race_ethnicity_0y_4', 'race_ethnicity_0y_5',
                 'parent_age_0y', 
                 'nihtbx_totalcomp_uncorrected_0y', 'history_ratio_0y')

W = data$rh_adi_bi_0y
X = data[, covariates]
Z = data$section8_0y
n = nrow(data)


####################### ATE: Total Score PLEs 2 year ###################################

y<-'totalscore_pps_2y'
  
Y <- data[, y]
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
cforest <- instrumental_forest(X, Y, W, Z,
                               num.trees = 40000,
                                 tune.parameters = c('sample.fraction', 'mtry', 'min.node.size', 'alpha', 'imbalance.penalty'),
                                 tune.num.trees = 3000,
                                 tune.num.reps = 200,
                                 tune.num.draws = 2000)
  
  
  
  ############### AIPW Average Treatment Effect ##################
  aipw.scores<-get_scores(cforest, num.trees.for.weights = 2000)
  ate.aipw.est <- mean(aipw.scores)
  ate.aipw.se <- sd(aipw.scores) / sqrt(n)
  ate.aipw.tstat <- ate.aipw.est / ate.aipw.se
  ate.aipw.pvalue <- 2*(1-pnorm(abs(ate.aipw.tstat)))
  lowci<- ate.aipw.est + -1 * qnorm(0.975) * ate.aipw.se 
  highci<- ate.aipw.est + 1 * qnorm(0.975) * ate.aipw.se 
  ate.aipw.results <- as.data.frame(cbind(ate.aipw.est, ate.aipw.se, ate.aipw.tstat, ate.aipw.pvalue, lowci, highci))
  rownames(ate.aipw.results)<-y
  colnames(ate.aipw.results)<-c("Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")
  print(ate.aipw.results)
  
    ################### Variable Importance of Heterogeneity #######################
  
  var_imp <- round(c(variable_importance(cforest)), 10)
  names(var_imp) <- covariates
  sorted_var_imp <- sort(var_imp, decreasing = TRUE)
  sorted_var_imp<-as.data.frame(sorted_var_imp)
  setDT(sorted_var_imp, keep.rownames=TRUE)[]
  print(sorted_var_imp)


ate.aipw.results$Outcome<-y
ate.aipw.results<-ate.aipw.results[, c("Outcome", "Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")]
colnames(sorted_var_imp)<-c("Variables", paste0("Importance_", y))
list_res<-list(ATE=ate.aipw.results, VarImp=sorted_var_imp)
openxlsx::write.xlsx(list_res, rowNames=FALSE, paste0("C:\\Users\\socra\\Desktop\\Brain discounting\\GRF Results_New_ATE\\", y, "_baseline.xlsx"))

```



```{r ATE No DD, No GPS, No Brain: Distress 2year}
rm(list=ls())
dd_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain discounting\\Data\\ABCD Delay discounting merged_wide_kNN imputed dataset.csv", header=TRUE)

dd<-c('subjectkey', 'sex_0y', 'married_0y', 'income_0y', 'age_0y', 'race_g', 'race_ethnicity_0y', 'high_educ_0y', 'bmi_0y',
      'history_ratio_0y', 'abcd_site_0y', 'discount_rate_1y',
      'totalscore_pps_1y', 'totalscore_pps_2y', 'distress_score_pps_1y', 'distress_score_pps_2y',
      'section8_0y', 'rh_adi_perc1_0y', 'parent_age_0y', 'parent_identity_0y', 
      'JB_val_total_1y', 'nihtbx_totalcomp_uncorrected_0y',
      'cpeur2', 'eaeur1', 'depeur4', 'mddeur6', 'depmulti', 'bmieur4', 'bmimulti', 'iqeur2', 'insomniaeur6', 'snoringeur1',
      'happieur4', 'ghappieur2', 'ghappimeaneur1', 'ghappihealth6', 'alcdep_eurauto', 'alcdep_afrauto', 'alcdep_metaauto',
      'asdauto', 'aspauto', 'bipauto', 'cannabisauto', 'crossauto', 'drinkauto', 'edauto', 'neuroticismauto', 'ocdauto',
      'risk4pcauto', 'risktolauto', 'scz_eurauto', 'scz_easauto', 'scz_metaauto', 'smokerauto', 'worryauto', 'anxietyauto',
      'ptsdeur4', 'ptsdmeta6', 'adhdeur6', 'vol')


smri_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\mor.some.qc.desikan.csv", header=TRUE)

MID_fMRI_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\ABCD MID task fMRI Desikan_all_baseline.csv", header=TRUE)

MID_fMRI_data<-subset(data.frame(MID_fMRI_data), select = -c(eventname, interview_date, 
                                                             interview_age, sex, visit,
                                                             imgincl_t1w_include, imgincl_t2w_include, imgincl_dmri_include,
                                                             imgincl_rsfmri_include, imgincl_mid_include, imgincl_nback_include,
                                                             imgincl_sst_include, tfmri_mid_all_b_tr, tfmri_mid_all_b_numtrs,
                                                             tfmri_mid_all_b_dof, tfmri_mid_all_b_nvols, tfmri_mid_all_b_subthreshnvols, 
                                                             tfmri_mid_all_b_meanmotion, tfmri_mid_all_b_maxmotion, 
                                                             tfmri_mid_all_b_meantrans, tfmri_mid_all_b_maxtrans, tfmri_mid_all_b_meanrot,
                                                             tfmri_mid_all_b_maxrot))

dd_s<-dd_data[,dd]
MERGED1<-merge(dd_s, smri_data, by='subjectkey')
MERGED2<-merge(MERGED1, MID_fMRI_data, by='subjectkey')
data<-subset(MERGED2, JB_val_total_1y==1)
data<-na.omit(data)
print(paste0("Sample Size = ", nrow(data)))

#########################Scale Continuous Variables#############################
#Structure MRI
# grep('lh_bankssts_area._.1', colnames(data))
# grep('CerebralWhiteMatterVol._.18', colnames(data))
data[, c(65:463)]<-scale(data.frame(data[, c(65:463)]))

#MID task fMRI
# grep("tfmri_ma_arvn_b_cds_bkslh", colnames(data))
# grep('tfmri_ma_alvsl_b_scs_vtdcrh', colnames(data))
data[, c(464:1443)]<-scale(data.frame(data[, c(464:1443)]))


#Other
contvar<-c("age_0y", "bmi_0y", "history_ratio_0y", 
       "income_0y", "high_educ_0y", "parent_age_0y", "vol", "rh_adi_perc1_0y",
       "discount_rate_1y", "totalscore_pps_1y", "totalscore_pps_2y", 
       "distress_score_pps_1y", "distress_score_pps_2y", "nihtbx_totalcomp_uncorrected_0y",
       'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y')

data[contvar]<-scale(data.frame(data[contvar]))


#################################Binary Treatment & Covariates######################################

data$rh_adi_bi_0y = ifelse(data$rh_adi_perc1_0y >= mean(data$rh_adi_perc1_0y), 1, 0)

# data$sex_0y<-as.factor(data$sex_0y)
# data$married_0y<-as.factor(data$married_0y)
# data$parent_identity_0y<-as.factor(data$parent_identity_0y)
# data$race_ethnicity_0y<-as.factor(data$race_ethnicity_0y)


library(fastDummies)

data<-dummy_cols(data, select_columns = c('sex_0y', 'married_0y', 'race_g', 'parent_identity_0y', 'race_ethnicity_0y'))


library(learnr)
library(DiagrammeR)
library(grf)
library(tidyverse)
library(broom)
library(fastDummies)
library(lmtest)
library(rpart)
library(glmnet)
library(splines)
library(MASS)
library(sandwich)
library(ggplot2)
library(data.table)

covariates<-c('age_0y', 'bmi_0y', 'income_0y', 
                 'high_educ_0y', 'sex_0y_F', 
                 'married_0y_2', 'married_0y_3', 'married_0y_4', 'married_0y_5', 'married_0y_6',
                 'parent_identity_0y_2','parent_identity_0y_3', 
                 'parent_identity_0y_4', 'parent_identity_0y_5',
                 'race_ethnicity_0y_2', 'race_ethnicity_0y_3', 
                 'race_ethnicity_0y_4', 'race_ethnicity_0y_5',
                 'parent_age_0y', 
                 'nihtbx_totalcomp_uncorrected_0y', 'history_ratio_0y')

W = data$rh_adi_bi_0y
X = data[, covariates]
Z = data$section8_0y
n = nrow(data)


####################### ATE: Distress Score PLEs 2 year ###################################

y<-'distress_score_pps_2y'
  
Y <- data[, y]
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
cforest <- instrumental_forest(X, Y, W, Z,
                               num.trees = 40000,
                                 tune.parameters = c('sample.fraction', 'mtry', 'min.node.size', 'alpha', 'imbalance.penalty'),
                                 tune.num.trees = 3000,
                                 tune.num.reps = 200,
                                 tune.num.draws = 2000)
  
  
  
  ############### AIPW Average Treatment Effect ##################
  aipw.scores<-get_scores(cforest, num.trees.for.weights = 2000)
  ate.aipw.est <- mean(aipw.scores)
  ate.aipw.se <- sd(aipw.scores) / sqrt(n)
  ate.aipw.tstat <- ate.aipw.est / ate.aipw.se
  ate.aipw.pvalue <- 2*(1-pnorm(abs(ate.aipw.tstat)))
  lowci<- ate.aipw.est + -1 * qnorm(0.975) * ate.aipw.se 
  highci<- ate.aipw.est + 1 * qnorm(0.975) * ate.aipw.se 
  ate.aipw.results <- as.data.frame(cbind(ate.aipw.est, ate.aipw.se, ate.aipw.tstat, ate.aipw.pvalue, lowci, highci))
  rownames(ate.aipw.results)<-y
  colnames(ate.aipw.results)<-c("Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")
  print(ate.aipw.results)
  
    ################### Variable Importance of Heterogeneity #######################
  
  var_imp <- round(c(variable_importance(cforest)), 10)
  names(var_imp) <- covariates
  sorted_var_imp <- sort(var_imp, decreasing = TRUE)
  sorted_var_imp<-as.data.frame(sorted_var_imp)
  setDT(sorted_var_imp, keep.rownames=TRUE)[]
  print(sorted_var_imp)


ate.aipw.results$Outcome<-y
ate.aipw.results<-ate.aipw.results[, c("Outcome", "Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")]
colnames(sorted_var_imp)<-c("Variables", paste0("Importance_", y))
list_res<-list(ATE=ate.aipw.results, VarImp=sorted_var_imp)
openxlsx::write.xlsx(list_res, rowNames=FALSE, paste0("C:\\Users\\socra\\Desktop\\Brain discounting\\GRF Results_New_ATE\\", y, "_baseline.xlsx"))

```


```{r ATE No DD, No GPS, No Brain: Delusional 1year}
rm(list=ls())
dd_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain discounting\\Data\\ABCD Delay discounting merged_wide_kNN imputed dataset.csv", header=TRUE)

dd<-c('subjectkey', 'sex_0y', 'married_0y', 'income_0y', 'age_0y', 'race_g', 'race_ethnicity_0y', 'high_educ_0y', 'bmi_0y',
      'history_ratio_0y', 'abcd_site_0y', 'discount_rate_1y',
      'totalscore_pps_1y', 'totalscore_pps_2y', 'distress_score_pps_1y', 'distress_score_pps_2y',
      'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y',
      'section8_0y', 'rh_adi_perc1_0y', 'parent_age_0y', 'parent_identity_0y', 
      'JB_val_total_1y', 'nihtbx_totalcomp_uncorrected_0y',
      'cpeur2', 'eaeur1', 'depeur4', 'mddeur6', 'depmulti', 'bmieur4', 'bmimulti', 'iqeur2', 'insomniaeur6', 'snoringeur1',
      'happieur4', 'ghappieur2', 'ghappimeaneur1', 'ghappihealth6', 'alcdep_eurauto', 'alcdep_afrauto', 'alcdep_metaauto',
      'asdauto', 'aspauto', 'bipauto', 'cannabisauto', 'crossauto', 'drinkauto', 'edauto', 'neuroticismauto', 'ocdauto',
      'risk4pcauto', 'risktolauto', 'scz_eurauto', 'scz_easauto', 'scz_metaauto', 'smokerauto', 'worryauto', 'anxietyauto',
      'ptsdeur4', 'ptsdmeta6', 'adhdeur6', 'vol')


smri_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\mor.some.qc.desikan.csv", header=TRUE)

MID_fMRI_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\ABCD MID task fMRI Desikan_all_baseline.csv", header=TRUE)

MID_fMRI_data<-subset(data.frame(MID_fMRI_data), select = -c(eventname, interview_date, 
                                                             interview_age, sex, visit,
                                                             imgincl_t1w_include, imgincl_t2w_include, imgincl_dmri_include,
                                                             imgincl_rsfmri_include, imgincl_mid_include, imgincl_nback_include,
                                                             imgincl_sst_include, tfmri_mid_all_b_tr, tfmri_mid_all_b_numtrs,
                                                             tfmri_mid_all_b_dof, tfmri_mid_all_b_nvols, tfmri_mid_all_b_subthreshnvols, 
                                                             tfmri_mid_all_b_meanmotion, tfmri_mid_all_b_maxmotion, 
                                                             tfmri_mid_all_b_meantrans, tfmri_mid_all_b_maxtrans, tfmri_mid_all_b_meanrot,
                                                             tfmri_mid_all_b_maxrot))

dd_s<-dd_data[,dd]
MERGED1<-merge(dd_s, smri_data, by='subjectkey')
MERGED2<-merge(MERGED1, MID_fMRI_data, by='subjectkey')
data<-subset(MERGED2, JB_val_total_1y==1)
data<-na.omit(data)
print(paste0("Sample Size = ", nrow(data)))

#########################Scale Continuous Variables#############################
#Structure MRI
# grep('lh_bankssts_area._.1', colnames(data))
# grep('CerebralWhiteMatterVol._.18', colnames(data))
data[, c(65:463)]<-scale(data.frame(data[, c(65:463)]))

#MID task fMRI
# grep("tfmri_ma_arvn_b_cds_bkslh", colnames(data))
# grep('tfmri_ma_alvsl_b_scs_vtdcrh', colnames(data))
data[, c(464:1443)]<-scale(data.frame(data[, c(464:1443)]))

#Other
contvar<-c("age_0y", "bmi_0y", "history_ratio_0y", 
       "income_0y", "high_educ_0y", "parent_age_0y", "vol", "rh_adi_perc1_0y",
       "discount_rate_1y", "totalscore_pps_1y", "totalscore_pps_2y", 
       "distress_score_pps_1y", "distress_score_pps_2y", "nihtbx_totalcomp_uncorrected_0y",
       'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y')

data[contvar]<-scale(data.frame(data[contvar]))


#################################Binary Treatment & Covariates######################################

data$rh_adi_bi_0y = ifelse(data$rh_adi_perc1_0y >= mean(data$rh_adi_perc1_0y), 1, 0)

# data$sex_0y<-as.factor(data$sex_0y)
# data$married_0y<-as.factor(data$married_0y)
# data$parent_identity_0y<-as.factor(data$parent_identity_0y)
# data$race_ethnicity_0y<-as.factor(data$race_ethnicity_0y)


library(fastDummies)

data<-dummy_cols(data, select_columns = c('sex_0y', 'married_0y', 'race_g', 'parent_identity_0y', 'race_ethnicity_0y'))


library(learnr)
library(DiagrammeR)
library(grf)
library(tidyverse)
library(broom)
library(fastDummies)
library(lmtest)
library(rpart)
library(glmnet)
library(splines)
library(MASS)
library(sandwich)
library(ggplot2)
library(data.table)

covariates<-c('age_0y', 'bmi_0y', 'income_0y', 
                 'high_educ_0y', 'sex_0y_F', 
                 'married_0y_2', 'married_0y_3', 'married_0y_4', 'married_0y_5', 'married_0y_6',
                 'parent_identity_0y_2','parent_identity_0y_3', 
                 'parent_identity_0y_4', 'parent_identity_0y_5',
                 'race_ethnicity_0y_2', 'race_ethnicity_0y_3', 
                 'race_ethnicity_0y_4', 'race_ethnicity_0y_5',
                 'parent_age_0y', 
                 'nihtbx_totalcomp_uncorrected_0y', 'history_ratio_0y')

W = data$rh_adi_bi_0y
X = data[, covariates]
Z = data$section8_0y
n = nrow(data)


####################### ATE: Delusional Score PLEs 1 year ###################################

y<-'distress_score_di_1y'
  
Y <- data[, y]
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
cforest <- instrumental_forest(X, Y, W, Z,
                               num.trees = 40000,
                                 tune.parameters = c('sample.fraction', 'mtry', 'min.node.size', 'alpha', 'imbalance.penalty'),
                                 tune.num.trees = 3000,
                                 tune.num.reps = 200,
                                 tune.num.draws = 2000)
  
  
  
  ############### AIPW Average Treatment Effect ##################
  aipw.scores<-get_scores(cforest)
  ate.aipw.est <- mean(aipw.scores)
  ate.aipw.se <- sd(aipw.scores) / sqrt(n)
  ate.aipw.tstat <- ate.aipw.est / ate.aipw.se
  ate.aipw.pvalue <- 2*(1-pnorm(abs(ate.aipw.tstat)))
  lowci<- ate.aipw.est + -1 * qnorm(0.975) * ate.aipw.se 
  highci<- ate.aipw.est + 1 * qnorm(0.975) * ate.aipw.se 
  ate.aipw.results <- as.data.frame(cbind(ate.aipw.est, ate.aipw.se, ate.aipw.tstat, ate.aipw.pvalue, lowci, highci))
  rownames(ate.aipw.results)<-y
  colnames(ate.aipw.results)<-c("Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")
  print(ate.aipw.results)
  
    ################### Variable Importance of Heterogeneity #######################
  
  var_imp <- round(c(variable_importance(cforest)), 10)
  names(var_imp) <- covariates
  sorted_var_imp <- sort(var_imp, decreasing = TRUE)
  sorted_var_imp<-as.data.frame(sorted_var_imp)
  setDT(sorted_var_imp, keep.rownames=TRUE)[]
  print(sorted_var_imp)


ate.aipw.results$Outcome<-y
ate.aipw.results<-ate.aipw.results[, c("Outcome", "Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")]
colnames(sorted_var_imp)<-c("Variables", paste0("Importance_", y))
list_res<-list(ATE=ate.aipw.results, VarImp=sorted_var_imp)
openxlsx::write.xlsx(list_res, rowNames=FALSE, paste0("C:\\Users\\socra\\Desktop\\Brain discounting\\GRF Results_New_ATE\\", y, "_baseline.xlsx"))

```



```{r ATE No DD, No GPS, No Brain: Hallucination 1year}
rm(list=ls())
dd_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain discounting\\Data\\ABCD Delay discounting merged_wide_kNN imputed dataset.csv", header=TRUE)

dd<-c('subjectkey', 'sex_0y', 'married_0y', 'income_0y', 'age_0y', 'race_g', 'race_ethnicity_0y', 'high_educ_0y', 'bmi_0y',
      'history_ratio_0y', 'abcd_site_0y', 'discount_rate_1y',
      'totalscore_pps_1y', 'totalscore_pps_2y', 'distress_score_pps_1y', 'distress_score_pps_2y',
      'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y',
      'section8_0y', 'rh_adi_perc1_0y', 'parent_age_0y', 'parent_identity_0y', 
      'JB_val_total_1y', 'nihtbx_totalcomp_uncorrected_0y',
      'cpeur2', 'eaeur1', 'depeur4', 'mddeur6', 'depmulti', 'bmieur4', 'bmimulti', 'iqeur2', 'insomniaeur6', 'snoringeur1',
      'happieur4', 'ghappieur2', 'ghappimeaneur1', 'ghappihealth6', 'alcdep_eurauto', 'alcdep_afrauto', 'alcdep_metaauto',
      'asdauto', 'aspauto', 'bipauto', 'cannabisauto', 'crossauto', 'drinkauto', 'edauto', 'neuroticismauto', 'ocdauto',
      'risk4pcauto', 'risktolauto', 'scz_eurauto', 'scz_easauto', 'scz_metaauto', 'smokerauto', 'worryauto', 'anxietyauto',
      'ptsdeur4', 'ptsdmeta6', 'adhdeur6', 'vol')

smri_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\mor.some.qc.desikan.csv", header=TRUE)

MID_fMRI_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\ABCD MID task fMRI Desikan_all_baseline.csv", header=TRUE)

MID_fMRI_data<-subset(data.frame(MID_fMRI_data), select = -c(eventname, interview_date, 
                                                             interview_age, sex, visit,
                                                             imgincl_t1w_include, imgincl_t2w_include, imgincl_dmri_include,
                                                             imgincl_rsfmri_include, imgincl_mid_include, imgincl_nback_include,
                                                             imgincl_sst_include, tfmri_mid_all_b_tr, tfmri_mid_all_b_numtrs,
                                                             tfmri_mid_all_b_dof, tfmri_mid_all_b_nvols, tfmri_mid_all_b_subthreshnvols, 
                                                             tfmri_mid_all_b_meanmotion, tfmri_mid_all_b_maxmotion, 
                                                             tfmri_mid_all_b_meantrans, tfmri_mid_all_b_maxtrans, tfmri_mid_all_b_meanrot,
                                                             tfmri_mid_all_b_maxrot))

dd_s<-dd_data[,dd]
MERGED1<-merge(dd_s, smri_data, by='subjectkey')
MERGED2<-merge(MERGED1, MID_fMRI_data, by='subjectkey')
data<-subset(MERGED2, JB_val_total_1y==1)
data<-na.omit(data)
print(paste0("Sample Size = ", nrow(data)))

#########################Scale Continuous Variables#############################
#Structure MRI
# grep('lh_bankssts_area._.1', colnames(data))
# grep('CerebralWhiteMatterVol._.18', colnames(data))
data[, c(65:463)]<-scale(data.frame(data[, c(65:463)]))

#MID task fMRI
# grep("tfmri_ma_arvn_b_cds_bkslh", colnames(data))
# grep('tfmri_ma_alvsl_b_scs_vtdcrh', colnames(data))
data[, c(464:1443)]<-scale(data.frame(data[, c(464:1443)]))

#Other
contvar<-c("age_0y", "bmi_0y", "history_ratio_0y", 
       "income_0y", "high_educ_0y", "parent_age_0y", "vol", "rh_adi_perc1_0y",
       "discount_rate_1y", "totalscore_pps_1y", "totalscore_pps_2y", 
       "distress_score_pps_1y", "distress_score_pps_2y", "nihtbx_totalcomp_uncorrected_0y",
       'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y')

data[contvar]<-scale(data.frame(data[contvar]))


#################################Binary Treatment & Covariates######################################

data$rh_adi_bi_0y = ifelse(data$rh_adi_perc1_0y >= mean(data$rh_adi_perc1_0y), 1, 0)

# data$sex_0y<-as.factor(data$sex_0y)
# data$married_0y<-as.factor(data$married_0y)
# data$parent_identity_0y<-as.factor(data$parent_identity_0y)
# data$race_ethnicity_0y<-as.factor(data$race_ethnicity_0y)


library(fastDummies)

data<-dummy_cols(data, select_columns = c('sex_0y', 'married_0y', 'race_g', 'parent_identity_0y', 'race_ethnicity_0y'))


library(learnr)
library(DiagrammeR)
library(grf)
library(tidyverse)
library(broom)
library(fastDummies)
library(lmtest)
library(rpart)
library(glmnet)
library(splines)
library(MASS)
library(sandwich)
library(ggplot2)
library(data.table)

covariates<-c('age_0y', 'bmi_0y', 'income_0y', 
                 'high_educ_0y', 'sex_0y_F', 
                 'married_0y_2', 'married_0y_3', 'married_0y_4', 'married_0y_5', 'married_0y_6',
                 'parent_identity_0y_2','parent_identity_0y_3', 
                 'parent_identity_0y_4', 'parent_identity_0y_5',
                 'race_ethnicity_0y_2', 'race_ethnicity_0y_3', 
                 'race_ethnicity_0y_4', 'race_ethnicity_0y_5',
                 'parent_age_0y', 
                 'nihtbx_totalcomp_uncorrected_0y', 'history_ratio_0y')

W = data$rh_adi_bi_0y
X = data[, covariates]
Z = data$section8_0y
n = nrow(data)


####################### ATE: Hallucinational Score PLEs 1 year ###################################

y<-'distress_score_pd_1y'
  
Y <- data[, y]
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
cforest <- instrumental_forest(X, Y, W, Z,
                               num.trees = 40000,
                                 tune.parameters = c('sample.fraction', 'mtry', 'min.node.size', 'alpha', 'imbalance.penalty'),
                                 tune.num.trees = 3000,
                                 tune.num.reps = 200,
                                 tune.num.draws = 2000)
  
  
  
  ############### AIPW Average Treatment Effect ##################
  aipw.scores<-get_scores(cforest, num.trees.for.weights = 5000)
  ate.aipw.est <- mean(aipw.scores)
  ate.aipw.se <- sd(aipw.scores) / sqrt(n)
  ate.aipw.tstat <- ate.aipw.est / ate.aipw.se
  ate.aipw.pvalue <- 2*(1-pnorm(abs(ate.aipw.tstat)))
  lowci<- ate.aipw.est + -1 * qnorm(0.975) * ate.aipw.se 
  highci<- ate.aipw.est + 1 * qnorm(0.975) * ate.aipw.se 
  ate.aipw.results <- as.data.frame(cbind(ate.aipw.est, ate.aipw.se, ate.aipw.tstat, ate.aipw.pvalue, lowci, highci))
  rownames(ate.aipw.results)<-y
  colnames(ate.aipw.results)<-c("Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")
  print(ate.aipw.results)
  
    ################### Variable Importance of Heterogeneity #######################
  
  var_imp <- round(c(variable_importance(cforest)), 10)
  names(var_imp) <- covariates
  sorted_var_imp <- sort(var_imp, decreasing = TRUE)
  sorted_var_imp<-as.data.frame(sorted_var_imp)
  setDT(sorted_var_imp, keep.rownames=TRUE)[]
  print(sorted_var_imp)


ate.aipw.results$Outcome<-y
ate.aipw.results<-ate.aipw.results[, c("Outcome", "Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")]
colnames(sorted_var_imp)<-c("Variables", paste0("Importance_", y))
list_res<-list(ATE=ate.aipw.results, VarImp=sorted_var_imp)
openxlsx::write.xlsx(list_res, rowNames=FALSE, paste0("C:\\Users\\socra\\Desktop\\Brain discounting\\GRF Results_New_ATE\\", y, "_baseline.xlsx"))

```


```{r ATE No DD, No GPS, No Brain: Delusional 2year}
rm(list=ls())
dd_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain discounting\\Data\\ABCD Delay discounting merged_wide_kNN imputed dataset.csv", header=TRUE)

dd<-c('subjectkey', 'sex_0y', 'married_0y', 'income_0y', 'age_0y', 'race_g', 'race_ethnicity_0y', 'high_educ_0y', 'bmi_0y',
      'history_ratio_0y', 'abcd_site_0y', 'discount_rate_1y',
      'totalscore_pps_1y', 'totalscore_pps_2y', 'distress_score_pps_1y', 'distress_score_pps_2y',
      'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y',
      'section8_0y', 'rh_adi_perc1_0y', 'parent_age_0y', 'parent_identity_0y', 
      'JB_val_total_1y', 'nihtbx_totalcomp_uncorrected_0y',
      'cpeur2', 'eaeur1', 'depeur4', 'mddeur6', 'depmulti', 'bmieur4', 'bmimulti', 'iqeur2', 'insomniaeur6', 'snoringeur1',
      'happieur4', 'ghappieur2', 'ghappimeaneur1', 'ghappihealth6', 'alcdep_eurauto', 'alcdep_afrauto', 'alcdep_metaauto',
      'asdauto', 'aspauto', 'bipauto', 'cannabisauto', 'crossauto', 'drinkauto', 'edauto', 'neuroticismauto', 'ocdauto',
      'risk4pcauto', 'risktolauto', 'scz_eurauto', 'scz_easauto', 'scz_metaauto', 'smokerauto', 'worryauto', 'anxietyauto',
      'ptsdeur4', 'ptsdmeta6', 'adhdeur6', 'vol')


smri_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\mor.some.qc.desikan.csv", header=TRUE)

MID_fMRI_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\ABCD MID task fMRI Desikan_all_baseline.csv", header=TRUE)

MID_fMRI_data<-subset(data.frame(MID_fMRI_data), select = -c(eventname, interview_date, 
                                                             interview_age, sex, visit,
                                                             imgincl_t1w_include, imgincl_t2w_include, imgincl_dmri_include,
                                                             imgincl_rsfmri_include, imgincl_mid_include, imgincl_nback_include,
                                                             imgincl_sst_include, tfmri_mid_all_b_tr, tfmri_mid_all_b_numtrs,
                                                             tfmri_mid_all_b_dof, tfmri_mid_all_b_nvols, tfmri_mid_all_b_subthreshnvols, 
                                                             tfmri_mid_all_b_meanmotion, tfmri_mid_all_b_maxmotion, 
                                                             tfmri_mid_all_b_meantrans, tfmri_mid_all_b_maxtrans, tfmri_mid_all_b_meanrot,
                                                             tfmri_mid_all_b_maxrot))

dd_s<-dd_data[,dd]
MERGED1<-merge(dd_s, smri_data, by='subjectkey')
MERGED2<-merge(MERGED1, MID_fMRI_data, by='subjectkey')
data<-subset(MERGED2, JB_val_total_1y==1)
data<-na.omit(data)
print(paste0("Sample Size = ", nrow(data)))

#########################Scale Continuous Variables#############################
#Structure MRI
# grep('lh_bankssts_area._.1', colnames(data))
# grep('CerebralWhiteMatterVol._.18', colnames(data))
data[, c(65:463)]<-scale(data.frame(data[, c(65:463)]))

#MID task fMRI
# grep("tfmri_ma_arvn_b_cds_bkslh", colnames(data))
# grep('tfmri_ma_alvsl_b_scs_vtdcrh', colnames(data))
data[, c(464:1443)]<-scale(data.frame(data[, c(464:1443)]))

#Other
contvar<-c("age_0y", "bmi_0y", "history_ratio_0y", 
       "income_0y", "high_educ_0y", "parent_age_0y", "vol", "rh_adi_perc1_0y",
       "discount_rate_1y", "totalscore_pps_1y", "totalscore_pps_2y", 
       "distress_score_pps_1y", "distress_score_pps_2y", "nihtbx_totalcomp_uncorrected_0y",
       'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y')

data[contvar]<-scale(data.frame(data[contvar]))


#################################Binary Treatment & Covariates######################################

data$rh_adi_bi_0y = ifelse(data$rh_adi_perc1_0y >= mean(data$rh_adi_perc1_0y), 1, 0)

# data$sex_0y<-as.factor(data$sex_0y)
# data$married_0y<-as.factor(data$married_0y)
# data$parent_identity_0y<-as.factor(data$parent_identity_0y)
# data$race_ethnicity_0y<-as.factor(data$race_ethnicity_0y)


library(fastDummies)

data<-dummy_cols(data, select_columns = c('sex_0y', 'married_0y', 'race_g', 'parent_identity_0y', 'race_ethnicity_0y'))


library(learnr)
library(DiagrammeR)
library(grf)
library(tidyverse)
library(broom)
library(fastDummies)
library(lmtest)
library(rpart)
library(glmnet)
library(splines)
library(MASS)
library(sandwich)
library(ggplot2)
library(data.table)

covariates<-c('age_0y', 'bmi_0y', 'income_0y', 
                 'high_educ_0y', 'sex_0y_F', 
                 'married_0y_2', 'married_0y_3', 'married_0y_4', 'married_0y_5', 'married_0y_6',
                 'parent_identity_0y_2','parent_identity_0y_3', 
                 'parent_identity_0y_4', 'parent_identity_0y_5',
                 'race_ethnicity_0y_2', 'race_ethnicity_0y_3', 
                 'race_ethnicity_0y_4', 'race_ethnicity_0y_5',
                 'parent_age_0y', 
                 'nihtbx_totalcomp_uncorrected_0y', 'history_ratio_0y')

W = data$rh_adi_bi_0y
X = data[, covariates]
Z = data$section8_0y
n = nrow(data)


####################### ATE: Delusional Score PLEs 2year ###################################

y<-'distress_score_di_2y'
  
Y <- data[, y]
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
cforest <- instrumental_forest(X, Y, W, Z,
                               num.trees = 40000,
                                 tune.parameters = c('sample.fraction', 'mtry', 'min.node.size', 'alpha', 'imbalance.penalty'),
                                 tune.num.trees = 3000,
                                 tune.num.reps = 200,
                                 tune.num.draws = 2000)
  
  
  
  ############### AIPW Average Treatment Effect ##################
  aipw.scores<-get_scores(cforest, num.trees.for.weights = 2000)
  ate.aipw.est <- mean(aipw.scores)
  ate.aipw.se <- sd(aipw.scores) / sqrt(n)
  ate.aipw.tstat <- ate.aipw.est / ate.aipw.se
  ate.aipw.pvalue <- 2*(1-pnorm(abs(ate.aipw.tstat)))
  lowci<- ate.aipw.est + -1 * qnorm(0.975) * ate.aipw.se 
  highci<- ate.aipw.est + 1 * qnorm(0.975) * ate.aipw.se 
  ate.aipw.results <- as.data.frame(cbind(ate.aipw.est, ate.aipw.se, ate.aipw.tstat, ate.aipw.pvalue, lowci, highci))
  rownames(ate.aipw.results)<-y
  colnames(ate.aipw.results)<-c("Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")
  print(ate.aipw.results)
  
    ################### Variable Importance of Heterogeneity #######################
  
  var_imp <- round(c(variable_importance(cforest)), 10)
  names(var_imp) <- covariates
  sorted_var_imp <- sort(var_imp, decreasing = TRUE)
  sorted_var_imp<-as.data.frame(sorted_var_imp)
  setDT(sorted_var_imp, keep.rownames=TRUE)[]
  print(sorted_var_imp)


ate.aipw.results$Outcome<-y
ate.aipw.results<-ate.aipw.results[, c("Outcome", "Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")]
colnames(sorted_var_imp)<-c("Variables", paste0("Importance_", y))
list_res<-list(ATE=ate.aipw.results, VarImp=sorted_var_imp)
openxlsx::write.xlsx(list_res, rowNames=FALSE, paste0("C:\\Users\\socra\\Desktop\\Brain discounting\\GRF Results_New_ATE\\", y, "_baseline.xlsx"))

```



```{r ATE No DD, No GPS, No Brain: Hallucination 2year}
rm(list=ls())
dd_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain discounting\\Data\\ABCD Delay discounting merged_wide_kNN imputed dataset.csv", header=TRUE)

dd<-c('subjectkey', 'sex_0y', 'married_0y', 'income_0y', 'age_0y', 'race_g', 'race_ethnicity_0y', 'high_educ_0y', 'bmi_0y',
      'history_ratio_0y', 'abcd_site_0y', 'discount_rate_1y',
      'totalscore_pps_1y', 'totalscore_pps_2y', 'distress_score_pps_1y', 'distress_score_pps_2y',
      'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y',
      'section8_0y', 'rh_adi_perc1_0y', 'parent_age_0y', 'parent_identity_0y', 
      'JB_val_total_1y', 'nihtbx_totalcomp_uncorrected_0y',
      'cpeur2', 'eaeur1', 'depeur4', 'mddeur6', 'depmulti', 'bmieur4', 'bmimulti', 'iqeur2', 'insomniaeur6', 'snoringeur1',
      'happieur4', 'ghappieur2', 'ghappimeaneur1', 'ghappihealth6', 'alcdep_eurauto', 'alcdep_afrauto', 'alcdep_metaauto',
      'asdauto', 'aspauto', 'bipauto', 'cannabisauto', 'crossauto', 'drinkauto', 'edauto', 'neuroticismauto', 'ocdauto',
      'risk4pcauto', 'risktolauto', 'scz_eurauto', 'scz_easauto', 'scz_metaauto', 'smokerauto', 'worryauto', 'anxietyauto',
      'ptsdeur4', 'ptsdmeta6', 'adhdeur6', 'vol')


smri_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\mor.some.qc.desikan.csv", header=TRUE)

MID_fMRI_data<-read.csv("C:\\Users\\socra\\Desktop\\Brain Discounting\\Data\\ABCD MID task fMRI Desikan_all_baseline.csv", header=TRUE)

MID_fMRI_data<-subset(data.frame(MID_fMRI_data), select = -c(eventname, interview_date, 
                                                             interview_age, sex, visit,
                                                             imgincl_t1w_include, imgincl_t2w_include, imgincl_dmri_include,
                                                             imgincl_rsfmri_include, imgincl_mid_include, imgincl_nback_include,
                                                             imgincl_sst_include, tfmri_mid_all_b_tr, tfmri_mid_all_b_numtrs,
                                                             tfmri_mid_all_b_dof, tfmri_mid_all_b_nvols, tfmri_mid_all_b_subthreshnvols, 
                                                             tfmri_mid_all_b_meanmotion, tfmri_mid_all_b_maxmotion, 
                                                             tfmri_mid_all_b_meantrans, tfmri_mid_all_b_maxtrans, tfmri_mid_all_b_meanrot,
                                                             tfmri_mid_all_b_maxrot))

dd_s<-dd_data[,dd]
MERGED1<-merge(dd_s, smri_data, by='subjectkey')
MERGED2<-merge(MERGED1, MID_fMRI_data, by='subjectkey')
data<-subset(MERGED2, JB_val_total_1y==1)
data<-na.omit(data)
print(paste0("Sample Size = ", nrow(data)))

#########################Scale Continuous Variables#############################
#Structure MRI
# grep('lh_bankssts_area._.1', colnames(data))
# grep('CerebralWhiteMatterVol._.18', colnames(data))
data[, c(65:463)]<-scale(data.frame(data[, c(65:463)]))

#MID task fMRI
# grep("tfmri_ma_arvn_b_cds_bkslh", colnames(data))
# grep('tfmri_ma_alvsl_b_scs_vtdcrh', colnames(data))
data[, c(464:1443)]<-scale(data.frame(data[, c(464:1443)]))


#Other
contvar<-c("age_0y", "bmi_0y", "history_ratio_0y", 
       "income_0y", "high_educ_0y", "parent_age_0y", "vol", "rh_adi_perc1_0y",
       "discount_rate_1y", "totalscore_pps_1y", "totalscore_pps_2y", 
       "distress_score_pps_1y", "distress_score_pps_2y", "nihtbx_totalcomp_uncorrected_0y",
       'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y')

data[contvar]<-scale(data.frame(data[contvar]))


#################################Binary Treatment & Covariates######################################

data$rh_adi_bi_0y = ifelse(data$rh_adi_perc1_0y >= mean(data$rh_adi_perc1_0y), 1, 0)

# data$sex_0y<-as.factor(data$sex_0y)
# data$married_0y<-as.factor(data$married_0y)
# data$parent_identity_0y<-as.factor(data$parent_identity_0y)
# data$race_ethnicity_0y<-as.factor(data$race_ethnicity_0y)


library(fastDummies)

data<-dummy_cols(data, select_columns = c('sex_0y', 'married_0y', 'race_g', 'parent_identity_0y', 'race_ethnicity_0y'))


library(learnr)
library(DiagrammeR)
library(grf)
library(tidyverse)
library(broom)
library(fastDummies)
library(lmtest)
library(rpart)
library(glmnet)
library(splines)
library(MASS)
library(sandwich)
library(ggplot2)
library(data.table)

covariates<-c('age_0y', 'bmi_0y', 'income_0y', 
                 'high_educ_0y', 'sex_0y_F', 
                 'married_0y_2', 'married_0y_3', 'married_0y_4', 'married_0y_5', 'married_0y_6',
                 'parent_identity_0y_2','parent_identity_0y_3', 
                 'parent_identity_0y_4', 'parent_identity_0y_5',
                 'race_ethnicity_0y_2', 'race_ethnicity_0y_3', 
                 'race_ethnicity_0y_4', 'race_ethnicity_0y_5',
                 'parent_age_0y', 
                 'nihtbx_totalcomp_uncorrected_0y', 'history_ratio_0y')

W = data$rh_adi_bi_0y
X = data[, covariates]
Z = data$section8_0y
n = nrow(data)


####################### ATE: Hallucinational Score PLEs 2 year ###################################

y<-'distress_score_pd_2y'
  
Y <- data[, y]
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
cforest <- instrumental_forest(X, Y, W, Z,
                               num.trees = 40000,
                                 tune.parameters = c('sample.fraction', 'mtry', 'min.node.size', 'alpha', 'imbalance.penalty'),
                                 tune.num.trees = 3000,
                                 tune.num.reps = 200,
                                 tune.num.draws = 2000)
  
  
  
  ############### AIPW Average Treatment Effect ##################
  aipw.scores<-get_scores(cforest, num.trees.for.weights = 2000)
  ate.aipw.est <- mean(aipw.scores)
  ate.aipw.se <- sd(aipw.scores) / sqrt(n)
  ate.aipw.tstat <- ate.aipw.est / ate.aipw.se
  ate.aipw.pvalue <- 2*(1-pnorm(abs(ate.aipw.tstat)))
  lowci<- ate.aipw.est + -1 * qnorm(0.975) * ate.aipw.se 
  highci<- ate.aipw.est + 1 * qnorm(0.975) * ate.aipw.se 
  ate.aipw.results <- as.data.frame(cbind(ate.aipw.est, ate.aipw.se, ate.aipw.tstat, ate.aipw.pvalue, lowci, highci))
  rownames(ate.aipw.results)<-y
  colnames(ate.aipw.results)<-c("Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")
  print(ate.aipw.results)
  
    ################### Variable Importance of Heterogeneity #######################
  
  var_imp <- round(c(variable_importance(cforest)), 10)
  names(var_imp) <- covariates
  sorted_var_imp <- sort(var_imp, decreasing = TRUE)
  sorted_var_imp<-as.data.frame(sorted_var_imp)
  setDT(sorted_var_imp, keep.rownames=TRUE)[]
  print(sorted_var_imp)


ate.aipw.results$Outcome<-y
ate.aipw.results<-ate.aipw.results[, c("Outcome", "Estimate", "Std.Error", "t", "P-value", "95% LowCI", "95% High CI")]
colnames(sorted_var_imp)<-c("Variables", paste0("Importance_", y))
list_res<-list(ATE=ate.aipw.results, VarImp=sorted_var_imp)
openxlsx::write.xlsx(list_res, rowNames=FALSE, paste0("C:\\Users\\socra\\Desktop\\Brain discounting\\GRF Results_New_ATE\\", y, "_baseline.xlsx"))

```

