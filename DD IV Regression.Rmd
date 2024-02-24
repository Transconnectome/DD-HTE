---
title: "DD IV Regression"
author: "PJH"
date: '2022-11-15'
output: html_document
---

```{r}
rm(list=ls())
dd_data<-read.csv("ABCD Delay discounting merged_wide_kNN imputed dataset.csv", header=TRUE)

dd<-c('subjectkey', 'sex_0y', 'married_0y', 'income_0y', 'age_0y', 'race_g', 'race_ethnicity_0y', 'high_educ_0y', 'bmi_0y',
      'history_ratio_0y', 'abcd_site_0y', 'discount_rate_1y',
      'distress_score_pps_1y', 'distress_score_pps_2y',
      'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y',
      'section8_0y', 'rh_adi_perc1_0y', 'parent_age_0y', 'parent_identity_0y', 
      'JB_val_total_1y', 'nihtbx_totalcomp_uncorrected_0y',
      'cpeur2', 'eaeur1', 'depeur4', 'mddeur6', 'depmulti', 'bmieur4', 'bmimulti', 'iqeur2', 'insomniaeur6', 'snoringeur1',
      'happieur4', 'ghappieur2', 'ghappimeaneur1', 'ghappihealth6', 'alcdep_eurauto', 'alcdep_afrauto', 'alcdep_metaauto',
      'asdauto', 'aspauto', 'bipauto', 'cannabisauto', 'crossauto', 'drinkauto', 'edauto', 'neuroticismauto', 'ocdauto',
      'risk4pcauto', 'risktolauto', 'scz_eurauto', 'scz_easauto', 'scz_metaauto', 'smokerauto', 'worryauto', 'anxietyauto',
      'ptsdeur4', 'ptsdmeta6', 'adhdeur6', 'vol')


smri_data<-read.csv("mor.some.qc.desikan.csv", header=TRUE)

MID_fMRI_data<-read.csv("ABCD MID task fMRI Desikan_all_baseline.csv", header=TRUE)

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

rm(dd_data, dd_s, MERGED1, MERGED2, MID_fMRI_data, smri_data)

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


data$rh_adi_bi_0y = ifelse(data$rh_adi_perc1_0y >= mean(data$rh_adi_perc1_0y), 1, 0)

data$sex_0y<-as.factor(data$sex_0y)
data$married_0y<-as.factor(data$married_0y)
data$parent_identity_0y<-as.factor(data$parent_identity_0y)
data$race_ethnicity_0y<-as.factor(data$race_ethnicity_0y)


library(fastDummies)

data<-dummy_cols(data, select_columns = c('sex_0y', 'married_0y', 'race_g', 'parent_identity_0y', 'race_ethnicity_0y'))

```



```{r IV Regression}
library(ivreg)

outcomes<-c('distress_score_pps_1y', 'distress_score_pps_2y',
            'distress_score_di_1y', 'distress_score_pd_1y', 'distress_score_di_2y', 'distress_score_pd_2y')
cov_con<-c('age_0y', 'bmi_0y', 'income_0y', 'high_educ_0y', 'parent_age_0y', 'history_ratio_0y')
cov_cat<-c('sex_0y', 'married_0y', 'parent_identity_0y', 'race_ethnicity_0y')

cov1<-paste0(cov_con, collapse="+")
cov2<-paste0(cov_cat, collapse = '+')
cov_list<-paste0(cov1, "+", cov2)


beta_iv<-data.frame()
se_iv<-data.frame()
p_iv<-data.frame()
weak_iv<-data.frame()
hausman_iv<-data.frame()
lowci_iv<-data.frame()
highci_iv<-data.frame()


formula <- formula(paste0("discount_rate_1y~nihtbx_totalcomp_uncorrected_0y+", cov_list, 
                            '| rh_adi_perc1_0y | section8_0y+nihtbx_totalcomp_uncorrected_0y+', cov_list))
res<-ivreg(formula, data = data)
beta<-as.matrix(round(summary(res)$coefficients[2,1],4))
se<-as.matrix(round(summary(res)$coefficients[2,2],4))
p<-as.matrix(round(summary(res)$coefficients[2,4],4))
weak<-(summary(res, diagnostics = TRUE)$diagnostics)[1, 3:4]
hausman<-round((summary(res, diagnostics = TRUE)$diagnostics)[2, 3:4], 4)
lowci<- beta + -1 * qnorm(0.975) * se 
highci<- beta + 1 * qnorm(0.975) * se 
  
beta_iv<-rbind(beta_iv, beta)
se_iv<-rbind(se_iv, se)
lowci_iv<-rbind(lowci_iv, lowci)
highci_iv<-rbind(highci_iv, highci)
p_iv<-rbind(p_iv, p)
weak_iv<-rbind(weak_iv, weak)
hausman_iv<-rbind(hausman_iv, hausman)


for (y in outcomes) {
  formula <- formula(paste0(y, "~nihtbx_totalcomp_uncorrected_0y+", cov_list, 
                            '| rh_adi_perc1_0y | section8_0y+nihtbx_totalcomp_uncorrected_0y+', cov_list))
  res<-ivreg(formula, data = data)
  beta<-as.matrix(round(summary(res)$coefficients[2,1],4))
  se<-as.matrix(round(summary(res)$coefficients[2,2],4))
  p<-as.matrix(round(summary(res)$coefficients[2,4],4))
  weak<-(summary(res, diagnostics = TRUE)$diagnostics)[1, 3:4]
  hausman<-round((summary(res, diagnostics = TRUE)$diagnostics)[2, 3:4], 4)
  lowci<- beta + -1 * qnorm(0.975) * se 
  highci<- beta + 1 * qnorm(0.975) * se 
  
  beta_iv<-rbind(beta_iv, beta)
  se_iv<-rbind(se_iv, se)
  lowci_iv<-rbind(lowci_iv, lowci)
  highci_iv<-rbind(highci_iv, highci)
  p_iv<-rbind(p_iv, p)
  weak_iv<-rbind(weak_iv, weak)
  hausman_iv<-rbind(hausman_iv, hausman)
  }

p_iv_fdr<-round((p.adjust(as.matrix(p_iv), method = 'fdr')),4)

res_all<-cbind(beta_iv, se_iv, lowci_iv, highci_iv, p_iv, p_iv_fdr, weak_iv, hausman_iv)
colnames(res_all)<-c("Estimates", "Std.Err", "95% Lower CI", "95% Upper CI", "P-value", "P-FDR", "stat_Weak IV", "p_Weak IV", "stat_Hausman", "p_Hausman")
rownames(res_all)<-c('Delay Discounting',
                     'Distress PLEs (1-year)', 'Distress PLEs (2-year)', 'Distress DI (1-year)', 
                     'Distress PD (1-year)', 'Distress DI (2-year)', 'Distress PD (2-year)')
print(res_all)


# openxlsx::write.xlsx(res_all, rowNames=TRUE, "ABCD DD IV Regression_results_new.xlsx")

```

