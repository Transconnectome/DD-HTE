
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
print("Sample Size=")
print(nrow(data))


#########################Scale Continuous Variables#############################
#Structure MRI
# grep('lh_bankssts_area._.1', colnames(data))
# grep('CerebralWhiteMatterVol._.18', colnames(data))
data[, c(61:459)]<-scale(data.frame(data[, c(61:459)]))

#MID task fMRI
# grep('tfmri_ma_arvn_b_cds_clatcgelh', colnames(data))
# grep('tfmri_ma_alvsl_b_scs_vtdcrh', colnames(data))
data[, c(461:1439)]<-scale(data.frame(data[, c(461:1439)]))

#Other
contvar<-c("age_0y", "bmi_0y", "history_ratio_0y", 
       "income_0y", "high_educ_0y", "parent_age_0y", "vol", "rh_adi_perc1_0y",
       "discount_rate_1y", "totalscore_pps_1y", "totalscore_pps_2y", 
       "distress_score_pps_1y", "distress_score_pps_2y", "nihtbx_totalcomp_uncorrected_0y")

data[contvar]<-scale(data.frame(data[contvar]))


#################################Binary Treatment & Covariates######################################

data$rh_adi_bi_0y = ifelse(data$rh_adi_perc1_0y >= mean(data$rh_adi_perc1_0y), 1, 0)

data$sex_0y<-as.factor(data$sex_0y)
data$married_0y<-as.factor(data$married_0y)
data$parent_identity_0y<-as.factor(data$parent_identity_0y)
data$race_ethnicity_0y<-as.factor(data$race_ethnicity_0y)


library(fastDummies)

data<-dummy_cols(data, select_columns = c('sex_0y', 'married_0y', 'race_g', 'parent_identity_0y', 'race_ethnicity_0y'))



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
openxlsx::write.xlsx(result, rowNames=TRUE, "~DD Boruta_GPS_results.xlsx")



####Structure MRI Variable Selection using Random Forests (Boruta)####

library(Boruta)

smri<-as.data.frame(data[, c(61:459)])
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

openxlsx::write.xlsx(result, rowNames=TRUE, "~DD Boruta_sMRI_results.xlsx")


####MID task fMRI Variable Selection using Random Forests (Boruta)####

library(Boruta)

fmri<-as.data.frame(data[, c(461:1439)])
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

openxlsx::write.xlsx(result, rowNames=TRUE, "~DD Boruta_MID task fMRI_results.xlsx")

