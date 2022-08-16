---
title: "DD Final2"
output: html_document
date: '2022-06-13'
---


rm(list=ls())
dd_data<-read.csv("~.csv", header=TRUE)

dd<-c('subjectkey', 'sex_0y', 'married_0y', 'income_0y', 'age_0y', 'race_g', 'race_ethnicity_0y', 'high_educ_0y', 'bmi_0y',
      'history_ratio_0y', 'abcd_site_0y', 'discount_rate_1y',
      'totalscore_pps_1y', 'totalscore_pps_2y', 'distress_score_pps_1y', 'distress_score_pps_2y',
      'section8_0y', 'rh_adi_perc1_0y', 'religion_prefer_0y', 'parent_age_0y', 'parent_identity_0y', 'gender_identity_0y', 'foreign_born_family_0y',
      'foreign_born_0y', 'gay_parent_0y', 'gay_youth_0y', 'JB_val_total_1y', 'nihtbx_totalcomp_uncorrected_0y',
      'cpeur2', 'eaeur1', 'depeur4', 'mddeur6', 'depmulti', 'bmieur4', 'bmimulti', 'iqeur2', 'insomniaeur6', 'snoringeur1',
      'happieur4', 'ghappieur2', 'ghappimeaneur1', 'ghappihealth6', 'alcdep_eurauto', 'alcdep_afrauto', 'alcdep_metaauto',
      'asdauto', 'aspauto', 'bipauto', 'cannabisauto', 'crossauto', 'drinkauto', 'edauto', 'neuroticismauto', 'ocdauto',
      'risk4pcauto', 'risktolauto', 'scz_eurauto', 'scz_easauto', 'scz_metaauto', 'smokerauto', 'worryauto', 'anxietyauto',
      'ptsdeur4', 'ptsdmeta6', 'adhdeur6', 'ddauto_scaled', 'vol',
      'KSADS_BD_y_2y', 'KSADS_Anx_y_2y', 'KSADS_Eat_y_2y',
      'KSADS_Suicide_y_2y', 'KSADS_Sleep_y_2y', 'KSADS_total_y_2y', 'KSADS_BD_p_2y',
      'KSADS_Anx_p_2y', 'KSADS_Eat_p_2y', 'KSADS_Subst_p_2y', 'KSADS_Sleep_p_2y', 'KSADS_Suicide_p_2y', 'KSADS_total_p_2y')

dd_s<-dd_data[,dd]
data<-subset(dd_s, JB_val_total_1y==1)
data<-na.omit(data)
nrow(data)


#########################Scale Continuous Variables#############################

data$age_0y_z<-scale(data$age_0y)
data$bmi_0y_z<-scale(data$bmi_0y)
data$income_0y_z<-scale(data$income_0y)
data$high_educ_0y_z<-scale(data$high_educ_0y)
data$parent_age_0y_z<-scale(data$parent_age_0y)
data$age_0y_z<-scale(data$age_0y)
data$vol_z<-scale(data$vol)
data$history_ratio_0y_z<-scale(data$history_ratio_0y)
data$discount_rate_1y_z<-scale(data$discount_rate_1y)
data$rh_adi_perc1_0y_z<-scale(data$rh_adi_perc1_0y)

data$totalscore_pps_1y_z<-scale(data$totalscore_pps_1y)
data$totalscore_pps_2y_z<-scale(data$totalscore_pps_2y) 
data$distress_score_pps_1y_z<-scale(data$distress_score_pps_1y)
data$distress_score_pps_2y_z<-scale(data$distress_score_pps_2y)

data$nihtbx_totalcomp_uncorrected_0y_z<-scale(data$nihtbx_totalcomp_uncorrected_0y)


data$KSADS_Anx_p_2y_z<-scale(data$KSADS_Anx_p_2y)
data$KSADS_BD_p_2y_z<-scale(data$KSADS_BD_p_2y)
data$KSADS_Eat_p_2y_z<-scale(data$KSADS_Eat_p_2y)
data$KSADS_Sleep_p_2y_z<-scale(data$KSADS_Sleep_p_2y)
data$KSADS_Suicide_p_2y_z<-scale(data$KSADS_Suicide_p_2y)
data$KSADS_total_p_2y_z<-scale(data$KSADS_total_p_2y)

data$KSADS_Anx_y_2y_z<-scale(data$KSADS_Anx_y_2y)
data$KSADS_BD_y_2y_z<-scale(data$KSADS_BD_y_2y)
data$KSADS_Eat_y_2y_z<-scale(data$KSADS_Eat_y_2y)
data$KSADS_Sleep_y_2y_z<-scale(data$KSADS_Sleep_y_2y)
data$KSADS_Suicide_y_2y_z<-scale(data$KSADS_Suicide_y_2y)
data$KSADS_total_y_2y_z<-scale(data$KSADS_total_y_2y)



#################################Binary Treatment & Covariates######################################

data$rh_adi_bi_0y = ifelse(data$rh_adi_perc1_0y_z >= mean(data$rh_adi_perc1_0y_z), 1, 0)
data$discount_rate_bi_1y = ifelse(data$discount_rate_1y_z>= mean(data$discount_rate_1y_z), 1, 0)

cov_con<-c('age_0y_z', 'bmi_0y_z', 'income_0y_z', 'high_educ_0y_z', 'parent_age_0y_z', 'history_ratio_0y_z')
cov_cat<-c('sex_0y', 'married_0y', 'parent_identity_0y', 'gender_identity_0y', 'foreign_born_family_0y', 
           'foreign_born_0y','gay_parent_0y', 'gay_youth_0y', 'race_ethnicity_0y', 'religion_prefer_0y')

cov1<-paste0(cov_con, collapse="+")
cov2<-paste0(cov_cat, collapse = '+')
cov_list<-paste0(cov1, "+", cov2)


data$sex_0y<-as.factor(data$sex_0y)
data$married_0y<-as.factor(data$married_0y)
data$race_g<-as.factor(data$race_g)
data$parent_identity_0y<-as.factor(data$parent_identity_0y)
data$gender_identity_0y<-as.factor(data$gender_identity_0y)
data$foreign_born_0y<-as.factor(data$foreign_born_0y)
data$foreign_born_family_0y<-as.factor(data$foreign_born_family_0y)
data$gay_parent_0y<-as.factor(data$gay_parent_0y)
data$gay_youth_0y<-as.factor(data$gay_youth_0y)
data$race_ethnicity_0y<-as.factor(data$race_ethnicity_0y)
data$religion_prefer_0y<-as.factor(data$religion_prefer_0y)


library(fastDummies)
data<-dummy_cols(data, select_columns = c('sex_0y', 'married_0y', 'race_g', 'parent_identity_0y', 'gender_identity_0y', 'religion_prefer_0y',
                                          'gay_parent_0y', 'gay_youth_0y', 'race_ethnicity_0y', 'foreign_born_family_0y', 'foreign_born_0y'))




############################ SPLS PRS Variable Selection: PLEs ######################
library(spls)
gps_list<-c('cpeur2', 'eaeur1', 'mddeur6', 'depmulti', 'bmimulti', 'iqeur2', 'insomniaeur6', 'snoringeur1',
       'happieur4', 'ghappieur2', 'ghappimeaneur1', 'ghappihealth6', 'alcdep_eurauto',
       'asdauto', 'aspauto', 'bipauto', 'cannabisauto', 'crossauto', 'drinkauto', 'edauto', 'neuroticismauto', 'ocdauto',
       'risk4pcauto', 'risktolauto', 'scz_eurauto', 'smokerauto', 'worryauto', 'anxietyauto',
       'ptsdeur4', 'adhdeur6')

outcomes_list<-c('totalscore_pps_1y_z', 'totalscore_pps_2y_z', 'distress_score_pps_1y_z', 'distress_score_pps_2y_z')

GPS<-data[gps_list]
PLE<-data[outcomes_list]

set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
invisible(capture.output(cv<-cv.spls(GPS, PLE, eta = seq(0, 0.999, 0.001), K = c(1: 10))))
fit<-spls(GPS, PLE, eta = cv$eta.opt, K = cv$K.opt)
print(fit)
ci.f <- ci.spls(fit, B=5000, plot.it=TRUE, plot.fix="x")
cf <- correct.spls(ci.f, plot.it=TRUE)
print(cf)

cis<-ci.f$cibeta
print(cis)

res_spls<-as.data.frame(cf)
res_spls_ci<-as.data.frame(cis)

colnames(res_spls)<-c("Total Score PLEs (1-year)", "Total Score PLEs (2-year)", "Distress Score PLEs (1-year)", "Distress Score PLEs (2-year)")
colnames(res_spls_ci)<-c("95% CI: Low", "95% CI: High")
openxlsx::write.xlsx(list(sheet1=res_spls, sheet2=res_spls_ci), rowNames=TRUE, "~ABCD DD SPLS_results.xlsx")



############################ SPLS PRS Variable Selection: DD ######################
library(spls)
gps_list<-c('cpeur2', 'eaeur1', 'mddeur6', 'depmulti', 'bmimulti', 'iqeur2', 'insomniaeur6', 'snoringeur1',
       'happieur4', 'ghappieur2', 'ghappimeaneur1', 'ghappihealth6', 'alcdep_eurauto',
       'asdauto', 'aspauto', 'bipauto', 'cannabisauto', 'crossauto', 'drinkauto', 'edauto', 'neuroticismauto', 'ocdauto',
       'risk4pcauto', 'risktolauto', 'scz_eurauto', 'smokerauto', 'worryauto', 'anxietyauto',
       'ptsdeur4', 'adhdeur6')

outcomes_list<-c('discount_rate_1y_z')

GPS<-data[gps_list]
DD<-data[outcomes_list]

set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
invisible(capture.output(cv<-cv.spls(GPS, DD, eta = seq(0, 0.999, 0.001), K = c(1: 10))))
fit<-spls(GPS, DD, eta = cv$eta.opt, K = cv$K.opt)
print(fit)


"ci.spls_y1" <-
function( object, coverage=0.95, B=1000,
        plot.it=FALSE, plot.fix='y', plot.var=NA,
        K=object$K, fit=object$fit )
{
    # initialization

    betahat <- object$betahat
    y <- object$y
    A <- object$A
    x <- object$x
    xA <- object$x[,A,drop=FALSE]
    n <- nrow(y)
    p <- ncol(x)
    q <- ncol(y)

    # bootstrap

    betamat <- array( 0, c(length(A),q,B) )

    for ( i in 1:B )
    {
        if ( i%%ceiling(B/10)==0 )
        {
            perc <- round( 10*i/ceiling(B/10) )
            cat( paste(perc,'% completed...\n') )
        }
        nbt <- sample( c(1:n), replace=TRUE )
        ybt <- y[nbt,,drop=FALSE]
        xAbt <- xA[nbt,,drop=FALSE]
        plsfit <- plsr( ybt~xAbt, ncomp=K, method=fit )
        betamat[,,i] <- coef(plsfit)
    }

    # calculate CI

    tailp <- ( 1 - coverage ) / 2
    qt <- function(x) { quantile( x, c(tailp,1-tailp) ) }
    cibeta <- list()
    lbmat <- ubmat <- matrix( 0, p, q )
    
    
    cii <- t(qt(betamat[,1,]))
    cibeta[[1]] <- cii
    rownames(cibeta[[1]]) <- colnames(xA)
    lbmat[A,1] <- cii[,1]
    ubmat[A,1] <- cii[,2]
    
    names(cibeta) <- colnames(y)

    # CI plot

    if ( plot.it==TRUE )
    {
        if ( plot.fix=='y' )
        {
            if ( is.na(plot.var[1]) ) { plot.var <- c(1:q) }
            k <- 1
            for ( i in plot.var )
            {
                if ( k>1 ) { dev.new() }
                ylimit <- c( min(lbmat[,i]), max(ubmat[,i]) )
                plot( A, betahat[A,i], type='p',
                    xlim=c(1,p), ylim=ylimit,
                    xlab='Predictors',
                    ylab='Coefficient Estimates',
                    main=paste( format(100*coverage,nsmall=0),
                    '% Bootstrapped CI of Coefficients',sep='') )
                for ( j in 1:length(A) )
                { lines( c(A[j],A[j]), c(lbmat[A[j],i],ubmat[A[j],i]) ) }
                abline( h=0, lty=2, col='red' )
                k <- k + 1
            }
        }
        if ( plot.fix=='x' )
        {
            if ( is.na(plot.var[1]) ) { plot.var <- A }
            k <- 1
            for ( i in plot.var )
            {
                if ( k>1 ) { dev.new() }
                ylimit <- c( min(lbmat[i,]), max(ubmat[i,]) )
                plot( c(1:q), betahat[i,], type='p',
                    xlim=c(1,q), ylim=ylimit,
                    xlab='Responses',
                    ylab='Coefficient Estimates',
                    main=paste( format(100*coverage,nsmall=0),
                    '% Bootstrapped CI of Coefficients',sep='') )
                for ( j in 1:q )
                { lines( c(j,j), c(lbmat[i,j],ubmat[i,j]) ) }
                abline( h=0, lty=2, col='red' )
                k <- k + 1
            }
        }
    }

    ci <- list( cibeta=cibeta, betahat=betahat,
                lbmat=lbmat, ubmat=ubmat )
    invisible(ci)
}


ci.f <- ci.spls_y1(fit, B=5000, plot.it=TRUE, plot.fix="x")
cf <- correct.spls(ci.f, plot.it=TRUE)
print(cf)


cis<-ci.f$cibeta
print(cis)

res_spls<-as.data.frame(cf)
res_spls_ci<-as.data.frame(cis)

colnames(res_spls)<-c("Delay Discounting (1-year)")
colnames(res_spls_ci)<-c("95% CI: Low", "95% CI: High")
openxlsx::write.xlsx(list(sheet1=res_spls, sheet2=res_spls_ci), rowNames=TRUE, "~ABCD DD SPLS_results2.xlsx")

```




```{r IV Regression}
library(ivreg)

outcomes<-c('totalscore_pps_1y_z', 'totalscore_pps_2y_z', 'distress_score_pps_1y_z', 'distress_score_pps_2y_z')


beta_iv<-data.frame()
se_iv<-data.frame()
p_iv<-data.frame()
weak_iv<-data.frame()
hausman_iv<-data.frame()
lowci_iv<-data.frame()
highci_iv<-data.frame()


formula <- formula(paste0("discount_rate_1y_z~nihtbx_totalcomp_uncorrected_0y_z+iqeur2+", cov_list, 
                            '| rh_adi_perc1_0y_z | section8_0y+nihtbx_totalcomp_uncorrected_0y_z+iqeur2+', cov_list))
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
  formula <- formula(paste0(y, "~nihtbx_totalcomp_uncorrected_0y_z+eaeur1+depmulti+smokerauto+adhdeur6+", cov_list, 
                            '| rh_adi_perc1_0y_z | section8_0y+nihtbx_totalcomp_uncorrected_0y_z+eaeur1+depmulti+smokerauto+adhdeur6+', cov_list))
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

p_iv_fdr<-round((p.adjust(as.matrix(p_iv), method = 'BH')),4)

res_all<-cbind(beta_iv, se_iv, lowci_iv, highci_iv, p_iv, p_iv_fdr, weak_iv, hausman_iv)
colnames(res_all)<-c("Estimates", "Std.Err", "95% Lower CI", "95% Upper CI", "P-value", "P-FDR", "stat_Weak IV", "p_Weak IV", "stat_Hausman", "p_Hausman")
rownames(res_all)<-c('Delay Discounting', 'Total PLEs (1-year)', 'Total PLEs (2-year)', 'Distress PLEs (1-year)', 'Distress PLEs (2-year)')
print(res_all)


write.csv(res_all, "~ABCD DD IV Regression_results.csv")

```



```{r IV Regression}
library(ivreg)

outcomes<-c('KSADS_BD_y_2y_z', 'KSADS_Anx_y_2y_z', 'KSADS_Eat_y_2y_z',
            'KSADS_Suicide_y_2y_z', 'KSADS_Sleep_y_2y_z', 'KSADS_total_y_2y_z', 
            'KSADS_BD_p_2y_z', 'KSADS_Anx_p_2y_z', 'KSADS_Eat_p_2y_z', 
            'KSADS_Sleep_p_2y_z', 'KSADS_Suicide_p_2y_z', 'KSADS_total_p_2y_z')



beta_iv<-data.frame()
se_iv<-data.frame()
p_iv<-data.frame()
weak_iv<-data.frame()
hausman_iv<-data.frame()
lowci_iv<-data.frame()
highci_iv<-data.frame()


for (y in outcomes) {
  formula <- formula(paste0(y, "~nihtbx_totalcomp_uncorrected_0y_z+iqeur2+", cov_list, 
                            '| rh_adi_perc1_0y_z | section8_0y+nihtbx_totalcomp_uncorrected_0y_z+iqeur2+', cov_list))
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

p_iv_fdr<-round((p.adjust(as.matrix(p_iv), method = 'BH')),4)

res_all<-cbind(beta_iv, se_iv, lowci_iv, highci_iv, p_iv, p_iv_fdr, weak_iv, hausman_iv)
colnames(res_all)<-c("Estimates", "Std.Err", "95% Lower CI", "95% Upper CI", "P-value", "P-FDR", "stat_Weak IV", "p_Weak IV", "stat_Hausman", "p_Hausman")
rownames(res_all)<-outcomes
print(res_all)

write.csv(res_all, "~ABCD DD IV Regression_KSADS results.csv")


```

