# DD-HTE

## Description
This contains codes used for [Heterogeneity in the Causal Effects of Neighborhood Deprivation on Value-based Decision Making and Risk for Psychosis].
All analyses were conducted in R version 4.1.2.


## DD Var Select.R
Variable selection for Polygenic Risk Scores (PRS), Structural MRI, and MID Task fMRI using Boruta

### Reference
Kursa, M. B., & Rudnicki, W. R. (2010). Feature Selection with the Boruta Package. Journal of Statistical Software, 36(11), 1 - 13. doi:10.18637/jss.v036.i11 


## DD IV Regression.R
Conventional Linear Instrumental Variable regression (IV regression)

### Reference
Angrist, J. D., Imbens, G. W., & Rubin, D. B. (1996). Identification of Causal Effects Using Instrumental Variables. Journal of the American Statistical Association, 91(434), 444-455. doi:10.1080/01621459.1996.10476902

John Fox, Christian Kleiber, Achim Zeileis, Nikolas Kuschnig (2022). ivreg: Instrumental-Variables Regression by '2SLS', '2SM', or '2SMM', with Diagnostics. Version 0.6-1, URL: https://john-d-fox.github.io/ivreg/



## DD GRF ATE.Rmd
Average Treatment Effects using Instrumental Random Forests (grf)

### Reference
Wager, S., & Athey, S. (2018). Estimation and Inference of Heterogeneous Treatment Effects using Random Forests. Journal of the American Statistical Association, 113(523), 1228-1242. doi:10.1080/01621459.2017.1319839 

Athey, S., Tibshirani, J., & Wager, S. (2019). Generalized random forests. The Annals of Statistics, 47(2), 1148-1178. doi:10.1214/18-AOS1709 



## DD GRF RATE.Rmd
Heterogeneity assessment using Rank-weighted Average Treatment Effects (grf)

### Reference
Yadlowsky, S., Fleming, S., Shah, N., Brunskill, E., & Wager, S. (2021). Evaluating Treatment Prioritization Rules via Rank-Weighted Average Treatment Effects. arXiv preprint arXiv:2111.07966. doi:arXiv:2111.07966v1 
