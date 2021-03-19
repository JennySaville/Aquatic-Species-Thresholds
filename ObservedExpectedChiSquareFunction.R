# Function used to analyze fish taxa and their responses to potential stressors
# (e.g., impervious surface area, conductivity, etc.). For each species two data
# sets are created - a dataset of stressor values for sites where a taxon was
# observed and a dataset that includes stressor values from sites where a taxon
# was expected to occur (based on watershed size and historical 6-digit basin
# observations; this also includes data from observed sites). A chi-square
# goodness of fit test is performed to determine if the expected and observed
# datasets significantly differ from each other. If a significant difference is
# detected it is assumed that the stressor impacts taxon distribution. Further
# testing elucidates if the response is positive or negative and whether or not
# minimum and maximum detectable limits exist that can be used to define
# stressor thresholds for each species.


ChiSquareOE.Function <- function(StrataIndVarOEdata) {
  
  # Only keep fish taxa that have been observed at more than 11 locations per strata
  sub.param <- subset(StrataIndVarOEdata, NSites_Acres >= 11) 
  
  # Change DF to long format and drop variables (rows) with no data 
  params.melted <- melt(sub.param, id.vars = c("SITEYR", "MDE6Digit", "MDE8Digit", "BIBIStrata"
                                               , "Year", "Taxon", "Observed", "Expected"
                                               , "NSites_Acres")
                        , variable.name = "IND_VAR", value.name = "RESULT", na.rm = TRUE)
  
  
  # Expected dataset -----------------------------------------------------------
  ## Calculate the percentage of records above and below the 90th and 10th percentiles 
  
  expected <- droplevels(subset(params.melted, Expected == 1))
  
  ## ID percentile values and count the # of values above and below percentiles per variable
  exp.quant.calcs <- ddply(expected, .(Taxon, BIBIStrata, IND_VAR), summarise
                           , Exp_p10 = quantile(RESULT, probs = 0.10, na.rm = TRUE)
                           , Exp_p90 = quantile(RESULT, probs = 0.90, na.rm = TRUE)
                           , Exp_N_Indvar = length(!is.na(RESULT))
                           , Exp_N_gt90_Indvar = sum(RESULT > Exp_p90, na.rm = T)
                           , Exp_N_lt90_Indvar = sum(RESULT < Exp_p90, na.rm = T)
                           , Exp_N_gt10_Indvar = sum(RESULT > Exp_p10, na.rm = T)
                           , Exp_N_lt10_Indvar = sum(RESULT < Exp_p10, na.rm = T)
                           , Exp_p95 = quantile(RESULT, probs = 0.95, na.rm = T))
  
  ## Calculate %'s
  exp.percents <- mutate(exp.quant.calcs, Exp_Percent_lt_q10 = ((Exp_N_lt10_Indvar/Exp_N_Indvar)*100)
                         , Exp_Percent_gt_q10 = ((Exp_N_gt10_Indvar/Exp_N_Indvar)*100)
                         , Exp_Percent_lt_q90 = ((Exp_N_lt90_Indvar/Exp_N_Indvar)*100)
                         , Exp_Percent_gt_q90 = ((Exp_N_gt90_Indvar/Exp_N_Indvar)*100))
  
  ## Merge back with long DF
  exp.melted <- QuickMerge.fcn(exp.percents, params.melted, "Y") 
  
  
  # Observed dataset -----------------------------------------------------------
  
  ## Calculate the percentage of records for the Observed dataset that are above
  ## and below the 90th and 10th percentiles for the Expected dataset
  
  observed <- subset(exp.melted, Observed == 1) 
  
  ## Count the # of records above and below the expected percentile values per variable
  obs.ns <- ddply(observed, .(Taxon, BIBIStrata, IND_VAR), summarise
                  , Obs_N_gtP90 = sum(RESULT >= Exp_p90, na.rm = T)
                  , Obs_N_ltP90 = sum(RESULT < Exp_p90, na.rm = T)
                  , Obs_N_gtP10 = sum(RESULT > Exp_p10, na.rm = T)
                  , Obs_N_ltP10 = sum(RESULT <= Exp_p10, na.rm = T)
                  , Obs_p95 = quantile(RESULT, probs = 0.95, na.rm = TRUE)
                  , Obs_N_Indvar = length(!is.na(RESULT)))
  
  ## Calculate %'s
  obs.percents <- mutate(obs.ns, Obs_Percent_gt_q90 = ((Obs_N_gtP90/Obs_N_Indvar)*100)
                         , Obs_Percent_lt_q90 = ((Obs_N_ltP90/Obs_N_Indvar)*100)
                         , Obs_Percent_gt_q10 = ((Obs_N_gtP10/Obs_N_Indvar)*100)
                         , Obs_Percent_lt_q10 = ((Obs_N_ltP10/Obs_N_Indvar)*100))
  
  ## Merge Expected and Observed percentages
  m1 <- QuickMerge.fcn(obs.percents, exp.percents, "X")
  
  # Calculate Chi^2 values to determine significance ---------------------------
  ## Is the expected dataset significantly different from the observed dataset,
  ## either upwards or downwards? Does the taxon respond favorably (q10) or #
  ## negatively (q90) to the variable? If more than 10% of the observations are #
  ## above the q90 value, it is assumed the taxon responds favorably (e.g., % #
  ## forested area), so the q10 values should be used to determine the lower #
  ## threshold. If less than 10% of the observations are above the q90 value, then
  ## the taxon responds negatively to the variable. The q95 result value is
  ## considered the point at which the taxon is no longer expected to be found,
  ## visa-versa for q10 and determining lower thresholds for variables with a
  ## positive interaction
  chi <- mutate(m1
                , GT90 = (((Exp_Percent_lt_q90 - Obs_Percent_lt_q90)^2)/Exp_Percent_lt_q90)
                , LT90 = (((Exp_Percent_gt_q90 - Obs_Percent_gt_q90)^2)/Exp_Percent_gt_q90)
                , CHI2_q90 = (GT90 + LT90)
                , chi_prob_lt = qchisq(0.95, df = 1, lower.tail = TRUE)
                , chi_prob_ut = qchisq(0.95, df = 1, lower.tail = FALSE) 
                , CHI_sig_dif_q90 = ifelse(is.infinite(CHI2_q90), 0
                                           , ifelse(is.nan(CHI2_q90), 0
                                                    , ifelse(CHI2_q90 > chi_prob_lt, 1 , 0)))
                , ptest_uptail_q90 = pchisq(CHI2_q90, df = 1, lower.tail = FALSE)
                , pvalsig_q90 = ifelse(ptest_uptail_q90 < 0.05, 1, 0) 
                , check = ifelse(CHI_sig_dif_q90 == pvalsig_q90, 1, 0)
                , ptest_lowtail_q90 = pchisq(CHI2_q90, df = 1, lower.tail = TRUE)
                
                , LT10 = (((Exp_Percent_lt_q10 - Obs_Percent_lt_q10)^2)/Exp_Percent_lt_q10)
                , GT10 = (((Exp_Percent_gt_q10 - Obs_Percent_gt_q10)^2)/Exp_Percent_gt_q10)
                , CHI2_q10 = (LT10 + GT10)
                , CHI_sig_dif_q10 = ifelse(is.infinite(CHI2_q10), 0
                                           , ifelse(is.nan(CHI2_q10), 0
                                                    , ifelse(CHI2_q10 > chi_prob_lt, 1, 0)))
                , ptest_uptail_q10 = pchisq(CHI2_q10, df = 1, lower.tail = FALSE)
                , pvalsig_q10 = ifelse(ptest_uptail_q10 < 0.05, 1, 0)              
                , check2 = ifelse(CHI_sig_dif_q90 == pvalsig_q10, 1, 0)
                , ptest_lowtail_q10 = pchisq(CHI2_q10, df = 1, lower.tail = TRUE))
  
  return(chi)
}