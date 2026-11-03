# parametric_survival

This repository contains the analysis codes for the parametric survival analyses in COSYCONET that resulted in the publication "Parametric estimation of age-, sex-, and spirometry grade-specific mortality in a cohort of COPD patients from Germany: Results from COSYCONET", published in Respiratory Medicine (Respir Med. 2025 Oct:247:108280. doi: 10.1016/j.rmed.2025.108280. Epub 2025 Jul 29.). The overarching aim was to update the mortality rates for the COPD Markov model by Petra Menn.

The prospective cohort COSYCONET comprises >2700 patients with COPD diagnosis who underwent post-bronchodilator spirometry (among many other physical assessments and questionnaires). Survival was tracked via mortality follow-up. Those information were used to estimate parametric survival models based on the exponential distribution. The latter was a prerequisite for deriving mortality rates applicable to the Markov model.

Sensitivity analyses were done with respect to the time frame considered (i.e., including vs. excluding the first and last year of follow-up). The rationale behind those exclusions were as follows: Participants were required to be in a stable condition to meet the study inclusion criteria (no acute exacerbation). Therefore, mortality was artificially reduced in the first year of follow-up. For the last year of follow-up, data collection was incomplete and its quality could not be guaranteed or verified. We thus focused on the time span in between.

Analyses based on the full data set should however be used when comparing the results to hazard ratios and mortality rates from (most) other studies because they typically did not apply such exclusions.

Mortality rates were derived by a regression model based on the exponential distribution. Regression coefficients were then used to predict 1-year mortality for each "cell" of the possible combinations of age group (in 10-year intervals) and COPD grade (according to the GOLD 2007 classification). Uncertainty was reflected by 95% confidence intervals.

Moratlity rates and 95% confidence intervals can also be transformed into alpha and beta parameters of the beta distribution, used for probabilistic sensitivity analysis.
