library(dplyr)
library(lmtest)
library(estimatr)
library(sandwich)
library(broom)
library(knitr)
library(kableExtra)

# Obtain cleaned decile portfolio returns dataframe
crsp_returns <- readRDS(here("data", "processed", "crsp_returns_clean.rds"))

# Sample years variable
sample_years <- as.numeric(difftime(max(crsp_returns$Date), min(crsp_returns$Date), units = "days")) / 365.25

# Average number of FOMC and non-FOMC observations per year 
fomc_days_per_year <- sum(crsp_returns$fomc == 1, na.rm = TRUE) / sample_years
nonfomc_days_per_year <- sum(crsp_returns$fomc == 0,na.rm = TRUE) / sample_years

# Equal weighted portfolio regression, Sharpe ratio, annual FOMC and non-FOMC returns
eqwt_lmrobust <- lm_robust(EqualLogExcess ~ fomc, data = crsp_returns, se_type = "HC1")
eqwt_sharpe <- sqrt(8) * (mean(crsp_returns$EqualLogExcess[crsp_returns$fomc == 1], na.rm = TRUE) / 
                            sd(crsp_returns$EqualLogExcess[crsp_returns$fomc == 1], na.rm = TRUE)) # multiply by square root 8 to annualize according to footnote 12
eqwt_annualfomcreturn <- fomc_days_per_year * mean(crsp_returns$EqualLogExcess[crsp_returns$fomc == 1], na.rm = TRUE)
eqwt_annualnonfomcreturn <- nonfomc_days_per_year * mean(crsp_returns$EqualLogExcess[crsp_returns$fomc == 0], na.rm = TRUE)


# Value weighted portfolio regression, Sharpe ratio, annual FOMC and non-FOMC returns
vwt_lmrobust <- lm_robust(ValueLogExcess ~ fomc, data = crsp_returns, se_type = "HC1")
vwt_sharpe <- sqrt(8) * (mean(crsp_returns$ValueLogExcess[crsp_returns$fomc == 1], na.rm = TRUE) / 
                            sd(crsp_returns$ValueLogExcess[crsp_returns$fomc == 1], na.rm = TRUE)) # multiply by square root 8 to annualize according to footnote 12
vwt_annualfomcreturn <- fomc_days_per_year * mean(crsp_returns$ValueLogExcess[crsp_returns$fomc == 1], na.rm = TRUE)
vwt_annualnonfomcreturn <- nonfomc_days_per_year * mean(crsp_returns$ValueLogExcess[crsp_returns$fomc == 0], na.rm = TRUE)

# Create list with regression models
crsp_lmrobustmodels <- list(
  "Equal Weighted"  = eqwt_lmrobust,
  "Value Weighted"  = vwt_lmrobust
)

# Create list of values of annual excess returns on FOMC days 
crsp_annualfomc <- c(
  eqwt_annualfomcreturn,
  vwt_annualfomcreturn
)

# Create list of values of annual excess returns on non-FOMC days 
crsp_nonannualfomc <- c(
  eqwt_annualnonfomcreturn,
  vwt_annualnonfomcreturn
)

# Create list of values of Sharpe ratios
crsp_sharpe <- c(
  eqwt_sharpe,
  vwt_sharpe
)

# Save lists as RDS files
saveRDS(crsp_lmrobustmodels, here("data", "processed", "crsp_lmrobustmodels.rds"))
saveRDS(crsp_annualfomc, here("data", "processed", "crsp_annualfomcvalues.rds"))
saveRDS(crsp_nonannualfomc, here("data", "processed", "crsp_annualnonfomcvalues.rds"))
saveRDS(crsp_sharpe, here("data", "processed", "crsp_sharpevalues.rds"))




