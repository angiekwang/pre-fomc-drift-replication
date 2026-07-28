library(dplyr)
library(lmtest)
library(estimatr)
library(sandwich)
library(broom)
library(knitr)
library(kableExtra)

# Obtain cleaned decile portfolio returns dataframe
decile_returns <- readRDS("/Users/angiewang/Desktop/FOMCDrift/data/processed/decile_returns_clean.rds")

# Sample years variable
sample_years <- as.numeric(difftime(max(decile_returns$Date), min(decile_returns$Date), units = "days")) / 365.25

# Average number of FOMC and non-FOMC observations per year
fomc_days_per_year <- sum(decile_returns$fomc == 1, na.rm = TRUE) / sample_years

nonfomc_days_per_year <- sum(decile_returns$fomc == 0,na.rm = TRUE) / sample_years

# Decile 1 regression, Sharpe ratio, annual FOMC and non-FOMC returns
dec1_lmrobust <- lm_robust(Dec1LogExcess ~ fomc, data = decile_returns, se_type = "HC1")
dec1_sharpe <- sqrt(8) * (mean(decile_returns$Dec1LogExcess[decile_returns$fomc == 1], na.rm = TRUE) / 
                          sd(decile_returns$Dec1LogExcess[decile_returns$fomc == 1], na.rm = TRUE)) # multiply by square root 8 to annualize according to footnote 12
dec1_annualfomcreturn <- fomc_days_per_year * mean(decile_returns$Dec1LogExcess[decile_returns$fomc == 1], na.rm = TRUE)
dec1_annualnonfomcreturn <- nonfomc_days_per_year * mean(decile_returns$Dec1LogExcess[decile_returns$fomc == 0], na.rm = TRUE)

# Decile 2 regression, Sharpe ratio, annual FOMC and non-FOMC returns
dec2_lmrobust <- lm_robust(Dec2LogExcess ~ fomc, data = decile_returns, se_type = "HC1")
dec2_sharpe <- sqrt(8) * (mean(decile_returns$Dec2LogExcess[decile_returns$fomc == 1], na.rm = TRUE) / 
                            sd(decile_returns$Dec2LogExcess[decile_returns$fomc == 1], na.rm = TRUE))
dec2_annualfomcreturn <- fomc_days_per_year * mean(decile_returns$Dec2LogExcess[decile_returns$fomc == 1], na.rm = TRUE)
dec2_annualnonfomcreturn <- nonfomc_days_per_year * mean(decile_returns$Dec2LogExcess[decile_returns$fomc == 0], na.rm = TRUE)

# Decile 3 regression, Sharpe ratio, annual FOMC and non-FOMC returns
dec3_lmrobust <- lm_robust(Dec3LogExcess ~ fomc, data = decile_returns, se_type = "HC1")
dec3_sharpe <- sqrt(8) * (mean(decile_returns$Dec3LogExcess[decile_returns$fomc == 1], na.rm = TRUE) / 
                            sd(decile_returns$Dec3LogExcess[decile_returns$fomc == 1], na.rm = TRUE))
dec3_annualfomcreturn <- fomc_days_per_year * mean(decile_returns$Dec3LogExcess[decile_returns$fomc == 1], na.rm = TRUE)
dec3_annualnonfomcreturn <- nonfomc_days_per_year * mean(decile_returns$Dec3LogExcess[decile_returns$fomc == 0], na.rm = TRUE)


# Decile 4 regression, Sharpe ratio, annual FOMC and non-FOMC returns
dec4_lmrobust <- lm_robust(Dec4LogExcess ~ fomc, data = decile_returns, se_type = "HC1")
dec4_sharpe <- sqrt(8) * (mean(decile_returns$Dec4LogExcess[decile_returns$fomc == 1], na.rm = TRUE) / 
                            sd(decile_returns$Dec4LogExcess[decile_returns$fomc == 1], na.rm = TRUE))
dec4_annualfomcreturn <- fomc_days_per_year * mean(decile_returns$Dec4LogExcess[decile_returns$fomc == 1], na.rm = TRUE)
dec4_annualnonfomcreturn <- nonfomc_days_per_year * mean(decile_returns$Dec4LogExcess[decile_returns$fomc == 0], na.rm = TRUE)

# Decile 5 regression, Sharpe ratio, annual FOMC and non-FOMC returns
dec5_lmrobust <- lm_robust(Dec5LogExcess ~ fomc, data = decile_returns, se_type = "HC1")
dec5_sharpe <- sqrt(8) * (mean(decile_returns$Dec5LogExcess[decile_returns$fomc == 1], na.rm = TRUE) / 
                            sd(decile_returns$Dec5LogExcess[decile_returns$fomc == 1], na.rm = TRUE))
dec5_annualfomcreturn <- fomc_days_per_year * mean(decile_returns$Dec5LogExcess[decile_returns$fomc == 1], na.rm = TRUE) 
dec5_annualnonfomcreturn <- nonfomc_days_per_year * mean(decile_returns$Dec5LogExcess[decile_returns$fomc == 0], na.rm = TRUE) 

# Decile 6 regression, Sharpe ratio, annual FOMC and non-FOMC returns
dec6_lmrobust <- lm_robust(Dec6LogExcess ~ fomc, data = decile_returns, se_type = "HC1")
dec6_sharpe <- sqrt(8) * (mean(decile_returns$Dec6LogExcess[decile_returns$fomc == 1], na.rm = TRUE) / 
                            sd(decile_returns$Dec6LogExcess[decile_returns$fomc == 1], na.rm = TRUE))
dec6_annualfomcreturn <- fomc_days_per_year * mean(decile_returns$Dec6LogExcess[decile_returns$fomc == 1], na.rm = TRUE) 
dec6_annualnonfomcreturn <- nonfomc_days_per_year * mean(decile_returns$Dec6LogExcess[decile_returns$fomc == 0], na.rm = TRUE) 

# Decile 7 regression, Sharpe ratio, annual FOMC and non-FOMC returns
dec7_lmrobust <- lm_robust(Dec7LogExcess ~ fomc, data = decile_returns, se_type = "HC1")
dec7_sharpe <- sqrt(8) * (mean(decile_returns$Dec7LogExcess[decile_returns$fomc == 1], na.rm = TRUE) / 
                            sd(decile_returns$Dec7LogExcess[decile_returns$fomc == 1], na.rm = TRUE))
dec7_annualfomcreturn <- fomc_days_per_year * mean(decile_returns$Dec7LogExcess[decile_returns$fomc == 1], na.rm = TRUE)
dec7_annualnonfomcreturn <- nonfomc_days_per_year * mean(decile_returns$Dec7LogExcess[decile_returns$fomc == 0], na.rm = TRUE) 

# Decile 8 regression, Sharpe ratio, annual FOMC and non-FOMC returns
dec8_lmrobust <- lm_robust(Dec8LogExcess ~ fomc, data = decile_returns, se_type = "HC1")
dec8_sharpe <- sqrt(8) * (mean(decile_returns$Dec8LogExcess[decile_returns$fomc == 1], na.rm = TRUE) / 
                            sd(decile_returns$Dec8LogExcess[decile_returns$fomc == 1], na.rm = TRUE))
dec8_annualfomcreturn <- fomc_days_per_year * mean(decile_returns$Dec8LogExcess[decile_returns$fomc == 1], na.rm = TRUE)
dec8_annualnonfomcreturn <- nonfomc_days_per_year * mean(decile_returns$Dec8LogExcess[decile_returns$fomc == 0], na.rm = TRUE)

# Decile 9 regression, Sharpe ratio, annual FOMC and non-FOMC returns
dec9_lmrobust <- lm_robust(Dec9LogExcess ~ fomc, data = decile_returns, se_type = "HC1")
dec9_sharpe <- sqrt(8) * (mean(decile_returns$Dec9LogExcess[decile_returns$fomc == 1], na.rm = TRUE) / 
                            sd(decile_returns$Dec9LogExcess[decile_returns$fomc == 1], na.rm = TRUE))
dec9_annualfomcreturn <- fomc_days_per_year * mean(decile_returns$Dec9LogExcess[decile_returns$fomc == 1], na.rm = TRUE)
dec9_annualnonfomcreturn <- nonfomc_days_per_year * mean(decile_returns$Dec9LogExcess[decile_returns$fomc == 0], na.rm = TRUE)

# Decile 10 regression, Sharpe ratio, annual FOMC and non-FOMC returns
dec10_lmrobust <- lm_robust(Dec10LogExcess ~ fomc, data = decile_returns, se_type = "HC1")
dec10_sharpe <- sqrt(8) * (mean(decile_returns$Dec10LogExcess[decile_returns$fomc == 1], na.rm = TRUE) / 
                            sd(decile_returns$Dec10LogExcess[decile_returns$fomc == 1], na.rm = TRUE))
dec10_annualfomcreturn <- fomc_days_per_year * mean(decile_returns$Dec10LogExcess[decile_returns$fomc == 1], na.rm = TRUE)
dec10_annualnonfomcreturn <- nonfomc_days_per_year * mean(decile_returns$Dec10LogExcess[decile_returns$fomc == 0], na.rm = TRUE) 


# Create list with regression models
decile_lmrobustmodels <- list(
  "1st Decile"  = dec1_lmrobust,
  "2nd Decile"  = dec2_lmrobust,
  "3rd Decile"  = dec3_lmrobust,
  "4th Decile"  = dec4_lmrobust,
  "5th Decile"  = dec5_lmrobust,
  "6th Decile"  = dec6_lmrobust,
  "7th Decile"  = dec7_lmrobust,
  "8th Decile"  = dec8_lmrobust,
  "9th Decile"  = dec9_lmrobust,
  "10th Decile" = dec10_lmrobust
)

# Create list of values of annual excess returns on FOMC days 
decile_annualfomc <- c(
  dec1_annualfomcreturn,
  dec2_annualfomcreturn,
  dec3_annualfomcreturn,
  dec4_annualfomcreturn,
  dec5_annualfomcreturn,
  dec6_annualfomcreturn,
  dec7_annualfomcreturn,
  dec8_annualfomcreturn,
  dec9_annualfomcreturn,
  dec10_annualfomcreturn
)

# Create list of values of annual excess returns on non-FOMC days 
decile_nonannualfomc <- c(
  dec1_annualnonfomcreturn,
  dec2_annualnonfomcreturn,
  dec3_annualnonfomcreturn,
  dec4_annualnonfomcreturn,
  dec5_annualnonfomcreturn,
  dec6_annualnonfomcreturn,
  dec7_annualnonfomcreturn,
  dec8_annualnonfomcreturn,
  dec9_annualnonfomcreturn,
  dec10_annualnonfomcreturn
)

# Create list of values of sharpe ratios
decile_sharpe <- c(
  dec1_sharpe,
  dec2_sharpe,
  dec3_sharpe,
  dec4_sharpe,
  dec5_sharpe,
  dec6_sharpe,
  dec7_sharpe,
  dec8_sharpe,
  dec9_sharpe,
  dec10_sharpe
)

# Save values as RDS files
saveRDS(decile_lmrobustmodels, "/Users/angiewang/Desktop/FOMCDrift/data/processed/decile_lmrobustmodels.rds")
saveRDS(decile_annualfomc, "/Users/angiewang/Desktop/FOMCDrift/data/processed/decile_annualfomcvalues.rds")
saveRDS(decile_nonannualfomc, "/Users/angiewang/Desktop/FOMCDrift/data/processed/decile_annualnonfomcvalues.rds")
saveRDS(decile_sharpe, "/Users/angiewang/Desktop/FOMCDrift/data/processed/decile_sharpevalues.rds")



