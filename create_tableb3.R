library(gt)
library(broom)
library(dplyr)

# Read RDS files from regression script
decile_lmrobustmodels <- readRDS("/Users/angiewang/Desktop/FOMCDrift/data/processed/decile_lmrobustmodels.rds")
decile_decile_annualfomc <- readRDS("/Users/angiewang/Desktop/FOMCDrift/data/processed/decile_annualfomcvalues.rds")
decile_annualnonfomc <- readRDS("/Users/angiewang/Desktop/FOMCDrift/data/processed/decile_annualnonfomcvalues.rds")
decile_decile_sharpe <- readRDS("/Users/angiewang/Desktop/FOMCDrift/data/processed/decile_sharpevalues.rds")

# Extract regression values
fomc_coef <- sapply(decile_lmrobustmodels, \(x) x$coefficients["fomc"])
fomc_se   <- sapply(decile_lmrobustmodels, \(x) x$std.error["fomc"])
constant  <- sapply(decile_lmrobustmodels, \(x) x$coefficients["(Intercept)"])
const_se  <- sapply(decile_lmrobustmodels, \(x) x$std.error["(Intercept)"])

# Build table directly in paper-style orientation
results_table <- data.frame(
  Statistic = c(
    "FOMC dummy",
    "FOMC SE",
    "Const.",
    "Constant SE",
    "Annual ex-return FOMC",
    "Annual ex-return non-FOMC",
    "FOMC Sharpe Ratio"
  ),
  `1st Decile`  = c(fomc_coef[1],  fomc_se[1],  constant[1],  const_se[1],  decile_annualfomc[1],  decile_annualnonfomc[1],  decile_sharpe[1]),
  `2nd Decile`  = c(fomc_coef[2],  fomc_se[2],  constant[2],  const_se[2],  decile_annualfomc[2],  decile_annualnonfomc[2],  decile_sharpe[2]),
  `3rd Decile`  = c(fomc_coef[3],  fomc_se[3],  constant[3],  const_se[3],  decile_annualfomc[3],  decile_annualnonfomc[3],  decile_sharpe[3]),
  `4th Decile`  = c(fomc_coef[4],  fomc_se[4],  constant[4],  const_se[4],  decile_annualfomc[4],  decile_annualnonfomc[4],  decile_sharpe[4]),
  `5th Decile`  = c(fomc_coef[5],  fomc_se[5],  constant[5],  const_se[5],  decile_annualfomc[5],  decile_annualnonfomc[5],  decile_sharpe[5]),
  `6th Decile`  = c(fomc_coef[6],  fomc_se[6],  constant[6],  const_se[6],  decile_annualfomc[6],  decile_annualnonfomc[6],  decile_sharpe[6]),
  `7th Decile`  = c(fomc_coef[7],  fomc_se[7],  constant[7],  const_se[7],  decile_annualfomc[7],  decile_annualnonfomc[7],  decile_sharpe[7]),
  `8th Decile`  = c(fomc_coef[8],  fomc_se[8],  constant[8],  const_se[8],  decile_annualfomc[8],  decile_annualnonfomc[8],  decile_sharpe[8]),
  `9th Decile`  = c(fomc_coef[9],  fomc_se[9],  constant[9],  const_se[9],  decile_annualfomc[9],  decile_annualnonfomc[9],  decile_sharpe[9]),
  `10th Decile` = c(fomc_coef[10], fomc_se[10], constant[10], const_se[10], decile_annualfomc[10], decile_annualnonfomc[10], decile_sharpe[10]),
  check.names = FALSE
)

# Format with gt
table_b3_gt <- results_table |>
  gt(rowname_col = "Statistic") |>
  tab_header(
    title = "CRSP Size Portfolio Regressions: 1994–2011",
    subtitle = "Dependent variable: % Daily log excess return"
  ) |>
  fmt_number(
    columns = everything(),
    decimals = 2
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  )

# Save table as tex file
gtsave(table_b3_gt, "/Users/angiewang/Desktop/FOMCDrift/output/table_b3.html")
