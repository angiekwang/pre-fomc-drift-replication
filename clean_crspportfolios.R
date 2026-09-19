library(readr)
library(dplyr)
library(here)
library(lubridate)

# Import data 
crsp_portfolio <- read.csv(here("data", "raw", "Equal_Value_Weighted_WRDS.csv"))
famafrench_data <- read.csv(here("data", "raw", "F-F_Research_Data_Factors_daily.CSV"))
famafrench_data <- subset(famafrench_data, select = c(Date, RF))

# Merge dataframes by date
crsp_portfolio$Date <- ymd(crsp_portfolio$Date)
famafrench_data$Date <- ymd(famafrench_data$Date)
crsp_portfolio <- merge(crsp_portfolio, famafrench_data, by = "Date")
crsp_portfolio <- crsp_portfolio %>% mutate(crsp_portfolio, across(c(vwretd, ewretd), as.numeric))

# Filter date from 1994-09-01 to 2011-03-30
crsp_portfolio <- crsp_portfolio %>% filter(Date >= "1994-09-01" & Date <="2011-03-30")

# Add dummy variable column: 1 if fomc announcement date, 0 otherwise
fomc_dates <- c("19940927", "19941115", "19941220",
                "19950201", "19950328", "19950523", "19950706","19950822", "19950926", "19951115", "19951219",
                "19960131", "19960521", "19960703", "19960820","19960924", "19961113", "19961217",
                "19970205", "19970325", "19970520", "19970702", "19970819", "19970930", "19971112", "19971216",
                "19980204", "19980331", "19980519", "19980701", "19980818", "19980929", "19981117", "19981222",
                "19990203", "19990330", "19990518", "19990630", "19990824", "19991005", "19991116", "19991221",
                "20000202", "20000321", "20000516", "20000628", "20000822", "20001003", "20001115", "20001219",
                "20010131", "20010320", "20010515", "20010627", "20010821", "20011002", "20011106", "20011211",
                "20020130", "20020319", "20020507", "20020626", "20020813", "20020924", "20021106", "20021210",
                "20030129", "20030318", "20030506", "20030625", "20030812", "20030916", "20031028", "20031209",
                "20040128", "20040316", "20040504", "20040630", "20040810", "20040921", "20041110", "20041214",
                "20050202", "20050322", "20050503", "20050630", "20050809", "20050920", "20051101", "20051213",
                "20060131", "20060328", "20060510", "20060629", "20060808", "20060920", "20061025", "20061212",
                "20070131", "20070321", "20070509", "20070628", "20070807", "20070918", "20071031", "20071211",
                "20080130", "20080318", "20080430", "20080625", "20080805", "20080916", "20081029", "20081216",
                "20090128", "20090318", "20090429", "20090624", "20090812", "20090923", "20091104", "20091216",
                "20100127", "20100316", "20100428", "20100623", "20100810", "20100921", "20101103", "20101214",
                "20110126", "20110315")
fomc_dates <- as.Date(fomc_dates, format = "%Y%m%d")
crsp_portfolio <- crsp_portfolio %>% mutate(fomc = ifelse(Date %in% fomc_dates, 1, 0))

# Add columns for excess returns and log excess returns
crsp_portfolio <- crsp_portfolio %>%
  mutate(
    EqualExcess = ewretd - RF,
    EqualLogExcess =
      100 * (log1p(ewretd) - log1p(RF / 100)),
    
    ValueExcess = vwretd - RF,
    ValueLogExcess =
      100 * (log1p(vwretd) - log1p(RF / 100)),
  )

# Save decile_returns dataframe as RDS file
saveRDS(crsp_portfolio, here("data", "processed", "crsp_returns_clean.rds"))

# Save decile_returns dataframe as .csv file (without row numbers)
write.csv(crsp_portfolio, here("data", "processed", "crspreturns.csv") row.names = FALSE)
