########################################################
# Auto Loan Originations Case Interview
# Data Creation
# Author: Daniel Schwindt
# Date: 8/2/2023
# Date Updated: 
# Purpose: Load individual datasets, clean formatting, and merge
########################################################

## 0. Housekeeping
rm(list=ls())
setwd("~/Documents/jobs/2023/interviews/nfcu/case_interview/code")

library(tidyverse)
library(haven)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lubridate)
library(proxy)
library(vars)

## 1. Load Data
members <- read_excel("../data/Data Dictionary.xlsx", sheet = "Membership")
apps <- read_excel("../data/Data Dictionary.xlsx", sheet = "Application Volume")
comps <- read_excel("../data/Data Dictionary.xlsx", sheet = "Competitor Rates")
moodys_names <- read_excel("../data/Data Dictionary.xlsx", sheet = "Moodys", 
                     range="A1:W1", col_names=F)
moodys <- read_xlsx("../data/Data Dictionary.xlsx", sheet = "Moodys", 
                     skip=5, col_names=F, na="ND", 
                    col_types=c("date", rep("numeric",22))
                    )

## 2. Convert Data formatting
## Membership dataset
# Create date variable (in date format) & change variable names
members <- members %>% 
  mutate(day = ifelse(Month %in% c(1,3,5,7,8,10,12), 31,
                                           ifelse(Month %in% c(4,6,9,11), 30,28)
                                           ),
                              date_str = paste(Year, Month, day, sep="-"),
                              date = as.Date(date_str, "%Y-%m-%d")
                              ) %>%
  dplyr::select(date, Total)
names(members) <- c("date","members")
members <- members %>% arrange(date) %>% mutate(members_chg = members/lag(members)-1)

## Applications dataset
# Create date variable (in date format) & change variable names
apps <- apps %>% 
  mutate(day = ifelse(MONTHENTERED %in% c(1,3,5,7,8,10,12), 31,
                      ifelse(MONTHENTERED %in% c(4,6,9,11), 30,28)
                      ),
         date_str = paste(YEARENTERED, MONTHENTERED, day, sep="-"),
         date = as.Date(date_str, "%Y-%m-%d"),
         PRODUCT_GROUP = tolower(PRODUCT_GROUP)
         )
# Rename vars
names(apps) <- c("month", "year", "product", "preapprove", "tier", "volume", 
                 "disc_bps25", "disc_bps100", "term", "day", "date_str", "date")
# Drop date_str var
apps <- apps %>% dplyr::select(-date_str, -month, -year, -day)

## Comps dataset
comps <- comps %>% 
  mutate(month = month(`Report Date`),
         year = year(`Report Date`),
         day = ifelse(month %in% c(1,3,5,7,8,10,12), 31,
                      ifelse(month %in% c(4,6,9,11), 30,28)
         ),
         date_str = paste(year, month, day, sep="-"),
         date = as.Date(date_str, "%Y-%m-%d"),
         `Product Group` = tolower(`Product Group`)
         )

# Rename vars
names(comps) <- c("term", "product", "comp_rate", "report_date", "month", 
                  "year", "day", "date_str", "date")
# Drop date_str var
comps <- comps %>% dplyr::select(-date_str, -report_date, -month, -year, -day)

## Moody's dataset
var_names <- as.character(unlist(moodys_names))
var_names[1] <- "date"
names(moodys) <- var_names
# Drop row with duplicated date in February 1900
moodys <- moodys %>% filter(!is.na(date))
# Create new date variables to facilitate merging with other datasets
moodys <- moodys %>% 
  mutate(month = month(date),
         day = ifelse(month %in% c(1,3,5,7,8,10,12), 31,
                      ifelse(month %in% c(4,6,9,11), 30,28)
                      ),
         year = year(date),
         date_str = paste(year, month, day, sep="-"),
         date = as.Date(date_str, "%Y-%m-%d")
         )
# Change macro variable names
macro_vars <- c("t3y", "gas_prc", "new_vehic_sale", "use_vehic_sale", 
                     "cpi_new_vehic", "cpi_use_vehic", "stock_auto", "ip_motor_vehic",
                     "rgdp", "ur", "sp500")
suffix <- c("_b", "_cf")
macro_vars_names <- paste(rep(macro_vars, each=2), suffix, sep="")
names(moodys) <- c("date", macro_vars_names, "month", "day", "year", "date_str")
# Drop date string variable
moodys <- moodys %>% dplyr::select(-date_str, -month, -day, -year)
## Remove unused datasets
rm(moodys_names, name_combos)

# Create change variables
## 4. Transform macro series to changes
macro_pchg_vars <- c("gas_prc", "new_vehic_sale", "use_vehic_sale", "cpi_new_vehic",
                     "cpi_use_vehic", "stock_auto", "ip_motor_vehic", "rgdp",
                     "sp500")
macro_chg_vars <- c("t3y", "ur")
moodys <- moodys %>% arrange(date) %>%
  mutate(across(all_of(starts_with(macro_pchg_vars)), ~ ./lag(.)-1, .names="{col}_chg"),
         across(all_of(starts_with(macro_chg_vars)), ~.-lag(.), .names="{col}_chg")
         )

## 3. Merge Datasets
loan_orig <- left_join(apps, members, by="date")
loan_orig <- left_join(loan_orig, comps, 
                            by=c("date"="date", "product"="product", "term"="term")
                            )
loan_orig <- left_join(loan_orig, moodys, by="date")

## 4. Filter out non-auto products & incorrect term = 999
auto_loan_orig <- loan_orig %>% 
  filter(product %in% c("late model used vehicle", "new vehicle", "used vehicle"),
         term !=999)

## 5. Save dataset
saveRDS(auto_loan_orig, file="../data/auto_loan_orig.rds")
saveRDS(moodys, file="../data/moodys.rds")
saveRDS(comps, file="../data/comps.rds")
