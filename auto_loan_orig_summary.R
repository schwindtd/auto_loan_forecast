########################################################
# Auto Loan Originations Case Interview
# Summary Statistics
# Author: Daniel Schwindt
# Date: 8/3/2023
# Date Updated:
# Purpose: Compute summary statistics and prelim data 
#          visualizations
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

##-------------------------------------------------------------
## 1. Load data
auto_loan_orig <- readRDS("../data/auto_loan_orig.rds")

##-------------------------------------------------------------
## 2. Summary statistics
## By PRODUCT
summary_prod <- auto_loan_orig %>% group_by(product) %>%
  summarise(volume = sum(volume, na.rm=T),
            term = mean(term, na.rm=T))
## By DATE and PRODUCT
summary <- auto_loan_orig %>% group_by(date, product) %>%
  summarise(volume = sum(volume, na.rm=T))

# Plot data
ggplot(summary) +
  geom_line(aes(x=date, y=volume,color=product))

## By DATE and PRODUCT and TERM
summary_term <- auto_loan_orig %>% group_by(date, product, term) %>%
  summarise(volume = sum(volume, na.rm=T))

# Plot data
# New Vehicle
ggplot(summary_term %>% filter(product=="new vehicle")) +
  geom_line(aes(x=date, y=volume, color=as.factor(term)))

# Used Vehicle
ggplot(summary_term %>% filter(product=="used vehicle")) +
  geom_line(aes(x=date, y=volume, color=as.factor(term)))

# Late Model Used Vehicle
ggplot(summary_term %>% filter(product=="late model used vehicle")) +
  geom_line(aes(x=date, y=volume, color=as.factor(term)))

## By DATE and PRODUCT and TERM and PREAPPROVE
summary_preapp <- auto_loan_orig %>% group_by(date, product, preapprove) %>%
  summarise(volume = sum(volume, na.rm=T))
# Plot data
# New Vehicle
ggplot(summary_preapp %>% filter(product=="new vehicle")) +
  geom_line(aes(x=date, y=volume, color=as.factor(preapprove)))
# Used Vehicle
ggplot(summary_preapp %>% filter(product=="used vehicle")) +
  geom_line(aes(x=date, y=volume, color=as.factor(preapprove)))
# Late Model Used Vehicle
ggplot(summary_preapp %>% filter(product=="late model used vehicle")) +
  geom_line(aes(x=date, y=volume, color=as.factor(preapprove)))

## Summary by PREAPPROVE
summary_preapp_only <- auto_loan_orig %>% group_by(preapprove) %>%
  summarise(volume = sum(volume, na.rm=T),
            term = mean(term, na.rm=T))

## Summary by TIER
summary_tier_only <- auto_loan_orig %>% group_by(tier) %>%
  summarise(volume = sum(volume, na.rm=T),
            term = mean(term, na.rm=T))

ggplot(summary_tier_only) +
  geom_col(aes(x=tier, y=volume))

## Summary by PRODUCT and TIER
summary_prod_tier <- auto_loan_orig %>% group_by(product,tier) %>%
  summarise(volume = sum(volume, na.rm=T),
            term = mean(term, na.rm=T))
# New Vehicle
ggplot(summary_prod_tier %>% filter(product=="new vehicle")) +
  geom_col(aes(x=tier, y=volume))
# Used Vehicle
ggplot(summary_prod_tier %>% filter(product=="used vehicle")) +
  geom_col(aes(x=tier, y=volume))
# Late Model Used Vehicle
ggplot(summary_prod_tier %>% filter(product=="late model used vehicle")) +
  geom_col(aes(x=tier, y=volume))

## Summary by PRODUCT and TERM and TIER
summary_term_tier <- auto_loan_orig %>% group_by(date, product, term, tier) %>%
  summarise(volume=sum(volume, na.rm=T))
# Used Vehicle - 3
ggplot(summary_term_tier %>% filter(product=="used vehicle" & term==3)) +
  geom_line(aes(x=date, y=volume, color=tier))

# New Vehicle - 4
ggplot(summary_term_tier %>% filter(product=="new vehicle" & term==4)) +
  geom_line(aes(x=date, y=volume, color=tier))

# Late Model Used Vehicle - 3
ggplot(summary_term_tier %>% filter(product=="late model used vehicle" & term==3)) +
  geom_line(aes(x=date, y=volume, color=tier))

## Competitor Rates by date, product, tier, and term
summary_comps <- auto_loan_orig %>% group_by(date, product, term) %>%
  summarise(comp_rate = mean(comp_rate))
ggplot(summary_comps %>% filter(product == "new vehicle")) +
  geom_line(aes(x=date, y=comp_rate, color=as.factor(term)))
ggplot(summary_comps %>% filter(product=="used vehicle")) +
  geom_line(aes(x=date, y=comp_rate, color=as.factor(term)))
ggplot(summary_comps %>% filter(product=="late model used vehicle")) +
  geom_line(aes(x=date, y=comp_rate, color=as.factor(term)))

## Membership by date
summary_members <- auto_loan_orig %>% group_by(date) %>%
  summarise(members = max(members))
ggplot(summary_members) + geom_line(aes(x=date, y=members))

summary_members <- summary_members %>% mutate(pct_chg_members = members/lag(members)-1)

## 3. Correlations
correl_comp_rate <- auto_loan_orig %>% group_by(product, term) %>%
  summarise(corr_v_r = cor(volume, comp_rate))

correl_macro <- auto_loan_orig %>% group_by(product, term) %>%
  summarise(corr_v_t3y = cor(volume, t3y_b),
            corr_v_gas = cor(volume, gas_prc_b),
            corr_v_pvn = cor(volume, cpi_new_vehic_b),
            corr_v_pvu = cor(volume, cpi_use_vehic_b),
            corr_v_pvum = cor(volume, use_vehic_sale_b),
            corr_v_ur = cor(volume, ur_b),
            corr_v_gdp = cor(volume, rgdp_b),
            corr_v_sp500 = cor(volume, sp500_b)
            )

correl_macro_chg <- auto_loan_orig %>% group_by(product, term) %>%
  summarise(corr_v_t3y = cor(volume, t3y_b_chg),
            corr_v_gas = cor(volume, gas_prc_b_chg),
            corr_v_pvn = cor(volume, cpi_new_vehic_b_chg),
            corr_v_pvu = cor(volume, cpi_use_vehic_b_chg),
            corr_v_pvum = cor(volume, use_vehic_sale_b_chg),
            corr_v_ur = cor(volume, ur_b_chg),
            corr_v_gdp = cor(volume, rgdp_b_chg),
            corr_v_sp500 = cor(volume, sp500_b_chg)
  )