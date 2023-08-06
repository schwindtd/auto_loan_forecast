########################################################
# Auto Loan Originations Case Interview
# Forecasting
# Author: Daniel Schwindt
# Date: 8/3/2023
# Date Updated: 8/4/2023
# Purpose: Estimate forecasting models for application volumes
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
library(urca)
library(forecast)
library(pdynmc)

# Set horizon
horizon=12

##-------------------------------------------------------------
## 1. Load data
auto_loan_orig <- readRDS("../data/auto_loan_orig.rds")
moodys <- readRDS("../data/moodys.rds")
comp_rate <- readRDS("../data/comps.rds")

##-------------------------------------------------------------
## 2. Prepare data for modeling
auto_loan_orig <- auto_loan_orig %>% arrange(product, term, tier, preapprove, date)
auto_loan_orig <- auto_loan_orig %>% dplyr::select(-ends_with("_cf"))
auto_loan_orig <- auto_loan_orig %>% mutate(tier_aa = ifelse(tier=="AA",1,0),
                                            tier_a = ifelse(tier %in% c("A", "A-", "A+"),1,0),
                                            tier_b = ifelse(tier %in% c("B", "B-", "B+"),1,0),
                                            tier_c = ifelse(tier %in% c("C", "C-", "C+"),1,0),
                                            tier_d = ifelse(tier %in% c("D", "D-", "D+"),1,0),
                                            tier_e = ifelse(tier %in% c("E", "E-", "E+"),1,0),
                                            tier_ft = ifelse(tier %in% c("FT"),1,0),
                                            preapprove_str = preapprove,
                                            preapprove = ifelse(preapprove=="YES", 1, 0),
                                            term_1 = ifelse(term==1,1,0),
                                            term_2 = ifelse(term==2,1,0),
                                            term_3 = ifelse(term==3,1,0),
                                            term_4 = ifelse(term==4,1,0),
                                            term_5 = ifelse(term==5,1,0)
                                            )

# Forecast membership for last month in data
members_ts <- auto_loan_orig %>% distinct(date, members, members_chg) %>% filter(!is.na(members) | !is.na(members))
members_arima <- auto.arima(members_ts$members)
members_forecast <- forecast(members_arima, h=horizon+1)
# Update NA value for last date in auto_loan_orig members
auto_loan_orig <- auto_loan_orig %>% 
  mutate(members = ifelse(date==as.Date("2022-09-30", "%Y-%m-%d"),
                          members_forecast$mean[1], 
                          members),
         members_chg = members/lag(members)-1)

##-------------------------------------------------------------
## 3. Regressor Forecasts (10Y)
## Membership - constructed above

## Macro Variables
macro_forecast_data <- moodys %>% 
  filter(date > as.Date("2022-09-30", "%Y-%m-%d") & 
           date <= as.Date("2022-09-30") %m+% months(horizon)) #& date <= as.Date("2032-9-30"))

## Competitor Rates
# fit_auto_arima <- function(data, y, x) {
#   arima_model <- auto.arima(data[[y]], xreg = data %>% dplyr::select(all_of(x)) %>% as.matrix())
#   return(arima_model)
# }
comp_rate <- comp_rate %>% 
  filter(product %in% c("used vehicle", "new vehicle", "late model used vehicle")) %>%
  arrange(product, term, date)

# Merge comp_rate and macro vars
comp_rate <- left_join(comp_rate, moodys, by="date")
comp_rate_hist <- comp_rate %>% filter(date >= as.Date("2012-03-31", "%Y-%m-%d"))

# Loop to compute ARIMA models
prod_term <- comp_rate %>% distinct(product, term)
prod_vec <- prod_term$product
term_vec <- prod_term$term
comp_rate_reg <- c("t3y_b") #, "ur_b", "rgdp_b")
arima_models <- list()
for (i in 1:length(prod_vec)){
  tmp <- comp_rate %>% filter(product==prod_vec[i] & term==term_vec[i])
  model <- Arima(tmp$comp_rate, order=c(1,0,1),
                      xreg=tmp %>% dplyr::select(all_of(comp_rate_reg)) %>% as.matrix())
  arima_models[[i]] <- model
}

# Forecast Competitor Rates
comp_rate_forecast <- list()
for (i in 1:length(arima_models)){
  forecast <- forecast(arima_models[[i]], 
                       xreg=as.matrix(macro_forecast_data %>% dplyr::select(t3y_b)))
  comp_rate_forecast[[i]] <- as.numeric(forecast$mean)
}
comp_rate_forecast_data <- do.call(data.frame, comp_rate_forecast)
names(comp_rate_forecast_data) <- c("lmuv_1", "lmuv_2", "lmuv_3",
                                    "nv_1", "nv_2", "nv_3", "nv_4", "nv_5",
                                    "uv_1", "uv_2", "uv_3")
comp_rate_forecast_data <- comp_rate_forecast_data %>%
  mutate(across(everything(), ~ . - lag(.), .names="{col}_chg")
         )

# Create wide version of comp rate historical data
cr_hist_prep <- comp_rate_hist %>% 
  mutate(prod_code = ifelse(product=="late model used vehicle", "lmuv",
                            ifelse(product=="new vehicle", "nv", "uv")
                            )
         ) %>%
  dplyr::select(date, prod_code, term, comp_rate)

cr_hist_wide <- cr_hist_prep %>%
  pivot_wider(
    names_from=c("prod_code", "term"),
    values_from=c("comp_rate")
  )

# Update the NA values in the first forecast period for changes
comp_rate_forecast_data[1,12:22] <- comp_rate_forecast_data[1,1:11] -cr_hist_wide[127,2:12]
  

##-------------------------------------------------------------
## 4. Simple ARIMA model
regressors <- c("comp_rate", "members_chg","t3y_b_chg", "gas_prc_b_chg", "new_vehic_sale_b_chg",
                                 "use_vehic_sale_b_chg", "cpi_new_vehic_b_chg", "cpi_use_vehic_b_chg", "rgdp_b_chg",
                                 "ur_b_chg", "sp500_b_chg")
prod_term_lvl <- auto_loan_orig %>% group_by(product, term, date) %>%
  summarise(volume=sum(volume, na.rm=T),
            across(all_of(regressors), ~ mean(., na.rm=T))
  )
acfs <- list()
pcfs <- list()
for (i in 1:length(prod_vec)){
  tmp <- prod_term_lvl %>% filter(product==prod_vec[i] & term==term_vec[i])
  acfs[[i]] <- acf(tmp$volume, plot=F)
  pcfs[[i]] <- pacf(tmp$volume, plot=F)
}
# Plot ACFs
cairo_ps("../output/acf_results.eps", width = 8, height = 11, pointsize = 12)
par(mfrow=c(4,3), mar=c(4,4,6,2))
for (i in 1:length(acfs)){
  plot(acfs[[i]], main=paste(toupper(prod_vec[i]), ": ", term_vec[i], sep=""))
}
dev.off()
  
# Plot PCFs
cairo_ps("../output/pcf_results.eps", width = 8, height = 11, pointsize = 12)
par(mfrow=c(4,3), mar=c(4,4,6,2))
for (i in 1:length(pcfs)){
  plot(pcfs[[i]], main=paste(toupper(prod_vec[i]), ": ", term_vec[i], sep=""))
}
dev.off()

#-------------------------------------------------------
# Fit Seasonal Random Trend model with MA(1) and SMA(1)
loan_arimas <- list()
for (i in 1:length(prod_vec)){
  tmp <- prod_term_lvl %>% filter(product==prod_vec[i] & term==term_vec[i])
  loan_arimas[[i]] <- Arima(tmp$volume, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
}

# Loop to create forecasts by Product & Term
loan_forecasts <- list()
for (i in 1:length(prod_vec)){
  loan_forecasts[[i]] <- forecast(loan_arimas[[i]], h=horizon)
  
}

# Plot forecasts
ymax = c(2000,15000, 100000, 
         15000, 100000, 200000,
         100000, 100000, 100000,
         250000, 1000000)
cairo_ps(paste("../output/simple_arima_forecast", "_", horizon, ".eps", sep=""), 
               width = 8, height = 11, pointsize = 12)
par(mfrow=c(4,3), mar=c(4,4,6,2))
for (i in 1:length(loan_forecasts)){
  plot(loan_forecasts[[i]], 
       main=paste(toupper(prod_vec[i]), ": ", term_vec[i], sep=""),
       ylim=c(0, ymax[i]))
}
dev.off()
checkresiduals(loan_arimas[[11]]) # 4, 6, 7, 8, 10, 11 have issues
accuracy_list <- lapply(loan_arimas, accuracy)

#-------------------------------------------------------
# Fit ARIMA(1,0,1)x(0,1,1) with constant
loan_arimas_alt <- list()
for (i in 1:length(prod_vec)){
  tmp <- prod_term_lvl %>% filter(product==prod_vec[i] & term==term_vec[i])
  loan_arimas_alt[[i]] <- Arima(tmp$volume, order=c(1,0,1), 
                                seasonal=list(order=c(0,1,1),period=12),
                                include.constant=TRUE)
}

# Loop to create forecasts by Product & Term
loan_forecasts_alt <- list()
for (i in 1:length(prod_vec)){
  loan_forecasts_alt[[i]] <- forecast(loan_arimas_alt[[i]], h=horizon)
  
}

# Plot forecasts
ymax = c(2000,15000, 100000, 
         15000, 100000, 200000,
         100000, 100000, 100000,
         250000, 1000000)
cairo_ps(paste("../output/simple_arima_forecast_alt", "_", horizon, ".eps", sep=""), 
         width = 8, height = 11, pointsize = 12)
par(mfrow=c(4,3), mar=c(4,4,6,2))
for (i in 1:length(loan_forecasts_alt)){
  plot(loan_forecasts_alt[[i]], 
       main=paste(toupper(prod_vec[i]), ": ", term_vec[i], sep=""),
       ylim=c(0, ymax[i]))
}
dev.off()
checkresiduals(loan_arimas_alt[[10]]) # 4, 6, 7, 8, 10, 11 have issues
accuracy_list_alt <- lapply(loan_arimas_alt, accuracy)

#-------------------------------------------------------
# Fit different ARIMA orders to series
orders <- list(c(1,0,1), c(1,0,1), c(1,0,1),
              c(1,1,1), c(1,0,1), c(1,1,1), c(1,1,1), c(1,1,1),
              c(1,0,1), c(1,0,1), c(1,1,1))
lambda <- c(TRUE, TRUE, TRUE, 0, 0, 0, 0, 0, FALSE, FALSE, 0)
loan_arimas_alt2 <- list()
for (i in 1:length(prod_vec)){
  tmp <- prod_term_lvl %>% filter(product==prod_vec[i] & term==term_vec[i])
  loan_arimas_alt2[[i]] <- Arima(tmp$volume, order=orders[[i]], 
                                 seasonal=list(order=c(0,1,1),period=12),
                                 include.constant=TRUE,
                                 lambda = lambda[i])
}

# Loop to create forecasts by Product & Term
loan_forecasts_alt2 <- list()
for (i in 1:length(prod_vec)){
  loan_forecasts_alt2[[i]] <- forecast(loan_arimas_alt2[[i]], h=horizon)
  
}

# Plot forecasts
ymax = c(2000,15000, 100000, 
         15000, 100000, 200000,
         100000, 100000, 100000,
         250000, 1000000)
cairo_ps(paste("../output/simple_arima_forecast_alt2", "_", horizon, ".eps", sep=""), 
         width = 8, height = 11, pointsize = 12)
par(mfrow=c(4,3), mar=c(4,4,6,2))
for (i in 1:length(loan_forecasts_alt2)){
  plot(loan_forecasts_alt2[[i]], 
       main=paste(toupper(prod_vec[i]), ": ", term_vec[i], sep=""),
       ylim=c(0, ymax[i]))
    
}
dev.off()
checkresiduals(loan_arimas_alt2[[11]]) # 4, 6, 7, 8, 10, 11 have issues
accuracy_list_alt2 <- lapply(loan_arimas_alt2, accuracy)
##################################################################
# Create data frames with model information
df_arima <- data.frame(do.call(rbind, accuracy_list))
df_arima$product <- prod_vec
df_arima$term <- term_vec
row.names(df_arima) <- NULL

df_arima_alt <- data.frame(do.call(rbind, accuracy_list_alt))
df_arima_alt$product <- prod_vec
df_arima_alt$term <- term_vec
row.names(df_arima_alt) <- NULL

df_arima_alt2 <- data.frame(do.call(rbind, accuracy_list_alt2))
df_arima_alt2$product <- prod_vec
df_arima_alt2$term <- term_vec
row.names(df_arima_alt2) <- NULL

write.csv(df_arima, file="../output/arima_stats.csv", row.names=F)
write.csv(df_arima_alt, file="../output/arima_alt_stats.csv", row.names=F)
write.csv(df_arima_alt2, file="../output/arima_alt2_stats.csv", row.names=F)

##-------------------------------------------------------------
## 5. Check cross-correlations between potential regressors and ARIMA residuals
macro_hist_data <- moodys %>% filter(date >= as.Date("2012-03-31", "%Y-%m-%d") &
                                       date <= as.Date("2022-09-30", "%Y-%m-%d"))
macro_regressors <- c("rgdp_b_chg", "ur_b_chg", "t3y_b_chg",
                      "cpi_new_vehic_b_chg", "cpi_use_vehic_b_chg",
                      "gas_prc_b_chg", "stock_auto_b_chg", "ip_motor_vehic_b_chg",
                      "new_vehic_sale_b_chg", "use_vehic_sale_b_chg",
                      "sp500_b_chg")
for (i in 1:length(prod_vec)){
  cairo_ps(paste("../output/ccf_", prod_vec[i],"_", term_vec[i],"_macro.eps",sep=""), width = 8, height = 11, pointsize = 12)
  par(mfrow=c(4,3), mar=c(4,4,6,2))
  for (j in 1:length(macro_regressors)){
    tmp <- ccf(loan_arimas_alt2[[i]]$residuals, macro_hist_data[[macro_regressors[j]]], plot=F)
    plot(tmp, main=macro_regressors[j])
  }
  dev.off()
}

# Check against members variables
new_row <- data.frame(date=as.Date("2022-09-30", "%Y-%m-%d"),
                      members=members_forecast$mean[1],
                      members_chg=NA)
members_ts <- rbind(members_ts, new_row)
members_ts <- members_ts %>% mutate(members_chg = ifelse(is.na(members_chg), 
                                                         members/lag(members_chg)-1, 
                                                         members_chg)
                                    )
cairo_ps(paste("../output/ccf_members.eps",sep=""), width = 8, height = 11, pointsize = 12)
par(mfrow=c(4,3), mar=c(4,4,6,2))
for (i in 1:length(prod_vec)){
  tmp <- ccf(loan_arimas_alt2[[i]]$residuals, members_ts$members_chg, plot=F)
  plot(tmp, main=paste(prod_vec[i], ": ", term_vec[i], sep=""))
}
dev.off()

# Check against competitor rates
comp_rate <- comp_rate %>% group_by(product, term) %>% arrange(product, term, date) %>%
  mutate(comp_rate_chg = comp_rate - lag(comp_rate))
comp_rate_hist <- comp_rate %>% ungroup() %>% 
  filter(date >= as.Date("2012-03-31", "%Y-%m-%d") & 
           date <= as.Date("2022-09-30", "%Y-%m-%d"))

cairo_ps(paste("../output/ccf_comp_rates.eps",sep=""), width = 8, height = 11, pointsize = 12)
par(mfrow=c(4,3), mar=c(4,4,6,2))
for (i in 1:length(prod_vec)){
  tmp <- comp_rate_hist %>% filter(product==prod_vec[i] & term==term_vec[i])
  tmp_ccf <- ccf(loan_arimas_alt2[[i]]$residuals, tmp$comp_rate_chg, plot=F)
  plot(tmp_ccf, main=paste(prod_vec[i], ": ", term_vec[i], sep=""))
}
dev.off()

##-------------------------------------------------------------
## 6. Run ARIMA models again but with macro and other regressors
macro_regressors <- c("rgdp_b_chg", "ur_b_chg",
                      "cpi_new_vehic_b_chg", "cpi_use_vehic_b_chg",
                      "new_vehic_sale_b_chg", #"use_vehic_sale_b_chg",
                      "sp500_b_chg")
loan_arimas_reg <- list()
for (i in 1:length(prod_vec)){
  # Create regressors
  tmp_cr <- comp_rate_hist %>% filter(product==prod_vec[i] & term==term_vec[i])
  xreg <- data.frame(comp_rate_chg = tmp_cr$comp_rate_chg,
                     #members_chg = members_ts$members_chg,
                     #l_members_chg = lag(members_ts$members_chg),
                     macro_hist_data %>% dplyr::select(all_of(macro_regressors))
                     )
  # Run ARIMA with regressors
  tmp <- prod_term_lvl %>% ungroup() %>% 
    filter(product==prod_vec[i] & term==term_vec[i])
  # loan_arimas_reg[[i]] <- auto.arima(tmp$volume,xreg=as.matrix(xreg))
  loan_arimas_reg[[i]] <- Arima(tmp$volume, order=orders[[i]],
                                seasonal=list(order=c(0,1,1),period=12),
                                include.constant=TRUE,
                                lambda = lambda[i],
                                xreg=as.matrix(xreg))
  


}
accuracy_reg_list <- lapply(loan_arimas_reg, accuracy)
# Create data frames with model information
df_arima_reg <- data.frame(do.call(rbind, accuracy_reg_list))
df_arima_reg$product <- prod_vec
df_arima_reg$term <- term_vec
row.names(df_arima_reg) <- NULL
write.csv(df_arima_reg, file="../output/arima_reg_stats.csv", row.names=F)

##-------------------------------------------------------------
## 7. FORECAST using ARIMA Regression model
# Loop to create forecasts by Product & Term
loan_arimas_reg_fcst <- list()
for (i in 1:length(prod_vec)){
  # Create regressor forecast matrix
  xreg_fcst <- data.frame(comp_rate_chg = comp_rate_forecast_data[i+11],
                          macro_forecast_data %>% dplyr::select(all_of(macro_regressors))
                          )
  loan_arimas_reg_fcst[[i]] <- forecast(loan_arimas_reg[[i]], h=horizon,
                                        xreg=as.matrix(xreg_fcst))
}

# Plot forecasts
ymax = c(2000,15000, 100000, 
         15000, 100000, 200000,
         100000, 100000, 100000,
         250000, 1000000)
cairo_ps(paste("../output/arima_reg_forecast", "_", horizon, ".eps"),
         width = 8, height = 11, pointsize = 12)
par(mfrow=c(4,3), mar=c(4,4,6,2))
for (i in 1:length(loan_arimas_reg_fcst)){
  plot(loan_arimas_reg_fcst[[i]], 
       main=paste(toupper(prod_vec[i]), ": ", term_vec[i], sep=""),
       ylim=c(0, ymax[i]))
}
dev.off()
################################################################################
################################################################################
################################################################################
# regressors <- c("comp_rate", "members","t3y_b_chg", "gas_prc_b_chg", "new_vehic_sale_b_chg",
#                                  "use_vehic_sale_b_chg", "cpi_new_vehic_b_chg", "cpi_use_vehic_b_chg", "rgdp_b_chg",
#                                  "ur_b_chg", "sp500_b_chg")
# # Loop to create ARIMA models by Product & Term
# prod_term_lvl <- auto_loan_orig %>% group_by(product, term, date) %>%
#   summarise(volume=sum(volume, na.rm=T),
#             across(all_of(regressors), ~ mean(., na.rm=T))
#             )
# loan_arimas <- list()
# for (i in 1:length(prod_vec)){
#   tmp <- prod_term_lvl %>% filter(product==prod_vec[i] & term==term_vec[i])
#   # model <- Arima(tmp$volume, order=c(1,1,3), seasonal=list(order=c(0,1,1),period=12),
#   #                xreg=tmp %>% ungroup() %>% dplyr::select(all_of(regressors)) %>% 
#   #                  as.matrix()
#   #                )
#   model <- auto.arima(tmp$volume,xreg=tmp %>% ungroup() %>% 
#                         dplyr::select(all_of(regressors)) %>% as.matrix()
#                       )
#   loan_arimas[[i]] <- model
# }
# 
# # Loop to create forecasts by Product & Term
# loan_forecasts <- list()
# macro_forecast <- macro_forecast_data %>% dplyr::select(regressors[3:length(regressors)])
# for (i in 1:length(prod_vec)){
#   tmp_reg <- data.frame(comp_rate = as.numeric(comp_rate_forecast_data[[i]]),
#                         members = members_forecast$mean[2:length(members_forecast$mean)],
#                         macro_forecast
#                         )
#   forecast <- forecast(loan_arimas[[i]], h=120, xreg=as.matrix(tmp_reg))
#   loan_forecasts[[i]] <- forecast
# }

## Plot forecast for late model used vehicle term 1
# plot(loan_forecasts[[1]], ylim=c(0,2000))
# plot(loan_forecasts[[6]], ylim=c(0,200000))
# plot(loan_forecasts[[7]], ylim=c(0,50000))
# plot(loan_forecasts[[10]],ylim=c(0,200000))
# plot(loan_forecasts[[11]])


# # Aggregate product level
# regressors <- c("comp_rate", "members","t3y_b_chg", "gas_prc_b_chg", "new_vehic_sale_b_chg",
#                 "use_vehic_sale_b_chg", "cpi_new_vehic_b_chg", "cpi_use_vehic_b_chg", "rgdp_b_chg",
#                 "ur_b_chg", "sp500_b_chg")
# prod_lvl <- auto_loan_orig %>% group_by(product, date) %>%
#   summarise(volume = sum(volume, na.rm=T),
#             across(all_of(regressors), ~ mean(., na.rm=T))
#             )
# 
# # Used Vehicle
# uv_prod_lvl <- prod_lvl %>% filter(product=="used vehicle")
# uv_prod_lvl_xreg <- uv_prod_lvl %>% ungroup() %>% dplyr::select(regressors) %>% as.matrix()
# #uv_prod_lvl_arima <- auto.arima(uv_prod_lvl$volume, xreg=uv_prod_lvl_xreg)
# uv_prod_lvl_arima <- Arima(uv_prod_lvl$volume, xreg=uv_prod_lvl_xreg, 
#                            order=c(1,1,3), seasonal=list(order=c(0,1,1),period=12)
#                            )
# 
# macro_forecast <- macro_forecast_data %>% dplyr::select(regressors[3:length(regressors)])
# uv_forecast_xreg <- data.frame(comp_rate_forecast$mean,
#                                members_forecast_data$members_mean,
#                                macro_forecast)
# uv_forecast <- forecast(uv_prod_lvl_arima, h=120, xreg=as.matrix(uv_forecast_xreg))
# plot(uv_forecast)

##-------------------------------------------------------------
## 4. Dynamic Panel model




