## Prueba

library(readxl)
library(tidyverse)
library(minpack.lm)
library(openxlsx)


## Fix MFO kinetics

MFO_kinetics_data <- prueba_result_MFO$MFO_db %>%
  mutate(porc_MFO = (FAT * 100) /  max(FAT))

# Fit cubic polynomial model# mod <- lm(porc_MFO ~ porc_VO2 + I(porc_VO2^2), data = MFO_kinetics_data)
mod <- lm(porc_MFO ~ poly(porc_VO2, 3), data = MFO_kinetics_data)

###
n_porc_VO2 <- as.numeric(dplyr::pull(prueba_result_MFO$MFO_db[nrow(prueba_result_MFO$MFO_db),"Load"]))

# Create new data to predict
data_nls <- data.frame(porc_VO2 = seq(from = 0, to = n_porc_VO2, length.out = n_porc_VO2))
# Create a vector of percentage VO2 for MFO basic
#porc_VO2 = seq(from = 0, to = 100, length.out = 100)

# Get fitted values
err <- predict(mod, newdata = data_nls, se.fit = F)

data_nls <- data_nls %>%
  mutate(porc_MFO_kinetics = ((err * 100) /  max(err))/100) %>%
  rename(porc_VO2_kinetics = porc_VO2)

n_NAs <- 100 - nrow(data_nls)



data_nls <- data.frame(porc_MFO_kinetics = c(data_nls$porc_MFO_kinetics, rep(NA, n_NAs)),
                       porc_VO2_kinetics = c(data_nls$porc_VO2_kinetics, rep(NA, n_NAs)))

########################### testthat

# Simulate MFO databases
# Basal Metabolism; rows = 48, HR, VO2, VCO2 and RER
HR <- rnorm(n = 48, mean = 66.21, sd = 4.15)
VO2 <- rnorm(n = 48, mean = 281.33, sd = 53.50)
VCO2 <- rnorm(n = 48, mean = 219.96, sd = 36.57)
RER <- rnorm(n = 48, mean = 66.21, sd = 4.15)

M_basal <- data.frame(HR, VO2, VCO2, RER)

# MFO test; rows = 45, HR, VO2, VCO2 and RER
# Random increment
random_inc <- rnorm(n = 45, mean = 0, sd = 1)

HR <- seq(82,162, by = 1.78) + random_inc
VO2 <- seq(837, 2179, by = 29.823) + random_inc
VCO2 <- seq(681, 2222, by = 34.25) + random_inc
RER <- seq(0.81, 1.02, by = 0.00467) + random_inc

MFO_test <- data.frame(HR, VO2, VCO2, RER)

# VO2max
VO2max <- rnorm(n = 1, mean = 2615, sd = 25)

# Random cv_var
cv_var <- sample(c("VO2", "VCO2", "RER"), 1)

# Random author
author <- sample(c("Frayn", "Jeukendrup"), 1)

prueba_result_MFO <- MFO::MFO(step_time = 20,
                              db_MFO = MFO_test,
                              db_basal = M_basal,
                              db_graded = NULL,
                              cv_var = "VO2",
                              author = author,
                              VO2max = VO2max)

expect_equal(ncol(prueba_result_MFO$MFO_db), 4)

prueba_result_MFO_kinetics <- MFO::MFO_kinetics(prueba_result_MFO$MFO_db)




