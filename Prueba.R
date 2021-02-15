## Prueba

library(readxl)
library(tidyverse)
library(minpack.lm)
library(openxlsx)

path = "/Users/jorge/Documents/Paquetes_R/MFO antiguo/NUTAF_xlsx"
participants <- list.files(path)

readxl::read_xlsx()

prueba_read <- read_MFO_databases(from = "files",
                                  path = paste(path,"/",participants[1], sep = ""),
                                  db_basal_name = "M.BASAL",
                                  db_MFO_name = "MFO",
                                  db_graded_name = "V02máx.",
                                  col_name_VO2 = "V'O2",
                                  col_name_VCO2 = "V'CO2",
                                  col_name_RER = "RER",
                                  col_name_HR = "HR",
                               remove_rows = 1)

sample_data <- prueba_read
usethis::use_data(sample_data, compress = "xz")
rm(list = ls())

system.file("extdata", "sample_data.xlsx", package = "MFO")

prueba_result_MFO <- MFO(step_time = 20,
                              db_MFO = prueba_read$participant_db_MFO,
                              db_basal = prueba_read$participant_db_basal,
                              db_graded = prueba_read$participant_db_graded,
                              cv_var = "RER",
                              author = "Frayn",
                              VO2max = NULL)

ncol(prueba_result_MFO$MFO_db)

apply(prueba_result_MFO$MFO_db[,-1], 2, abs)

prueba_result_MFO_kinetics <- MFO_kinetics(prueba_result_MFO$MFO_db)


# Examples MFOs
MFOs(from = "files",
            path = "/Users/jorge/Documents/Paquetes_R/MFO/NUTAF_xlsx",
            db_basal_name = "M.BASAL",
            db_MFO_name = "MFO",
            db_graded_name = "V02máx.",
            step_time = 20,
            cv_var = "RER",
            author = "Frayn",
            VO2max = NULL,
            remove_rows = 1,
            col_name_VO2 = "V'O2",
            col_name_VCO2 = "V'CO2",
            col_name_RER = "RER",
            col_name_HR = "HR",
            save_plot = T)


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
VO2 <- seq(837, 2179, by = 29.83) + random_inc
VCO2 <- seq(681, 2222, by = 34.25) + random_inc
RER <- seq(0.81, 1.02, by = 0.00467) + random_inc

MFO_test <- data.frame(HR, VO2, VCO2, RER)

# VO2max
VO2max <- rnorm(n = 1, mean = 2615, sd = 10)

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

prueba_result_MFO_kinetics <- MFO_kinetics(prueba_result_MFO$MFO_db)


n_porc_VO2 <- as.numeric(pull(prueba_result_MFO$MFO_db[nrow(prueba_result_MFO$MFO_db),"Load"]))

# Fit cubic polynomial model# mod <- lm(porc_MFO ~ porc_VO2 + I(porc_VO2^2), data = MFO_kinetics_data)
mod <- lm(porc_MFO ~ poly(porc_VO2, 3), data = prueba_result_MFO_kinetics$MFO_kinetics_data)

# Create new data
data_nls <- data.frame(porc_VO2 = seq(from = 0, to = n_porc_VO2, length.out = n_porc_VO2))

# Get fitted values
err <- predict(mod, newdata = data_nls, se.fit = F)

data_nls <- data_nls %>%
  mutate(porc_MFO_kinetics = ((err * 100) /  max(err))/100) %>%
  rename(porc_VO2_kinetics = porc_VO2)


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





