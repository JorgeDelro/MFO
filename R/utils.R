
#' Basal metabolic rate
#'
#' @param step_time how often the data was collected (in seconds).
#' @param db a database
#' @param cv_var variable to calculate coefficient of variation
#'
#' @importFrom dplyr pull mutate
#' @importFrom tibble add_column
#' @importFrom magrittr %>%
#'
met_basal <- function(step_time, db, cv_var) {

  n_row <- calculate_steps(step_time = step_time,
                           db = NULL,
                           db_type = "basal")

  # First rows
  cv_VO2_1 <- rep(NA, n_row)
  cv_VCO2_1 <- rep(NA, n_row)
  cv_RER_1 <- rep(NA, n_row)

  n_row <- n_row + 1

  # Empty vectors
  cv_VO2_2 <- rep(NA, length(pull(db["VO2"])) - n_row)
  cv_VCO2_2 <- rep(NA, length(pull(db["VCO2"])) - n_row)
  cv_RER_2 <- rep(NA, length(pull(db["RER"])) - n_row)

  # VO2
  ############ VO2
  j <- 1
  for (i in n_row:length(pull(db["VO2"]))) {
    cv_VO2_2[j] <- sd(pull(db[j:i, "VO2"])) / mean(pull(db[j:i, "VO2"]))
    j <- j + 1
  }

  ############ VCO2
  j <- 1
  for (i in n_row:length(pull(db["VCO2"]))) {
    cv_VCO2_2[j] <- sd(pull(db[j:i, "VCO2"])) / mean(pull(db[j:i, "VCO2"]))
    j <- j + 1
  }

  ############ RER
  j <- 1
  for (i in n_row:length(pull(db["RER"]))) {
    cv_RER_2[j] <- sd(pull(db[j:i, "RER"])) / mean(pull(db[j:i, "RER"]))
    j <- j + 1
  }

  # Merge variables
  cv_VO2 <- round(c(cv_VO2_1, cv_VO2_2*100),2)
  cv_VCO2 <- round(c(cv_VCO2_1, cv_VCO2_2*100),2)
  cv_RER <- round(c(cv_RER_1, cv_RER_2*100),2)

  # Add columns to db
  db <- db %>%
    add_column(cv_VO2, cv_VCO2, cv_RER)

  # get 5 min db
  cv_var <- paste("cv_", cv_var, sep = "")
  db_5min <- get_5min(db, cv_var = cv_var, n_row = n_row)

  db_5min <- db_5min %>%
    mutate(CHO_Frayn = (4.55 * VCO2 - 3.21 * VO2) / 1000,
           FAT_Frayn = (1.67 * VO2 - 1.67 * VCO2) / 1000,
           Kcal_total_Frayn = CHO_Frayn * 4 + FAT_Frayn * 9)

  ## Return
  # mean VO2
  x_VO2 <- mean(pull(db_5min["VO2"]))
  # mean HR
  x_HR <- mean(pull(db_5min["HR"]))
  # mean RER
  x_RER <- mean(pull(db_5min["RER"]))
  # mean CHO
  x_CHO <- mean(pull(db_5min["CHO_Frayn"]))
  # mean FAT
  x_FAT <- mean(pull(db_5min["FAT_Frayn"]))
  # mean Kcal
  x_Kcal <- mean(pull(db_5min["Kcal_total_Frayn"]))

  return(list(db = db,
              db_5min = db_5min,
              x_VO2 = x_VO2,
              #x_HR = x_HR,
              x_RER = x_RER,
              x_CHO = x_CHO,
              x_FAT = x_FAT,
              x_Kcal = x_Kcal))
}


#
#' Calculation of CHO, FAT and Kcal
#'
#' @param step_time how often the data was collected (in seconds).
#' @param db_MFO dtabase with MFO test
#' @param VO2max maximum oxygen uptake
#' @param author eithe "Frayn" or "Jeukendrup"
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
calculate_vars <- function(step_time, db_MFO, VO2max, author) {

  steps <- calculate_steps(step_time = step_time,
                           db = db_MFO,
                           db_type = "MFO")

  db_vars <- data.frame(matrix(ncol = 5, nrow = steps$n_steps))
  colnames(db_vars) <- c("VO2", "HR", "CHO", "FAT", "Kcal")

  if(author == "Frayn") {
    # CHO_Frayn (4.55 * VCO2 - 3.21 * VO2) / 1000
    # FAT_Frayn (1.67 * VO2 - 1.67 * VCO2) / 1000
    # Kcal_total_Frayn - CHO_Frayn * 4 + FAT_Frayn * 9
    db_MFO <- db_MFO %>%
      mutate(CHO_Frayn = (4.55 * VCO2 - 3.21 * VO2) / 1000,
             FAT_Frayn = (1.67 * VO2 - 1.67 * VCO2) / 1000,
             Kcal_total_Frayn = CHO_Frayn * 4 + FAT_Frayn * 9)


    for (i in seq(1:steps$n_steps)) {

      # mean VO2
      db_vars[i, "VO2"] <- mean(as.vector(db_MFO[steps$lower_bound:steps$upper_bound, "VO2"]))
      # mean HR
      db_vars[i, "HR"] <- mean(as.vector(db_MFO[steps$lower_bound:steps$upper_bound, "HR"]))
      # mean CHO
      db_vars[i, "CHO"] <- mean(as.vector(db_MFO[steps$lower_bound:steps$upper_bound, "CHO_Frayn"]))
      # mean FAT
      db_vars[i, "FAT"] <- mean(as.vector(db_MFO[steps$lower_bound:steps$upper_bound, "FAT_Frayn"]))
      # mean Kcal
      db_vars[i, "Kcal"] <- mean(as.vector(db_MFO[steps$lower_bound:steps$upper_bound, "Kcal_total_Frayn"]))

      # next bounds
      if(step_time == 20){
        steps$upper_bound <- (9 * (i+1))
        steps$lower_bound <- steps$upper_bound - 3
      }
    }

  }  else if (author == "Jeukendrup") {

    # VO2 < 50% VO2max
    # CHO_Jeukendrup (4.344 * VCO2 - 3.061 * VO2) / 1000
    # FAT_Jeukendrup (1.695 * VO2 - 1.701 * VCO2) / 1000
    # Kcal_total_Jeukendrup - CHO_Jeukendrup * 4 + FAT_Jeukendrup * 9
    # VO2 > 50% VO2max
    # CHO_Jeukendrup (4.210 * VCO2 - 2.962 * VO2) / 1000
    # FAT_Jeukendrup (1.695 * VO2 - 1.701 * VCO2) / 1000
    # Kcal_total_Jeukendrup - CHO_Jeukendrup * 4 + FAT_Jeukendrup * 9

    db_MFO <- db_MFO %>%
      mutate(CHO_Jeukendrup_40_50_VO2 = (4.344 * VCO2 - 3.061 * VO2) / 1000,
             FAT_Jeukendrup_40_50_VO2 = (1.695 * VO2 - 1.701 * VCO2) / 1000,
             Kcal_total_Jeukendrup_40_50_VO2 = CHO_Jeukendrup_40_50_VO2 * 4 + FAT_Jeukendrup_40_50_VO2 * 9,
             CHO_Jeukendrup_50_75_VO2 = (4.210 * VCO2 - 2.962 * VO2) / 1000,
             FAT_Jeukendrup_50_75_VO2 = (1.695 * VO2 - 1.701 * VCO2) / 1000,
             Kcal_total_Jeukendrup_50_75_VO2 = CHO_Jeukendrup_50_75_VO2 * 4 + FAT_Jeukendrup_50_75_VO2 * 9)

    for (i in seq(1:steps$n_steps)) {

      # mean VO2
      db_vars[i, "VO2"] <- mean(as.vector(db_MFO[steps$lower_bound:steps$upper_bound, "VO2"]))
      # mean HR
      db_vars[i, "HR"] <- mean(as.vector(db_MFO[steps$lower_bound:steps$upper_bound, "HR"]))

      if((db_vars[i, "VO2"] / VO2max) < 50) {
        # mean CHO
        db_vars[i, "CHO"] <- mean(as.vector(db_MFO[steps$lower_bound:steps$upper_bound, "CHO_Jeukendrup_40_50_VO2"]))
        # mean FAT
        db_vars[i, "FAT"] <- mean(as.vector(db_MFO[steps$lower_bound:steps$upper_bound, "FAT_Jeukendrup_40_50_VO2"]))
        # mean Kcal
        db_vars[i, "Kcal"] <- mean(as.vector(db_MFO[steps$lower_bound:steps$upper_bound, "Kcal_total_Jeukendrup_40_50_VO2"]))
      } else {
        # mean CHO
        db_vars[i, "CHO"] <- mean(as.vector(db_MFO[steps$lower_bound:steps$upper_bound, "CHO_Jeukendrup_50_75_VO2"]))
        # mean FAT
        db_vars[i, "FAT"] <- mean(as.vector(db_MFO[steps$lower_bound:steps$upper_bound, "FAT_Jeukendrup_50_75_VO2"]))
        # mean Kcal
        db_vars[i, "Kcal"] <- mean(as.vector(db_MFO[steps$lower_bound:steps$upper_bound, "Kcal_total_Jeukendrup_50_75_VO2"]))
      }

      # next bounds
      if(step_time == 20){
        steps$upper_bound <- (9 * (i+1))
        steps$lower_bound <- steps$upper_bound - 3
      }
    }

  }

  return(db_vars)

}

#' Calculate steps
#'
#' @param step_time how often the data was collected (in seconds).
#' @param db a database
#' @param db_type either "basal" or "MFO"
#'
#'
#' @examples
calculate_steps <- function(step_time, db, db_type) {

  if(db_type == "basal") {
    if(step_time == 20){
      n_steps <- 14
    }
    return(n_steps)
  }

  if(db_type == "MFO") {
    if(step_time == 20){
      n_steps <- nrow(db)/9
      upper_bound <- 9
      lower_bound <- upper_bound - 3
    }
    return(list(n_steps = n_steps,
                upper_bound = upper_bound,
                lower_bound = lower_bound))
  }
}

#' Get a 5 minutes database
#'
#' @param db a database
#' @param cv_var variable to calculate coefficient of variation
#' @param n_row number of rows
#'
#' @importFrom dplyr pull
#'
get_5min <- function(db, cv_var, n_row) {

  pos_final <- which.min(pull(db[,cv_var]))
  pos_ini <- which.min(pull(db[,cv_var])) - n_row

  db_5min <- db[pos_ini:pos_final,]

  return(db_5min)

}
