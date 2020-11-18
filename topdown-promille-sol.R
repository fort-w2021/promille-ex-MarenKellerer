
library(checkmate)
library(tidyr)
library(dplyr)
library(tibble)


# predefined functions ----------------------------------------------------


# data.frame with the type of the drink, the alcohol and the volume
drinks_information <-
  data.frame(
    "type" = c("massn", "hoibe", "wein", "schnaps"),
    "alcohol" = c(0.06, 0.06, 0.11, 0.4),
    "volume" = c(1000, 500, 200, 40)
  )



# function for computing the mass of alcohol (A)
# input: drinks as list or vector
# output: absorbed mass of alcohol

compute_alcohol_mass <- function(drinks) {

  # suming up multiple drinks of the same type
  drinks <- tapply(drinks, names(drinks), sum)

  alcohol_density <- 0.8

  drinks_df <-
    data.frame(drinks) %>%
    rownames_to_column(var = "type") %>%
    right_join(drinks_information, by = "type") %>%
    mutate_all(~ replace_na(., 0)) %>%
    mutate(mass = drinks * alcohol * volume * alcohol_density)


  return(sum(drinks_df$mass))
}


# function for computing the total body water
# input: sex, age, height and weight
# output: total body water depending on the sex

compute_total_body_water <- function(age, sex, height, weight) {
  if (sex == "male" | sex == "m") {
    total_body_water <- 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight
    return(total_body_water)
  }

  total_body_water <- 0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight
  total_body_water
}



# per mill function -------------------------------------------------------


# function for computing the per mill value
# input: age, sex, height, weight, drinking_time, drinks
# output: per mill value

# checks for the inputs:
# age: count
# sex: accept several options for "male" or "female"
# height: numeric, with finite values above 1
# weight: numeric, with finite values above 1
# drinking_time: type POSIXct
# drinks: one of the drinks massn, hoibe, wein, schnaps have to be chosen


tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight, drinking_time, drinks) {
  
  sex <- match.arg(tolower(sex), c("female", "male"))
  drinks <- unlist(drinks) # for allowing list or vector as input


  assert_count(age)
  assert_numeric(height, lower = 1, finite = TRUE)
  assert_numeric(weight, lower = 1, finite = TRUE)
  assert_posixct(drinking_time)

  if (!any(names(drinks) %in% drinks_information[, 1])) {
    warning("please choose between the drinks: massn, hoibe, wein, schnaps")
  }

  if (age < 16 | (age >= 16 & age < 18) & any(names(drinks) == "schnaps")) {
    warning("anything under age < 16 or hard liquor under age 18 is illegal")
  }

  if (diff(drinking_time) < 0) {
    stop("beginning of drinking is after end of drinkint")
  }

  # defining all parameters needed for calculating the blood alcohol concentration
  alcohol_density <- 0.8
  alcohol_mass <- compute_alcohol_mass(drinks)
  blood_density <- 1.055
  total_body_water <- compute_total_body_water(age, sex, height, weight)
  drinking_time <- as.numeric(diff.POSIXt(drinking_time), units = "hours")


  per_mill <-
    (alcohol_density * alcohol_mass) / (blood_density * total_body_water) - ((drinking_time - 1) * 0.15)
  

  # reducing the alcohol is only taken into account from the second hour
  if (drinking_time < 2) {
    per_mill <- (alcohol_density * alcohol_mass) / (blood_density * total_body_water)
  }

  # per mill value can't be negative
  if (drinking_time > 1) {
    per_mill <- max(0, per_mill)
  }

  per_mill
}



# tests -------------------------------------------------------------------

testthat::test_file("topdown-promille-tests.R")
