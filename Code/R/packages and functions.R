
library(haven)     # read Stata .dta
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(purrr)
library(survey)    # weighted H, A, M0
library(readr)
library(haven)
library(Hmisc)
library(skimr)
library(ggplot2)
library(fixest)
library(broom)

## variable check 
var_check <- function(df, x) {
  print(unique(df[[x]]))
}

recode_beating <- function(x) {
  x <- as.numeric(x)
  dplyr::case_when(
    x == 1  ~ 1L,     # justifies beating -> deprived
    x == 0  ~ 0L,     # does not justify -> not deprived
    TRUE ~ NA_integer_
  )
}
## quick check to ensure all beating values follow this pattern 

recode_decision <- function(x) {
  x <- as.numeric(x)
  dplyr::case_when(
    x %in% c(1, 3)       ~ 0L,     # respondent alone or joint -> not deprived
    x %in% c(2, 4, 5)    ~ 1L,     # husband alone / someone else / other -> deprived
    TRUE                 ~ NA_integer_
  )
}

recode_v632a <- function(x) {
  x <- as.numeric(x)
  dplyr::case_when(
    x %in% c(1, 3)       ~ 0L,     # respondent or joint -> not deprived
    x %in% c(2, 6)       ~ 1L,     # husband/other -> deprived
    TRUE                 ~ NA_integer_
  )
}

recode_v850a <- function(x) {
  x <- as.numeric(x)
  dplyr::case_when(
    x == 1               ~ 0L,     # can refuse -> not deprived
    x == 2               ~ 1L,     # cannot refuse -> deprived
    TRUE                 ~ NA_integer_
  )
}

mk_domain_weights <- function(df, vars, domain_total_weight) {
  n_nonmiss <- df %>% 
    select(all_of(vars)) %>%
    #creates observed matrix is indicator is not NA then 1 if is NA then 0
    mutate(across(everything(), ~ !is.na(.))) %>%
    transmute(n = rowSums(across(all_of(vars))))
  # Each observed indicator gets equal share of the domain weight; missing get weight 0
  w <- df %>% 
    select(all_of(vars)) %>%
    mutate(across(everything(), ~ ifelse(!is.na(.), 1, 0))) %>%
    #for each person how many items were observed, so den_i number of 
    # non missing indicators in this domain for person i 
    mutate(den = pmax(rowSums(across(all_of(vars))), 0)) %>%
    mutate(across(all_of(vars), ~ ifelse(den > 0, domain_total_weight / den, 0))) %>%
    select(-den)
  w
}
