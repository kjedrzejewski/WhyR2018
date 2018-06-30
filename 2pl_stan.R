library(rstan)
library(dplyr)
library(tidyr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

xxx_calc_2pl_stan_prep_data <- function(dat){
  dat$test_data %>%
    mutate(
      person = factor(person),
      person_1ind = as.integer(as.integer(person)),
      item = factor(item),
      item_1ind = as.integer(as.integer(item)),
      success = as.numeric(success)
    ) %>%
    filter(!is.na(success))
}

xxx_calc_2pl_stan_prep_data_list <- function(stan_data){
  list(
    I = length(levels(stan_data$item)),
    P = length(levels(stan_data$person)),
    O = nrow(stan_data),
    items = stan_data$item_1ind,
    persons = stan_data$person_1ind,
    success = stan_data$success
  )
}


calc_2pl_stan <- function(dat, iter = 4000, warmup = floor(iter/4)){
  t1 = Sys.time()
  
  stan_data = xxx_calc_2pl_stan_prep_data(dat)
  stan_data_list = xxx_calc_2pl_stan_prep_data_list(stan_data)
  
  
  stan_2pl_fit = stan(
    file = '2pl_stan.stan',
    data = stan_data_list,
    chains = 4,
    warmup = warmup,
    iter = iter
  )
  
  diffs_calc = apply(extract(stan_2pl_fit)$diffs, -1, mean)
  diffs_calc_ci = t(apply(extract(stan_2pl_fit)$diffs, -1, quantile, c(0.025,0.975)))
  discs_calc = apply(extract(stan_2pl_fit)$discs, -1, mean)
  discs_calc_ci = t(apply(extract(stan_2pl_fit)$discs, -1, quantile, c(0.025,0.975)))
  skills_calc = apply(extract(stan_2pl_fit)$skills, -1, mean)
  skills_calc_ci = t(apply(extract(stan_2pl_fit)$skills, -1, quantile, c(0.025,0.975)))
  
  
  diffs_discs = tibble(
    diffs_discs_id = names(dat$diffs),
    diffs_orig = dat$diffs,
    diffs_calc = diffs_calc,
    diffs_calc_ci_l = diffs_calc_ci[,1],
    diffs_calc_ci_u = diffs_calc_ci[,2],
    discs_orig = dat$discs,
    discs_calc = discs_calc,
    discs_calc_ci_l = discs_calc_ci[,1],
    discs_calc_ci_u = discs_calc_ci[,2]
  )
  
  skills = tibble(
    skills_id = names(dat$skills),
    skills_orig = dat$skills,
    skills_calc = skills_calc,
    skills_calc_ci_l = skills_calc_ci[,1],
    skills_calc_ci_u = skills_calc_ci[,2]
  )
  
  res = list(
    diffs_discs = diffs_discs,
    skills = skills
  )
  
  t2 = Sys.time()
  
  res$time = t2 - t1
  
  res
}





