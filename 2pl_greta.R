library(greta)
library(dplyr)
library(tidyr)

tensorflow::use_python('/usr/local/anaconda3/bin/python')

xxx_calc_2pl_hint_greta_prep_data <- function(dat){
  dat$test_data %>%
    mutate(
      person = factor(person),
      person_1ind = as.integer(as.integer(person)),
      item = factor(item),
      item_1ind = as.integer(as.integer(item)),
      success = as.numeric(success),
      hint_used = as.numeric(hint_used)
    )
}

xxx_calc_2pl_hint_greta_prep_model <- function(greta_data){
  item_idx = as.integer(greta_data$item)
  person_idx = as.integer(greta_data$person)
  hint_used = as_data(as.integer(greta_data$hint_used))
  y = as_data(as.integer(greta_data$success))
  skill_exp_sd = as_data(1)
  
  diffs = uniform(-20, 20, dim = length(levels(greta_data$item)))
  hint_changes = normal(0, 1, dim = length(levels(greta_data$item)))
  discs = normal(1, 3, dim = length(levels(greta_data$item)))
  skills = normal(0, 1, dim = length(levels(greta_data$person)))
  
  item_diffs = diffs[item_idx]
  item_hint_changes = hint_changes[item_idx]
  item_discs = discs[item_idx]
  person_skills = skills[person_idx]
  
  logit = item_discs * person_skills - item_diffs - item_hint_changes * hint_used
  prob = ilogit(logit)
  distribution(y) = binomial(1, prob)
  
  # skill_exp_mean = as_data(0)
  # skill_est_mean = mean(skills)
  # distribution(skill_exp_mean) = normal(skill_est_mean, 0.001)
  # 
  # skill_exp_sd = as_data(1)
  # skill_est_sd = sqrt(mean((skills - skill_est_mean) ^ 2))
  # distribution(skill_exp_sd) = normal(skill_est_sd, 0.001)
  
  
  m <- model(diffs, hint_changes, discs, skills, precision = 'double')
}

# xxx_calc_2pl_hint_greta_initial_values = function(greta_data){
#   init_diffs =   greta_data %>%
#     group_by(item) %>%
#     summarise(p = mean(1 - success)) %>%
#     mutate(
#       logodds = log(p/(1 - p + 1e-7)),
#       logodds = case_when(
#         logodds > 5 ~ 5,
#         logodds < -5 ~ -5,
#         TRUE ~ logodds
#       )
#     ) %>%
#     {-.$logodds}
#   names(init_diffs) = paste0('diffs', 1:length(levels(greta_data$item)))
#   
#   init_hint_changes = rnorm(length(levels(greta_data$item)), 0, 0.1)
#   names(init_hint_changes) = paste0('hint_changes', 1:length(levels(greta_data$item)))
#   
#   init_discs = rnorm(length(levels(greta_data$item)), 1, 0.1)
#   names(init_discs) = paste0('discs', 1:length(levels(greta_data$item)))
#   
#   init_skills = greta_data %>%
#     group_by(person) %>%
#     summarise(p = mean(success)) %>%
#     mutate(
#       logodds = log(p/(1 - p + 1e-7)),
#       logodds = case_when(
#         logodds > 5 ~ 5,
#         logodds < -5 ~ -5,
#         TRUE ~ logodds
#       )
#     ) %>%
#     {.$logodds}
#   init_skills = (init_skills - mean(init_skills)) / sd(init_skills)
#   names(init_skills) = paste0('skills', 1:length(levels(greta_data$person)))
#   
#   c(init_diffs, init_hint_changes, init_discs, init_skills)
# }

xxx_calc_2pl_hint_greta_get_stats = function(d){
  q = quantile(d, probs = c(0.025, 0.5, 0.975))
  tibble(
    q_2_5p = q[1],
    median = q[2],
    mean = mean(d),
    q_97_5p = q[3],
    se = sd(d)
  )
}



calc_2pl_hint_greta <- function(dat, iter = 4000, warmup = floor(iter/4)){
  t1 = Sys.time()
  
  greta_data = xxx_calc_2pl_hint_greta_prep_data(dat)
  greta_model = xxx_calc_2pl_hint_greta_prep_model(greta_data)
  
  # greta_init_vals = xxx_calc_2pl_hint_greta_initial_values(greta_data)
  
  greta_draws <- mcmc(greta_model, warmup = warmup, n_samples = iter)
  
  greta_draws_m = as.matrix(greta_draws[[1]])
  
  # TODO: possibly calculation of sampling stats may be speed up 
  greta_draws_tidy = greta_draws_m %>%
    as_tibble() %>%
    gather() %>%
    mutate(
      var_name = {
        tmp = regexpr('[a-zA-Z_]+', .$key)
        regmatches(.$key, tmp)
      },
      index = as.integer({
        tmp = regexpr('[0-9]+', .$key)
        regmatches(.$key, tmp)
      })
    ) %>%
    select(var_name, index, value)
  
  pos_disc_rate = greta_draws_tidy %>%
    filter(var_name == 'discs') %>%
    {mean(.$value >= 0)}
  
  greta_draws_tidy = greta_draws_tidy %>%
    mutate(
      value = case_when(
        var_name %in% c('skills', 'discs') & pos_disc_rate < 0.5 ~ -value,
        TRUE ~ value
      )
    )
  
  greta_draws_stats = greta_draws_tidy %>%
    group_by(var_name, index) %>%
    do(xxx_calc_2pl_hint_greta_get_stats(.$value))
  
  diffs_sampled = greta_draws_stats %>%
    filter(var_name == 'diffs')
  
  hint_diff_changes_calc_sampled = greta_draws_stats %>%
    filter(var_name == 'hint_changes')
  
  discs_sampled = greta_draws_stats %>%
    filter(var_name == 'discs')
  
  skills_sampled = greta_draws_stats %>%
    filter(var_name == 'skills')
  
  
  
  item_params = dat$items %>%
    rename(
      diffs_orig = diff,
      hint_diff_changes_orig = hint_diff_change,
      discs_orig = disc
    ) %>%
    mutate(
      diffs_calc = diffs_sampled$mean,
      diffs_calc_ci_l = diffs_sampled$q_2_5p,
      diffs_calc_ci_u = diffs_sampled$q_97_5p,
      hint_diff_changes_calc = hint_diff_changes_calc_sampled$mean,
      hint_diff_changes_calc_ci_l = hint_diff_changes_calc_sampled$q_2_5p,
      hint_diff_changes_calc_ci_u = hint_diff_changes_calc_sampled$q_97_5p,
      discs_calc = discs_sampled$mean,
      discs_calc_ci_l = discs_sampled$q_2_5p,
      discs_calc_ci_u = discs_sampled$q_97_5p
    )
  
  person_params = dat$persons %>%
    rename(skills_orig = skill) %>%
    mutate(
      skills_calc = skills_sampled$mean,
      skills_calc_ci_l = skills_sampled$q_2_5p,
      skills_calc_ci_u = skills_sampled$q_97_5p
    )
  
  res = list(
    items = item_params,
    persons = person_params
  )
  
  t2 = Sys.time()
  
  res$time = t2 - t1
  
  res
}

