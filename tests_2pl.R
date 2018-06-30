library(plotly)

source('2pl_generate_data.R')
source('2pl_functions.R')
source('2pl_irt.R')
source('1pl_me.R')
source('2pl_tf.R')
source('2pl_stan.R')

dat_2pl = generate_data_2pl(100, 1000, 1)

res_2pl_irt = calc_2pl_irt(dat_2pl)
res_1pl_me = calc_1pl_me(dat_2pl)
res_2pl_tf = calc_2pl_tf(dat_2pl)
res_2pl_stan = calc_2pl_stan(dat_2pl)


save(dat_2pl, res_2pl_irt, res_2pl_tf, res_2pl_stan, file = 'data/2pl.RData')

plot_ly(res_2pl_tf$losses) %>% add_lines(x = ~step, y = ~loss_total) %>% layout(xaxis = list(rangemode = "tozero"))
plot_ly(res_2pl_tf$losses) %>% add_lines(x = ~step, y = ~loss) %>% layout(xaxis = list(rangemode = "tozero"))
plot_ly(res_2pl_tf$losses) %>% add_lines(x = ~step, y = ~skills_mean) %>% layout(xaxis = list(rangemode = "tozero"))
plot_ly(res_2pl_tf$losses) %>% add_lines(x = ~step, y = ~skills_sd) %>% layout(xaxis = list(rangemode = "tozero"))
plot_ly(res_2pl_tf$losses) %>% add_lines(x = ~step, y = ~neg_disc_rate) %>% layout(xaxis = list(rangemode = "tozero"))


# original difficulties vs. calculated
plot_ly() %>%
  add_markers(
    data = res_1pl_me$items,
    x = ~diffs_orig,
    y = ~diffs_calc,
    text = ~paste0(
      'diffs_orig: ', diffs_orig,
      '\ndiffs_calc: ', diffs_calc
    ),
    name = 'me, 1pl'
  ) %>%
  add_markers(
    data = res_2pl_irt$items,
    x = ~diffs_orig,
    y = ~diffs_calc,
    text = ~paste0(
      'diffs_orig: ', diffs_orig,
      '\ndiffs_calc: ', diffs_calc
    ),
    name = 'irt'
  ) %>%
  add_markers(
    data = res_2pl_tf$items,
    x = ~diffs_orig,
    y = ~diffs_calc,
    text = ~paste0(
      'diffs_orig: ', diffs_orig,
      '\ndiffs_calc: ', diffs_calc
    ),
    name = 'tf'
  )
# %>%
#   add_markers(
#     data = res_2pl_stan$diffs_diffs,
#     x = ~diffs_orig,
#     y = ~diffs_calc,
#     text = ~paste0(
#       'diffs_orig: ', diffs_orig,
#       '\ndiffs_calc: ', diffs_calc
#     ),
#     name = 'stan'
#   )


# original discriminations vs. calculated
plot_ly() %>%
  add_markers(
    data = res_2pl_irt$items,
    x = ~discs_orig,
    y = ~discs_calc,
    text = ~paste0(
      'discs_orig: ', discs_orig,
      '\ndiscs_calc: ', discs_calc
    ),
    name = 'irt'
  ) %>%
  add_markers(
    data = res_2pl_tf$items,
    x = ~discs_orig,
    y = ~discs_calc,
    text = ~paste0(
      'discs_orig: ', discs_orig,
      '\ndiscs_calc: ', discs_calc
    ),
    name = 'tf'
  )
# %>%
#   add_markers(
#     data = res_2pl_stan$discs_discs,
#     x = ~discs_orig,
#     y = ~discs_calc,
#     text = ~paste0(
#       'discs_orig: ', discs_orig,
#       '\ndiscs_calc: ', discs_calc
#     ),
#     name = 'stan'
#   )


# original skills vs. calculated
plot_ly() %>%
  add_markers(
    data = res_2pl_irt$persons,
    x = ~skills_orig,
    y = ~skills_calc,
    name = 'irt'
  ) %>%
  add_markers(
    data = res_2pl_tf$skills,
    x = ~skills_orig,
    y = ~skills_calc,
    name = 'tf'
  )
# %>%
#   add_markers(
#     data = res_2pl_stan$skills,
#     x = ~skills_orig,
#     y = ~skills_calc,
#     name = 'stan'
#   )



items = seq(100, 1000, 100)
persons = seq(1000, 10000, 1000)


results_irt = tibble()

for(i in 1:length(items)) {
  cat('####################################################################\n')
  cat('####################################################################\n')
  cat('Started:', i, '\n')
  
  dat = generate_data_2pl(items[i], persons[i], 1)
  res = calc_2pl_irt(dat)
  
  cat('Time:', res$time, '\n')
  cat('####################################################################\n')
  cat('####################################################################\n')
  
  results_irt = union_all(
    results_irt,
    tibble(
      lib = 'tam',
      items = items[i],
      persons = persons[i],
      time = res$time
    )
  )
}

save(results_irt, file = 'data/results_irt.RData')






items = seq(50, 500, 50)
persons = seq(500, 5000, 500)

results_me = tibble()

for(i in 1:length(items)) {
  cat('####################################################################\n')
  cat('####################################################################\n')
  cat('Started:', i, '\n')
  
  dat = generate_data_2pl(items[i], persons[i], 1)
  res = calc_1pl_me(dat)
  
  cat('Time:', res$time, '\n')
  cat('####################################################################\n')
  cat('####################################################################\n')
  
  results_me = union_all(
    results_me,
    tibble(
      lib = 'lme4',
      items = items[i],
      persons = persons[i],
      time = res$time
    )
  )
}

save(results_me, file = 'data/results_me.RData')



items = seq(50, 500, 50)
persons = seq(500, 5000, 500)

results_tf = tibble()

for(i in 1:length(items)) {
  cat('####################################################################\n')
  cat('####################################################################\n')
  cat('Started:', i, '\n')
  
  dat = generate_data_2pl(items[i], persons[i], 1)
  res = calc_2pl_tf(dat)
  
  cat('Time:', res$time, '\n')
  cat('####################################################################\n')
  cat('####################################################################\n')
  
  results_tf = union_all(
    results_tf,
    tibble(
      lib = 'tf',
      items = items[i],
      persons = persons[i],
      time = res$time
    )
  )
}

save(results_tf, file = 'data/results_tf.RData')
