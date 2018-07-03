library(plotly)

source('2pl_generate_data.R')
source('2pl_irt.R')
source('1pl_me.R')
source('2pl_tf.R')
source('2pl_greta.R')
source('2pl_stan.R')

# tensorflow::use_python('/home/ubuntu/anaconda3/envs/tensorflow_p36/bin/python')
# tensorflow::use_python('/usr/local/anaconda3/bin/python')

dat_2pl = generate_data_2pl(100, 1000, 1)

# estimation using the specialised package TAM
res_2pl_irt = calc_2pl_irt(dat_2pl)
# estimation using logistic regression with random effects. It's 1PL model run on 2PL data, but we do it only to obtain processing time
res_1pl_me = calc_1pl_me(dat_2pl)
# estimation using MLE with logloss and gradient descent (tensorflow)
res_2pl_tf = calc_2pl_tf(dat_2pl)  # By default it does up to 50 000 iterations and ends earlier if loss stops changing (minimum 10 000 iterations)
# estimation using greta (probabilitic programming)
res_2pl_greta = calc_2pl_greta(dat_2pl) # By default 16000 samples with 4000 samples of warmup
# estimation using stan (probabilitic programming)
res_2pl_stan = calc_2pl_stan(dat_2pl) # By default 4 chains of 4000 samples with 1000 samples of warmup


save(dat_2pl, res_2pl_irt, res_2pl_tf, res_1pl_me, res_2pl_greta, res_2pl_stan, file = 'data/2pl.RData')

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



items = seq(50, 1000, 50)
persons = seq(500, 10000, 500)


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






items = seq(600, 1000, 50)
persons = seq(6000, 10000, 500)

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



items = seq(50, 1000, 50)
persons = seq(500, 10000, 500)

results_tf_gpu = tibble()

for(i in 1:length(items)) {
  cat('####################################################################\n')
  cat('####################################################################\n')
  cat('Started:', i, '\n')
  
  dat = generate_data_2pl(items[i], persons[i], 1)
  res = calc_2pl_tf(dat)
  
  cat('Time:', res$time, '\n')
  cat('####################################################################\n')
  cat('####################################################################\n')
  
  results_tf_gpu = union_all(
    results_tf_gpu,
    tibble(
      lib = 'tf',
      items = items[i],
      persons = persons[i],
      time = res$time
    )
  )
}

save(results_tf_gpu, file = 'data/results_tf_gpu.RData')



items = seq(50, 500, 50)
persons = seq(500, 5000, 500)

results_greta = tibble()

for(i in 1:length(items)) {
  cat('####################################################################\n')
  cat('####################################################################\n')
  cat('Started:', i, '\n')
  
  tf$reset_default_graph()
  dat = generate_data_2pl(items[i], persons[i], 1)
  res = calc_2pl_greta(dat)
  
  cat('Time:', res$time, '\n')
  cat('####################################################################\n')
  cat('####################################################################\n')
  
  results_greta = union_all(
    results_greta,
    tibble(
      lib = 'greta',
      items = items[i],
      persons = persons[i],
      time = res$time
    )
  )
}

save(results_greta, file = 'data/results_greta.RData')



items = seq(50, 500, 50)
persons = seq(500, 5000, 500)

results_greta_gpu = tibble()

for(i in 1:length(items)) {
  cat('####################################################################\n')
  cat('####################################################################\n')
  cat('Started:', i, '\n')
  
  tf$reset_default_graph()
  dat = generate_data_2pl(items[i], persons[i], 1)
  res = calc_2pl_greta(dat)
  
  cat('Time:', res$time, '\n')
  cat('####################################################################\n')
  cat('####################################################################\n')
  
  results_greta_gpu = union_all(
    results_greta_gpu,
    tibble(
      lib = 'greta',
      items = items[i],
      persons = persons[i],
      time = res$time
    )
  )
}

save(results_greta_gpu, file = 'data/results_greta_gpu.RData')




items = seq(10, 200, 10)
persons = seq(100, 2000, 100)

results_stan = tibble()

for(i in 1:length(items)) {
  cat('####################################################################\n')
  cat('####################################################################\n')
  cat('Started:', i, '\n')
  
  dat = generate_data_2pl(items[i], persons[i], 1)
  res = calc_2pl_stan(dat)
  
  cat('Time:', res$time, '\n')
  cat('####################################################################\n')
  cat('####################################################################\n')
  
  results_stan = union_all(
    results_stan,
    tibble(
      lib = 'stan',
      items = items[i],
      persons = persons[i],
      time = res$time
    )
  )
}

save(results_stan, file = 'data/results_stan.RData')

