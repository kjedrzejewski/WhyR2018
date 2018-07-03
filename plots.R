library(plotly)

load("data/results_greta.RData")
units(results_greta$time) = 'mins'
results_greta = results_greta %>%
  mutate(observations = items * persons)

load("data/results_greta_gpu.RData")
units(results_greta_gpu$time) = 'mins'
results_greta_gpu = results_greta_gpu %>%
  mutate(observations = items * persons)

load("data/results_tf.RData")
units(results_tf$time) = 'mins'
results_tf = results_tf %>%
  mutate(observations = items * persons)

load("data/results_tf_gpu.RData")
units(results_tf_gpu$time) = 'mins'
results_tf_gpu = results_tf_gpu %>%
  mutate(observations = items * persons)

load("data/results_irt.RData")
units(results_irt$time) = 'mins'
results_irt = results_irt %>%
  mutate(observations = items * persons)

load("data/results_me.RData")
units(results_me$time) = 'mins'
results_me = results_me %>%
  mutate(observations = items * persons)

load("data/results_stan.RData")
units(results_stan$time) = 'mins'
results_stan = results_stan %>%
  mutate(observations = items * persons)



plotly_save <- function(p, ..., width = 5, height = 3, dpi = 144){
  invisible(plotly::export(
    p = p,
    vwidth = width * dpi,
    vheight = height * dpi,
    zoom = 3,
    ...
  ))
}

p = plot_ly() %>%
  layout(
    xaxis = list(title='Number of observations', range = list(0,10000000)),
    yaxis = list(title='Time [min.]', range = list(0,450)),
    showlegend = TRUE
  )


p = p %>%
  add_lines(
    data = results_irt,
    x = ~observations,
    y = ~time,
    name = 'tam'
  )
plotly_save(p, file = 'plots/1.png')

p = p %>%
  add_lines(
    data = results_me,
    x = ~observations,
    y = ~time,
    name = 'log. reg.'
  )
plotly_save(p, file = 'plots/2.png')


p = p %>%
  add_lines(
    data = results_stan,
    x = ~observations,
    y = ~time,
    name = 'stan'
  )
plotly_save(p, file = 'plots/3.png')


p = p %>%
  add_lines(
    data = results_greta,
    x = ~observations,
    y = ~time,
    name = 'greta'
  )
plotly_save(p, file = 'plots/4.png')


p = p %>%
  add_lines(
    data = results_greta_gpu,
    x = ~observations,
    y = ~time,
    name = 'greta, gpu'
  )
plotly_save(p, file = 'plots/5.png')


p = p %>%
  add_lines(
    data = results_tf,
    x = ~observations,
    y = ~time,
    name = 'tf'
  )
plotly_save(p, file = 'plots/6.png')


p = p %>%
  add_lines(
    data = results_tf_gpu,
    x = ~observations,
    y = ~time,
    name = 'tf, gpu'
  )
plotly_save(p, file = 'plots/7.png')

