# devtools::install_github("rstudio/tensorflow")
# tensorflow::install_tensorflow()

library(tensorflow)
library(dplyr)
library(tidyr)
library(purrr)

# conversion from matrix representation to facts
xxx_calc_2pl_tf_prep_data <- function(dat){
  dat$test_data %>%
    mutate(
      person_0ind = as.integer(as.integer(person) - 1), # -1 because Python
      item_0ind = as.integer(as.integer(item) - 1), # -1 because Python
      success = as.numeric(success)
    ) %>%
    group_by(person) %>%
    mutate(person_obs = n()) %>%
    group_by(item) %>%
    mutate(item_obs = n())
}

# model construction
xxx_calc_2pl_tf_prep_model <- function(tf_data, normalisation_rate){
  item_indices = tf$constant(tf_data$item_0ind)
  person_indices = tf$constant(tf_data$person_0ind)
  
  t_diffs = tf$Variable(
    tf$random_normal(
      shape = shape(length(levels(tf_data$item))),
      mean = 0,
      stddev = 6
    ),
    name = 'diffs'  
  )
  
  t_discs = tf$Variable(
    tf$random_normal(
      shape = shape(length(levels(tf_data$item))),
      mean = 1, # we assume that discrmination is around 1
      stddev = 0.01
    ),
    name = 'discs'
  )
  
  t_skills = tf$Variable(
    tf$random_normal(
      shape = shape(length(levels(tf_data$person))),
      mean = 0,
      stddev = 6
    ),
    name = 'skills'
  )
  
  # let's calculate this percentage
  neg_disc_rate = tf$divide(
    tf$cast(tf$count_nonzero(tf$less(t_discs, 0)), tf$float32),
    tf$cast(length(levels(tf_data$item)), tf$float32)
  )
  
  # let's find respective values for each row
  t_diffs_gathered = tf$gather(t_diffs, item_indices)
  t_discs_gathered = tf$gather(t_discs, item_indices)
  t_skills_gathered = tf$gather(t_skills, person_indices)
  
  # and calculate logit value
  t_rel_diff = t_discs_gathered * t_skills_gathered - t_diffs_gathered
  y_ = tf$sigmoid(t_rel_diff)
  
  y = tf$constant(tf_data$success)
  
  # standard loss function
  loss_main = tf$losses$log_loss(y, y_)
  
  mm = tf$nn$moments(t_skills, axes = 0L)
  
  loss = loss_main +                                # main cost function
    normalisation_rate * tf$abs(mm[[1]]) +          # skill level should have mean = 0, and sd = 1
    0.5 * normalisation_rate * tf$abs(mm[[2]] - 1)  # this way other parameters are also standarised
  
  list(
    t_diffs = t_diffs,
    t_discs = t_discs,
    t_skills = t_skills,
    t_diffs_gathered = t_diffs_gathered,
    t_discs_gathered = t_discs_gathered,
    t_skills_gathered = t_skills_gathered,
    y_ = y_,
    loss_main = loss_main,
    skills_mean = mm[[1]],
    skills_sd = tf$sqrt(mm[[2]]),
    neg_disc_rate = neg_disc_rate,
    loss = loss
  )
}

xxx_calc_2pl_tf_init_train <- function(m, sess, learning_rate = 0.001){
  optimizer = tf$train$AdamOptimizer(learning_rate)
  train = optimizer$minimize(m$loss)
  
  sess$run(tf$global_variables_initializer())
  
  m$optimizer = optimizer
  m$train = train
  
  m
}

xxx_calc_2pl_tf_train <- function(m, sess, stop_threshold, step_size, window_size, min_step, step_limit){
  step = 0
  stalled = FALSE
  
  loss = sess$run(m$loss)
  
  losses = tibble(
    step = step,
    loss_total = sess$run(m$loss),
    loss = sess$run(m$loss_main),
    skills_mean = sess$run(m$skills_mean),
    skills_sd = sess$run(m$skills_sd),
    neg_disc_rate = sess$run(m$neg_disc_rate)
  )
  cat(
    sprintf(
      "[%s] %6d: %2.8f (window mean: %2.8f) l: %2.8f sm: %+2.8f ssd: %2.8f ndr: %8f\n",
      as.character(Sys.time()),
      step, loss, loss,
      sess$run(m$loss_main),
      sess$run(m$skills_mean),
      sess$run(m$skills_sd),
      sess$run(m$neg_disc_rate)
    )
  )
  
  while (
    (step < min_step) | # do at least min_step steps
    (step >= (window_size * step_size) & !stalled & (step < step_limit)) # end when progress will stall or maximum of steps will be achieved
  ) {
    for (i in 1:step_size) {
      sess$run(m$train)
      losses = rbind(
        losses,
        tibble(
          step = step + i,
          loss_total = sess$run(m$loss),
          loss = sess$run(m$loss_main),
          skills_mean = sess$run(m$skills_mean),
          skills_sd = sess$run(m$skills_sd),
          neg_disc_rate = sess$run(m$neg_disc_rate)
        )
      )
    }
    step = step + step_size
    curr = sess$run(m$loss)
    
    last = mean(tail(losses$loss, step_size))
    stalled = FALSE
    if(step >= window_size * step_size) {
      stalled = TRUE
      for(w in 2:window_size){
        longer = mean(tail(losses$loss, w * step_size))
        stalled = stalled & (2 * abs(longer - last) / (longer + last) < stop_threshold)
      }
    }
    
    cat(
      sprintf(
        "[%s] %6d: %2.8f (window mean: %2.8f) l: %2.8f sm: %+2.8f ssd: %2.8f ndr: %8f\n",
        as.character(Sys.time()),
        step, curr, last,
        sess$run(m$loss_main),
        sess$run(m$skills_mean),
        sess$run(m$skills_sd),
        sess$run(m$neg_disc_rate)
      )
    )
  }
  
  losses
}

calc_2pl_tf <- function(dat,
                             stop_threshold = 0.0001, step_size = 1000, window_size = 10, min_step = 10000, step_limit = 50000,
                             learning_rate = 0.005, normalisation_rate = 1
){
  t1 = Sys.time()
  
  tf_data = xxx_calc_2pl_tf_prep_data(dat)
  
  m = xxx_calc_2pl_tf_prep_model(tf_data, normalisation_rate)
  
  sess = tf$Session()
  m = xxx_calc_2pl_tf_init_train(m, sess, learning_rate)
  
  losses = xxx_calc_2pl_tf_train(m, sess, stop_threshold, step_size, window_size, min_step, step_limit)
  
  diffs_calc = sess$run(m$t_diffs)
  discs_calc = sess$run(m$t_discs)
  skills_calc = sess$run(m$t_skills)
  
  if(mean(discs_calc < 0) > 0.5){
    discs_calc = -discs_calc
    skills_calc = -skills_calc
  }
  
  item_params = tibble(
    item = dat$items$item,
    diffs_orig = dat$items$diff,
    diffs_calc = diffs_calc,
    discs_orig = dat$items$disc,
    discs_calc = discs_calc
  )
  
  person_params = tibble(
    person = dat$persons$person,
    skills_orig = dat$persons$skill,
    skills_calc = skills_calc
  )
  
  res = list(
    items = item_params,
    persons = person_params,
    losses = losses
  )
  
  t2 = Sys.time()
  
  res$time = t2 - t1
  
  res
}

