library(dplyr)
library(purrr)

generate_data_2pl <- function(items = 100, persons = 1000, noise = 0, sample_perc = 1){
  items_t = tibble(
    item = factor(sprintf('item%05d',1:items)),
    diff = rnorm(items, 0, 1),
    disc = rnorm(items, 1, 1)
  )
  
  persons_t = tibble(
    person = factor(sprintf('person%05d', 1:persons)),
    skill = rnorm(persons, 0, 1)
  )
  
  full_data = persons_t %>%
    mutate(tmp = 1) %>%
    inner_join(mutate(items_t, tmp = 1), by = 'tmp') %>%
    select(-tmp) %>%
    mutate(
      logit = disc * skill - diff,
      prob = 1/(1 + exp(-logit)),
      success = prob >= runif(n())
    )
  
  test_data = full_data %>%
    select(person, item, success) %>%
    sample_frac(sample_perc)
  
  list(
    items = items_t,
    persons = persons_t,
    full_data = full_data,
    test_data = test_data
  )
}
