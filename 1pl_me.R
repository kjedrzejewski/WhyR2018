library(lme4)
library(dplyr)
library(tidyr)


calc_1pl_me <- function(dat){
  t1 = Sys.time()
  
  m = glmer(success ~ -1 + (1 | person) + (1 | item), data = dat$test_data, family=stats::binomial(logit))
  
  item_params = dat$items %>%
    rename(diffs_orig = diff) %>%
    inner_join(
      coefficients(m)$item %>%
        rename(diffs_calc = '(Intercept)') %>%
        tibble::rownames_to_column('item') %>%
        mutate(
          item = factor(
            item,
            levels = levels(dat$test_data$item)
          ),
          diffs_calc = -1 * diffs_calc
        ),
      by = 'item'
    )
  
  person_params = dat$persons %>%
    rename(skills_orig = skill) %>%
    inner_join(
      coefficients(m)$person %>%
        rename(skills_calc = '(Intercept)') %>%
        tibble::rownames_to_column('person') %>%
        mutate(
          person = factor(
            person,
            levels = levels(dat$test_data$person)
          )
        ),
      by = 'person'
    )
  
  res = list(
    items = item_params,
    persons = person_params
  )
  
  t2 = Sys.time()
  
  res$time = t2 - t1
  
  res
}
