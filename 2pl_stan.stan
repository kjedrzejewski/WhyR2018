data {
  int<lower=1> I;
  int<lower=1> P;
  int<lower=1> O;
  int<lower=1, upper=I> items[O];
  int<lower=1, upper=P> persons[O];
  int<lower=0, upper=1> success[O];
}
parameters {
  vector[I] diffs;
  vector[I] discs;
  vector[P] skills;
}
model {
  diffs ~ normal(0,6);
  discs ~ normal(1,0.2);
  skills ~ normal(0,1);
  
  success ~ bernoulli_logit((discs[items] .* skills[persons]) - diffs[items]);
}
