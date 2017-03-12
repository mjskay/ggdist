data {
    int<lower=1> n;
    int<lower=1> n_condition;
    int<lower=1, upper=n_condition> condition[n];
    real response[n];
}
parameters {
    real overall_mean;
    vector[n_condition] condition_zoffset;
    real<lower=0> y_sd;
    real<lower=0> condition_mean_sd;
}
transformed parameters {
    vector[n_condition] condition_mean;
    condition_mean = overall_mean + condition_zoffset * condition_mean_sd;
}
model {
    y_sd ~ cauchy(0, 1);                # => half-cauchy(0, 1)
    condition_mean_sd ~ cauchy(0, 1);   # => half-cauchy(0, 1)
    overall_mean ~ normal(0, 5);
    condition_zoffset ~ normal(0, 1);   # => condition_mean ~ normal(overall_mean, condition_mean_sd)
    for (i in 1:n) {
        response[i] ~ normal(condition_mean[condition[i]], y_sd);
    }
}
