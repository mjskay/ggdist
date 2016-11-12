data {
    int<lower=1> n;
    int<lower=1> n_group;
    int<lower=1, upper=n_group> group[n];
    real y[n];
}
parameters {
    real overall_mean;
    vector[n_group] group_zoffset;
    real<lower=0> y_sd;
    real<lower=0> group_mean_sd;
}
transformed parameters {
    vector[n_group] group_mean;
    group_mean = overall_mean + group_zoffset * group_mean_sd;
}
model {
    y_sd ~ cauchy(0, 1);            # => half-cauchy(0, 1)
    group_mean_sd ~ cauchy(0, 1);   # => half-cauchy(0, 1)
    overall_mean ~ normal(0, 5);
    group_zoffset ~ normal(0, 1);   # => group_mean ~ normal(overall_mean, group_mean_sd)
    for (i in 1:n) {
        y[i] ~ normal(group_mean[group[i]], y_sd);
    }
}
