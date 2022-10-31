library(tidyverse)
library(rstan)

# Countrywide treatment fractions are from DHS reports
# Baseline treatment fraction are from 'Adherence adjustments'!B41-D41
# External validity morbidity are from 'External validity adjustment'
# External validity mortality are from 'External validity adjustment'
# Frac deaths by category are from https://docs.google.com/spreadsheets/d/1TGlqAp3aZvsG8lagYajP1h27z_nN-4flLsHVhQrhDMU/edit#gid=0
# The categories for frac_deaths_by_category are:
# 1. Enteric Disease
# 2. Respiratory infections, incl TB
# 3. Other infections: Neonatal sepsis, NTDs and malaria, HIV/AIDS and STDs, 
#    Other infectious diseases, and Nutritional deficiencies
# 4. Other neonatal disorders
data_for_stan_all = 
  tribble(~intervention, ~countrywide_treatment_fraction, 
        ~baseline_treatment_fraction, ~external_validity_morbidity, 
        ~external_validity_mortality, ~frac_deaths_by_category, 
        "Kenya ILC", 0.453, 0.224, 1.12 , 1.27, c(17.4, 12.5, 31.8, 7.01)/100,
        "Kenya DSW", 0.453, 0.224, 0.52, 0.58, c(17.4, 12.5, 31.8, 7.01)/100,
        "Uganda DSW", 0.498, 0.172, 0.83, 0.59, c(5.2, 8.6, 44.5, 5.51)/100,
        "Malawi DSW", 0.266, 0.079, 1.08, 1.16, c(10.3, 14.3, 33.5, 6.38)/100) %>%
  mutate(
    internal_validity_mortality = 0.74,
    morbidity_adherence = 0.53,
    mortality_adherence = 0.49,
    mortality_lrr_est = -0.214,
    mortality_lrr_sd = 0.120
  )


data_for_stan_split =
  data_for_stan_all %>%
  split(., .$intervention) %>%
  map(unclass) %>%
  map(\(x) {x$frac_deaths_by_category = x$frac_deaths_by_category[[1]]; 
              x$intervention = NULL; x})

nchains = 4
set.seed(123)
init_list = list(mortality_effect = c(0.85 + rnorm(nchains, 0, 0.05)),
                 morbidity_effect = c(0.71 + rnorm(nchains, 0, 0.05)),
                 internal_validity = c(0.90 + rnorm(nchains, 0, 0.025)),
                 mortality_morbidity_scaling = c(1 + rnorm(nchains, 0, 0.07)),
                 frac_of_deaths_impacted_base = rep(0.42, nchains)) %>% 
  purrr::transpose()
unwanted_pars = c("params", "n_edU", "adj_morbidity_est",
                  "frac_of_deaths_impacted")
n_iter = 8000
stan_file = "stan_model_for_water_quality_v3.stan"

fit_kenya_ilc = stan(stan_file, 
                     iter = n_iter, warmup = 500, 
                     cores = 4, open_progress = FALSE, verbose = FALSE, 
                     chains = nchains,
                     data = data_for_stan_split$`Kenya ILC`,
                     init = init_list,
                     pars = unwanted_pars,
                     include = FALSE)
(s_kenya_ilc = 
  summary(fit_kenya_ilc)$summary[, c("mean", "se_mean", "sd", "2.5%", "97.5%")] %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  mutate(intervention = "Kenya ILC", .before = 1) )

fit_kenya_dsw = stan(stan_file,
                     data = data_for_stan_split$`Kenya DSW`,
                     iter = n_iter, warmup = 500, 
                     cores = 4, open_progress = FALSE, verbose = FALSE, 
                     chains = nchains,
                     init = init_list, 
                     pars = unwanted_pars, include = FALSE)
(s_kenya_dsw = 
    summary(fit_kenya_dsw)$summary[, c("mean", "se_mean", "sd", "2.5%", "97.5%")] %>%
    as.data.frame() %>%
    rownames_to_column("parameter") %>%
    mutate(intervention = "Kenya DSW", .before = 1) )

fit_uganda_dsw = stan(stan_file,
                      data = data_for_stan_split$`Uganda DSW`,
                      iter = n_iter, warmup = 500, 
                      cores = 4, open_progress = FALSE, verbose = FALSE, 
                      chains = nchains,
                      init = init_list, 
                      pars = unwanted_pars, include = FALSE)
(s_uganda_dsw = 
    summary(fit_uganda_dsw)$summary[, c("mean", "se_mean", "sd", "2.5%", "97.5%")] %>%
    as.data.frame() %>%
    rownames_to_column("parameter") %>%
    mutate(intervention = "Uganda DSW", .before = 1) )

fit_malawi_dsw = stan(stan_file,
                      data = data_for_stan_split$`Malawi DSW`,
                      iter = n_iter, warmup = 500, 
                      cores = 4, open_progress = FALSE, verbose = FALSE, 
                      chains = nchains,
                      init = init_list, 
                      pars = unwanted_pars, include = FALSE)
(s_malawi_dsw = 
    summary(fit_malawi_dsw)$summary[, c("mean", "se_mean", "sd", "2.5%", "97.5%")] %>%
    as.data.frame() %>%
    rownames_to_column("parameter") %>%
    mutate(intervention = "Malawi DSW", .before = 1) )

combined_summary = 
  bind_rows(s_kenya_ilc, s_kenya_dsw, s_uganda_dsw, s_malawi_dsw)
write_csv(combined_summary, "results_from_mcmc.csv")
combined_summary_mean_only = combined_summary %>%
  select(intervention, parameter, mean) %>%
  pivot_wider(names_from = intervention, values_from = mean)
write_csv(combined_summary_mean_only, "results_from_mcmc_mean_only.csv")
