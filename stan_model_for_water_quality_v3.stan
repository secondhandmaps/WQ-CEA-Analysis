functions {
  // Part 1 of correcting the fraction of affected deaths for self-consistency. 
  // Since the intervention areas have different levels of water treatment
  // than the rest of the country, and water treatment only affects certain
  // causes of death, then areas with e.g., lower water treatment levels will
  // have a greater fraction of deaths from enteric disease and other illnesses
  // than the country-wide average. 
   vector algebra_system(vector y, vector params,
                              real[] x_r, int[] x_i) {
      vector[1] z;
      
      real countrywide_fraction_treated = x_r[1]; 
      real mortality_reduction_size = params[1];
      real frac_of_deaths_affected = params[2];
      
      int n_other_deaths = x_i[1];
      real  countrywide_fraction_untreated = 1 - countrywide_fraction_treated;
      real n_affected_deaths_if_treated = y[1] * (1-mortality_reduction_size);
      
      z[1] = y[1]/(y[1] + n_other_deaths) * countrywide_fraction_untreated + 
        n_affected_deaths_if_treated/(n_affected_deaths_if_treated + n_other_deaths) * countrywide_fraction_treated - 
        frac_of_deaths_affected;
    return z;
  }
  
  // Part 2 of correction the fraction of affected deaths. 
  real calc_ed_fraction(real n_edU, int nO, real reduction_size, 
                          real baseline_treatment) {
    real frac_edU = n_edU/(n_edU + nO);
    real n_edT = n_edU * (1 - reduction_size);
    real frac_edT = n_edT / (n_edT + nO);
    real frac_ed_treatment_area =   frac_edT * baseline_treatment +
        frac_edU * (1 - baseline_treatment);
    return frac_ed_treatment_area;
  }

}

data {
  // Country-specific factors
  real countrywide_treatment_fraction;
  real baseline_treatment_fraction;
  real external_validity_morbidity;
  real external_validity_mortality;
  vector[4] frac_deaths_by_category; //Enteric, Respiratory, Other, Other Neonatal
  
  // Universal factors (but dependent on if Haushofer 2021 is included)
  real internal_validity_mortality;
  real morbidity_adherence;
  real mortality_adherence;
  real mortality_lrr_est;
  real mortality_lrr_sd;
}

transformed data {
  real self_reporting_bias = 0.85;
  real morbidity_adherence_adjustment = mortality_adherence/morbidity_adherence;
  
  //Only used in correcting the fraction of deaths affected
  vector[1] y_guess = [75]';
  real x_r[1] = {countrywide_treatment_fraction};
  int n_other_deaths = 100;
  int x_i[1] = {n_other_deaths};
  
  //Setting up 
  real frac_deaths_mean = dot_product(frac_deaths_by_category, 
                                      [1, 1, 0.25, 0]);
 
  real frac_deaths_uci =  dot_product(frac_deaths_by_category, 
                                      [1, 1, 1, 0.5]);         
  real frac_deaths_sd = (frac_deaths_uci - frac_deaths_mean) / 1.96;
  
}

parameters {
  real<lower=0> mortality_effect;
  
  real<lower=0> morbidity_effect;
  real<lower=0> internal_validity_morbidity;
  real<lower=0> mortality_morbidity_scaling;
  
  real<lower=frac_deaths_by_category[1]> frac_of_deaths_impacted_base;
}
transformed parameters {
  real adj_morbidity_est = 1-((1-morbidity_effect)*self_reporting_bias*morbidity_adherence_adjustment);
  real morbidity_effect_size = ((1 - adj_morbidity_est) * internal_validity_morbidity * external_validity_morbidity);

  vector[2] params = [morbidity_effect_size * mortality_morbidity_scaling,
                      frac_of_deaths_impacted_base]';
  
  vector[1] n_edU = algebra_solver(algebra_system, y_guess, params, x_r, x_i);
  real frac_of_deaths_impacted = calc_ed_fraction(n_edU[1],
                                                  n_other_deaths,
                                                  morbidity_effect_size, 
                                                  baseline_treatment_fraction);
  
  
  real mu_indirect = morbidity_effect_size * mortality_morbidity_scaling * frac_of_deaths_impacted;
  
  real mu_direct = (1 - mortality_effect) * internal_validity_mortality * external_validity_mortality;
  real mu_combined = (mu_indirect + mu_direct)/2;
  
  
}


model {
  
  morbidity_effect ~ normal(0.77, 0.13); 
  internal_validity_morbidity ~ normal(0.9, 0.05);
  mortality_morbidity_scaling ~ lognormal(0, 0.171); 

  mortality_effect ~ lognormal(mortality_lrr_est, mortality_lrr_sd);
  frac_of_deaths_impacted_base ~ normal(frac_deaths_mean, frac_deaths_sd);
  target += -(mu_direct - mu_indirect)^2 * 10000;
}

generated quantities {
  real mills_reincke_factor = frac_of_deaths_impacted/frac_deaths_by_category[1];
  real frac_et_else_impacted = (frac_of_deaths_impacted - frac_deaths_by_category[1] - frac_deaths_by_category[2]) /
                                frac_deaths_by_category[3];
}
