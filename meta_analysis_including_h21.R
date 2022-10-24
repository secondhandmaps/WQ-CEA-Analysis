# Ok, let's just take it at face value, assume we have to use the
# relative risk #'s, because that's what we have. 
# We can debate whether it's more appropriate to use the full dataset or the
# restricted (post WASH-B) dataset. 
# 
# I'm currently thinking that it's probably reasonable to include both
# the Null '18 and this result, especially if I take the restricted dataset -
# I think that might involve completely non-overlapping groups of people. 
# No, the passive control group shouldn't be affected by leaving people out. 
# 
# 
# Ok, so that's a plan. Attempt to take the results most consistent with the 
# other studies included in GiveWell's meta-analysis. 
# We therefore take the results from Table 7, column 4. That looks at the effect
# of water chlorination on under-5 mortality, "excluding all children of mothers with any children born within one year of WASH-B", and *not* controlling for pre-intervention mortality. 
# The change in the mortality rate  (*not* log relative risk) result from that table is
# -0.0167 +/- 0.0070
# Elsewhere in the paper (p13) they say the under5 mortality rate in the passive control group was 
# 0.0223. I take this as a fixed constant, assuming that the uncertainty in this number is incorporated
# into the reported uncertainty for the decrease. 
# 
# I see two ways of calculating the standard error: 
# 1. Take the reported standard error and log-transform it. 
# 2. Try to calculate it directly using the number of observations. I'm not sure if 
#   I have the requisite data or not to do that one. 

x = -0.0167
dx = 0.007

# First transformation: divide by 0.0223
# Transform into a relative risk
B = 0.0223
q1 = (B + x) / B
dq1 = dx / B

# Second transformation: take the log
q2 = log(q1)
derivative_q1_q2 = 1/q1 # Derivative of log(q1) = 1/q1
dq2 = dq1 * abs(derivative_q1_q2)
vec = rnorm(5000, mean = q1, sd = dq1)
hist(vec)
hist(log(vec))
sd(log(vec), na.rm = TRUE)

# Ok, I'm running into errors here, because assuming a standard normal distribution 
# is giving me negative chances of mortality. 
# Seems like a better option would be to try to approximate the number of deaths in each group. 
# Looks like I need treatment n, treatment deaths, control n, and control deaths. 
# Table 7 says there are 1405 "observations", but I think that's including both treatment and control. 
# Based on the # of villages, I'm assuming half and half treatment and control ) 
# 
# 


# Excluding Children born w/i 1 year of WASH-B (Table 6, column 4)
# overall_n = 1597
# baseline_mortality = 0.0223
# treatment_effect = -0.0128

# Excluding children born w/i 1 year of WASH-B and their siblings (Table 7, column 4)
overall_n = 1405
baseline_mortality = 0.0223
treatment_effect = -0.0167
# 
# # All Children (Table 4, column 4)
# overall_n = 1981
# baseline_mortality = 0.0223
# treatment_effect = -0.0143

# This breakdown based on Figure 2, 
# 38 + 27 villages in the treatment; 45 + 22 in the control
treatment_n = overall_n/2
control_n = overall_n/2

treatment_deaths = treatment_n * (baseline_mortality + treatment_effect)
control_deaths = control_n * baseline_mortality
rr =  (treatment_deaths/treatment_n) / (control_deaths/control_n) 
rr
Haushofer_lrr = log(rr)
Haushofer_lrr_se = sqrt(1/control_deaths + 1/treatment_deaths - 1/control_n - 1/treatment_n)

# Now combine it with the other resutls:

trial_df = tribble(
  ~trial, ~lnrr, ~se, ~similarity, 
  "Reller 03", -0.97, 0.91, 0.88,
  "Boisson 13", 0.68, 1.22, 1,
  "Luby 18", -0.14, 0.23, 0.75,
  "Null 18", -0.18, 0.20, 1.0,
  "Humphrey 19", -0.06, 0.20,  0.5,
  "Haushofer 21", Haushofer_lrr, Haushofer_lrr_se, 1
)
base = trial_df %>%
  filter(trial != "Haushofer 21") %>%
  mutate(label = "Base")
all = trial_df %>%
  mutate(label = "All")
replace_null = trial_df %>%
  filter(trial != "Null 18") %>%
  mutate(label = "Replace Null")

df1 = trial_df %>%
  filter(trial %in% c("Haushofer 21", "Null 18")) %>%
  mutate(weight = 1/se^2 * similarity,
         frac_weight = weight / sum(weight)) %>%
  summarize(lnrr = weighted.mean(lnrr, weight),
            se = sum(se * frac_weight), # I think that's appropriate for perfectly correlated errors
            # se = sqrt(sum((se * frac_weight)^2)),
            trial = "Null + Haushofer",
            similarity = 1)
df2 = trial_df %>%
  filter(!trial %in% c("Haushofer 21", "Null 18"))
average_wash = bind_rows(df1, df2) %>% mutate(label = "Average WASH")

half_weight = trial_df %>%
  mutate(similarity = ifelse(trial %in% c("Haushofer 21", "Null 18"), 
                             similarity/2,
                             similarity),
         label = "Half Weight")
all_options = bind_rows(base, all, replace_null, average_wash, half_weight)
all_options %>%
  group_by(label) %>%
  mutate(weight = 1/se^2 * similarity,
         frac_weight = weight / sum(weight)) %>%
  summarize(lrr_est = weighted.mean(lnrr, weight),
            lrr_se = sqrt(sum((se * frac_weight)^2)),
            pooled_relative_risk_est = exp(lrr_est))

# Alright, I get the final results for the log relative risk:
# Base: -0.146 +/- 0.123
# Add H21: -0.215 +/- 0.121
# Average H21 and N18, correlated errors: -0.192 +/- 0.131
# Of these, I think that "Add H21", aka "All" is the most appropriate. 

## I would like to repeat this using the 'meta' package
library("meta")
library(tidyverse)

library(readxl)
studies = read_xlsx("data_for_meta_analysis.xlsx")
studies = studies %>%
  mutate(log_mortality_rr_se_adj = log_mortality_rr_se / sqrt(similarity_weight))
# 
# studies = tribble(~Study, ~event.e, ~n.e, ~event.c, ~n.c,
#                   "Reller 2003",	3,	739,	2,	185,
#                   "Boisson 2013",	2,	1507,	1,	1483,
#                   "Luby 2018",	27,	656,	62,	1244,
#                   "Null 2018",	30,	888,	114,	2697,
#                   "Humphrey 2019",	49,	995,	50,	909)
#                   # "Haushofer 2021", 4,	702,	16,	702)
mm = metagen(data = studies,
        TE = log_mortality_rr_est,
        seTE = log_mortality_rr_se,
        studlab = Study,
        exclude = Study == "Haushofer 2021",
        sm = "SMD",
        common = TRUE,
        random = FALSE)
mm
mm2 = update.meta(mm, exclude = FALSE, sm = "RR")
forest.meta(mm2, common = FALSE, text.common = "Fixed effects model", text.addline1 = "Effect on all-cause mortality")
funnel.meta(mm2, studlab = TRUE)
title("WQ interventions to reduce all-cause mortality")

metabias(mm2, method.bias = "Egger", k.min = 5)
dmetar::eggers.test(mm2)

# Ok, that's good to know so far. 
# Can I pull information about the adherence in the different studies? 
# 
# Might be easiest to do this manually:
# 

### Reller 2003, Table 5:

intervention1_e = 315
intervention1_p = 0.36
intervention2_e = 397
intervention2_p = 0.44
intervention1_n = intervention1_e/intervention1_p
intervention2_n = intervention2_e/intervention2_p
intervention_e = intervention1_e + intervention2_e
intervention_n = intervention1_n + intervention2_n
intervention_p = intervention_e / intervention_n

control_e = 20
control_p = 0.02
control_n = control_e/control_p

control_se = sqrt(control_p * (1-control_p)/control_n)
intervention_se = sqrt(intervention_p * (1 - intervention_p)/ intervention_n)
delta_se = sqrt(control_se^2 + intervention_se^2)
delta_se
delta = intervention_p - control_p
delta
### Boisson 2013, Table 3:
control_n = 11407
control_e = 223
intervention_n = 11397
intervention_e = 3630
control_p = control_e / control_n
intervention_p = intervention_e / intervention_n
control_se = sqrt(control_p * (1-control_p)/control_n)
intervention_se = sqrt(intervention_p * (1 - intervention_p)/ intervention_n)
delta_se = sqrt(control_se^2 + intervention_se^2)
delta_se
delta = intervention_p - control_p
delta

### Luby 2018, Table 3:
intervention_n = mean(c(611, 598))
intervention_e = mean(c(467, 488))
intervention_p = intervention_e / intervention_n
intervention_se = sqrt(intervention_p * (1 - intervention_p)/ intervention_n)
# Assuming 0 for the control group

### Null 2018, Table 3:
intervention_n = mean(c(385, 637))
intervention_e = mean(c(151, 144))
intervention_p = intervention_e / intervention_n
intervention_se = sqrt(intervention_p * (1 - intervention_p)/ intervention_n)
intervention_p
intervention_se

### Humphrey 2019
intervention1_n = 752
intervention1_e = 438

# Self-reported numbers
self_report_bias_adjustment = 0.9 # From "Adjustment for self-report bias", {{Adherence Adjustments}}

control_n = 667
control_e = 90 * self_report_bias_adjustment
intervention2_n = 650
intervention2_e = 567 * self_report_bias_adjustment


intervention_e = (intervention1_e + intervention2_e)/2
intervention_n = (intervention1_n + intervention2_n)/2
intervention_p = intervention_e / intervention_n
control_p = control_e / control_n
intervention_p = intervention_e / intervention_n
control_se = sqrt(control_p * (1-control_p)/control_n)
intervention_se = sqrt(intervention_p * (1 - intervention_p)/ intervention_n)
delta_se = sqrt(control_se^2 + intervention_se^2)
delta_se
delta = intervention_p - control_p
delta

studies_per_coverage = 
  studies %>%
  transmute(
    Study,
    log_mortality_rr_est,
    intervention_coverage,
    eps1 = log_mortality_rr_se / log_mortality_rr_est,
         eps2 = coverage_se / intervention_coverage,
         eps_tot = sqrt(eps1^2 + eps2^2),
         rr_per_coverage = log_mortality_rr_est / intervention_coverage,
         rr_per_coverage_se = abs(rr_per_coverage) * eps_tot,
    rr_per_coverage_se_adj = rr_per_coverage_se / sqrt(similarity_weight))

mm_per_coverage2 = 
  metagen(data = studies_per_coverage,
             TE = rr_per_coverage,
             seTE = rr_per_coverage_se,
             studlab = Study,
             sm = "RR",
             common = TRUE,
             random = FALSE)

forest.meta(mm_per_coverage2, common = FALSE, xlim = c(0.001, 100), #xlim = c(-40, 20),
                text.addline1 = "Effect on all-cause mortality, scaled for intervention coverage")#, 
 title("test title")
funnel.meta(mm_per_coverage2, studlab = TRUE, pos.studlab = 4)
title("WQ interventions to reduce all-cause mortality\nScaled for intervention coverage")
metabias(mm_per_coverage2, method.bias = "Egger", k.min = 5)
dmetar::eggers.test(mm_per_coverage2)
