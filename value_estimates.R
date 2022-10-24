library(tidyverse)

value_estimates = tribble(~mechanism, ~Kenya, ~Uganda, ~Malawi, ~Kenya_ILC,
        "Under-5 Deaths", 902, 1020, 2263, 1252,
        "Over-5 Deaths", 370, 942, 1267, 718,
        "Morbidity", 35, 60, 90, 69,
        "Developmental Effects", 557, 555, 1101, 1224,
        "Medical costs", 906, 737, 1632, 1009) |>
  pivot_longer(-mechanism) 


ggplot(value_estimates, aes(x = name, y= value, fill = mechanism)) + 
  geom_col(color = "black") + 
  scale_fill_brewer(palette = "Set2")

value_estimates |>
  group_by(name) |>
  mutate(pcnt_value = value / sum(value)) |>
  ggplot(aes(x = name, y= pcnt_value, fill = mechanism)) + 
  geom_col(color = "black") + 
  scale_fill_brewer(palette = "Set2")

value_estimates |>
  group_by(mechanism) |>
  summarize(avg_value = mean(value)) |>
  mutate(pcnt_value = avg_value / sum(avg_value)) |>
  knitr::kable()
# Roughly:
# 30% Under-5 deaths
# 20% Over-5 deaths
# 20% Developmental Effects
# 25% Medical Costs