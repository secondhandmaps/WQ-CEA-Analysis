library(tidyverse)

value_estimates_o = tribble(~mechanism, ~`Kenya DSW`, ~`Uganda DSW`, ~`Malawi DSW`, ~`Kenya ILC`,
        "Under-5 Deaths", 902, 1020, 2263, 1252,
        "Over-5 Deaths", 370, 942, 1267, 718,
        "Morbidity", 35, 60, 90, 69,
        "Developmental Effects", 557, 555, 1101, 1224,
        "Medical Costs", 906, 737, 1632, 1009) |>
  pivot_longer(-mechanism) %>%
  mutate(Version = "GiveWell Original")

value_estimates_u = tribble(~mechanism, ~`Kenya DSW`, ~`Uganda DSW`, ~`Malawi DSW`, ~`Kenya ILC`,
                          "Under-5 Deaths", 689, 835, 1612, 998,
                          "Over-5 Deaths", 327, 735, 987, 649,
                          "Morbidity", 40, 66, 101, 73,
                          "Developmental Effects", 490, 488, 968, 916,
                          "Medical Costs", 1040, 680, 1327, 1128) |>
  pivot_longer(-mechanism) %>%
  mutate(Version = "Updated")

value_estimates = bind_rows(value_estimates_o, value_estimates_u) %>%
  mutate(mechanism = factor(mechanism, levels = c("Under-5 Deaths", "Over-5 Deaths", 
                            "Developmental Effects", "Medical Costs", "Morbidity")),
         location = factor(name, levels = c("Kenya DSW", "Uganda DSW", "Malawi DSW", "Kenya ILC")))

g1 = ggplot(value_estimates, aes(x = mechanism, y= value, fill = Version)) + 
  geom_col(color = "black", position = "dodge2") + 
  scale_fill_brewer(palette = "Set2") +
  facet_wrap("location", scales = "free_x") +
  guides(x = guide_axis(n.dodge = 2)) +
  ylab("Units of value per 100,000 people covered")  +
  xlab("Mechanism") +
  theme_gray(base_size = 18) +
  theme(#strip.text.x = element_text(size = 20),
        legend.position = "bottom")
g1
library(ragg)
agg_png(width = 1200, height = 600, scaling = 1.55)
g1
dev.off()



x = value_estimates %>%
  group_by(location, version) %>%
  summarize(value = sum(value),
            .groups = "drop")

ggplot(x, aes(x = location, y = value, fill = version)) + 
  geom_col(color = "black", position = "dodge2") + 
  scale_fill_brewer(palette = "Set2") +
  facet_wrap("location", scales = "free_y") 

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