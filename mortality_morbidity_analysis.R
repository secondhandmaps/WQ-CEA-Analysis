# From the appendix of https://docs.google.com/document/d/1m_VU_vSyz1KlcDN69ZNU1X7bzYUpDn9yN8CW0VQcXU4/edit
# 
studies = 
  tribble(~study, ~overall, ~severe, ~severe_type,
        "Chiller 06", 39, 46, "persistent",
        "Luby 06", mean(c(55, 51, 64, 55)), mean(c(54, 48, 61, 55)), "provider",
        "Luby 06", mean(c(55, 51, 64, 55)), mean(c(39, 17, 24, -56)), "hospital",
        "Luby 06", mean(c(55, 51, 64, 55)), mean(c(44, 55, 70, 62)), "persistent",
        "Peletz 12", 53, 37, "persistent",
        "Kirby 19", 29, 28, "health facility", # I think I'm getting that analysis right
        "Reller 03", mean(c(24, 29, 25, 12)), mean(c(16, 9, 20, 13)), "severe",
        "Reller 03", mean(c(24, 29, 25, 12)), mean(c(-1, 9, -16, -21)), "prolonged")        
studies_1 = studies %>%
  group_by(study) %>%
  summarize(overall = mean(overall),
            severe = mean(severe))

studies_1 %>%
  mutate(severe_over_overall = severe/overall) %>%
  summarize(avg = mean(severe_over_overall),
            sd = sd(severe_over_overall) / sqrt(n()))
# That makes me much more confident that the correct scaling factor is *not* greater than 1. 
# I think probably keeping the mean estimate at 1 is probably the right approach, but I think 
# it might actually be generous. 


# There's another line of evidence from challenge trials on diarrhea-causing
# organisms, and seeing if there is a consistent dose-response pattern. 
# I think I'm happy to just leave that as qualitative evidence and say we'll
# take a lognormal distribution with a mean of 1 and a sd of 0.171 
# (the magic of having being centered at 1)
sd(log(rnorm(5000, 1, 0.171)))
