##== descriptives of cleaned dataset=========================================
##here it would be sensible to think about sorting exp_new and old together in one exp


#define variables

screen <- dat$screen_width * dat$screen_height

  # states elicited by different conditions
dat %>%
  group_by(condition) %>%
    summarise(mean_awe = mean(awe, na.rm = TRUE),
              sd_awe = sd(awe, na.rm = TRUE),
              mean_happiness = mean(happiness, na.rm = TRUE),
               sd_happiness = sd(happiness, na.rm = TRUE),
              mean_fear = mean(fear, na.rm = TRUE),
              sd_fear = sd(fear, na.rm = TRUE),
              mean_amazement = mean(amazement, na.rm = TRUE),
              sd_amazement = sd(amazement, na.rm = TRUE),
              mean_fascination = mean(happiness, na.rm = TRUE),
              sd_fascination = sd(happiness, na.rm = TRUE)
              )


# IV ~ number of Strengths and Weaknesses listed
dat %>%
    group_by(condition)%>%
      summarise(mean_strengths = mean(strengths, na.rm = TRUE),
                sd_strengths = sd(strengths, na.rm = TRUE),
                mean_weaks = mean(weaks, na.rm = TRUE),
                sd_weaks = sd(weaks , na.rm = TRUE))

##insert balance Strengths/weaknesses column with mutate()
