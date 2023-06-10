library(ggplot2)
library(tidyverse)
library(car)
library(ggpubr)


# "Stats 100 project data.csv" contains the same information as the raw retrosheet data
# featured in "Pitch Count Data Final.xlsx" in the GitHub repository.
dat <- read.csv("Stats 100 project data.csv") %>%
  mutate(wOBA = outcome - mean(outcome))


# MULTIPLE REGRESSION

linear_model <- lm(wOBA ~ pitch.count + batting.num + matchups + location + start.relief, data = dat)
summary(linear_model)

avPlots(linear_model)


# Assumptions
plot(linear_model, cex = 0.6, which = 2)


# Multicollinearity
vif(linear_model)




# EXPLORATORY DATA ANALYSIS

# Average wOBA vs batting number
wOBA_bat_num <- dat %>%
  group_by(batting.num) %>%
  summarize(avg_wOBA = mean(wOBA))

ggplot(data = wOBA_bat_num, aes(x = batting.num, y = avg_wOBA)) +
  geom_line(color = "steelblue", size = 1) +
  labs(x = "Spot in Batting Order", y = "Average wOBA", title = "Batter Preformance by Location in Lineup") +
  geom_smooth(method="lm", color = "grey1", alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 1))


# Average wOBA vs previous match ups
wOBA_matchups <- dat %>%
  group_by(matchups) %>%
  summarize(avg_wOBA = mean(wOBA))

ggplot(data = wOBA_matchups, aes(x = matchups, y = avg_wOBA)) +
  geom_line(color = "steelblue", size = 1) +
  labs(x = "Previous In-Game Matchups", y = "Average wOBA", title = "Batter Preformance by Prior Matchups") +
  geom_smooth(method="lm", color = "grey1", alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 1.5))


# Average wOBA vs pitch count for starters
wOBA_pitch_count_starters <- dat %>%
  filter(start.relief == "starter") %>%
  group_by(pitch.count) %>%
  summarize(avg_wOBA = mean(wOBA)) %>%
  filter(pitch.count <= 110)

ggplot(data = wOBA_pitch_count_starters, aes(x = pitch.count, y = avg_wOBA)) +
  geom_line(color = "steelblue", size = 1) +
  labs(x = "Pitches Thrown", y = "Average wOBA", title = "wOBA by Pitches Thrown by Starter") +
  geom_smooth(method="lm", color = "grey1", alpha = 0.2) +
  theme_minimal()





  