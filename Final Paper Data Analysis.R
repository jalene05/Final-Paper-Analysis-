dat = read.csv("Final Mikula Results.csv", header=TRUE, na.strings=c("", "NA", " ", "N/A"))  

require(tidyverse)
require(lme4)
require(lmerTest)
require(ggplot2)
require(rstatix)

# Turn off scientific notation (optional)
options(scipen = 999)

# Keep Variables necessary for viewing and analyzing the data, remove unneeded variables
dat <- dat %>%
  select(participant, hitPercent, pointsGiven, horOrTilt, hitOrMiss, trialType, trialsNum, tasksNum, wallOrient, Sound)

# Outlier analysis - Keep people who hit the ball more than 50% of the time. Exclude participants who hit the ball 50% or less of the time
dat = dat %>% group_by(participant) %>%
  mutate(hitPercent2 = mean(hitOrMiss == "hit", na.rm = TRUE))  %>%
  filter(hitPercent2 > 0.5)

# Linear Mixed Model (Equivalent to Repeated Measures ANOVA): x1 = Wall Orientation, x2 = Sound, y = Points Given 
# Recode Variables
dat$Sound <- as.factor(dat$Sound)

dat$Sound <- dat$Sound %>%
  recode("0.4" = "Sound", "5" = "No Sound")

dat$wallOrient <- as.factor(dat$wallOrient)
dat$pointsGiven <- as.numeric(dat$pointsGiven)

# Linear Model
Model3 = lmer(pointsGiven ~ wallOrient*Sound + (1 | participant), 
              data = dat)
# Results
summary(Model3)


#Descriptive Statistics Table of points earned by wall orientation and sound
dat %>%
  group_by(wallOrient, Sound) %>%
  get_summary_stats(pointsGiven) 
