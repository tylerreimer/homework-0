library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, rpg = R/G, hrpg = HR/G) %>%
  select(avg_attendance, rpg, hrpg, W, yearID) 
  


runs <- Teams_small %>% 
  lm(avg_attendance ~ hrpg, data = .)
tidy(runs, conf.int = TRUE)

fit = lm(attendance ~ W, data = .)
wins <- Teams_small %>%
  mutate(R_hat = predict(fit, data = .)) %>%
  ggplot(aes(R_hat, attendance)) +
  geom_point() +
  geom_abline()

att <- Teams_small %>% 
  lm(avg_attendance ~ W, data = .)
tidy(att, conf.int = TRUE)
summary(att)

year <- Teams_small %>% 
  lm(avg_attendance ~ yearID, data = .)
tidy(year, conf.int = TRUE)
plot(Teams_small$yearID, Teams_small$attendance)

cor(Teams_small$W, Teams_small$hrpg)

# stratfy HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

teams2 <- Teams_small %>% 
  mutate(wins = round(W/10), HR_per_game = HR/G) %>%
  filter(wins %in% 5:10) 

sum(teams2$wins == 8)

teams2 %>% 
  ggplot(aes(avg_attendance, W)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ wins)

Team_strata %>% group_by(newWins)
head(Team_strata)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

dat1 <- teams2 %>% group_by(wins) %>% 
  summarize(slope = cor(avg_attendance, HR_per_game)*sd(HR_per_game)/sd(avg_attendance)) 

head(dat) 

max(dat1$slope)


# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

teams2 <- Teams_small %>% 
  mutate(wins = round(W/10), R_per_game = R/G, hrpg = HR/G) %>%
  filter(wins %in% 5:10) %>%
  select(wins, avg_attendance, hrpg)

teams2 %>% group_by(wins) %>%
  summarize(slope = cor(avg_attendance, hrpg)*sd(avg_attendance)/sd(hrpg))

#Multivariate LM for AVG Attendance
model <- Teams_small %>% filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, rpg=R/G, hrpg=H/G) %>% 
  do(tidy(lm(avg_attendance ~ W + rpg + hrpg + yearID, data=.),conf.int=TRUE))
model

o2 <- Teams %>% 
  mutate(avg_attenance = attendance/G, rpg=R/G, hrpg=HR/G) 
  

predict(model, o2)
mutate(R_hat = predict(data, newdata = .)) 
  

#Attendance if 5runs, 1.2hr and 80 wins

Teams_small2002 <- Teams %>% filter(yearID==2002) %>% 
  mutate(avg_attendance = attendance/G, rpg = R/G, hrpg = HR/G) %>% 
  select(rpg,hrpg,W,yearID,avg_attendance) 
  
fit <- lm(avg_attendance ~ rpg+hrpg+W+yearID,data=Teams_small)
tidy(fit)
Teams_small2002 %>% mutate(newattendance=predict(fit, newdata = TRUE))

new <- predict(fit, data.frame(Teams_small2002))
new
cor(new, Teams_small2002$avg_attendance)
