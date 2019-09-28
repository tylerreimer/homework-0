install.packges("Lahman")
library(Lahman)
str(Lahman)
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()


Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(trips = X3B / G, doubs = X2B / G) %>%
  ggplot(aes(trips, doubs)) + 
  geom_point(alpha = 0.5)

R <-Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(doubs = X2B / G, trips = X3B / G) %>%
  summarize(cor(doubs, trips))
R
#Heights
install.packages("HistData")
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))
# scatterplot of father and son heights
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

  # number of fathers with height 72 or 72.5 inches
  sum(galton_heights$father == 72)
  sum(galton_heights$father == 72.5)
  # predicted height of a son with a 72 inch tall father
  conditional_avg <- galton_heights %>%
    filter(round(father) == 72) %>%
    summarize(avg = mean(son)) %>%
    pull(avg)
  conditional_avg
  # stratify fathers' heights to make a boxplot of son heights
  galton_heights %>% mutate(father_strata = factor(round(father))) %>%
    ggplot(aes(father_strata, son)) +
    geom_boxplot() +
    geom_point()
  # center of each boxplot
  galton_heights %>%
    mutate(father = round(father)) %>%
    group_by(father) %>%
    summarize(son_conditional_avg = mean(son)) %>%
    ggplot(aes(father, son_conditional_avg)) +
    geom_point()
  # calculate values to plot regression line on original data
  mu_x <- mean(galton_heights$father)
  mu_y <- mean(galton_heights$son)
  s_x <- sd(galton_heights$father)
  s_y <- sd(galton_heights$son)
  r <- cor(galton_heights$father, galton_heights$son)
  m <- r * s_y/s_x
  b <- mu_y - m*mu_x
  # add regression line to plot
  galton_heights %>%
    ggplot(aes(father, son)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = b, slope = m)
  
  # compute a regression line to predict the son's height from the father's height
  mu_x <- mean(galton_heights$father)
  mu_y <- mean(galton_heights$son)
  s_x <- sd(galton_heights$father)
  s_y <- sd(galton_heights$son)
  r <- cor(galton_heights$father, galton_heights$son)
  m_1 <-  r * s_y / s_x
  b_1 <- mu_y - m_1*mu_x
  # compute a regression line to predict the father's height from the son's height
  m_2 <-  r * s_x / s_y
  b_2 <- mu_x - m_2*mu_y
  
  set.seed(1989) #if you are using R 3.5 or earlier
  set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
  library(HistData)
  data("GaltonFamilies")
  
  female_heights <- GaltonFamilies%>%     
    filter(gender == "female") %>%     
    group_by(family) %>%     
    sample_n(1) %>%     
    ungroup() %>%     
    select(mother, childHeight) %>%     
    rename(daughter = childHeight)
head(female_heights)
mean(female_heights$mother)
sd(female_heights$mother)
mean(female_heights$daughter)
sd(female_heights$daughter)
cor(female_heights$mother, female_heights$daughter)

# calculate values to plot regression line on original data
mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
m <- r * s_y/s_x
b <- mu_y - m*mu_x
c <- .327877^2/100
.5^2/100*3
s_x/s_y
mu_y <- mean(female_heights$daughter)
mu_x <- mean(female_heights$mother)
mu_y - (r * s_y/s_x)*mu_x
b
(1-r^2)*
#Daughter's height conditional of mom's height
sum(female_heights$mother == 60)
sum(female_heights$mother == 60.5)
conditional_avg <- female_heights %>% 
  filter(round(mother) == 60) %>%
  summarize(avg = mean(daughter)) %>% 
  pull(avg)
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
female_heights %>% mutate(mother_strata = factor(round(mother))) %>%
  ggplot(aes(mother_strata, daughter)) +
  geom_boxplot() +
  geom_point()
# center of each boxplot
female_heights %>%
  mutate(mother = round(mother)) %>%
  group_by(mother) %>%
  summarize(daughter_conditional_avg = mean(daughter)) %>%
  ggplot(aes(mother, daughter_conditional_avg)) +
  geom_point()
# calculate values to plot regression line on original data
mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
m <- r * s_y/s_x
b <- mu_y - m*mu_x
# add regression line to plot
female_heights %>%
  ggplot(aes(mother, daughter)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

# compute a regression line to predict the daughter's height from the mother's height
mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x
# compute a regression line to predict the mother's height from the daughter's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y
m_2
b_2

