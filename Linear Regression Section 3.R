library(dslabs)
data("research_funding_rates")
research_funding_rates

tab <- matrix(c(sum(research_funding_rates$awards_men), sum(research_funding_rates$applications_men) - sum(research_funding_rates$awards_men), sum(research_funding_rates$awards_women), sum(research_funding_rates$applications_women) - sum(research_funding_rates$awards_women)),ncol=2)
colnames(tab) <- c('Men', 'Women')
rownames(tab) <- c('Awarded', 'Not')
tab1 <- as.table(tab)
tab1

tidy(chisq.test(tab1))
chisq.test(tab1)

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

dat %>% ggplot(aes(success, discipline, col = gender, size = applications)) + geom_point()
  