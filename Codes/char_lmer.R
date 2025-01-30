
# Run the first 4 chunk of codes in Root_graph_v4.Rmd 

char_test <- 
  root_raw %>% 
  select(c(Stands, Plot, Collar, Date, ID, MY, Charcoal, Charcoal_reweigh))

# update values from reweight
for (i in 1:nrow(char_test)){
  if(is.na(char_test$Charcoal_reweigh[i]) == "FALSE")
    {char_test$Charcoal[i] <- char_test$Charcoal_reweigh[i]}  
}

# soil core area
area <- (5.2 * 0.01 / 2 ) ^ 2 * pi # area (m2)

# get plot avg
char_plmean <- 
  char_test %>% 
  mutate(char_per_area = Charcoal / area) %>% 
  group_by(Stands, Plot, MY) %>%
  summarise(across(
    char_per_area,
    list(mean = mean, sd = sd, n = ~ n()))
  ) %>% 
  arrange(match(MY, MY_order)) %>%
  arrange(Stands, Plot) 

# correct values - check the Root_graph_v4 for the details
temp_imp <- 
  char_plmean %>% 
  filter(char_per_area_n == 10) %>%
  left_join(
    data.frame(
      Stands = rep("AB", 2), 
      Plot = c("1", "2"), 
      MY = c("Feb21", "Jun21")
    ), by = c("Stands", "MY")
  ) %>%
  select(- Plot.x) %>%
  rename(Plot = Plot.y) %>%
  mutate(char_per_area_n = 0)

# combine two dfs
char_plmean_imp <- 
  rbind(char_plmean, temp_imp) %>%
  arrange(match(MY, MY_order)) %>%
  arrange(Stands, Plot) 

# plot - temporal charcoal production
char_plmean_imp %>% filter(!is.na(MY)) %>% 
  mutate(
    date = as.Date.character(paste0("01-", substring(MY, 1, 3), "-20", substring(MY, 4, 5)), 
                             format = "%d-%b-%Y")
    ) %>% 
  ggplot() +
  geom_boxplot(
    aes(x = date, y = char_per_area_mean, col = Stands, group = interaction(date, Stands))
    ) +
  theme_classic() +
  #xlab("MonthYear") + 
  ylab(expression("Charcoal mass (g m"^-2~")")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

  

# Q1 - are there monthly diffs
# Q2 - how to quantify C inputs 
# Q3 - sig differences b/w stands






# Q1 - see the monthly diffs by lmer

char_plmean_yr <-
  char_plmean_imp %>%
  filter(!is.na(MY)) %>% 
  filter(!MY %in% c("Jul20", "Aug20"))


#write.csv(char_mean_yr_plot, "G:/My Drive/Research/Projects/CBC/charcoal/char_mo_Sep2020_Sep2021.csv")

# AB
char_test_AB <-
  char_plmean_yr %>% 
  filter(Stands == "AB") 

char_AB_model <- 
  lmerTest::lmer(char_per_area_mean ~ MY + (1|Plot), data = char_test_AB)
anova(char_AB_model)

char_AB_posthoc <- emmeans::emmeans(char_AB_model, ~ MY)
summary(char_AB_posthoc)
lsmeans::lsmeans(char_AB_model, pairwise ~ MY, adjust = "tukey")


# AB - before after fires
char_test_AB_fire <-
  char_plmean_yr %>% 
  filter(Stands == "AB") %>% 
  filter(MY == "Sep20" | MY == "Oct20" |MY == "Nov20" )

char_AB_fire_model <- 
  lmerTest::lmer(char_per_area_mean ~ MY + (1|Plot), data = char_test_AB_fire)
anova(char_AB_fire_model)

char_AB_fire_posthoc <- emmeans::emmeans(char_AB_fire_model, ~ MY)
summary(char_AB_fire_posthoc)
lsmeans::lsmeans(char_AB_fire_model, pairwise ~ MY, adjust = "tukey")


ggplot(char_test_AB_fire) +
  geom_boxplot(aes(x = factor(MY, level = c("Sep20", "Oct20", "Nov20")), char_per_area_mean)) + 
  theme_classic() +
  xlab("MonthYear") +
  ylab("Charcoal (g/m2)") +
  ggtitle("Before and after a fire in AB")

# IB
char_test_IB <-
  char_plmean_yr %>% 
  filter(Stands == "IB") 

char_IB_model <- 
  lmerTest::lmer(char_per_area_mean ~ MY + (1|Plot), data = char_test_IB)
anova(char_IB_model)

char_IB_posthoc <- emmeans::emmeans(char_IB_model, ~ MY)
summary(char_IB_posthoc)
lsmeans::lsmeans(char_IB_model, pairwise ~ MY, adjust = "tukey")


# NB
char_test_NB <-
  char_plmean_yr %>% 
  filter(Stands == "NB") 

char_NB_model <- 
  lmerTest::lmer(char_per_area_mean ~ MY + (1|Plot), data = char_test_NB)
anova(char_NB_model)

char_NB_posthoc <- emmeans::emmeans(char_NB_model, ~ MY)
summary(char_NB_posthoc)
lsmeans::lsmeans(char_NB_model, pairwise ~ MY, adjust = "tukey")




# Q3 - sig differences in stands
char_mean_yr_plot <-
  char_plmean_yr %>%
  group_by(Stands, Plot) %>%
  summarise(across(
    char_per_area_mean,
    list(
      mean = ~mean(.x, na.rm = TRUE), 
      sd = ~sd(.x, na.rm = TRUE), 
      n = ~sum(!is.na(.x))
    )
  )) %>% 
  arrange(Stands, Plot) %>% 
  rename(
    mean = char_per_area_mean_mean,
    sd = char_per_area_mean_sd, 
    n = char_per_area_mean_n
  )

library(stats)
# simple anova on annual avg of charcoal
char_yr_anova <- aov(mean ~ Stands, data = char_mean_yr_plot)
summary(char_yr_anova)

ggplot(char_mean_yr_plot) +
  geom_boxplot(aes(Stands, mean)) +
  theme_classic() +
  ylab("Charcoal (g/m2)") +
  ggtitle("Annual mean")


# stand mean
char_mean_yr_stand <-
  char_mean_yr_plot %>% 
  group_by(Stands) %>%
  summarise(across(
    mean,
    list(
      mean = ~mean(.x, na.rm = TRUE), 
      sd = ~sd(.x, na.rm = TRUE), 
      n = ~sum(!is.na(.x))
    )
  ))
