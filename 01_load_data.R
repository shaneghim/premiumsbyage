library(tidyverse)
library(janitor)
library(data.table)

#how do health plans vary by age?
#load only the columns we want from Rate.csv and PlanAttributes.csv (for our question we want to answer)
#convert to a tibble so it can work with tidyverse tools and standardize the column names
#individual rate is the premium, age is what we need, business year is what
#we are looking at, state code to compare states, planid to match each rate to its plan, tobacco impacts rates,
#ratingareaid since premiums vary by area within a state
rates <- fread("data/Rate.csv",
         select = c("BusinessYear", "StateCode", "PlanId", "RatingAreaId",
                    "Tobacco", "Age", "IndividualRate")) %>%
  as_tibble() %>% clean_names()

plans <- fread("data/PlanAttributes.csv",
         select = c("BusinessYear", "StateCode", "StandardComponentId",
                    "MetalLevel", "PlanType")) %>%
  as_tibble() %>% clean_names()

glimpse(rates)
glimpse(plans)

#review our data
nrow(rates)
rates %>% count(business_year)
rates %>% count(tobacco)
rates %>% count(age) %>% print(n = 80)
summary(rates$individual_rate)
colSums(is.na(rates))

#review our filters
#keep single-year ages 21-64 (drops age bands like "0-20" and "65 and over", and "Family Option")
rates_pre <- rates %>%
  filter(business_year %in% 2014:2016, str_detect(age, "^[0-9]+$")) %>%
  mutate(age = as.numeric(age)) %>%
  filter(age >= 21, age <= 64)

#tobacco: how much of the data is each category? (no preference has the most rows)
rates_pre %>% count(tobacco) %>% mutate(share = round(n / sum(n), 3))

#how many rows would the rate filters remove?
sum(is.na(rates_pre$individual_rate) | rates_pre$individual_rate <= 0, na.rm = TRUE)
sum(rates_pre$individual_rate >= 5000, na.rm = TRUE)
quantile(rates_pre$individual_rate, c(.5, .9, .99, .999, 1), na.rm = TRUE)


#clean all three years' rates
#same filters for every year: only the "No Preference" tobacco category,
#and no missing, zero, or implausible rates
rates_all <- rates_pre %>%
  filter(tobacco == "No Preference",
         !is.na(individual_rate),
         individual_rate > 0,
         individual_rate < 5000)
summary(rates_all$individual_rate)

#check for duplicates (same plan, area, and age in the same year)
rates_all %>%
  count(business_year, state_code, plan_id, rating_area_id, age) %>%
  filter(n > 1) %>%
head()

#the file lists a rate for each quarter, so one plan, area, and age
#can have up to four rows a year; we average them into one yearly premium
rates_all <- rates_all %>%
  group_by(business_year, state_code, plan_id, rating_area_id, age) %>%
  summarise(individual_rate = mean(individual_rate), .groups = "drop")

#prep plans
#the plan file has several rows per plan, so keep one per plan per year
plans_all <- plans %>%
  filter(business_year %in% 2014:2016) %>%
  distinct(business_year, state_code, standard_component_id, .keep_all = TRUE) %>%
  select(business_year, state_code, standard_component_id, metal_level, plan_type)

plans_all %>% count(business_year, metal_level)
plans_all %>% count(plan_type)

#joining of rates and plans tables, ahhh similar to SQL
#inner_join keeps rates that have a matching plan
#plan ids can repeat across years, so the year is part of the match too
#filter removes dental and catastrophic plans -> only standard plans
df_all <- rates_all %>%
  inner_join(plans_all,
  by = c("plan_id" = "standard_component_id", "state_code", "business_year")) %>%
  filter(metal_level %in% c("Bronze", "Silver", "Gold", "Platinum"))

nrow(rates_all)
nrow(df_all)
df_all %>% count(business_year)
df_all %>% count(business_year, metal_level)

#which states are in the data? (the public file does not cover every state)
n_distinct(df_all$state_code)
df_all %>% distinct(state_code) %>% arrange(state_code) %>% pull(state_code)

#2016 only, for the single-year charts
df_2016 <- df_all %>% filter(business_year == 2016)

#check the 5000 cutoff on the final sample
#same plans and same year(2016) as the main chart, only the cutoff changes
cutoff_check <- map_dfr(c(2000, 3000, 5000, 10000, Inf), function(limit) {r <- rates_pre %>%
    filter(business_year == 2016, tobacco == "No Preference", !is.na(individual_rate),
           individual_rate > 0, individual_rate < limit) %>%
    inner_join(plans_all %>% filter(business_year == 2016),
           by = c("plan_id" = "standard_component_id", "state_code", "business_year")) %>%
    filter(metal_level %in% c("Bronze", "Silver", "Gold", "Platinum")) %>%
    group_by(state_code, plan_id, rating_area_id, age) %>%
    summarise(individual_rate = mean(individual_rate), .groups = "drop")
m <- r %>% group_by(age) %>% summarise(avg = mean(individual_rate), .groups = "drop")
  tibble(cutoff = limit, rows_kept = nrow(r),
    ratio_64_to_21 = round(m$avg[m$age == 64] / m$avg[m$age == 21], 3))})
print(cutoff_check, width = Inf)

#one plan, area, and age can have up to four rows a year (one for each quarter) and average them
#here we compare that to using only the first, lowest, or highest quarter
df_quarters <- rates_pre %>%
  filter(tobacco == "No Preference", !is.na(individual_rate),individual_rate > 0, individual_rate < 5000) %>%
inner_join(plans_all, by = c("plan_id" = "standard_component_id", "state_code", "business_year")) %>%
  filter(metal_level %in% c("Bronze", "Silver", "Gold", "Platinum"))

method_check <- map_dfr(c("first listed quarter", "average of quarters (used)",
                          "lowest quarter", "highest quarter"), function(m) {r <- df_quarters %>%
  group_by(business_year, state_code, plan_id, rating_area_id, age) %>%
  summarise(rate = switch(m, "average of quarters (used)" = mean(individual_rate),
                                         "lowest quarter" = min(individual_rate),
                                        "highest quarter" = max(individual_rate), individual_rate[1]),
                                                  .groups = "drop")
r %>%
group_by(business_year, age) %>%
summarise(avg = mean(rate), .groups = "drop") %>%

filter(age %in% c(21, 64)) %>%
  pivot_wider(names_from = age, values_from = avg, names_prefix = "age_") %>%
  mutate(method = m, ratio_64_to_21 = round(age_64 / age_21, 3),
                             age_21 = round(age_21),
                             age_64 = round(age_64))}) %>%
select(method, business_year, age_21, age_64, ratio_64_to_21)
print(method_check, n = Inf, width = Inf)

#average premium by age and relative to age 21
#works for any grouping like age_curve, data, metal_level
age_curve <- function(data, ...) {
  data %>%
    group_by(..., age) %>%
    summarise(avg_rate = mean(individual_rate), .groups = "drop") %>%
    group_by(...) %>%
    mutate(ratio_to_21 = avg_rate / avg_rate[age == 21]) %>%
ungroup()}

#2016 age curve
by_age <- age_curve(df_2016)

#tables and charts start here:
age_ratio_table <- by_age %>%
  filter(age %in% c(21, 30, 40, 50, 60, 64)) %>%
  mutate(avg_rate = round(avg_rate, 2), ratio_to_21 = round(ratio_to_21, 2)) %>%
  select(age, avg_rate, ratio_to_21)
age_ratio_table

#dashed horizontal line sits at a ratio of 3
#indicates ACA rule that a 64-year old cant be charged more than three times what a 21-year old can be charged
p_premium <- ggplot(by_age, aes(age, ratio_to_21)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 3, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(20, 65, 5)) +
  labs(title = "Average premium relative to age 21 (2016)",
    subtitle = "Dashed line: 3:1 max age rating ratio under the ACA",
           x = "Age",
           y = "Premium ratio to age 21") + theme_minimal()
p_premium

#2016 metal level
by_age_tier <- age_curve(df_2016, metal_level) %>%
  mutate(metal_level = factor(metal_level, levels = c("Bronze", "Silver", "Gold", "Platinum")))

#theme_minimal to clean up the graph
#scale_x_continuous to extend x-axis
p_curvegroup <- ggplot(by_age_tier, aes(age, ratio_to_21, color = metal_level)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(20, 65, 5)) +
  labs(title = "Premium age curve by metal level (2016)",
    subtitle = "Average premium relative to age 21. Dashed line: 3:1 ACA limit",
           x = "Age",
           y = "Premium ratio to age 21",
       color = "Metal level") + theme_minimal()
p_curvegroup

p_tier <- ggplot(by_age_tier, aes(age, avg_rate, color = metal_level)) +
  geom_line(linewidth = 1) + scale_x_continuous(breaks = seq(20, 65, 5)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Average monthly premium by age & metal level (2016)",
           x = "Age",
           y = "Average monthly premium ($)",
       color = "Metal level") + theme_minimal()
p_tier

#comparison of all three years
by_age_year <- age_curve(df_all, business_year) %>%
  mutate(business_year = factor(business_year))

#premiums at the youngest and oldest ages by year
age_year_table <- by_age_year %>%
  filter(age %in% c(21, 64)) %>%
  mutate(avg_rate = round(avg_rate)) %>%
  select(business_year, age, avg_rate) %>%
  arrange(age, business_year)
age_year_table

#summary numbers by year
year_summary <- df_all %>%
  group_by(business_year) %>%
  summarise(avg_rate = round(mean(individual_rate)),
         median_rate = round(median(individual_rate)),
               plans = n_distinct(state_code, plan_id))
year_summary

p_byyear <- ggplot(by_age_year, aes(age, ratio_to_21, color = business_year)) +
  geom_line(linewidth = 1) + geom_hline(yintercept = 3, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(20, 65, 5)) +
  labs(title = "Premium age curve by year",
    subtitle = "Average premium relative to age 21, Dashed line: 3:1 ACA limit",
           x = "Age",
           y = "Premium ratio to age 21",
       color = "Year") + theme_minimal()
p_byyear

p_dollars <- ggplot(by_age_year, aes(age, avg_rate, color = business_year)) +
  geom_line(linewidth = 1) + scale_x_continuous(breaks = seq(20, 65, 5)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Average monthly premium by age & year",
           x = "Age",
           y = "Average monthly premium ($)",
       color = "Year") + theme_minimal()
p_dollars

#clearly indicates that the average premiums among plans by age goes up yearly, *not necessarily for the same plans

#the plans offered change every year, so a higher average could just mean different plans
#keep only plans (state + plan id) that show up in all three years and compare again
plans_in_all <- df_all %>%
  distinct(state_code, plan_id, business_year) %>%
  count(state_code, plan_id) %>%
  filter(n == 3)
nrow(plans_in_all)

df_matched <- df_all %>%
semi_join(plans_in_all, by = c("state_code", "plan_id"))
df_matched %>% count(business_year)

matched_summary <- df_matched %>%
  group_by(business_year) %>%
  summarise(avg_rate = mean(individual_rate),
               plans = n_distinct(state_code, plan_id)) %>%
  mutate(pct_change_vs_prior_year = round(100 * (avg_rate / lag(avg_rate) - 1), 1),
               pct_change_vs_2014 = round(100 * (avg_rate / avg_rate[1] - 1), 1),
                         avg_rate = round(avg_rate))
matched_summary

by_age_matched <- age_curve(df_matched, business_year) %>%
  mutate(business_year = factor(business_year))

p_matched <- ggplot(by_age_matched, aes(age, avg_rate, color = business_year)) +
  geom_line(linewidth = 1) + scale_x_continuous(breaks = seq(20, 65, 5)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Average monthly premium by age, same plans only",
    subtitle = "Plans offered in all three years",
           x = "Age",
           y = "Average monthly premium ($)",
       color = "Year") + theme_minimal()
p_matched

#2016 by state
#the 3:1 limit is federal, but a few states set their own age rules
#so check the age ratio by state
state_ratio <- df_2016 %>%
  filter(age %in% c(21, 64)) %>%
  group_by(state_code, age) %>%
  summarise(avg_rate = mean(individual_rate), .groups = "drop") %>%
  pivot_wider(names_from = age, values_from = avg_rate, names_prefix = "age_") %>%
  mutate(ratio_64_to_21 = round(age_64 / age_21, 2)) %>%
  arrange(ratio_64_to_21)
print(state_ratio, n = Inf)

#save it all
dir.create("output", showWarnings = FALSE)

ggsave("output/age_curve_2016.png", p_premium, width = 7, height = 5, dpi = 300, bg = "white")
ggsave("output/age_curve_by_tier.png", p_curvegroup, width = 7, height = 5, dpi = 300, bg = "white")
ggsave("output/premium_dollars_by_tier.png", p_tier, width = 7, height = 5, dpi = 300, bg = "white")
ggsave("output/age_curve_by_year.png", p_byyear, width = 7, height = 5, dpi = 300, bg = "white")
ggsave("output/premium_dollars_by_year.png", p_dollars, width = 7, height = 5, dpi = 300, bg = "white")
ggsave("output/premium_dollars_same_plans.png", p_matched, width = 7, height = 5, dpi = 300, bg = "white")

write_csv(cutoff_check, "output/cutoff_check.csv")
write_csv(method_check, "output/method_check.csv")
write_csv(age_ratio_table, "output/age_ratio_table.csv")
write_csv(age_year_table, "output/age_year_table.csv")
write_csv(year_summary, "output/year_summary.csv")
write_csv(matched_summary, "output/matched_summary.csv")


saveRDS(df_all, "data/df_2014_2016.rds")
writeLines(capture.output(sessionInfo()), "output/session_info.txt")
list.files("output")


#DONE