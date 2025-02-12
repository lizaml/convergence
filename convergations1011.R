library("readxl")
d1<- read_excel("data1310.xlsx", 
                  sheet = "population")

library(data.table)
d1<-dcast(melt(setDT(d1), id.var=c("indicator","i"), 
                 variable.name="t"), 
          i+t~indicator, value.var='value')
attach(d1)

d2<-  read_excel("data1310.xlsx", 
                    sheet = "vrp")

d2<-dcast(melt(setDT(d2), id.var=c("indicator","i"), 
               variable.name="t"), 
          i+t~indicator, value.var='value')
attach(d2)

library(tidyverse)
library(nycflights13)

df = merge(x = d2, y = d1, by = c("i", 't'))
df


d2<-  read_excel("data1011.xlsx", 
                 sheet = "internet_share")

d2<-dcast(melt(setDT(d2), id.var=c("indicator","i"), 
               variable.name="t"), 
          i+t~indicator, value.var='value')
attach(d2)
head(d2)

d3<-  read_excel("data1011.xlsx", 
                 sheet = "rnd_share")

d3<-dcast(melt(setDT(d3), id.var=c("indicator","i"), 
               variable.name="t"), 
          i+t~indicator, value.var='value')
attach(d3)
head(d3)

d4<-  read_excel("data1011.xlsx", 
                 sheet = "ifo_invest")

d4<-dcast(melt(setDT(d4), id.var=c("indicator","i"), 
               variable.name="t"), 
          i+t~indicator, value.var='value')
attach(d4)
head(d4)

df <- merge(x = d2, y = d3, by = c("i", "t"), all = TRUE) 
df <- merge(x = df, y = d4, by = c("i", "t"), all = TRUE)

df <- df[!(df$t %in% c(2010, 2011, 2022, 2023)), ]
df1 <- read_excel("df.xlsx")
df <- merge(x = df, y = df1, by = c("i", "t"), all = TRUE)
df


library("openxlsx")
write.xlsx(df, "df.xlsx")


library('reshape2')
library("ggplot2")   
library("stargazer") 
library("lmtest")    
library('sandwich')
library('readxl')
library("plm")
library("ellipsis")
library('car')
library(dplyr)
library(ggplot2)

# Если вам нужно очистить рабочее пространство от старых объектов:
rm(list = ls())

cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}


clse = function(reg) { 
  # index(reg, "id") returns the id or entity variable vector 
  G = length(unique(index(reg,"id")))
  N = length(index(reg,"id"))
  dfa = (G/(G - 1))   # note Bluhm multiplies this by finite-sample df adjustment
  rob = sqrt(diag(dfa*vcovHC(reg, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

df <- read_excel("data1011.xlsx", sheet = "data")

head(df)


# Типо графики
ggplot(df, aes(x = t, y = vrp_per_capita, color = as.factor(i), group = i)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none")

df <- df %>%
  group_by(i) %>%
  mutate(
    years_since_2012 = t - 2012,
    log_annual_growth = log(vrp_per_capita / vrp_per_capita[t == 2012]) / years_since_2012,
    annual_growth = (vrp_per_capita / vrp_per_capita[t == 2012]) #/ years_since_2012
  ) %>%
  ungroup()

ggplot(df, aes(x = t, y = annual_growth, color = as.factor(i), group = i)) +
  geom_line() +
  # labs(title = "Average Annual Growth Rate of VRP per Capita Over Time by Region",
  #      x = "Year",
  #      y = "Log Average Annual Growth Rate (relative to 2012)") +
  theme_minimal() +
  theme(legend.position = "none") 

ggplot(df, aes(x = t, y = log_annual_growth, color = as.factor(i), group = i)) +
  geom_line() +
  # labs(title = "Average Annual Growth Rate of VRP per Capita Over Time by Region",
  #      x = "Year",
  #      y = "Log Average Annual Growth Rate (relative to 2012)") +
  theme_minimal() +
  theme(legend.position = "none") 


set.seed(123)
selected_regions <- sample(unique(df$i), 3)
filtered_data <- df %>% filter(i %in% selected_regions)
head(filtered_data)

### Безусловная конверегенция
df$t <- as.numeric(as.character(df$t))

min_t <- min(df$t)
y_min <- df %>%
  filter(t == min_t) %>%
  select(vrp_per_capita)  %>%
  pull()

max_t <- max(df$t)
y_max <- df %>%
  filter(t == max_t) %>%
  select(vrp_per_capita) %>%
  pull()

T <- max_t - min_t
y <- log(y_max / y_min) / T
x <- log(y_min)

df_unconditional <- data.frame(x = x, y = y)

model_0 <- lm(y ~ x, data = df_unconditional)
summary(model_0)
summary(model_0, vcov = function(x) vcovHC(x, cluster = "group", type = "HC1"))

b_hat <- model_0$coefficients["x"]
b_hat
beta_hat <- -log(1 + b_hat*T) / T
beta_hat
beta_hat*100

### Условная конверегенция
df <- read_excel("data1011.xlsx", sheet = "data")

# собираю данные для прироста 2021-2012
min_t <- min(df$t)
max_t <- max(df$t)

df_filtered <- df %>%
  group_by(i) %>%
  filter(any(t == min_t) & any(t == max_t)) %>%
  mutate(
    min_t = min_t, 
    T = max_t - min_t,
    vrp_start = vrp_per_capita[t == min_t],
    vrp_end = vrp_per_capita[t == max_t],
    annual_growth = vrp_end / vrp_start,
    log_annual_growth = (1 / T) * log(annual_growth)
  ) %>%
  ungroup() %>%
  distinct(i, .keep_all = TRUE) %>%  
  select(i, min_t, vrp_start, annual_growth, log_annual_growth) %>%  rename(t = min_t)

df_filtered_controls <- df %>%
  filter((t == min_t)) %>% 
  select(-vrp_per_capita, -population, -innovation_expenditure_total, -average_consumer_expenditure_per_capita)

df_joined <- df_filtered %>%
  inner_join(df_filtered_controls, by = c("i", "t"))

colnames(df_joined)
df_joined


# собираю данные для панели 
df_filtered2 <- df %>%
  arrange(i, t) %>%  
  group_by(i) %>%
  mutate(
    prev_year_vrp = lag(vrp_per_capita), 
    year_over_year_growth = (vrp_per_capita / prev_year_vrp) - 1, 
    log_annual_growth = log(vrp_per_capita / prev_year_vrp) 
  ) %>%
  ungroup() %>%
  filter(!is.na(year_over_year_growth)) %>%  
  select(i, t, prev_year_vrp, year_over_year_growth, log_annual_growth)

df_filtered2$t_1 <- df_filtered2$t - 1 

df_filtered2_controls <- df %>%
  select(-vrp_per_capita, -population, -innovation_expenditure_total, -average_consumer_expenditure_per_capita)

df_joined2 <- df_filtered2 %>%
  inner_join(df_filtered2_controls, by = c("t_1" = "t", "i" = "i"))

pdata <- pdata.frame(df_joined2, index = c("i", "t"))
pdata

# data_for_cov <- df_joined %>%
#   select(internet_share, rnd_share, ifo_invest,
#          students_per_10000, railroad_density, paved_road_density, hospital_beds_per_10000,
#          housing_fund_total, infant_mortality_rate, innovation_expenditure_percent_goods_services,
#          innovative_goods_services_percent, life_expectancy_at_birth_total_population,
#          natural_population_growth_per_1000, pensioners_per_1000, physicians_per_10000,
#          transport_services_per_capita, urban_population_share, migration_growth_per_10000)
# 
# cov_matrix <- cov(data_for_cov, use = "complete.obs")
# print(cov_matrix)
# library(corrplot)
# cor_matrix <- cor(data_for_cov, use = "complete.obs")
# corrplot(cor_matrix, method = "color", type = "upper", 
#          tl.col = "black", tl.srt = 45, 
#          title = "Correlation Matrix", mar = c(0, 0, 1, 0))



model <- lm(log_annual_growth ~ vrp_start + internet_share + rnd_share + ifo_invest +
              students_per_10000 + railroad_density  + hospital_beds_per_10000 +
              housing_fund_total + infant_mortality_rate +  life_expectancy_at_birth_total_population +
              + urban_population_share, 
            data = df_joined)
summary(model, vcov = function(x) vcovHC(x, cluster = "group", type = "HC1"))
# natural_population_growth_per_1000 + pensioners_per_1000 + physicians_per_10000 + transport_services_per_capita + migration_growth_per_10000
# innovation_expenditure_percent_goods_services + innovative_goods_services_percent + paved_road_density + railroad_density

panel_data <- pdata.frame(df_joined2, index = c("i", "t"))
panel_data

formula <- log_annual_growth ~ prev_year_vrp + internet_share + rnd_share + ifo_invest +
  students_per_10000 + hospital_beds_per_10000 +
  housing_fund_total + infant_mortality_rate + life_expectancy_at_birth_total_population +
  urban_population_share

pooled_ols <- plm(formula, data = panel_data, model = "pooling")
summary(pooled_ols)

fixed_effects <- plm(formula, data = panel_data, model = "within")
summary(fixed_effects)

random_effects <- plm(formula, data = panel_data, model = "random")
summary(random_effects)


pool_se <- sqrt(diag(vcovHC(pooled_ols, type = "HC1", cluster = "group")))
fe_se <- sqrt(diag(vcovHC(fixed_effects, type = "HC1", cluster = "group")))
re_se <- sqrt(diag(vcovHC(random_effects, type = "HC1", cluster = "group")))
stargazer(pooled_ols, fixed_effects, random_effects, type = "text",
          se = list(pool_se, fe_se, re_se),
          df=FALSE, digits=4)


