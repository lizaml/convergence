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
library(tidyverse)

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

df <- read_csv("data1302.csv")

head(df)


# Типо графики
ggplot(df, aes(x = t, y = vrp_per_capita, color = as.factor(i), group = i)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none")

df <- df %>%
  group_by(i) %>%
  mutate(
    years_since_2012 = t - 2004,
    log_annual_growth = log(vrp_per_capita / vrp_per_capita[t == 2004]) / years_since_2012,
    annual_growth = (vrp_per_capita / vrp_per_capita[t == 2004]) #/ years_since_2012
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

