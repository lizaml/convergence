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

library(data.table)
d2<-dcast(melt(setDT(d2), id.var=c("indicator","i"), 
               variable.name="t"), 
          i+t~indicator, value.var='value')
attach(d2)

library(tidyverse)
library(nycflights13)

df = merge(x = d2, y = d1, by = c("i", 't'))
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

df <- read_excel("df.xlsx")

#df$vrp_per_capita <- df$vrp*1000000/df$population
#df[, vrp_rolling_avg := frollmean(vrp_per_capita, n = 3, align = "center"), by = i]
#df<- df[!is.na(vrp_rolling_avg)]
head(df)

ggplot(data_summary, aes(x = t, y = total_vrp_per_capita, group = 1)) +
  #geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  theme_minimal()

set.seed(123)
selected_regions <- sample(unique(df$i), 3)
filtered_data <- df[df$i %in% selected_regions]
head(filtered_data)


### Безусловная конверегенция
df$t <- as.numeric(as.character(df$t))

min_t <- min(df$t)
y_min <- df %>%
  filter(t == min_t) %>%
  select(vrp_rolling_avg)  %>%
  pull()

max_t <- max(df$t)
y_max <- df %>%
  filter(t == max_t) %>%
  select(vrp_rolling_avg) %>%
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

