#money supply A2.2.1 - Components of M4 https://www.bankofengland.co.uk/statistics/Tables
library(plm)
library(AER)
library(quantmod)
library(dynlm)
library(ARDL)
library(vars)
library(tseries)
library(nlme)
library(stargazer)
library(lmtest)
library(dynlm)
library(dplyr)
library(vars)
library(sandwich)
library(tidyr)
library(stargazer)
library(ggplot2)
library(lubridate)
library(quantmod)
library(tidyverse)
library(stringr)
library(scales)
library(ggthemes)
##Retail deposits and cash, M4 - excluding wholsesale 

df <- read.csv("data/Bank of England  Database (3).csv") %>% 
  rename(M4 = 2)%>%
  mutate(Date = as.Date(Date, format = "%d %b %y"),
         Date = floor_date(Date, "month"),
         log_annual_growth = 1200 * (log(M4/lag(M4))))
  
#growth
df_growth <- df %>% 
  arrange(Date) %>% 
  mutate(
    basic = (M4 - lag(M4)) / lag(M4),
    mom = 100 * (M4 - lag(M4)) / lag(M4),
    growth_3m_ann = ((M4 / lag(M4, 3))^4 - 1) * 100,
    growth_6m_ann = ((M4 / lag(M4, 6))^2 - 1) * 100,
    money_growth_yoy = 100 * (M4 - lag(M4, 12)) / lag(M4, 12)
  )

#Inflation
inf <- read.csv("data/series-290326.csv") %>% 
  slice(193:638) %>% 
  rename(inflation = 2,
         Date = Title) %>% 
  mutate(
    # 1. Parse "1989 JAN" → Date
    Date = as.Date(paste0(Date, " 01"), format = "%Y %b %d"),
    
    # 2. Strip day → first of month
    Date = floor_date(Date, "month")
  ) %>% 
  left_join(df_growth, by = "Date") %>% 
  mutate(inflation = as.numeric(inflation))

inf <- inf %>% 
  dplyr::select(c("inflation", "Date", "money_growth_yoy")) %>% 
  na.omit()

#----
#plots

ggplot(df_growth, aes(x = Date, y = money_growth_yoy)) + 
  geom_line(color = "yellow", linewidth = 0.5) +
  labs(x = "Year", x = "Growth") +
  scale_y_continuous(labels = comma) +
  ggtitle("% M4 growth - Year on year") +
  theme(plot.title = element_text(size = 12 , face = "bold", colour = "white"))+
  theme(panel.background = element_rect("black")) +
  theme(plot.background = element_rect("black"))


ggplot(data = inf, aes(x = Date)) +
  geom_line(aes(y = inflation, color = "inflation"), linewidth = 0.8) +
  geom_line(aes(y = money_growth_yoy, color = "money_growth_yoy"), linewidth = 0.8) +
  scale_color_manual(
    name = "Legend",
    values = c("inflation" = "red", "money_growth_yoy" = "blue"),
    labels = c("Inflation", "Money growth")
  ) +
  labs(x = "Date", y = "Year") +
  ggtitle("Money Supply and Inflation 1989 - 2026") +
  theme(axis.title.x = element_text(color = "white")) +
  theme(axis.title.y = element_text(color = "white")) +
  theme(plot.title = element_text(color = "white", face = "bold", size = 9)) +
  theme(plot.background = element_rect("black")) +
  theme(panel.background = element_rect("black"))


#----
#timeseries

m4 <- ts(inf$money_growth_yoy, start = c(1989, 1), frequency = 12)
inflationn <- ts(inf$inflation, start = c(1989, 1), frequency = 12)

##optimal lag check
max_lag <- 12

results <- data.frame(lag = 1:max_lag, AIC = NA, BIC = NA)

for (p in 1:max_lag) {
  model <- dynlm(inflationn ~ L(inflationn, 1:3) + L(m4, 1:p))
  results$AIC[p] <- AIC(model)
  results$BIC[p] <- BIC(model)
}

results

#regs

reg1 <- dynlm(inflationn ~  m4 + L(m4,0:6) + L(inflationn, 1:6))
coeftest(reg1, vcov. = vcovHAC)

## tests
#adf test for stationarity (if p <0.05, then it's stationary which is good)
adf.test(m4)
adf.test(inflationn)

#bg test for serial correlation. If p>0.05 then good. 
bgtest(reg1, order = 6)

#breusch pagan test for heteroskedasticity - i.e. if variance of residuals is constant. If p>0.05, then it's homoskedastic which is good.
bptest(reg1) #p<0.05 but HAC standard errors corrects for this. 
plot(residuals(reg1), type = "l")

##ARDL model
ardl_model <- ardl(inflation ~ money_growth_yoy, data = inf, order = 5)
summary(ardl_model)

##
#VAR
var_data <- cbind(inflationn = inf$inflation, 
                  m4 = inf$money_growth_yoy)

var_data <- ts(var_data, start = start(inf$inflation), frequency = 12)
lagselect <- VARselect(var_data, lag.max = 12, type = "const")
lagselect
var_model <- VAR(var_data, p = lagselect$selection["SC(n)"], type = "const")
summary(var_model)


##impulse response
irf_model <- irf(var_model, impulse = "m4", response = "inflationn",
                 n.ahead = 24, boot = TRUE)
plot(irf_model)


