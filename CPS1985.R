library(AER)
library(plm)
library(lmtest)
library(dplyr)
library(tidyr)
library(stargazer)
library(webshot2)
library(htmltools)
library(webshot2)
library(ggplot2)
library(tidyverse)
data(package = "AER")
data("CPS1985")
df <- CPS1985
head(CPS1985)
?CPS1985
dim(df)

#----
df <- df %>% 
  mutate(id = seq(1,534, 1),
         year = 1985)
logit <- glm(married ~ wage + age + ethnicity + education + gender, 
             data = df,
             family = binomial(link = "logit"))
summary(df$education)

predictions <- predict(logit, 
                       newdata = data.frame("wage" = c(mean(df$wage), mean(df$wage)),
                                            "age" = c(mean(df$age),mean(df$age)),
                                            "ethnicity" = c("cauc", "cauc"),
                                            "education" = c(mean(df$education),18),
                                            "gender" = c("male", "female")),
                                            type = "response")

edu_predic <- diff(predictions)

#----
#plot
ggplot(df, aes(x = education)) +
  geom_histogram(fill = "#69b3a2",binwidth = 1) +
  labs(x = "years of education", y = "count") +
  theme(plot.title = element_text(size = 9, face = "bold"))

max(df$education)
