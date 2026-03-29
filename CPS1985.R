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
?CPS1985
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
                       newdata = data.frame("wage" = c(1, 44.5),
                                            "age" = c(mean(df$age),mean(df$age)),
                                            "ethnicity" = c("cauc", "cauc"),
                                            "education" = c(mean(df$education),mean(df$education)),
                                            "gender" = c("male", "female")),
                                            type = "response")

wage_list <- list(
  c(mean(df$wage), mean(df$wage)),
  c(mean(df$wage), 10),
  c(mean(df$wage), 15),
  c(mean(df$wage), 20),
  c(mean(df$wage), 25),
  c(mean(df$wage), 30),
  c(mean(df$wage), 40),
  c(mean(df$wage), max(df$wage))
)

# Pre‑allocate a numeric vector
effects <- numeric(length(wage_list))

# Loop over each wage pair
for (i in seq_along(wage_list)) {
  w <- wage_list[[i]]
  
  newdata <- data.frame(
    wage       = w,
    age        = rep(mean(df$age), 2),
    ethnicity  = rep("cauc", 2),
    education  = rep(mean(df$education), 2),
    gender     = rep("male", 2)
  )
  
  preds <- predict(logit, newdata = newdata, type = "response")
  effects[i] <- 100 * (preds[2] - preds[1])
}

# Turn into a data frame for plotting
results_df <- data.frame(
  wage_start = round(sapply(wage_list, `[`, 1),2),
  wage_end   = round(sapply(wage_list, `[`, 2),2),
  effect     = paste0(round(effects,2), "%"))





edu_predic <- diff(predictions)
diff(predictions)
summary(df$wage)
#----
#plot
ggplot(df, aes(x = education)) +
  geom_histogram(fill = "#69b3a2",binwidth = 1) +
  labs(x = "years of education", y = "count") +
  theme(plot.title = element_text(size = 9, face = "bold"))

max(df$education)

##ethnicity dataframe
eth <- df %>% 
  count(ethnicity, name = "count") %>% 
  mutate(ethnicity = recode(ethnicity, cauc = "Caucasian", hispanic = "Hispanic"))

