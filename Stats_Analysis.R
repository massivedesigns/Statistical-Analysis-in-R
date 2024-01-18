library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library('corrplot')
library(lubridate)
mlm <- read_csv("/Users/mac/Desktop/STATS_COURSEWORK/melanoma.csv")
# view(mlm)
head(mlm)

mlm <- mlm |>  select(time:ulcer)

# view(mlm)


# -- data preprocessing and feature engineering --------------------
mlm1 <- mlm
mlm2 <- mlm
summary(mlm1)

mlm
mlm$sex = factor(mlm$sex)
mlm$ulcer = factor(mlm$ulcer)
mlm$status = factor(mlm$status)

summary(mlm)

mlm2
mlm2$sex = factor(mlm2$sex, levels = c(0, 1), labels = c("Female", "Male"))
mlm2$ulcer = factor(mlm2$ulcer, levels = c(0, 1), labels = c("Absent", "Present"))
mlm2$status = factor(mlm2$status, levels = c(1, 2, 3), labels = c("Melanoma_Death", "Alive", "Other_Death"))
 
summary(mlm2)

mlm1$months = round(mlm1$time/30)
mlm1
# -------------------------------------------------------------------------------
# correlation_matrix <- cor(mlm)
# corrplot(correlation_matrix, method = "color", addCoef.col = "black", number.cex = 0.9)

#df$time_in_years <- floor(df$time / 365)


view(mlm)
 # summary statistics - mean, vairance, standard deviation, five number summary
 # graphical summary - histograms, boxplots, barplots dot charts, pie charts
# taking time first 
mean(mlm$time)
mean(mlm$time, na.rm = T)

# median 
median(mlm$time, na.rm =T)

# variance
var(mlm$time)

# standard deviation
sd_time <- sqrt(var(mlm$time))
sd(mlm$time)
sd(mlm$age)
sd(mlm$year)
sd(mlm$thickness)

# standard deviation
sd_time <- sqrt(var(mlm2$time))
sd(mlm2$time)
sd(mlm2$age)
sd(mlm2$year)
sd(mlm2$thickness)

cv_age <- sd(mlm2$age) / mean(mlm2$age)
cv_age
cv_time <- sd(mlm2$time) / mean(mlm2$time)
cv_time
cv_thickness <- sd(mlm2$thickness) / mean(mlm2$thickness)
cv_thickness
cv_year <- sd(mlm2$year) / mean(mlm2$year)
cv_year
# median 
median(mlm$time)

# Calculate quartiles
q1 <- quantile(mlm$time, 0.25)
q3 <- quantile(mlm$time, 0.75)

# Calculate interquartile range
iqr_value <- q3 - q1

# Display the results
print(q1)
print(q3)
print(iqr_value)
#mode
most_frequent_value <- as.numeric(names(table(mlm$time))[which.max(table(mlm$time))])
most_frequent_value

get_mode <- function(x) {
  unique_values <- unique(x)
  frequencies <- table(x)
  mode_values <- unique_values[which.max(frequencies)]
  return(mode_values)
}

result <- get_mode(mlm$time)
print(result)

# five number summary 
summary(mlm$time)


df$sex = factor(mlm$sex)
df$year = factor(mlm$year)
df$ulcer = factor(mlm$ulcer)
df$status = factor(mlm$status)

summary(mlm)

mlm
summary(factor(df$year))

#. ---- Graphical summary ------

# hist(mlm$time, main = "Time histogram")
# 
# help(hist)
# 
# par(mfrow=c(2,2))
# 
# 
# hist(mlm$time)
# hist(mlm$status)
# hist(mlm$sex)
# hist(mlm$age)
# hist(mlm$year)
# hist(mlm$thickness)
# hist(mlm$ulcer)

boxplot(mlm$age)

boxplot(mlm$thickness ~ df$sex )

class(mlm$status)
barplot(mlm$status)

table(mlm$sex)
table(mlm$status)
table(mlm$ulcer)
table(mlm$year)
table(mlm$age)

ggplot(mlm, aes(x = status)) +
  geom_bar(position = "dodge") +
  labs(title = " status distribution") +
  theme_minimal()

ggplot(mlm, aes(x=ulcer)) +
  geom_bar(position = "dodge")+
  labs(title = "ulcer distribution") + 
  theme_minimal()

ggplot(mlm, aes(x=sex)) +
  geom_bar(position = "dodge")+
  labs(title = "sex tribution") + 
  theme_minimal()

ggplot(mlm, aes(x = year)) +
  geom_bar(position = "dodge") +
  labs(title = " year of operation ") +
  theme_minimal()
  
# Histogram of Tumour Thickness
ggplot(mlm, aes(x = thickness)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Tumour Thickness", x = "Thickness (mm)", y = "Frequency") +
  theme_minimal()
#ii --------------------------------------------------
par(mfrow=c(2,2))
library(gridExtra)
plt1 <- ggplot(mlm2, aes(x = thickness)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Tumour Thickness", x = "Thickness (mm)", y = "Frequency") +
  theme_minimal()

plt2 <- ggplot(mlm2, aes(x = time)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000)) +
  labs(title = "Distribution of Survival time by day", x = "Time (days)", y = "Frequency") +
  theme_minimal()

plt3 <- ggplot(mlm2, aes(x = age)) +
  geom_histogram(binwidth = 7, fill = "skyblue", color = "black") +
  labs(title = "Distribution of age", x = "Age ", y = "Frequency") +
  theme_minimal()

plt4 <- ggplot(mlm2, aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of surgery year", x = "Year", y = "Frequency") +
  theme_minimal()

grid.arrange(plt1, plt2, plt3, plt4, ncol = 2)

ggplot(mlm2, aes(x = factor(year))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of surgery year", x = "Year", y = "Frequency") +
  theme_minimal()
#-------------------------------------------------------
# # Convert 'ulcer' to a factor if it's not already
# df$ulcer <- factor(df$ulcer, levels = c(0, 1), labels = c("Absent", "Present"))


# ----  Bar plot for Ulceration with modified labels -------------------------------
pt1 <- ggplot(mlm, aes(x = ulcer, fill = ulcer)) +
  geom_bar(position = "dodge", width = 0.9) +
  labs(title = "Ulceration Distribution", x = "Ulceration", y = "Count") +
  scale_x_discrete(labels = c("0" = "Absent", "1" = "Present")) +
  scale_fill_manual(values = c("0" = "limegreen", "1" = "red"), name = "Ulcer Indicator", labels = c("0" = "Absent", "1" = "Present")) +
  theme_minimal()

# Bar plot for Status with modified labels ------------------------------
pt2 <- ggplot(mlm, aes(x = status, fill = status)) +
  geom_bar(position = "dodge", width = 0.9) +
  labs(title = "Patient Status Distribution at end of study", x = "Status", y = "Count") +
  scale_x_discrete()  +
  scale_fill_discrete(name = "Patient Status", labels = c("1" = "melanoma_Death", "2" = "Alive", "3" = "Other_death"))
 
# -- OR
 
 ggplot(mlm, aes(x = status, fill = status)) +
   geom_bar(position = "dodge", width = 0.9) +
   labs(title = "Patient Status at end of study", x = "Status", y = "Count") +
   scale_x_discrete(labels = c("1" = "melanoma_Death", "2" = "Alive", "3" = "Other_death")) +
   scale_fill_discrete(name = "Patient Status", labels = c("1" = "melanoma_Death", "2" = "Alive", "3" = "Other_death"))
 
 # Bar plot for Sex with modified labels ------------------------------
 ggplot(mlm, aes(x = sex, fill = sex)) +
   geom_bar(position = "dodge", width = 0.9) +
   labs(title = "Gender distribution of Patients", x = "Sex", y = "Count") +
   scale_x_discrete( labels = c("0" = "Female", "1" = "Male")) +
   scale_fill_discrete(name = "Gender", labels = c("0" = "Female", "1" = "Male"))

 
pt3 <-  ggplot(mlm, aes(x = factor(sex), fill = factor(sex))) +
   geom_bar(position = "dodge", width = 0.9) +
   labs(title = "Gender distribution of Patients", x = "Sex", y = "Count") +
   scale_x_discrete(labels = c("0" = "Female", "1" = "Male")) +
   scale_fill_manual(values = c("0" = "purple", "1" = "forestgreen"), name = "Gender", labels = c("0" = "Female", "1" = "Male")) +
   theme_minimal()
 
grid.arrange(pt1, pt2, pt3, ncol = 2)

grid.arrange(pt1, pt2, pt3, ncol = 1)
 
 stem(df$status)
 # Create a stem-and-leaf plot using ggplot2
 ggplot(mlm, aes(thickness)) +
   geom_dotplot(binwidth = 10, dotsize = 0.5) +
   labs(title = "Stem-and-Leaf Plot", x = "thickness")   
 
 # --- box plots here -----------
 
 ggplot(mlm2, aes(y = age)) +
   geom_boxplot() + labs(title = "Age Distribution ", y= "Age", y = " ") +
   theme_minimal()+ scale_y_continuous()
 

 
 a <- ggplot(mlm2, aes(x = "", y = age)) +
   geom_boxplot(color = "darkblue", fill = "lightblue", alpha = 0.7) +
   labs(title = "Age Distribution", y = "Age") +
   theme_minimal() +
   theme(
     axis.text.x = element_blank(),
     axis.line = element_line(color = "black")
   ) +
   scale_y_continuous(labels = scales::comma) +
   coord_cartesian(ylim = c(0, 100))  # Adjust the y-axis limits as needed
 

 ggplot(mlm2, aes(y = time)) +
   geom_boxplot() + labs(title = "Boxplot of survival time ", y= "time (days)", y = " ") +
   theme_minimal()+ scale_y_continuous()
 
 
 b <- ggplot(mlm2, aes(x = "", y = time)) +
   geom_boxplot(color = "darkblue", fill = "lightblue", alpha = 0.7) +
   labs(title = "Boxplot of survival time", y = "time (days)") +
   theme_minimal() + 
   theme(
     axis.text.x = element_blank(),
     axis.line = element_line(color = "black")
   ) 
 
 
 c <- ggplot(mlm2, aes(x = "", y = thickness)) +
   geom_boxplot(color = "darkblue", fill = "lightblue", alpha = 0.7) +
   labs(title = "Boxplot of Tumour thickness ", y = "thickness (mm)") +
   theme_minimal() +
   theme(
     axis.text.x = element_blank(),
     axis.line = element_line(color = "black")
   ) 
  
 d <- ggplot(mlm2, aes(x = "", y = year)) +
   geom_boxplot(color = "darkblue", fill = "lightblue", alpha = 0.7) +
   labs(title = "Boxplot of surgery year", y = "year") +
   theme_minimal() +
   theme(
     axis.text.x = element_blank(),
     axis.line = element_line(color = "black")
   ) 
 grid.arrange(a, b, c, d, ncol = 2)
 
 #------- multiple box plots to use for comparing the diustributions of data that has been classified into several category ---------
 
 mp1 <- ggplot(mlm2, aes(x = status, y = thickness)) +
   geom_boxplot(color = "darkblue", fill = "lightblue", alpha = 0.7) +
   labs(title = "A) Tumour thickness vs patient status", y = "thickness (mm)") +
   theme_minimal() 
# with this boxplot we can see that people that die from the malignant melanoma tends to have a high tumour thickness than those that survived, 
 # it is also important to note that there exist extreme values of thickness in both those alive and those who died from the melanoma
 
 mp2 <- ggplot(mlm2, aes(x = sex, y = thickness)) +
   geom_boxplot(color = "darkblue", fill = "lightblue", alpha = 0.7) +
   labs(title = "B) Tumour thickness  vs Gender ", y = "thickness (mm)") +
   theme_minimal() 
 # females tend to have a smaller tumour thickness than male, it is also imperative that we note the large amount of extreme values on the female 
 # group , which shows that the largest size of tumour thickness we have in the whole dataset comes from a female and this female
 # seems to have died from this disease as our previous plot shows 
 
 
mp3 <- ggplot(mlm2, aes(x = sex, y = time)) +
   geom_boxplot(color = "darkblue", fill = "lightblue", alpha = 0.7) +
   labs(title = "C) survival time  vs Gender ", y = "time (days)") +
   theme_minimal()
 # this shows us that females have a high surviavl time than men, this could have some correlation that female has a smaller tumour thickness than men 
 # so the smaller the tumour thickness the high chances of long survival time
 # also note worthy that our highest survival time seems to be a female in this study 
 
mp4 <- ggplot(mlm2, aes(x = ulcer, y = time)) +
   geom_boxplot(color = "darkblue", fill = "lightblue", alpha = 0.7) +
   labs(title = "D) survival time vs ulcer status ", y = "time (days)") +
   theme_minimal()
 
 # ulcerated tumour have a lower survival time  while no prsence of ulcer in the tumour has a high chance of surviving longer
 # the longest survival time is also found on the group with no ulcer , this backs this theory up 
 
mp5 <- ggplot(mlm2, aes(x = ulcer, y = thickness)) +
   geom_boxplot(color = "darkblue", fill = "lightblue", alpha = 0.7) +
   labs(title = "E) tumour thickness vs ulcer status", y = "thickness (mm)") +
   theme_minimal()
# presence of ulcer also causes a larger tumour thickness than wheen there is absence of ulcer , we also take note of the outliers in this 
# groups as they can make effects on our descriptive statistic.
 
mp6 <- ggplot(mlm2, aes(x = ulcer, y = age)) +
   geom_boxplot(color = "darkblue", fill = "lightblue", alpha = 0.7) +
   labs(title = "F) ulcer status vs Age", y = "Age") +
   theme_minimal()
# age dont have much effect on whether tumour will be ulcerated, but a slight presence of ulcerated tumour in older people in this sample

grid.arrange(mp1, mp2, mp3, mp4, mp5, mp6, ncol = 2)

 ggplot(mlm2, aes(x = ulcer, y = year)) +
   geom_boxplot(color = "darkblue", fill = "lightblue", alpha = 0.7) +
   labs(title = "ulcerated tumour  vs year", y = "year") +
   theme_minimal() 
 # noticed nothing
 
 # Scatter plot: Year vs. Thickness
 ggplot(mlm2, aes(x = year, y = thickness)) +
   geom_point() +
   labs(title = "Scatter Plot: Year vs. Thickness", x = "Year", y = "Thickness (mm)")
 
# Scatter plot: Year vs. Time
 ggplot(mlm2, aes(x = year, y = time)) +
   geom_point() +
   labs(title = "Scatter Plot: Year vs. Time", x = "Year", y = "Time (days)")
# increase in year decrease in survival time , means nothing because the year the study ended was the last time
 # survival time was calculated so it doesnt mean much as its expected who did the surgery on first year has a higher chance of long survival
 # time by the study 
 
 # ------ REGRESSION ANALYSIS AND CORRELATION COMPUTATIONS 
# TIME - THICHKNESS
 ggplot(data = mlm2, mapping = aes(x = thickness, y = time))+
   geom_point(shape = "x", color = "black", size = 3)+
   labs(title = "Tumour Thickness with Survival Time, r = -0.2354087",x = "Thickness (mm)",y = "Time (days)")

 
 # lets attach the dataset that helps us compute 
 attach(mlm2)
 cor(thickness, time, method="pearson")

 # ORDINARY LEAST SQUARES REGRESSION
 # here thickness is our x in graph and .hb our y dependent
 ols_model = lm(formula = mlm2$time ~ mlm2$thickness) 
 ols_model <- lm(formula = mlm2$time ~ mlm2$thickness)
 ols_model
 summary(ols_model)
 
 abline(ols_model, col = "green", lwd = 2)

 ggplot(data = mlm2, mapping = aes(x = thickness, y = time)) +
   geom_point(shape = 16, color = "black", size = 2) +
   labs(title = "Tumour Thickness with Survival Time, r = -0.2354087", x = "Thickness (mm)", y = "Time (days)") +
   geom_abline(intercept = coef(ols_model)[1], slope = coef(ols_model)[2], col = "blue", lwd = 1)
 
 coef(ols_model)[1]
 coef(ols_model)[2]
 
 #making predictions 
 
 ols_model1 <- lm(formula = time ~ thickness, data = mlm2)
 summary(ols_model1)
 predict(ols_model1, newdata = data.frame(thickness = 15))
 
# # Create a scatter plot
# plot(mlm2$thickness, mlm2$time)
# # Add a regression line using abline
# abline(ols_model, col = "green", lwd = 2) 

 # 2 #####
 
 # TIME - AGE

 # attach(mlm2)
 # detach(mlm2)
 cor(age, time, method="pearson")
 
 length(age)
 length(time)
 age
 mlm2$age
 # ORDINARY LEAST SQUARES REGRESSION
 # here thickness is our x in graph and hb our y dependent
 ols_model2 = lm(formula = mlm2$time ~ mlm2$age) 
 ols_model2
 summary(ols_model2)
 
 #making predictions 
 
 ols_model24 <- lm(formula = time ~ age, data = mlm2)
 summary(ols_model24)
 predict(ols_model24, newdata = data.frame(age = 25))
 
#  new_data <- data.frame(age = 25)
# predict(ols_model2, newdata = list(age =25))
 
 
 ggplot(data = mlm2, mapping = aes(x = age, y = time))+
   geom_point(shape = "x", color = "black", size = 3)+
   labs(title = "Age with Time, r = -0.3015179 ",x = "AGE",y = "TIME")
 
 # model regline
 ggplot(data = mlm2, mapping = aes(x = age, y = time)) +
   geom_point(shape = 16, color = "black", size = 2) +
   labs(title = "Age with Time, r = -0.3015179 ", x = "Age", y = "Time (days)") +
   geom_abline(intercept = coef(ols_model2)[1], slope = coef(ols_model2)[2], col = "blue", lwd = 1)
 
 # 3 ####
 
 # THICKNESS - AGE
 
 # attach(mlm2)
 cor(age, thickness, method="pearson")
 
 # ORDINARY LEAST SQUARES REGRESSION
 # here thickness is our x in graph and hb our y dependent
 ols_model3 = lm(formula = mlm2$thickness~ mlm2$age) 
 ols_model3
 summary(ols_model3)
 
 
 ggplot(data = mlm2, mapping = aes(x = age, y = thickness))+
   geom_point(shape = "x", color = "black", size = 3)+
   labs(title = "Age with Tumour Thickness, r = 0.2124798 ",x = "Age",y = "THICKNESS (mm)")
 
 #model regline
 ggplot(data = mlm2, mapping = aes(x = age, y = thickness)) +
   geom_point(shape = 16, color = "black", size = 2) +
   labs(title = "Age with Tumour Thickness, r = 0.2124798 ", x = "Age", y = "Thickness (mm)") +
   geom_abline(intercept = coef(ols_model3)[1], slope = coef(ols_model3)[2], col = "blue", lwd = 1)
 
 #making predictions 
 
 ols_model33 <- lm(formula = thickness ~ age, data = mlm2)
 summary(ols_model33)
 predict(ols_model33, newdata = data.frame(age = 25))
# ---------------------------------------------------------------------------------------------------- 
 mlm2$time 
 mlm2$thickness
 
 # Extract residuals
 residuals <- residuals(ols_model)
 
 # Histogram of residuals
 hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")
 
 # Q-Q plot of residuals
 qqnorm(residuals)
 qqline(residuals)
 
 
#------------ T-TESTING -------------------------------------------------------
 
#Appropriate two sample significance tests for the variables in part (iii) grouped by gender 
 # THICKNESS, TIME, AGE 
 # 5A
 # THICKNES grouped by gender 
qplot(x = sex , y = thickness,
       geom = "boxplot", data = mlm2,
       xlab = "Gender",
       ylab = "Thickness (mm)",
       fill = I("pink"))
 

male <- filter(mlm2, sex == "Male")
female <- filter(mlm2, sex == "Female")
 
mean(male$thickness)
mean(female$thickness)

# further Eda

mlm2 |> 
  group_by(sex) |> 
  summarize(num.obs = n(),
            mean_thickness = round( mean(thickness), 2),
            sd_thickness = round( sd(thickness), 2),
            se_thickness = round( sd(thickness) / sqrt(num.obs), 2))


# Now to discover how statistically significant this difference is 

# -- Null hypothesis Ho: the mean thickness are the same i.e u1 = u2 .
# -- ALternative  hypothesis H1: the mean thickness are different i.e u1 =/ u2 

thickness_t_test <- t.test(thickness ~ sex, data = mlm2)
thickness_t_test 

# The p-value is less than the typical significance level of 0.05, so you would 
# reject the null hypothesis.
# Based on the results of the t-test, you can conclude that there is a statistically
# significant difference in the mean thickness between Male and Female. The negative t-value 
# and the negative lower bound of the confidence interval suggest that the mean thickness in 
# the Female group is significantly lower than that in the Male group.

#5B
# TIME grouped by gender 
qplot(x = sex , y = time,
      geom = "boxplot", data = mlm2,
      xlab = "Gender",
      ylab = "Time (days)",
      fill = I("pink"))
mlm2 |> 
  group_by(sex) |> 
  summarize(num.obs = n(),
            mean_time = round( mean(time), 0),
            sd_time = round( sd(time), 0),
            se_time = round( sd(time) / sqrt(num.obs), 0))
# Now to discover how statistically significant this difference is 

# -- Null hypothesis Ho: the mean time are the same i.e u1 = u2 .
# -- ALternative  hypothesis H1: the mean time are different i.e u1 =/ u2 

time_t_test <- t.test(time ~ sex, data = mlm2)
time_t_test 
# The p-value is less than the typical significance level of 0.05, so you would 
# reject the null hypothesis.
# Based on the results of the t-test, you can conclude that there is a statistically 
# significant difference in the mean time between Male and Female. The positive t-value 
# and the positive lower bound of the confidence interval suggest that the mean time in the 
# Female group is significantly higher than that in the Male group

#5C
# AGE grouped by gender 
qplot(x = sex , y = age,
      geom = "boxplot", data = mlm2,
      xlab = "Gender",
      ylab = "Age",
      fill = I("pink"))
mlm2 |> 
  group_by(sex) |> 
  summarize(num.obs = n(),
            mean_age = round( mean(age), 0),
            sd_age = round( sd(age), 0),
            se_age = round( sd(age) / sqrt(num.obs), 0))

# Now to discover how statistically significant this difference is 

# -- Null hypothesis Ho: the mean age are the same i.e u1 = u2 .
# -- ALternative  hypothesis H1: the mean age are different i.e u1 =/ u2 

age_t_test <- t.test(age ~ sex, data = mlm2)
age_t_test 

# The p-value is greater than the typical significance level of 0.05, 
# so you would fail to reject the null hypothesis
# The confidence interval for the difference in means includes 0, 
# indicating that the difference is not statistically significant.
# Therefore, there is no strong evidence to suggest a significant difference 
# in mean age between Male and Female.
# 
# Based on the results of the t-test, you cannot conclude that there is a statistically 
# significant difference in mean age between Male and Female. The p-value is relatively
# high, and the confidence interval includes 0, suggesting that any observed difference 
# in mean age could be due to random chance.

#-- END  OF T test -------------------------------------------


# 6 QQ PLOT ----------------Normality test
# sample 
p_bwt <- ggplot(data = birthwt, aes(sample = bwt))
p_bwt + stat_qq() + stat_qq_line()
p_bwt + stat_qq() + stat_qq_line() + facet_grid(. ~ smoke)

 # THICKNES grouped by gender 
p_thickness <- ggplot(data = mlm2, aes(sample = thickness))
p_thickness + stat_qq() + stat_qq_line()
p_thickness + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)+
  xlab("Theoretical Quantiles") +  
  ylab("Thickness Sample Quantiles")  

# TIME grouped by gender 
p_time <- ggplot(data = mlm2, aes(sample = time))
p_time + stat_qq() + stat_qq_line()
p_time + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)+
  xlab("Theoretical Quantiles") +  
  ylab("Time Sample Quantiles") 

# AGE grouped by gender
p_age <- ggplot(data = mlm2, aes(sample = age))
p_age + stat_qq() + stat_qq_line()
p_age + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)+
  xlab("Theoritical Quantitles") + 
  ylab("Age Sample Quantiles") 

citation()
