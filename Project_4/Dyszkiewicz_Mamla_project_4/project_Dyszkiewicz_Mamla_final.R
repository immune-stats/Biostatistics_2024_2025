# Project 4 - Survival Analysis 
# Authors: Weronika Dyszkiewicz, Katarzyna Mamla

# Load packages
library(survival)
library(survminer)
library(ggplot2)

data<-read.csv('data_sydo_etal.csv')
data<-na.omit(data)
data$female <- as.factor(data$female)
data$Finished.test <- as.numeric(data$Finished.test)


# 1. Perform a descriptive analysis  (e.g., summary statistics/plots) for each variable.
# Construct plots that allow the visualization of the relationship between age and the maximum heart.
# Calculate appropriate measures of associations for the relationship between age and the maximum heart
# and provide an initial interpretation of the data. 

head(data)
str(data)
summary(data)
# 1 observation is missing in rest_hr

table(data$female)
table(data$Finished.test)

# a) Histogram of Age
hist(data$age, 
     col = "lightblue", 
     main = "Histogram of Age", 
     xlab = "Age (years)", 
     ylab = "Frequency", 
     breaks = 20,
     cex.main = 2,
     cex.lab = 1,
     font.lab = 2,
     cex.axis = 1.1,
     font.axis = 1.6)

# b) Histogram of finished tests
finished_test_counts <- table(data$Finished.test)
barplot(
  finished_test_counts,
  col = c("lightblue", "lightcoral"),
  main = "Histogram of finished tests", 
  xlab = "Is finished", 
  ylab = "Frequency",
  cex.lab = 1,
  font.lab = 2,
  cex.main = 2,
  cex.axis = 1.1,
  font.axis = 1.6
)

# c) Density Function of Peak Heart Rate and Density Function of Rest Heart Rate
par(mfrow = c(1, 2))
density_peak_hr <- density(data$peak_hr)

plot(
  density_peak_hr, 
  main = "Density Function of Peak Heart Rate",
  xlab = "Peak Heart Rate (bpm)", 
  ylab = "Density",
  col = "blue", 
  lwd = 2 ,
  cex.lab = 1.3,
  font.lab = 2,
  cex.main = 1.8,
  cex.axis = 1.1,
  font.axis = 1.6
)

density_rest_hr <- density(data$rest_hr, na.rm=TRUE)

plot(
  density_rest_hr, 
  main = "Density Function of Rest Heart Rate",
  xlab = "Rest Heart Rate (bpm)", 
  ylab = "Density",
  col = "blue", 
  lwd = 2,
  cex.lab = 1.3,
  font.lab = 2,
  cex.main = 1.8,
  cex.axis = 1.1,
  font.axis = 1.6
)

# d) Histogram of Gender
par(mfrow = c(1, 1))

gender_factor <- factor(data$female, levels = c(0, 1), labels = c("Male", "Female"))

gender_counts <- table(gender_factor)

barplot(
  gender_counts,
  col = c("lightblue", "pink"),
  main = "Histogram of Gender", 
  xlab = "Gender", 
  ylab = "Frequency",
  names.arg = c("Male", "Female"),
  cex.lab = 1.3,
  font.lab = 2,
  cex.main = 1.8,
  cex.axis = 1.1,
  font.axis = 1.6
)


# e) Boxplot of Peak Heart Rate by Gender
boxplot(peak_hr ~ female, 
        data = data, 
        col = c("lightblue", "pink"), 
        names = c("Male", "Female"), 
        main = "Boxplot of Peak Heart Rate by Gender",
        ylab = "Peak Heart Rate (bpm)",
        xlab = "Gender",
        cex.lab = 1.5,
        font.lab = 2,
        cex.main = 1.8,
        cex.axis = 1.4,
        font.axis = 1.6)

# Kolmogorov-Smirnov Test for Normality
male_peak_hr <- data$peak_hr[data$female == 0]
female_peak_hr <- data$peak_hr[data$female == 1]
ks_test_male <- ks.test(male_peak_hr, "pnorm", mean = mean(male_peak_hr), sd = sd(male_peak_hr))
ks_test_female <- ks.test(female_peak_hr, "pnorm", mean = mean(female_peak_hr), sd = sd(female_peak_hr))


print("Kolmogorov-Smirnov Test for Male Peak HR:")
print(ks_test_male)

print("Kolmogorov-Smirnov Test for Female Peak HR:")
print(ks_test_female)
#Both the male and female peak heart rate distributions are not normally distributed.

wilcox_test <- wilcox.test(peak_hr ~ female, data = data)
print("Mann-Whitney U Test Results:")
print(wilcox_test)

# f) Boxplot of Age by Gender
boxplot(age ~ female, 
        data = data, 
        col = c("lightblue", "pink"), 
        names = c("Male", "Female"), 
        main = "Boxplot of Age by Gender",
        ylab = "Age",
        xlab = "Gender",
        cex.lab = 1.5,
        font.lab = 2,
        cex.main = 1.8,
        cex.axis = 1.4,
        font.axis = 1.6)

# g) Age vs. Peak Heart Rate
plot(data$age, data$peak_hr, 
     main = "Age vs. Peak Heart Rate", 
     xlab = "Age (years)", 
     ylab = "Peak Heart Rate (bpm)", 
     pch = 16, col = "blue",
     cex.lab = 1.3,
     font.lab = 2,
     cex.main = 1.8,
     cex.axis = 1.5,
     font.axis = 1.7)
#regression line
abline(lm(peak_hr ~ age, data = data), col = "red", lwd = 2)

# h) Age vs. Peak Heart Rate by Gender
ggplot(data, aes(x = age, y = peak_hr, color = gender_factor, shape = gender_factor)) +
  geom_point(size = 3, alpha = 0.7) +  
  geom_smooth(method = "lm", aes(color = gender_factor), se = FALSE,  size = 1.2) +  
  labs(
    title = "Age vs. Peak Heart Rate",
    x = "Age (years)",
    y = "Peak Heart Rate (bpm)",
    color = "Gender",
    shape = "Gender"
  ) +
  scale_color_manual(values = c("blue", "#D75485")) +  
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.title = element_text(size = 16, face = "bold"),            
    axis.text = element_text(size = 14, face = "bold"),              
    legend.title = element_text(size = 16, face = "bold"),           
    legend.text = element_text(size = 14)                            
  )



#Appropriate  measures of associations for the relationship between age and the maximum heart

# Pearson correlation between Age and Peak Heart Rate
correlation <- cor(data$age, data$peak_hr, method = "pearson", use = "complete.obs")
cat("Pearson correlation coefficient:", correlation, "\n")
# corr= -0.54 so there is a moderate negative relationship between the two variables, 
# meaning that as one variable increases, the other tends to decrease, but not strongly. 
# as age increases, the maximum heart rate decreases

model <- lm(peak_hr ~ age, data = data)
summary(model)
# slope=-0.9 
# This indicates that for each additional year of age,
# the maximum heart rate (peak_hr) decreases by 0.9 bpm on average.
# The relationship is considered statistically significant.


# 2. Now consider this data set from a perspective of survival analysis.
# For that, you need to interpret “peak_hr” as the “time to an event of interest”
# and Finished.test as indicative of right censoring (0 –  completely observed; 1 – censored).
# Apply the Kaplan-Meier method to estimate the overall survival 
# curve of “peak_hr”.
# Do the same for males and females separately.      

#(0 –  completely observed -event finished test; 1 – censored - not finished test)

surv_obj <- Surv(time = data$peak_hr, event =1- data$Finished.test, type='right')

# Fit the Kaplan-Meier model for the entire dataset
km.model <- survfit(surv_obj ~ 1, type='kaplan-meier', data=data) # we not consider X variables so we put ~1

summary(km.model)
km.model


# Plot Kaplan-Meier survival curve
ggsurvplot(
  km.model,
  data = data,
  conf.int = FALSE, 
  censor = FALSE,
  break.time.by = 20,   
  title = "Kaplan-Meier Survival Curve for Peak Heart Rate",
  xlab = "Peak Heart Rate",
  ylab = "Survival Probability",
  font.main = c(25, "bold"),      
  font.x = c(20, "bold"),  
  font.y = c(20, "bold"), 
  font.tickslab = c(18, "plain"), 
  size = 1.5
)


# Second model gender as covariate
km_model_gender <- survfit(surv_obj ~ female, data = data)

summary(km_model_gender)

ggsurvplot(km_model_gender, 
           data = data,
           conf.int = FALSE, 
           tables.height = 0.12,
           pval = FALSE,
           censor = FALSE,
           legend.title = "Gender",
           legend.labs = c("Male", "Female"),
           break.time.by = 25,
           title = "Kaplan-Meier Survival Curves by Gender",
           xlab = "Peak Heart Rate",
           ylab = "Survival Probability",
           palette = c("blue", "red"),
           fontsize=5,
font.main = c(25, "bold"),      
font.x = c(20, "bold"),
font.y = c(20, "bold"),
font.tickslab = c(18, "plain"),
size = 1.5 ,
font.legend = c(18)  
)


#3. Compare “peak_hr” between females and males using an appropriate statistical test.
# Provide your reasoning for the choice of the test. 

# a) Log-rank test
log_rank_test<-survdiff(surv_obj~data$female, data = data,rho=0)
log_rank_test
log_rank_test$pvalue
# pvalue is less than 0.05, it suggests that there is a statistically 
# significant difference in survival between males and females.
# it means that the null hypothesis of no difference between the groups is rejected

# b) Peto & peto test
diff_peto_peto<-survdiff(surv_obj~data$female, data = data, rho=1)
diff_peto_peto

# Log-rank: tests the overall difference in survival distributions between groups.

# Peto-Peto: focuses more on the early event times 
# (i.e., differences between groups in the initial stages of the event).

# In both tests p-value is <2e-16 and this indicates a significant difference
# between the survival curves for males and females.
# It means that the null hypothesis of no difference between
# the groups should be rejected.

# 4. Perform a Weibull regression analysis using “Peak_hr” as the outcome
# and the remaining variables in  the data set as covariates.
# Is “Age” significantly influencing the outcome variable?
# Is the sex of the individuals significantly influencing the outcome variable?
model_weibull_null <-survreg(surv_obj ~ 1, dist='weibull', data=data)
model_weibull<-survreg(surv_obj ~ data$age+data$female+data$rest_hr, dist='weibull', data=data)

summary(model_weibull)

step(model_weibull_null, scope = list(lower = model_weibull_null, upper = model_weibull), 
     direction = "forward")


model_weibull_no_age<-survreg(surv_obj ~data$female+data$rest_hr, dist='weibull')
model_weibull_no_gender<-survreg(surv_obj ~data$age+data$rest_hr, dist='weibull')
AIC(model_weibull_no_age, model_weibull_no_gender) # only for comparing models with the same number of variables



anova( model_weibull_no_age,model_weibull, test = "Chisq")
anova( model_weibull_no_gender,model_weibull, test = "Chisq")



# Check if model fits well
x<-log(data$peak_hr)
F<-ecdf(data$peak_hr)
F(data$peak_hr)

plot(log(data$peak_hr), log(-log(1-F(data$peak_hr))))


y<-log(-log(1-F(data$peak_hr)))
x<-log(data$peak_hr)


inf_indices <- is.infinite(y)

y_clean <- y[!inf_indices]
x_clean <- x[!inf_indices]


model <- lm(y_clean ~ x_clean)
summary(model)


plot(x_clean, y_clean, main = "Weibull plot", xlab = "log(data$peak_hr)", ylab = "log(-log(1-F(data$peak_hr)))")
abline(model)


# 5. Perform a residual analysis of the estimated model in 4.
# Use Cox-Snel residuals and perform a statistical test to validate the respective
# distribution. 

#A model fits the data well if the Cox-Snell residuals follow an exponential distribution
# of parameter 1;
#the Komologorov-Smirnov Goodness of Fit Test (KS-test) is used
# to assess whether this is the case.

rC<-(data$peak_hr*exp(-log(predict(model_weibull,type = "response", data,
                                   na.action='na.omit'))))^(1/model_weibull$scale)

par(mfrow = c(1, 2))

# Q-Q plot comparing Cox-Snell residuals with the exponential distribution
qqplot(qexp(ppoints(length(rC))), rC, 
       main = "Q-Q plot of Cox-Snell Residuals", 
       xlab = "Theoretical Quantiles (Exponential)", 
       ylab = "Sample Quantiles (Residuals)",
       cex.lab = 1.3,
       cex.axis = 1.3) 
qqline(rC, distribution = qexp, col = "red", lty = 2, lwd = 2)

# Perform a statistical test (Kolmogorov-Smirnov test) to validate the exponential distribution
ks_test <- ks.test(rC, "pexp", rate = 1)
print(ks_test)

Q1 <- quantile(data$peak_hr, 0.25, na.rm = TRUE)  
Q3 <- quantile(data$peak_hr, 0.75, na.rm = TRUE)  
IQR <- Q3 - Q1                                   
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

data2 <- data[data$peak_hr >= lower_bound & data$peak_hr <= upper_bound, ]
surv_obj2 <- Surv(time = data2$peak_hr, event =1- data2$Finished.test, type='right')
model_weibull2<-survreg(surv_obj2 ~ data2$age+data2$female+data2$rest_hr, dist='weibull', data=data2)

rC2<-(data2$peak_hr*exp(-log(predict(model_weibull2,type = "response", data2,
                                     na.action='na.omit'))))^(1/model_weibull2$scale)

qqplot((qexp(ppoints(length(rC2)))), (rC2),
       main = "Q-Q plot of Cox-Snell Residuals (After Removing Outliers)", 
       xlab = "Theoretical Quantiles (Exponential)", 
       ylab = "Sample Quantiles (Residuals)",
       cex.lab = 1.3, 
       cex.axis = 1.3) 
qqline(rC2, distribution = qexp, col = "red", lty = 2, lwd = 2) 

# Perform a statistical test (Kolmogorov-Smirnov test) to validate the exponential distribution
ks_test2 <- ks.test(rC2, "pexp", rate = 1)
print(ks_test2)

par(mfrow = c(1, 1))

# 6. Estimate a Cox’s proportional hazard model using “Peak_hr” as the outcome
# and the remaining variables in the data set as covariates.
# Is “Age” significantly influencing the outcome variable?
# Is the sex of the individuals significantly influencing the outcome variable?

model_cph<-coxph(surv_obj ~ data$age+data$female+data$rest_hr)
summary(model_cph)

reduced_model <- coxph(surv_obj ~ data$age + data$rest_hr)
anova(model_cph, reduced_model, test = "LRT")

reduced_model2 <- coxph(surv_obj ~ data$female + data$rest_hr)
anova(model_cph, reduced_model2, test = "LRT")

model_cph_age<-coxph(surv_obj ~ data$age)
summary(model_cph_age)

model_cph_gender<-coxph(surv_obj ~ data$female)
summary(model_cph_gender)

library(MASS)
?stepAIC
stepwise_model <- stepAIC(model_cph, direction = "both")
summary(stepwise_model)
#The results from the stepwise selection (stepAIC) and the Cox model summary indicate
# that all three variables (age, female, and rest_hr) are significant predictors
#of the outcome variable (Peak_hr).
#Coefficients (coef) and Hazard Ratios (exp(coef)):
#  data$age:
  
#  Coefficient: 0.0683 — Age positively impacts the hazard (risk).
#Hazard ratio: 1.0707 — Each additional year of age increases the risk by approximately 7%.
#p-value: <2e-16 — Age is highly statistically significant.

#data$female (sex):
  
#  Coefficient: 0.3023 — Being female increases the hazard compared to males.
#Hazard ratio: 1.3529 — Females have a 35% higher risk than males.
#p-value: <2e-16 — Sex is highly statistically significant.
#data$rest_hr (resting heart rate):
  
#  Coefficient: -0.0258 — A higher resting heart rate decreases the hazard.
#Hazard ratio: 0.9745 — Each 1 bpm increase in resting heart rate reduces the risk by about 2.5%.
#p-value: <2e-16 — Resting heart rate is highly statistically significant.

# 7. Discuss the idea of using the estimated model to predict the age of an individual.

