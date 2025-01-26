# ==============================================================================
#                     Biostatistics 2024/2025 -  PROJECT 5
#                     Authors: Iza Danielewska, Dawid Poławski
# ==============================================================================

# Load necessary libraries for survival analysis and data visualization
library(ggplot2)
library(dplyr)
library(reshape2)
library(survminer)
library(survival)
library(fitdistrplus)

# Import the dataset
data <- read.csv("MusicianData.csv")

# ==============================================================================
# --------------------------- Exploratory Data Analysis ------------------------
# ==============================================================================

# Convert columns to specific type of data
data$Person.num <- as.factor(data$Person.num)
data$Band.num <- as.factor(data$Band.num)

# Convert date columns to Date format
data$Date.of.death.censored <- as.Date(data$Date.of.death.censored)
data$Date.of.birth <- as.Date(data$Date.of.birth)
data$Date.of.number.one <- as.Date(data$Date.of.number.one)

# Extract and format years of birth from dates
data$Year.of.birth <- format(data$Date.of.birth, "%Y")
data$Year.of.birth <- as.numeric(data$Year.of.birth)

# Extract and format years of death from dates
Year.of.death <- format(data$Date.of.death.censored, "%Y")

# Create column with age of death or input NA = if the data is censored.
data$Year.of.death <- ifelse(data$status==1, as.numeric(Year.of.death), NA)

# Calculate the age at the time of censoring or death
data$Age.censored <- difftime(data$Date.of.death.censored, data$Date.of.birth)
data$Age.censored <- as.numeric(data$Age.censored, units = "days")/365.25

# Create Age.alive and Age.dead columns
data$Age.alive <- ifelse(data$status == 0, data$Age.censored, NA) 
# We input Age if the person is still alive (to Aug 2011) or NA if is dead.
data$Age.dead <- ifelse(data$status == 1, data$Age.censored, NA)  
# We input age at the moment of Death  or NA if the person is still alive.

# Summarize age data
summary(data$Age.dead)
summary(data$Age.alive)
summary(data$Age.censored)

# Check whether we've got censored data on variable: Date.of.number.one
all(difftime(as.Date("2011-08-01"),data$Date.of.number.one) > 0) 
# We don't have censored data at this variable :). 

# Calculate time to first #1 album
data$Time.number.one <- difftime(data$Date.of.number.one, data$Date.of.birth)
data$Time.number.one <- as.numeric(data$Time.number.one, units = "days")/365.25

# Summarize time to be number one data
summary(data$Time.number.one)

min(data$Year.of.birth) # 1912
max(data$Year.of.birth) # 1988

min(Year.of.death) # 1965
max(Year.of.death) # 2011

# Categorize birth and death years into decades as instructed
data$Birth.Decade <- cut(as.numeric(data$Year.of.birth), 
                         breaks = seq(1910, 1990, by = 10), 
                         labels = paste0(seq(1910, 1980, by = 10), "-", 
                                         seq(1919, 1989, by = 10)))

data$Death.Decade <- cut(as.numeric(data$Year.of.death),
                         breaks = seq(1960, 2020, by = 10),
                         labels = paste0(seq(1960, 2010, by = 10), "-", 
                                         seq(1969, 2019, by = 10)))

# Check if censored data (Date.of.death.censored) are censored in terms of status.
length(data$status[data$Date.of.death.censored == "2011-08-01"])
length(data$status[data$status==0])

# Count unique bands and individuals
length(unique(data$Band.num))          # Unique number of bands
length(unique(data$Person.num))        # Unique number of individuals


# Check if each person is assigned a unique band
all(table(data$Person.num)==1)

# count how many people are alive and dead in each band.
counts <- table(data$status, data$Band.num)
counts[1,] # how many alive members of bands we've got
counts[2,] # how many dead members of bands we've got

# Create a bar chart showing the number of people in each band
Band.count <- data.frame(table(data$Band.num))  
Band.count$Freq <- as.factor(Band.count$Freq)   


# Number of the people in the band
ggplot(Band.count, aes(x = Freq)) +
  geom_bar(fill = "skyblue", , color = "black", alpha=0.5) +
  labs(title = "Number of members in the band",
       x = "Number of members",
       y = "Number of bands")+
  scale_y_continuous(breaks = seq(0, 130, by = 10)) +
  theme(panel.background = element_rect(fill = "#e9e9e9", colour = "#DCDCDC"), 
        plot.title = element_text(hjust = 0.5))

# Distribution of Birth Years
ggplot(data, aes(x = Year.of.birth)) +
  geom_bar(fill = "skyblue", , color = "black", alpha = 0.5) +
  labs(title = "Distribution of Birth Years", x = "Year", y = "Count") +
  scale_y_continuous(breaks = seq(0, 160, by = 5))+
  theme(panel.background = element_rect(fill = "#e9e9e9", colour = "#DCDCDC"), 
        plot.title = element_text(hjust = 0.5))

# Number of Deaths Over Time
ggplot(data, aes(x = Year.of.death)) +
  geom_bar(fill = "tomato", color = "black", alpha = 0.5) +
  labs(title = "Number of Deaths Over Time", x = "Year", y = "Count") +
  scale_y_continuous(breaks = seq(0, 7, by = 1))+
  theme(panel.background = element_rect(fill = "#e9e9e9", colour = "#DCDCDC"), 
        plot.title = element_text(hjust = 0.5))

# Distribution of Age (alive) at Censoring
ggplot(data, aes(x = Age.alive)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Age (alive) at Censoring", x = "Age (alive)", 
       y = "Count") +
  scale_y_continuous(breaks = seq(0, 40, by = 5))+
  theme(panel.background = element_rect(fill = "#e9e9e9", colour = "#DCDCDC"), 
        plot.title = element_text(hjust = 0.5))

ggplot(data, aes(x = Age.dead)) +
  geom_histogram(binwidth = 1, fill = "tomato", color = "black") +
  labs(title = "Distribution of Age at Death", x = "Age", y = "Count") +
  scale_y_continuous(breaks = seq(0, 4, by = 1))+
  theme(panel.background = element_rect(fill = "#e9e9e9", colour = "#DCDCDC"), 
        plot.title = element_text(hjust = 0.5))

# Distribution of age (at death or censoring) for every single person in data
ggplot(data, aes(x = Age.censored)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Age at Death or Censoring", x = "Age", y = "Count") +
  scale_y_continuous(breaks = seq(0, 40, by = 5))+
  theme(panel.background = element_rect(fill = "#e9e9e9", colour = "#DCDCDC"), 
        plot.title = element_text(hjust = 0.5))

# Age vs. Year
ggplot(data, aes(x = Year.of.death, y = Age.censored)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.7) +
  geom_segment(aes(x = Year.of.birth, 
                   xend = Year.of.death, 
                   y = 0, yend = Age.censored), 
               color = "gray", alpha = 0.5) +
  geom_hline(yintercept = 27, color = "red", linewidth = 1) +
  labs(
    title = "Age at death vs. Year",
    x = "Year",
    y = "Age (years)"
  ) +
  scale_y_continuous(breaks = seq(0, 90, by = 10))+
  scale_x_continuous(breaks = seq(1910, 2020, by = 10)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Age at #1 Album vs. Year of Birth
ggplot(data, aes(x = Date.of.birth, y = Time.number.one)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_line(aes(y = difftime(as.Date("2011-08-01"), Date.of.birth)/365.25), 
            color = "purple", linetype = 2, linewidth = 1) + 
  labs(title = "Age at #1 Album vs. Year of Birth", x = "Year of Birth", 
       y = "Age at #1 Album") +
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  scale_x_continuous(breaks = 
                       seq(as.Date("1910/1/1"), as.Date("2000/1/2"), 
                                  by = 10*365.25), 
                     labels = c("1910", "1920", "1930", "1940", "1950", "1960",
                                "1970", "1980", "1990","2000"))+
  theme(panel.background = element_rect(fill = "#e9e9e9", colour = "#DCDCDC"), 
        plot.title = element_text(hjust = 0.5))

# Create a contingency table of death and birth decades
heatmap_data <- table(data$Death.Decade, data$Birth.Decade)

# Reshape the data for visualization using ggplot2
heatmap_data_melt <- melt(heatmap_data)

# Plot a heat map showing the relation between birth and death decades
ggplot(heatmap_data_melt, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "#e9e9e9", high = "red", breaks = seq(0,10,2)) +
  labs(title = "Heatmap of Birth and Death Counts", x = "Birth Decade", 
       y = "Death Decade", fill = "Count") +
  theme(panel.background = element_rect(fill = "#e9e9e9", colour = "#DCDCDC"), 
        axis.text.x = element_text(angle = 90, hjust = 1))

# Sort IDs
id_sort <- rownames(sort(table(data$Band.num)))
counts <- as.data.frame(table(data$Band.num, data$status))
colnames(counts) <- c("id", "status", "count")

# Add a total count for each ID
counts <- counts %>%
  group_by(id) %>%
  mutate(total_count = sum(count)) %>%
  ungroup()

# Arrange the data by total count in descending order and ID
counts <- counts %>%
  arrange(desc(total_count), id)

# Negate the count values for status = 0 to create a two-sided bar plot
counts$count <- ifelse(counts$status == 0, -counts$count, counts$count)

# Create a two-sided barplot with counts of members for each Band and status
ggplot(counts, aes(x =  reorder(id, -total_count), y = count, 
                   fill = factor(status))) +
  geom_bar(stat = "identity", position = "identity") + 
  coord_flip() +
  labs(
    title = "Two-sided barplot for Band ID",
    x = "Band ID",
    y = "Count",
    fill = "Status"
  ) +
  scale_fill_manual(values = c("lightgreen", "tomato")) + 
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# ==============================================================================
# ------------------------- Survival analysis part 1 ---------------------------
# ==============================================================================


# Create a survival object for censored data (Age and status)
Surv.object.age.censored <- Surv(time = data$Age.censored, 
                                 event = data$status)

# Fit a survival model for each birth decade
model <- survfit(Surv.object.age.censored ~ Birth.Decade, data = data) 

# Generate survival curves for each birth decade
ggsurvplot(model[c(1)], data = data, linetype = 1, size = 0.7, palette = "#1E90FF",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           legend.lab = "1910-1919",
           title = "Survival curve (1910-1919)",
           conf.int.alpha = 0.12)

ggsurvplot(model[c(2)], data = data, linetype = 1, size = 0.7, palette = "#DC143C",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth", 
           title = "Survival curve (1920-1929)",
           legend.lab = "1920-1929",
           conf.int.alpha = 0.12)

ggsurvplot(model[c(3)], data = data, linetype = 1, size = 0.7, palette = "#FFA500",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           title = "Survival curve (1930-1939)",
           legend.lab = "1930-1939",
           conf.int.alpha = 0.12)

ggsurvplot(model[c(4)], data = data, linetype = 1, size = 0.7, palette = "#FF00FF",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           title = "Survival curve (1940-1949)",
           legend.lab = "1940-1949",
           conf.int.alpha = 0.12)

ggsurvplot(model[c(5)], data = data, linetype = 1, size = 0.7, palette = "#4B0082",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           title = "Survival curve (1950-1959)",
           legend.lab = "1950-1959",
           conf.int.alpha = 0.12)

ggsurvplot(model[c(6)], data = data,  linetype = 1, size = 0.7, palette = "#808000",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           title = "Survival curve (1960-1969)",
           legend.lab = "1960-1969",
           conf.int.alpha = 0.20)

ggsurvplot(model[c(7)], data = data, linetype = 1, size = 0.7, palette = "#696969",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           title = "Survival curve (1970-1979)",
           legend.lab = "1970-1979",
           conf.int.alpha = 0.12)

ggsurvplot(model[c(8)], data = data, linetype = 1, size = 0.7, palette = "#C71585",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           title = "Survival curve (1980-1989)",
           legend.lab = "1980-1989",
           conf.int.alpha = 0.12)

ggsurvplot(model, data = data, pval = TRUE, conf.int = TRUE, linetype = 1, 
           size = 0.7,
           palette = c("#1E90FF","#DC143C", "#FFA500", "#FF00FF", "#4B0082", 
                       "#808000", "#696969", "#C71585"), xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth", 
           title = "Survival curves",
           conf.int.alpha = 0.12)

# On the plot, the p-value for the Log-rank test is presented.
# p-value = 0.039, so we reject the null hypothesis.
# So here are significant statistical differences between the survival curves.
# The interpretation continues as detailed below.

# ------------------- Comparison survival curves for each decade ---------------

# a) Log-rank test (rho = 0)
survdiff_test_logrank <- survdiff(Surv.object.age.censored ~ Birth.Decade, 
                                  data = data, rho = 0)

survdiff_test_logrank$chisq      # 14.79181
survdiff_test_logrank$pvalue     # 0.03876264
print(survdiff_test_logrank)

# Results: The p-value is less than 0.05, so here are significant statistical 
#          differences between the survival curves.
#          The musician born in different decades have different probability of long life.


# b) Peto-Peto test (rho = 1)
survdiff_test_petopeto <- survdiff(Surv.object.age.censored ~ Birth.Decade, 
                                   data = data, rho = 1)

survdiff_test_petopeto$chisq     # 14.72923 
survdiff_test_petopeto$pvalue    # 0.03963116
print(survdiff_test_petopeto)

# Results: The p-value is less than 0.05, so here are significant statistical 
#          differences between the survival curves.
#          The same interpretation as in Log-rank test.


# We use these tests because we have censored data, 
# and we want to compare survival curves using non-parametric tests.
# Both of these tests included censored data in their calculations.

# ------------------- Compare survival curves using regression model -----------

# At the beginning, we would like to see behavior of log(Age.censored) 
# for each decade.

colors <- ifelse(data$status == 1, "red", "green")

par(mfrow = c(2,4))
plot(log(data$Age.censored[data$Birth.Decade == "1910-1919"]), 
     xlab = "Number\nDecade 1910-1919", ylab = "log(Age)", 
     col = colors[data$Birth.Decade == "1910-1919"])
plot(log(data$Age.censored[data$Birth.Decade == "1920-1929"]), 
     xlab = "Number\nDecade 1920-1929", ylab = "log(Age)", 
     col = colors[data$Birth.Decade == "1920-1929"])
plot(log(data$Age.censored[data$Birth.Decade == "1930-1939"]), 
     xlab = "Number\nDecade 1930-1939", ylab = "log(Age)", 
     col = colors[data$Birth.Decade == "1930-1939"])
plot(log(data$Age.censored[data$Birth.Decade == "1940-1949"]), 
     xlab = "Number\nDecade 1940-1949", ylab = "log(Age)", 
     col = colors[data$Birth.Decade == "1940-1949"])
plot(log(data$Age.censored[data$Birth.Decade == "1950-1959"]), 
     xlab = "Number\nDecade 1950-1959", ylab = "log(Age)", 
     col = colors[data$Birth.Decade == "1950-1959"])
plot(log(data$Age.censored[data$Birth.Decade == "1960-1969"]), 
     xlab = "Number\nDecade 1960-1969", ylab = "log(Age)", 
     col = colors[data$Birth.Decade == "1960-1969"])
plot(log(data$Age.censored[data$Birth.Decade == "1970-1979"]), 
     xlab = "Number\nDecade 1970-1979", ylab = "log(Age)", 
     col = colors[data$Birth.Decade == "1970-1979"])
plot(log(data$Age.censored[data$Birth.Decade == "1980-1989"]), 
     xlab = "Number\nDecade 1980-1989", ylab = "log(Age)", 
     col = colors[data$Birth.Decade == "1980-1989"])
par(mfrow = c(1,1))

# Censored data mostly align in a straight line, 
# while data on time of death do not exhibit this pattern.
# It’s difficult to determine which model we should choose.

# Create models and compare them using AIC.
weimodel <- survreg(Surv.object.age.censored ~ Birth.Decade, data = data, 
                    dist = "weibull")
lognmodel <- survreg(Surv.object.age.censored ~ Birth.Decade, data = data, 
                     dist = "lognormal")
loglmodel <- survreg(Surv.object.age.censored ~ Birth.Decade, data = data, 
                     dist = "loglogistic")

AIC(weimodel,lognmodel,loglmodel)

min(AIC(weimodel,lognmodel,loglmodel)[2]) # 950.0137

# The best model "weimodel". 

weimodel$coef
summary(weimodel) # reference decade: 1910-1919
#The birth decade has an impact on survival time, particularly for individuals 
# born in the 1950-1959 and 1960-1969 periods, where the results are 
# statistically significant. The other decades do not show significant differences.

# But we need to check the model assumptions to ensure our conclusions are valid.

scale <- weimodel$scale   # Scale parameter

# Now we would like to check the adequacy of Weibull model for our data.

# Compute Cox-Snell residuals
cox_snell_residuals <- (data$Age.censored * exp(-predict(weimodel, type = "link")))^(1/scale)

residuals_data <- data.frame(residuals = cox_snell_residuals)

# Creating an empirical cumulative distribution function (ECDF) 
# and adding the theoretical one.
ggplot(residuals_data, aes(x = residuals)) +
  stat_ecdf(aes(color = "ECDF"), geom = "step", linewidth = 1, alpha = 0.8) +
  stat_function(fun = pexp, args = list(rate = 1), 
                aes(color = "Theoretical distribution function (Exp(1))"), 
                linetype = "dashed", linewidth = 1) +
  scale_color_manual(
    values = c("ECDF" = "blue", 
               "Theoretical distribution function (Exp(1))" = "red"),
    name = ""
  ) +
  labs(
    title = "Comparison between ECDF and theoretical distribution function",
    x = "Residuals",
    y = "ECDF"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )

# Comparison between cox snell residuals and exponential theoretical distribution
# with rate = 1.
ks.test(cox_snell_residuals, "pexp", 1) # p-value < 2.2e-16

# Standardized residuals
rstand <- (log(data$Age.censored)-predict(weimodel, type = "link"))/weimodel$scale

residuals_data <- data.frame(residuals = rstand)

gumbel_density <- function(x, mu = 0, sigma = 1) {
  return((1/sigma) * exp(-(x - mu)/sigma) * exp(-exp(-(x - mu)/sigma)))
}

# Comparison between density of standardized residuals and Gumbel density
# with parameters 0 and 1.
ggplot(residuals_data, aes(x = residuals)) +
  geom_density(aes(color = "Empirical Density"), fill = "lightblue", alpha = 0.7, linewidth = 1) +
  stat_function(fun = gumbel_density, args = list(mu = 0, sigma = 1), 
                aes(color = "Theoretical Gumbel(0,1)"), linewidth = 1) +
  scale_color_manual(
    values = c(
      "Empirical Density" = "blue",
      "Theoretical Gumbel(0,1)" = "red"
    )
  ) +
  labs(
    title = "Density of Standardized Residuals and Gumbel(0,1)",
    x = "Residuals",
    y = "Density (value)",
    color = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")

# Results: 
# Based on the above plots and the results of the KS test, it appears that 
# the Weibull model is inadequate for this data.
# As the model assumptions are not satisfied, the conclusions drawn from 
# this model may not be entirely accurate or reliable.
# Additionally, the uncensored sample is relatively small and includes only 
# about 70 observations, which is why the obtained results are not fully sufficient.
# Therefore, we need to use another (for example non-parametric) test to draw 
# valid conclusions from this data.


# ==============================================================================
# ------------------------- Survival analysis part 2 ---------------------------
# ==============================================================================

# Create a survival object for time data with all events
Surv.object.time.number.one <- Surv(time = data$Time.number.one, 
                                    event = rep(1, length(data$Time.number.one)))

model <- survfit(Surv.object.time.number.one ~ Birth.Decade, data = data)

# Plot survival probability for specific models
ggsurvplot(model[c(1)], data = data, linetype = 1, size = 0.7, palette = "#1E90FF",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           legend.lab = "1910-1919",
           conf.int.alpha = 0.12)

ggsurvplot(model[c(2)], data = data, linetype = 1, size = 0.7, palette = "#DC143C",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth", 
           legend.lab = "1920-1929",
           conf.int.alpha = 0.12)

ggsurvplot(model[c(3)], data = data, linetype = 1, size = 0.7, palette = "#FFA500",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           legend.lab = "1930-1939",
           conf.int.alpha = 0.12)

ggsurvplot(model[c(4)], data = data, linetype = 1, size = 0.7, palette = "#FF00FF",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           legend.lab = "1940-1949",
           conf.int.alpha = 0.12)

ggsurvplot(model[c(5)], data = data, linetype = 1, size = 0.7, palette = "#4B0082",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           legend.lab = "1950-1959",
           conf.int.alpha = 0.12)

ggsurvplot(model[c(6)], data = data,  linetype = 1, size = 0.7, palette = "#808000",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           legend.lab = "1960-1969",
           conf.int.alpha = 0.20)

ggsurvplot(model[c(7)], data = data, linetype = 1, size = 0.7, palette = "#696969",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           legend.lab = "1970-1979",
           conf.int.alpha = 0.12)

ggsurvplot(model[c(8)], data = data, linetype = 1, size = 0.7, palette = "#C71585",
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth",
           legend.lab = "1980-1989",
           conf.int.alpha = 0.12)

ggsurvplot(model, data = data, pval = TRUE, conf.int = TRUE, linetype = 1, size = 0.7,
           palette = c("#1E90FF","#DC143C", "#FFA500", "#FF00FF", "#4B0082", 
                       "#808000", "#696969", "#C71585"), xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Decade of Birth", 
           conf.int.alpha = 0.12)

# We can see at plot (method of directly looking at plot) that the curves are 
# not the same, so we can check our minds by statistical tests :)

# Due to p-value (presented on the plot) we can say that survival curves for 
# each decade are different.

# a) Log-rank test
survdiff_test <- survdiff(Surv.object.time.number.one ~ Birth.Decade, 
                          data = data, rho = 0)
survdiff_test$chisq    # 445.041
survdiff_test$pvalue   # 5.155283e-92
print(survdiff_test)

# b) Peto-Peto test
survdiff_test <- survdiff(Surv.object.time.number.one ~ Birth.Decade, 
                          data = data, rho = 1)
survdiff_test$chisq    # 396.9094
survdiff_test$pvalue   # 1.097146e-81
print(survdiff_test)

# We can also use KS test and Kruskal-Wallis test because we have complete data at this case.
kruskal.test(Time.number.one ~ Birth.Decade, data = data)

decades <- unique(levels(data$Birth.Decade))
results <- combn(decades,2)
test <- function(pair) {
  g1 <- data$Time.number.one[data$Birth.Decade == pair[1]]
  g2 <- data$Time.number.one[data$Birth.Decade == pair[2]]
  ks <- ks.test(g1, g2)
  ks$p.value
}

p_val <- c()
col1 <- c()
col2 <- c()
for (i in 1:28){
  p_val <- c(p_val, test(results[,i]))
  col1 <- c(col1, results[1,i])
  col2 <- c(col2, results[2,i])
}
res <- as.data.frame(cbind(col1, col2, round(p_val,10)))
colnames(res) <- c("Group1", "Group2", "p_value")
res

# Due to multiple testing, we apply the Bonferroni correction.
sum(res$p_value < 0.05/28) # 10

# The Log-rank test, Peto-Peto test, and Kruskal-Wallis test indicate 
# that the time to reach number 1 on the UK charts 
# is significantly different for each decades. 
# However, the KS test suggests 
# that some decades have similar times to reach number 1 on the UK charts.

#-------------------------- Regression models ----------------------------------

# Let's have a look how behave observations (time to be #1) for each decade.
par(mfrow = c(2,4))
plot(log(data$Time.number.one[data$Birth.Decade == "1910-1919"]), 
     xlab = "Number\nDecade 1910-1919", ylab = "log(Time to be #1)")
plot(log(data$Time.number.one[data$Birth.Decade == "1920-1929"]),
     xlab = "Number\nDecade 1920-1929", ylab = "log(Time to be #1)")
plot(log(data$Time.number.one[data$Birth.Decade == "1930-1939"]), 
     xlab = "Number\nDecade 1930-1939", ylab = "log(Time to be #1)")
plot(log(data$Time.number.one[data$Birth.Decade == "1940-1949"]), 
     xlab = "Number\nDecade 1940-1949", ylab = "log(Time to be #1)")
plot(log(data$Time.number.one[data$Birth.Decade == "1950-1959"]), 
     xlab = "Number\nDecade 1950-1959", ylab = "log(Time to be #1)")
plot(log(data$Time.number.one[data$Birth.Decade == "1960-1969"]), 
     xlab = "Number\nDecade 1960-1969", ylab = "log(Time to be #1)")
plot(log(data$Time.number.one[data$Birth.Decade == "1970-1979"]), 
     xlab = "Number\nDecade 1970-1979", ylab = "log(Time to be #1)")
plot(log(data$Time.number.one[data$Birth.Decade == "1980-1989"]), 
     xlab = "Number\nDecade 1980-1989", ylab = "log(Time to be #1)")
par(mfrow = c(1,1))

# We don't see any specific lines, but the point clouds form distinct shapes.

# Let's check some of models and choose the best one, using AIC. 
weimodel <- survreg(Surv.object.time.number.one ~ Birth.Decade, data = data, 
                    dist = "weibull")
lognmodel <- survreg(Surv.object.time.number.one ~ Birth.Decade, data = data, 
                     dist = "lognormal")
loglmodel <- survreg(Surv.object.time.number.one ~ Birth.Decade, data = data, 
                     dist = "loglogistic")

AIC(weimodel, lognmodel, loglmodel)

min(AIC(weimodel, lognmodel, loglmodel)[2]) # 6408.636

# The best model is model with distribution log-logistic.

# Let's check the distribution of the residuals in this model.
res <- (log(data$Time.number.one) - predict(loglmodel, type = "link"))/loglmodel$scale
residuals_data <- data.frame(residuals = res)

parameter <- fitdistr(res, dlogis, start = list(location = 1, scale = 1))
# Based on the model estimation results, we observe that:
#      location          scale    
# -6.425331e-07    9.999993e-01 

# Therefore, it is reasonable to simplify the distribution parameters by assuming:
# location = 0 and scale = 1.

# Empirical density of standardized residuals vs. density of Logistic(0,1)
ggplot(residuals_data, aes(x = residuals)) +
  geom_density(aes(color = "Empirical Density"), fill = "lightblue", alpha = 0.7, linewidth = 1) +
  stat_function(fun = dlogis, args = list(location = 0, scale = 1), 
                aes(color = "Theoretical Logistic(0,1)"), linewidth = 1) +
  scale_color_manual(
    values = c(
      "Empirical Density" = "blue",
      "Theoretical Logistic(0,1)" = "red"
    )
  ) +
  labs(
    title = "Density of Standardized Residuals",
    x = "Residuals",
    y = "Density (value)",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )


ks.test(res, "plogis", location = 0 , scale = 1)  # p-value = 0.6048

# Hence, the standardized residuals follow a Logistic(0,1) distribution, 
# indicating that the log-logistic model is adequate.


coef(loglmodel)
summary(loglmodel)

# All variables in the model are significant according to the Wald test. 
# Therefore, we can conclude that the times to reach #1 album  
# are significantly different for each decades.

# We consider nonparametric methods to be better 
# because they do not rely on distributional assumptions, 
# which in our case turned out to be quite significant.

