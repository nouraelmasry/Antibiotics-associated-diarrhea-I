#0) Load data
load("RawData/AAD.RData")
head(AAD)
################

#1) descriptive statistics
#a) •	Summarize your data and calculate the following: mean, median, minimum, maximum, first and third quartile (for each variable). 
summary(AAD)
statistics <- sapply(AAD, function(x) {
  if(is.numeric(x)) {
    c(mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      q1 = quantile(x, probs = 0.25, na.rm = TRUE),
      q3 = quantile(x, probs = 0.75, na.rm = TRUE))
  } else {
    NA
  }
})
statistics_df <- t(data.frame(statistics))
colnames(statistics_df) <- c("Mean", "Median", "Minimum", "Maximum", "First_Quartile", "Third_Quartile")
statistics_df

#b)•	For the categorical variable existing, calculate a frequency table
freq <- AAD
n <- names(freq)
n <- n[sapply(n, function(var) is.factor(freq[[var]]))]
for(var in n){
  cnt <- dplyr::count(freq, get(var))
  print(cnt)
}
#c) •	Calculate the correlation coefficient (D1 Shannon and D6 Shannon) and (D1 Chao D6 Chao)
correlation_coefficient_Shannon <- cor(AAD$D1.Shannon, AAD$D6.Shannon, use = "complete.obs")
correlation_coefficient_Shannon
correlation_coefficient_Chao <- cor(AAD$D1.Chao1.diversity, AAD$D6.Chao1.diversity, use = "complete.obs")
correlation_coefficient_Chao

######
#2) Graphs
#a) •	Generate a bar chart of a categorical variable for the Outcome (AAD, CDI ,ND
outcome_counts <- table(AAD$Outcome)
barplot(outcome_counts, main = "Outcome Distribution", xlab = "Outcome", ylab = "Frequency", col = "skyblue")
#b)  •	Generate a bar chart graph with mean Outcome in BOL, FQ, OBL 
mean_outcome <- aggregate(AAD$Outcome, by = list(AAD$Antibiotic.class), FUN = function(x) {
  mean(as.numeric(as.factor(x)))
})
colnames(mean_outcome) <- c("Antibiotic_Class", "Mean_Outcome")
barplot(mean_outcome$Mean_Outcome, 
        names.arg = mean_outcome$Antibiotic_Class, 
        main = "Mean Outcome by Antibiotic Class", 
        xlab = "Antibiotic Class", 
        ylab = "Mean Outcome",
        col = "skyblue")
#c) •	Make a histogram of a continuous variable: “D1 Shannon” as well as “D6 Shannon”.
par(mfrow = c(1, 2))  # Set up a side-by-side layout for the histograms
hist(AAD$D1.Shannon.diversity,
     main = "Histogram of D1 Shannon Diversity",
     xlab = "D1 Shannon Diversity",
     col = "skyblue")
hist(AAD$D6.Shannon.diversity,
     main = "Histogram of D6 Shannon Diversity",
     xlab = "D6 Shannon Diversity",
     col = "skyblue")
#d)•	Make a scatterplot of 2 continuous variables D1 Shannon and D6 Shannon, and add the regression lines for each antibiotics
plot(AAD$D1.Shannon.diversity[AAD$Antibiotic.class == "OBL"], 
     AAD$D6.Shannon.diversity[AAD$Antibiotic.class == "OBL"],
     xlab = "D1 Shannon Diversity",
     ylab = "D6 Shannon Diversity",
     main = "Scatterplot of D1 Shannon vs D6 Shannon",
     col = "blue")
abline(lm(D6.Shannon.diversity ~ D1.Shannon.diversity, 
          data = subset(AAD, Antibiotic.class == "OBL")), 
       col = "blue")

# Add points and regression line for 'FQN' antibiotic class
points(AAD$D1.Shannon.diversity[AAD$Antibiotic.class == "FQN"], 
       AAD$D6.Shannon.diversity[AAD$Antibiotic.class == "FQN"],
       col = "red")
abline(lm(D6.Shannon.diversity ~ D1.Shannon.diversity, 
          data = subset(AAD, Antibiotic.class == "FQN")), 
       col = "red")

# Add points and regression line for 'PBL' antibiotic class
points(AAD$D1.Shannon.diversity[AAD$Antibiotic.class == "PBL"], 
       AAD$D6.Shannon.diversity[AAD$Antibiotic.class == "PBL"],
       col = "green")
abline(lm(D6.Shannon.diversity ~ D1.Shannon.diversity, 
          data = subset(AAD, Antibiotic.class == "PBL")), 
       col = "green")

legend("bottomright", 
       legend = unique(AAD$Antibiotic.class), 
       col = c("blue", "red", "green"), 
       pch = 0.2, 
       title = "Antibiotic Class")
#e)•	Make a boxplot of Jacard distance   and a separate boxplots per Antbiotics (as.factors). 
AAD$Antibiotic.class <- as.factor(AAD$Antibiotic.class)
boxplot(D1.D6.Jaccard.distance ~ Antibiotic.class, data = AAD,
        main = "Boxplot of Jacard Distance by Antibiotic Class",
        xlab = "Antibiotic Class",
        ylab = "Jacard Distance",
        col = "skyblue")

#####
#3) Outlier Detection
#a) •	Explore the data for any existing outliers, identify them (do NOT remove them if found).
numeric_variables <- AAD[, sapply(AAD, is.numeric)]
outliers <- lapply(numeric_variables, function(x) boxplot(x, plot = T)$out)
outliers
#b) •	What do you think?
#we observe outliers in several numeric variables in the dataset:
#1. D1.Shannon.diversity: There are 12 outliers ranging from 0.13 to 1.56.
#2. D6.Shannon.diversity: There are 26 outliers ranging from 0.07 to 1.09.
#3. D1.Chao1.diversity: There are 9 outliers ranging from 420.59 to 552.93.
#4. D6.Chao1.diversity: There are 3 outliers ranging from 383.66 to 422.75.
#there are no identified outliers for the variable D1.D6.Jaccard.distance.
#These outliers should be investigated to determine if they are real data points or if they are errors or anomalies in the dataset.

#4.Testing for normality/ homoscedasticity
#Testing Normality
#QQ plot and histogram
hist(AAD$D1.Shannon.diversity, main = "Histogram of D1.Shannon.diversity")

hist(AAD$D6.Shannon.diversity, main = "Histogram of D6.Shannon.diversity")

hist(AAD$D1.Chao1.diversity, main = "Histogram of D1.Chao1.diversity")

hist(AAD$D6.Chao1.diversity, main = "Histogram of D6.Chao1.diversity")


#Shapiro test
shapiro.test(AAD$D1.Shannon.diversity) 
#W = 0.91168, p-value = 4.134e-13
shapiro.test(AAD$D6.Shannon.diversity)
#W = 0.91549, p-value = 8.795e-13
shapiro.test(AAD$D1.Chao1.diversity)
#W = 0.94928, p-value = 2.489e-09
shapiro.test(AAD$D6.Chao1.diversity)
#W = 0.96965, p-value = 1.776e-06

#D1.Shannon.diversity is not normally distributed
#D6.Shannon.diversity is not normally distributed
#D1.Chao1.diversity is not normally distributed
#D6.Chao1.diversity is not normally distributed

#Homoscedasticity
#Residual Plot
model <- lm(D1.Shannon.diversity ~ D6.Shannon.diversity, data = AAD)
plot(model, which = 1)

model <- lm(D1.Chao1.diversity ~ D6.Chao1.diversity, data = AAD)
plot(model, which = 1)


# Bartlett’s test
bartlett.test(list(AAD$D1.Shannon.diversity, AAD$D6.Shannon.diversity))
#Bartlett's K-squared = 22.141, df = 1, p-value = 2.533e-06
bartlett.test(list(AAD$D1.Chao1.diversity, AAD$D6.Chao1.diversity))
#Bartlett's K-squared = 4.3857, df = 1, p-value = 0.03624

#AAD$D1.Shannon.diversity and AAD$D6.Shannon.diversity are significantly different from each other. not homoscedastic
#AAD$D1.Chao1.diversity and AAD$D6.Chao1.diversity are significantly different from each other. not homoscedastic

#5. Statistical Inference
# Calculate the confidence intervals for the means of Jacard distance for each Antibiotic class
antibiotics <- unique(AAD$Antibiotic.class)
confidence_levels <- c(0.90, 0.95, 0.99)

for (antibiotic in antibiotics) {
  cat("Antibiotic class:", antibiotic, "\n")
  data_subset <- AAD$D1.D6.Jaccard.distance[AAD$Antibiotic.class == antibiotic]
  for (confidence_level in confidence_levels) {
    ci <- t.test(data_subset, conf.level = confidence_level)$conf.int
    cat("Confidence Level:", confidence_level * 100, "%")
    cat(", Confidence Interval:", ci, "\n")
  }
  cat("\n")
}
#The confidence intervals provide a range of values within which we can reasonably estimate the true mean Jacard distance for each Antibiotic class.
#At lower confidence levels (e.g., 90% and 95% confidence), the intervals are narrower, indicating higher confidence in the estimated range of the mean.
#At higher confidence levels (e.g., 99% confidence), the intervals become wider, reflecting greater uncertainty for the true mean.
#When requesting higher confidence levels (e.g., 99% confidence interval), the interval width increases noticeably compared to lower confidence levels (e.g., 90% and 95% confidence intervals).
#This increase in interval width signifies a trade-off between confidence and precision. Higher confidence levels require wider intervals to capture a larger range of potential values for the true mean, resulting in less precise estimates.