#1) Load data
load("RawData/AAD.RData")
head(AAD)
################

#2) descriptive statistics
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
freqq <- AAD
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
#3) Graphs
#a) •	Generate a bar chart of a categorical variable for the Outcome (AAD, CDI ,ND
outcome_counts <- table(AAD$Outcome)
barplot(outcome_counts, main = "Outcome Distribution", xlab = "Outcome", ylab = "Frequency", col = "skyblue")
#b) 
