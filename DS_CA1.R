#Creating a data frame

df <- data.frame(
  Student = 1:17,
  No_Visual_Aids = c(50, 60,58, 72, 36, 51, 49, 49, 25, 52, 41, 32, 58, 39, 25, 40, 61),
  With_Visual_Aids = c(58, 70, 60, 73, 40, 63, 54, 60, 29, 57, 66, 37, 50, 48, 80, 65, 70)
)

#Transposing dataframe
transposed_data <- as.data.frame(t(df[-1]))
colnames(transposed_data) <- df$Student
rownames(transposed_data) <- c("No_Visual_Aids", "With_Visual_Aids")
transposed_data


summary(df$No_Visual_Aids)
summary(df$With_Visual_Aids)


#Box Plot
#Vector of colors
windows(20,12)
colors <- c("skyblue", "lightgreen")

boxplot(df$No_Visual_Aids, df$With_Visual_Aids,
        names = c("No Visual Aids", "With Visual Aids"),
        main = "Quality Scores with and without Visual Aids",
        xlab = "Visual Aids",
        ylab = "Quality Score",
        col = colors)

#Anderson-Darling test for normality

install.packages("nortest")
library(nortest)

ad_test_no_visual <- ad.test(df$No_Visual_Aids)
ad_test_with_visual <- ad.test(df$With_Visual_Aids)

print(ad_test_no_visual)
print(ad_test_with_visual)

#Descriptive statistics
n <- nrow(df)

mean_no_visual <- mean(df$No_Visual_Aids)
median_no_visual <- median(df$No_Visual_Aids)
sd_no_visual <- sd (df$No_Visual_Aids)
skewness_no_visual <- sum((df$No_Visual_Aids-mean_no_visual)^3) / (length(df$No_Visual_Aids) * sd_no_visual ^3)

mean_with_visual <-mean(df$With_Visual_Aids)
median_with_visual <- median(df$With_Visual_Aids)
sd_with_visual <- sd(df$With_Visual_Aids)
skewness_with_visual <- sum((df$With_Visual_Aids - mean_with_visual)^3) / (length(df$With_Visual_Aids) * sd_with_visual^3)

cat("Descriptive statistics for quality scores without visual aids:\n")

cat("Mean:", mean_no_visual, "\n")

cat("Median:", median_no_visual, "\n")

cat("Skewness:", skewness_no_visual, "\n\n")

cat("Descriptive statistics for quality scores with visual aids:\n")

cat("Mean:", mean_with_visual, "\n")

cat("Median:", median_with_visual, "\n")

cat("Skewness:", skewness_with_visual, "\n\n")




#Confidence Interval

quality_difference <- df$With_Visual_Aids - df$No_Visual_Aids

# mean and standard deviation of the difference
mean_diff <- mean(quality_difference)
sd_diff <- sd(quality_difference)

# degrees of freedom
n <- nrow(df)
df <- n - 1

# t-value for 95% confidence interval
t_value <- qt(0.975, df = df)
ci_diff <- t_value * (sd_diff / sqrt(n))

lower_bound <- mean_diff - ci_diff
upper_bound <- mean_diff + ci_diff

cat("Mean Difference:", mean_diff, "\n")

cat("Standard Deviation of Difference:", sd_diff, "\n")

cat("95% Confidence Interval for the Difference:", lower_bound, "-", upper_bound, "\n")


#Difference Test between the groups

install.packages("readr")
library(readr)


df <- read.csv("quality_score.csv") #Similar data saved in csv file

View(df)
#Paired test
t_test <- t.test(df$With_Visual_Aids, df$No_Visual_Aids, paired = TRUE)
print(t_test)
