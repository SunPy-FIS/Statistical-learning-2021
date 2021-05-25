rm(list=ls())
setwd("C:/Users/Amministratore/Desktop/Università/Anno 2/Statistical Learning")
df = read.csv("new_2_FIFA21_official_data.csv", header = T, stringsAsFactors = T)

# ------------------------------------------------------------
# TOP-5 Most valuable players
# ------------------------------------------------------------
df[order(df$Value, decreasing = T)[1:5],]

# ------------------------------------------------------------
# Some graphical representations
# ------------------------------------------------------------

# Age

hist(df$Age, xlab = "Age", main = "Histogram of Age", col = "blue")
abline(v = mean(df$Age), lwd = 2, col = "red")
abline(v = median(df$Age), lwd = 2, col = "green")
c(mean(df$Age), median(df$Age))

# Overall

dOver = density(df$Overall)
dPotential = density(df$Potential)
plot(dOver, col = "blue", xlab = "", main = "Density of Overall and Potential", ylim = c(0,0.10))
lines(dPotential, col = "red", xlab = "")
abline(v = mean(df$Overall), col = "blue", lwd = 2, lty = 3)
abline(v = mean(df$Potential), col = "red", lwd = 2, lty = 3)
c(mean(df$Overall), mean(df$Potential))

# Preferred foot

barplot(table(df$Preferred.Foot), main = "Bar plot of Preferred Foot", col = "blue")

# Best position

barplot(sort(table(df$Best.Position), decreasing = T), main = "Bar plot of Best Position", col = "blue")

# Value

par(mfrow = c(1,2))
hist(df$Value, xlab = "Value", main = "Histogram of Value", col = "blue")
hist(log(df$Value), xlab = "log(Value)", main = "Histogram of log(Value)", col = "blue")
par(mfrow = c(1,1))

# Wage vs Internation reputation

boxplot(df$Wage~df$International.Reputation, 
        col = "blue",
        xlab = "Internation Reputation",
        ylab = "Wage",
        main = "Boxplot of Internation Reputation vs Wage")

# ------------------------------------------------------------
# Principal Component Analysis
# ------------------------------------------------------------

nums = unlist(lapply(df, is.numeric))

df_nums = as.data.frame(scale(df[,nums]))

df_nums = df_nums[,-5]

prc = prcomp(df_nums)

plot(prc, type = "l")

plot(cumsum(prc$sdev^2/(sum(prc$sdev^2))), type = "o", col = "blue",
     ylab = "Variance",
     xlab = "Number of Principal Components",
     main = "Explained Cumulative Variance")













