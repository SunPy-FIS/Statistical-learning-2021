rm(list=ls())
setwd("C:/Users/Amministratore/Desktop/Università/Anno 2/Statistical Learning")
df = read.csv("FIFA21_official_data.csv", 
              header = T,
              na.strings = c("NA", "NAN", "nan", "na"))
library(readr)

# ------------------------------------------------------------
# Dataset description
# ------------------------------------------------------------
dim(df)
str(df)
summary(df)
colnames(df)

# ------------------------------------------------------------
# Select only usefull variables
# ------------------------------------------------------------
df = df[,-c(4,6,10,13, 16,19,20,21,22,23,24,54,64)]

# ------------------------------------------------------------
# Find na values in the dataset and omit them
# ------------------------------------------------------------
sum.is.na = function(vector){
  sum(is.na(vector))
}
apply(df, 2, sum.is.na)
sum(apply(df, 2, sum.is.na))/prod(dim(df)) # NA values before 

df = na.omit(df)
sum(apply(df, 2, sum.is.na))/prod(dim(df)) # NA values after

# ------------------------------------------------------------
# Convert variable Value from character to numeric
# ------------------------------------------------------------
ValueToEuro = function(string){
  
  val = grepl(pattern = "M", string)
  
  if(val == T){
    num = parse_number(string) * 1000000
  }
  else{
    num = parse_number(string)*1000
  }
  
  num
}

n = dim(df)[1]
Value.In.Euro = rep(0, n)
for(i in 1:n){
  Value.In.Euro[i] = ValueToEuro(df$Value[i])
}
head(Value.In.Euro)

# ------------------------------------------------------------
# Convert variable Wage from character to numeric
# ------------------------------------------------------------
WageToEuro = function(string){
  
  val = grepl(pattern = "K", string)
  
  if(val == T){
    num = parse_number(string) * 1000
  }
  else{
    num = parse_number(string)
  }
  
  num
}

Wage.In.Euro = rep(0, n)
for(i in 1:n){
  Wage.In.Euro[i] = WageToEuro(df$Wage[i])
}
head(Wage.In.Euro) # Wage/Week

# ------------------------------------------------------------
# Replace each converted variables
# ------------------------------------------------------------
df$Value = Value.In.Euro
df$Wage = Wage.In.Euro

# ------------------------------------------------------------
# Convert Weight (from lbs to kg) and Height (from feet to cm)
# ------------------------------------------------------------
for(i in 1:n){
  df$Weight[i] = round(parse_number(df$Weight[i]) * 0.45359237, 2)
}

for(i in 1:n){
  df$Height[i] = round(parse_number(df$Height[i]) * 30.48 + 
    parse_number(sub(".'", "", df$Height[i])) * 2.54, 2)
}

# ------------------------------------------------------------
# Remova last 0 components
# ------------------------------------------------------------
df[c(df$Value == 0), "Value"] = NA
df = na.omit(df)

write_csv(df, file = "new_2_FIFA21_official_data.csv")




