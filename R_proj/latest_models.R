## data loading

df = data.frame(new_FIFA21_official_data)
is.data.frame(df)

## selecting only appropriate variables
colnames(df)
selected_df <- df[,-c(1,4,7,13,16,17,56)]
summary(selected_df)
colnames(selected_df)


# checking NAs
na_count <-sapply(selected_df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count # number of NA per column

# dropping NAs rows
length(selected_df[,1]) #17108 rows
mask = complete.cases(selected_df)
n <- sum(mask) #15479 rows without any NA
names <- df$Name[mask]
selected_df <- selected_df[complete.cases(selected_df),] #dropping all rows with at least a NA

detach(selected_df)
attach(selected_df)


# how many with value 0?
sum(Value == 0) # 0 after removing rows with NA, no need for further cleaning

# checking type, looking for factors
lapply(selected_df,class) #3 factors: Preferred.Foot, work.Rate and Best.Position
levels(Preferred.Foot) #will produce 1 dummy
selected_df$Work.Rate <- droplevels(selected_df$Work.Rate) #there's a "N/A/ N/A" level unused
levels(selected_df$Work.Rate) #9 levels, will produce 8 dummies
levels(Best.Position) #15 levels, will produce 14 dummies
 
# Exploratory data analysis (tables)
sorting_df <- data.frame(value = Value, x.index = seq.int(1, n), overall=Overall,names=Name)
sorting_df <- sorting_df[order(sorting_df$value,decreasing=TRUE),]
head(sorting_df[,c("names","value")], 20)
sorting_df <- sorting_df[order(sorting_df$overall,decreasing=TRUE),]
head(sorting_df[,c("names","overall","value")], 20)

# graphs
labels <- levels(Best.Position)
#pct <- round(table(Best.Position)/length(Best.Position)*100,2)
#labels <- paste(labels,pct)
#labels <- paste(labels,"%",sep="")
pie(table(Best.Position),
    labels=labels,
    col=rainbow(length(labels)),
    main="Player's Position Distribution")

boxplot(log10(Value)~Best.Position,
        notch = TRUE,
        border="brown",
        col="orange",
        xlab="Player's position",
        ylab="log(value)",
        main="Player's value distribution by position")

par(mfrow=c(1,2))
hist(Value,breaks=100,col="orange",freq=FALSE,main="Players value distribution")
hist(log10(Value),breaks=20,col="orange",freq=FALSE,main="Player log10 value distribution")
par(mfrow=c(1,1))


#pairs(selected_df[,1:10])

par(mfrow=c(2,2))
hist(Age,main="Age distribution",freq=FALSE,col="yellow")
hist(Overall,main="Overall rating distribution",freq=FALSE,col="orange")
plot(Overall,Value,main="value as a function of overall rating")
plot(Age,Value,main="value as a function of age")
par(mfrow=c(1,1))

# correlations
## heatmaps
corr <- round(cor(selected_df[,-c(1,8,11,47)]),3)
p.mat <- cor_pmat(selected_df[,-c(1,8,11,47)])
heatmap(corr)
# install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr)
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")
ggcorrplot(corr, hc.order = TRUE, outline.col = "white",p.mat=p.mat)

var(Release.Clause,Value)/(sd(Release.Clause)*sd(Value))
# correlation between value and release clause is almost 1, almost like cheating
colnames(selected_df)

# complete model 
x <- selected_df[,-c(1,49)] #release clause and name are taken out
hist(x$Value) # for now we try to not use any transformation of the response variable

mod.out <- lm(Value~.,data=x)
summary(mod.out)
par(mfrow=c(2,2))
plot(mod.out) 
# residuals not randomly distributed horizontally (plot 1)
# normality not satisfied (plot 2)
# homoschedasticity not really respected, line isn't straight (plot 3)
# player 45 (T. Partey) seems to be an outlier, its true value is way above what's predicted (plot 4)
# player 19 (L. Messi) seems to be a high leverage point, it completely stands out of others (plot 4)
par(mfrow=c(1,1))
qqnorm(residuals(mod.out))
qqline(residuals(mod.out))
predicted <- predict(mod.out)
plot(predicted,x$Value,xlab="predicted values",ylab="actual values")
abline(a=0,b=1)

# complete model log value
x$Value = log10(x$Value)
hist(x$Value,breaks=20)

log.mod.out <- lm(x$Value~.,data=x)
summary(log.mod.out)
par(mfrow=c(2,2))
plot(log.mod.out)
# player 14674 is Van Damme (overvalued)
par(mfrow=c(1,1))
qqnorm(residuals(log.mod.out))
qqline(residuals(log.mod.out))
log.predicted <- predict(log.mod.out)
par(mfrow=c(1,2))
plot(log.predicted,x$Value,xlab="Predicted Value (log)",ylab="Actual value (log)")
abline(a=0,b=1)
plot(10^log.predicted,10^x$Value,xlab="Predicted Value",ylab="Actual value")
abline(a=0,b=1)
par(mfrow=c(1,1))
 

x$Value = log10(x$Value)
hist(x$Value,breaks=20)
qqnorm(x$Value)
qqline(x$Value)
alt.mod.out <- lm(x$Value~.+x$Age*.+x$Potential*.+x$Overall*.+x$International.Reputation*.+x$Wage*.,data=x)
summary(alt.mod.out)
par(mfrow=c(2,2))
plot(alt.mod.out)
par(mfrow=c(1,1))
qqnorm(residuals(alt.mod.out))
qqline(residuals(alt.mod.out))
alt.predicted <- predict(alt.mod.out)
par(mfrow=c(1,2))
plot(alt.predicted,x$Value,xlab="Predicted Value (log)",ylab="Actual value (log)")
abline(a=0,b=1)
plot(10^alt.predicted,10^x$Value,xlab="Predicted Value",ylab="Actual value")
abline(a=0,b=1)
par(mfrow=c(1,1))
