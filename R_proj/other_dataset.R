## data preprocessing

## TO ADD

# data loading

players_21_preproc_pl <- read.csv("C:/Users/alvis/Desktop/laurea_data_science/statistical learning/progetto/players_21_preproc_pl.csv",
                                  encoding="UTF-8")
df <- data.frame(players_21_preproc_pl)
is.data.frame(df)

boxplot(df$overall)
hist(df$overall,breaks=20)

## selecting only appropriate variables
colnames(df)
selected_df <- df[df$league_rank <= 1,-c(1,6,7,8,14,17,18,9)]
#selected_df$league_name <- droplevels(selected_df$league_name) # dropping all levels relative to minor leagues

#selected_df <- selected_df[-19,] #without messi?
#selected_df <- selected_df[!(selected_df$Value < 25000 | selected_df$Age > 40),]
summary(selected_df)
colnames(selected_df)
names <- selected_df[,1]

detach(selected_df)
attach(selected_df)


#checking NAs
na_count <-sapply(selected_df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count # number of NA per column

# checking we don't have people with value 0
sum(value_eur == 0)

# checking type, looking for factors, anova and tukey's on the factors
lapply(selected_df,class) #3 factors: preferred_foot, work_rate and team_position
levels(team_position) #will produce 27 dummy
levels(work_rate) #9 levels, will produce 8 dummies
levels(preferred_foot) #15 levels, will produce 1 dummy
#levels(league_name) #43 levels, will produce 42 dummies

mod <- lm(value_eur~team_position,data=selected_df)
summary(mod)
anova(mod)
result <- aov(mod)
summary(result)
TukeyHSD(result)

mod <- lm(value_eur~preferred_foot,data=selected_df)
summary(mod)
anova(mod)
result <- aov(mod)
summary(result)


mod <- lm(value_eur~work_rate,data=selected_df)
summary(mod)
anova(mod)
result <- aov(mod)
summary(result)
TukeyHSD(result)


#mod <- lm(value_eur~league_name,data=selected_df)
#summary(mod)
#anova(mod)
#result <- aov(mod)
#summary(result)
#TukeyHSD(result)


# Exploratory data analysis (tables)

n <- length(selected_df[,1])
sorting_df <- data.frame(value = value_eur, x.index = seq.int(1, n), overall=overall,names=short_name)
sorting_df <- sorting_df[order(sorting_df$value,decreasing=TRUE),]
head(sorting_df[,c("names","value")], 20)
sorting_df <- sorting_df[order(sorting_df$overall,decreasing=TRUE),]
head(sorting_df[,c("names","overall","value")], 20)

# graphs
labels <- levels(team_position)
#pct <- round(table(Best.Position)/length(Best.Position)*100,2)
#labels <- paste(labels,pct)
#labels <- paste(labels,"%",sep="")
pie(table(team_position),
    labels=labels,
    col=rainbow(length(labels)),
    main="Player's Position Distribution")

par(las=2)
par(mar=c(8,8,1,1))
boxplot(log10(value_eur)~team_position,
        notch = TRUE,
        border="brown",
        col="orange",
        xlab="Player's position",
        ylab="log(value)",
        main="Player's value distribution by position")

par(las=1)
par(mfrow=c(1,2))
hist(value_eur,breaks=100,col="orange",freq=FALSE,main="Players value distribution")
hist(log10(value_eur),breaks=20,col="orange",freq=FALSE,main="Player log10 value distribution")
par(mfrow=c(1,1))


#pairs

colnames(selected_df)
pairs_df <- selected_df[,c("value_eur","overall","potential","international_reputation","age")]
pairs(pairs_df)

plot(selected_df$age,selected_df$value_eur,ylim=c(0,1e7))

out1 <- lm(value_eur~age,data=selected_df)
summary(out1)
myPredict <- predict(out1) 
ix <- sort(selected_df$age,index.return=T)$ix
lines(selected_df$age[ix], myPredict[ix], col=2, lwd=2 )  


out2 <- lm(value_eur~age+I(age^2),data=selected_df)
summary(out2)
myPredict <- predict(out2) 
ix <- sort(selected_df$age,index.return=T)$ix
lines(selected_df$age[ix], myPredict[ix], col=2, lwd=2, lty=2 )
             
## it's necessary to include a quadratic term into age, otherwise the model overestimate people the older they get

selected_df$squared_age <- selected_df$age^2
#selected_df$Old <- factor(selected_df$age > 32)
#selected_df$Over30 <- factor(selected_df$age > 30)
#selected_df$Over35 <- factor(selected_df$age > 35)
selected_df$phase <- factor(cut(selected_df$age,breaks = c(15,20,32,35,38,40),right = TRUE,labels = c("young","prime","old","near_retirement","last year")))

            
out.factor <- lm(value_eur~age+squared_age+phase,data=selected_df)
summary(out.factor)
                   
# correlations
library(ggcorrplot)
## heatmaps
colnames(selected_df)
corr <- round(cor(selected_df[,-c(1,9,11,12,53)]),3)
p.mat <- cor_pmat(selected_df[,-c(1,9,11,12,53)])
heatmap(corr)
# install.packages("ggcorrplot")
ggcorrplot(corr)
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")
ggcorrplot(corr, hc.order = TRUE, outline.col = "white",p.mat=p.mat)


x <- selected_df[,-1] #names are taken out


detach(selected_df)
detach(x)
attach(x)

hist(value_eur,col="orange") # for now we try to not use any transformation of the response variable


# initial model, 4 variables
initial.mod <- lm(value_eur~overall+potential+age+international_reputation,data=x)
summary(initial.mod)
par(mfrow=c(2,2))
plot(initial.mod) 
par(mfrow=c(1,1))
initial.predicted <- predict(initial.mod)
plot(initial.predicted,value_eur,xlab="predicted values",ylab="actual values")
abline(a=0,b=1)

# initial log model, 4/5 variables
initial.log.mod <- lm(log10(value_eur)~overall+potential+age+international_reputation,data=x)
summary(initial.log.mod)
par(mfrow=c(2,2))
plot(initial.log.mod) 
par(mfrow=c(1,1))
initial.log.predicted <- predict(initial.log.mod)
par(mfrow=c(1,2))
plot(initial.log.predicted,log10(value_eur),xlab="predicted values (log)",ylab="actual values (log)")
abline(a=0,b=1)
plot(10^initial.log.predicted,value_eur,xlab="predicted values",ylab="actual values")
abline(a=0,b=1)
par(mfrow=c(1,1))

# complete model
mod.out <- lm(value_eur~.,data=x)
summary(mod.out)
par(mfrow=c(2,2))
plot(mod.out) 
par(mfrow=c(1,1))
predicted <- predict(mod.out)
plot(predicted,value_eur,xlab="predicted values",ylab="actual values")
abline(a=0,b=1)

lin_sq_res <- (predicted-value_eur)^2
lin.mod.mse <- 1/n*sum(lin_sq_res)
lin.mod.mse


# complete model log value
x$value_eur <- log10(value_eur)
detach(x)
attach(x)
hist(value_eur,breaks=20,col="orange")
qqnorm(x$value_eur)
qqline(x$value_eur)
log.mod.out <- lm(value_eur~.,data=x)
summary(log.mod.out)
par(mfrow=c(2,2))
plot(log.mod.out)
par(mfrow=c(1,1))
log.predicted <- predict(log.mod.out)
par(mfrow=c(1,2))
plot(log.predicted,value_eur,xlab="Predicted Value (log)",ylab="Actual value (log)")
abline(a=0,b=1)
plot(10^log.predicted,10^value_eur,xlab="Predicted Value",ylab="Actual value")
abline(a=0,b=1)
par(mfrow=c(1,1))

log_sq_res <- (10^log.predicted-10^value_eur)^2
log.mod.mse <- 1/n*sum(log_sq_res)
log.mod.mse

## model with all interactions
hist(x$value_eur,breaks=20,col="orange")
alt.mod.out <- lm(value_eur~.+age*.+potential*.+overall*.+international_reputation*.,data=x)
summary(alt.mod.out)
par(mfrow=c(2,2))
plot(alt.mod.out)
par(mfrow=c(1,1))
alt.predicted <- predict(alt.mod.out)
par(mfrow=c(1,2))
plot(alt.predicted,x$value_eur,xlab="Predicted Value (log)",ylab="Actual value (log)")
abline(a=0,b=1)
plot(10^alt.predicted,10^x$value_eur,xlab="Predicted Value",ylab="Actual value")
abline(a=0,b=1)
par(mfrow=c(1,1))

  
alt_sq_res <- (10^alt.predicted-10^x$value_eur)^2
alt.mod.mse <- 1/n*sum(alt_sq_res)
alt.mod.mse

# only interactions that make sense
dif.mod.out <- lm(value_eur~.+age:overall+age:potential+age:wage_eur+age:international_reputation+potential:international_reputation+potential:overall+potential:wage_eur+overall:wage_eur+overall:international_reputation+overall:wage_eur+international_reputation:wage_eur,data=x)
summary(dif.mod.out)
par(mfrow=c(2,2))
plot(dif.mod.out)
par(mfrow=c(1,1))
dif.predicted <- predict(dif.mod.out)
par(mfrow=c(1,2))
plot(dif.predicted,value_eur,xlab="Predicted Value (log)",ylab="Actual value (log)")
abline(a=0,b=1)
plot(10^dif.predicted,10^value_eur,xlab="Predicted Value",ylab="Actual value")
abline(a=0,b=1)
par(mfrow=c(1,1))

dif_sq_res <- (10^dif.predicted-10^value_eur)^2
dif.mod.mse <- 1/n*sum(dif_sq_res)
dif.mod.mse

### regsubsets

#install.packages("caret")
library(caret)

dmy <- dummyVars(" ~ .", data = x)
trsf <- data.frame(predict(dmy, newdata = x))
colnames(trsf)
trsf <- trsf[-1,] #if we take out messi

detach(x)
detach(trsf)
attach(trsf)

#install.packages("leaps")
library(leaps)



regfit.fwd <- regsubsets(value_eur~.+age:overall+age:potential+age:international_reputation+age:wage_eur+potential:international_reputation+potential:overall+potential:wage_eur+overall:wage_eur+overall:international_reputation+overall:wage_eur+international_reputation:wage_eur,
                         data=trsf,
                         nvmax=80,
                         method="forward")
regfit.back <- regsubsets(value_eur~.+age:overall+age:potential+age:international_reputation+age:wage_eur++potential:international_reputation+potential:overall+potential:wage_eur+overall:wage_eur+overall:international_reputation+overall:wage_eur+international_reputation:wage_eur,
                          data=trsf,
                          nvmax=80,
                          method="backward")
reg.summary <- summary(regfit.fwd)
back.summary <- summary(regfit.back)


par(mfrow=c(2,2))

# residual sum of squares
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")

# adjusted-R^2 with its largest value
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(65,reg.summary$adjr2[65], col="red",cex=2,pch=20)

# Mallow's Cp with its smallest value
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(60,reg.summary$cp[60],col="red",cex=2,pch=20)

# BIC with its smallest value
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(31,reg.summary$bic[31],col="red",cex=2,pch=20)

par(mfrow=c(1,1))


par(mfrow=c(2,2))

# residual sum of squares
plot(back.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")

# adjusted-R^2 with its largest value
plot(back.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(back.summary$adjr2)
points(65,back.summary$adjr2[65], col="red",cex=2,pch=20)

# Mallow's Cp with its smallest value
plot(back.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(back.summary$cp)
points(52,back.summary$cp[52],col="red",cex=2,pch=20)

# BIC with its smallest value
plot(back.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(back.summary$bic)
points(31,back.summary$bic[31],col="red",cex=2,pch=20)

par(mfrow=c(1,1))

variables <- coef(regfit.fwd,31)
#variables <- coef(regfit.back,28)
col_names <- names(variables)
col_names
chosen_variables <- paste(col_names[2:length(col_names)], collapse = '+')
formula <- as.formula(paste("value_eur", "~", chosen_variables)) # ,"+","I(age^2)"
formula



best.bic <- lm(formula,data=trsf)
summary(best.bic)

par(mfrow=c(2,2))
plot(best.bic)
par(mfrow=c(1,1))


bic.predicted <- predict(best.bic)
par(mfrow=c(1,2))
plot(bic.predicted,trsf$value_eur,xlab="Predicted Value (log)",ylab="Actual value (log)")
abline(a=0,b=1)
plot(10^bic.predicted,10^trsf$value_eur,xlab="Predicted Value",ylab="Actual value")
abline(a=0,b=1)
par(mfrow=c(1,1))



## STUDIO SOVRA-SOTTO STIMATI

ratio <- 10^bic.predicted/10^value_eur
hist(ratio,breaks=20,col="orange")
boxplot(ratio)
ratio_df <- data.frame(value = 10^value_eur,
                       predicted_value = 10^bic.predicted,
                       ratio = ratio, 
                       names=names[-1])
ratio_df <- ratio_df[order(ratio_df$ratio,decreasing=TRUE),] # più sottovalutati in percentuale
head(ratio_df, 20)
ratio_df <- ratio_df[order(ratio_df$ratio,decreasing=FALSE),] # più overvalutati in percentuale
head(ratio_df, 20)


residuals <- best.bic$residuals
residuals_df <- data.frame(value = 10^value_eur,
                           predicted_value = 10^bic.predicted,
                           residual = 10^value_eur - 10^bic.predicted,
                           names = names[-1])
residuals_df <- residuals_df[order(residuals_df$residual,decreasing = TRUE),]  #sopravvalutati
head(residuals_df,20)
residuals_df <- residuals_df[order(residuals_df$residual, decreasing = FALSE),] #sottovalutati
head(residuals_df,20)


log.ratio <- bic.predicted/value_eur
log.ratio.df <- data.frame(value = value_eur,
                           predicted_value = bic.predicted,
                           ratio = log.ratio, 
                           names= names[-1])
log.ratio.df <- log.ratio.df[order(log.ratio.df$ratio,decreasing=TRUE),] # sottovalutati log
head(log.ratio.df,20)
log.ratio.df <- log.ratio.df[order(log.ratio.df$ratio,decreasing=FALSE),] # sopravalutati log
head(log.ratio.df,20)


min(ratio)
unname(which.min(ratio))
selected_df[4,]

max(ratio)
unname(which.max(ratio))
selected_df[c(104),]

# prediction intervals
predicted_intervals <- predict(best.bic,interval = "prediction")
comparison <- data.frame(actual_value = 10^trsf$value_eur/1e6,
                         prediction = 10^predicted_intervals[,1]/1e6,
                         lower = 10^predicted_intervals[,2]/1e6,
                         uppper = 10^predicted_intervals[,3]/1e6)
head(comparison,100)
lower_bounds <- 10^predicted_intervals[,2]
upper_bounds <- 10^predicted_intervals[,3]
sum( (10^value_eur > lower_bounds) & (10^value_eur < upper_bounds) ) / n




boxplot(best.bic$residuals~trsf$international_reputation)
boxplot(best.bic$residuals~trsf$age)
abline(h=0)
hist(best.bic$residuals)
best.bic$residuals - (trsf$value_eur - bic.predicted) < 0.0001
trsf$value_eur - bic.predicted

idxs <- sort(best.bic$residuals,index.return=T)$ix
trsf$age[idxs][1:100]



## idee per modello simul europei
glm(result~atth+defh+cmph+attt+deft+cmpt)
