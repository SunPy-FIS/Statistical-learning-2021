## data preprocessing

## TO ADD

# data loading

players_21_preproc_pl <- read.csv("C:/Users/alvis/Desktop/laurea_data_science/statistical learning/progetto/players_21_preproc_pl.csv",
                                  encoding="UTF-8")
df <- data.frame(players_21_preproc_pl)
is.data.frame(df)

boxplot(df$overall)
hist(df$overall,breaks=20)

# simpify position of the players
levels(df$team_position)

att <- c("LW","LF","CF","ST","RW","RF","LS","RS","LAM") # 9 attack positions
mid <- c("LM","CDM","CM","CAM","RM","RCM","LCM","LDM","RAM","RDM","RES") # 11 mid positions
def <- c("LB","LWB","RB","RWB","GK","LCB","RCB","CB") # 8 def positions

length(levels(df$team_position))


df$position = rep(NA,length(df[,1]))
for (i in 1:length(df[,1])){
  if (df$team_position[i] %in% att)
    df$position[i] <- "att"
  else if (df$team_position[i] %in% mid)
    df$position[i] <- "mid"
  else if (df$team_position[i] %in% def)
    df$position[i] <- "def"
  else
    df$position[i] <- "sub"
}
df$position <- factor(df$position)
table(df$position)

# highlight most important leagues


imp_leagues <- c("English Premier League", "French Ligue 1", "German 1. Bundesliga", "Holland Eredivisie", "Italian Serie A", "Portuguese Liga ZON SAGRES", "Russian Premier League", "Spain Primera Division")
df$imp_league <- as.factor(df$league_name %in% imp_leagues)


# dividing into categories

df$category <- as.factor(cut(df$overall,breaks=c(0,49,69,79,87,100),labels=c("iron","bronze","silver","gold","elite"),right = T))
table(df$category)

## selecting only appropriate variables
colnames(df)

selected_df <- df[df$league_rank <= 1,-c(1,6,7,8,9,13,14,17,18,20,55,56,57,58,59)]
#test_set <- selected_df
#selected_df$rank <- cut(selected_df$overall,breaks = c(0,50,60,70,80,88,100),labels = c("weak","Bronze","Silver","Gold","Diamond","Elite"))
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
levels(position) #will produce 3 dummies
levels(work_rate) #9 levels, will produce 8 dummies
levels(preferred_foot) #15 levels, will produce 1 dummy
levels(phase) #5 levels, will produce 4 dummies
levels(category) #5 levels, will produce 4 dummies
#levels(league_name) #43 levels, will produce 42 dummies

mod <- lm(value_eur~position,data=selected_df)
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

mod <- lm(value_eur~phase,data=selected_df)
summary(mod)
anova(mod)
result <- aov(mod)
summary(result)
TukeyHSD(result)


mod <- lm(value_eur~category,data=selected_df)
summary(mod)
anova(mod)
result <- aov(mod)
summary(result)
TukeyHSD(result)

# Exploratory data analysis (tables)

n <- length(selected_df[,1])
sorting_df <- data.frame(value = value_eur, x.index = seq.int(1, n), overall=overall,names=short_name)
sorting_df <- sorting_df[order(sorting_df$value,decreasing=TRUE),]
head(sorting_df[,c("names","value")], 20)
sorting_df <- sorting_df[order(sorting_df$overall,decreasing=TRUE),]
head(sorting_df[,c("names","overall","value")], 20)

# graphs
labels <- levels(position)
#pct <- round(table(Best.Position)/length(Best.Position)*100,2)
#labels <- paste(labels,pct)
#labels <- paste(labels,"%",sep="")
pie(table(position),
    labels=labels,
    col=rainbow(length(labels)),
    main="Player's Position Distribution")

par(las=2)
par(mar=c(8,8,1,1))
boxplot(log10(value_eur)~position,
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


## it might be necessary to include a quadratic term into age, otherwise the model overestimate people the older they get
# actually it's better to use a factor to say which part of the career the player is in based on the age

#selected_df$squared_age <- selected_df$age^2
selected_df$phase <- factor(cut(selected_df$age,breaks = c(15,20,32,35,38,40),right = TRUE,labels = c("young","prime","old","near_retirement","last_year")))

            
out.factor <- lm(value_eur~age+phase,data=selected_df)
summary(out.factor)

# note that the transormation to log doesn't really help the situation of age vs log value:
plot(selected_df$age,log10(selected_df$value_eur))


## the transformation does help in case of overall and potential:
par(mfrow=c(2,2))
plot(selected_df$overall,selected_df$value_eur)
plot(selected_df$overall,log10(selected_df$value_eur))
plot(selected_df$potential,selected_df$value_eur)
plot(selected_df$potential,log10(selected_df$value_eur))
par(mfrow=c(1,1))

                   
# correlations
library(ggcorrplot)
## heatmaps
colnames(selected_df)
corr <- round(cor(selected_df[,-c(1,8,10,45,46,47,48)]),3)
p.mat <- cor_pmat(selected_df[,-c(1,8,10,45,46,47,48)])
heatmap(corr)
# install.packages("ggcorrplot")
ggcorrplot(corr)
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")
ggcorrplot(corr, hc.order = TRUE, outline.col = "white",p.mat=p.mat)

## if we want a test set run the next 3 lines, otherwise don't
random_idxs <- runif(500,min=1,max=length(df[,1]))
test_set <- selected_df[random_idxs,]
selected_df <- selected_df[-random_idxs,]

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

# initial log model, 4 variables
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


# only interactions that make sense
dif.mod.out <- lm(value_eur~.+age:overall+age:potential+age:international_reputation+potential:international_reputation+potential:overall+overall:international_reputation,data=x)
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


## anova to check that there's actually a difference between the two models
anova(log.mod.out,dif.mod.out)


## now we proceed to model selection since some of the variables used aren't really significative

### regsubsets

#install.packages("caret")
library(caret)

dmy <- dummyVars(" ~ .", data = x)
trsf <- data.frame(predict(dmy, newdata = x))
trsf <- trsf[-1,] #if we take out messi

colnames(trsf)
trsf <- trsf[,-c(7,10,53,57,59,64)]


detach(x)
detach(trsf)
attach(trsf)



#install.packages("leaps")
library(leaps)



regfit.fwd <- regsubsets(value_eur~.+age:overall+age:potential+age:international_reputation+potential:international_reputation+potential:overall+overall:international_reputation,
                         data=trsf,
                         nvmax=60,
                         method="forward")
regfit.back <- regsubsets(value_eur~.+age:overall+age:potential+age:international_reputation+potential:international_reputation+potential:overall+overall:international_reputation,
                          data=trsf,
                          nvmax=60,
                          method="backward")
reg.summary <- summary(regfit.fwd)
back.summary <- summary(regfit.back)

par(mfrow=c(2,2))

# residual sum of squares
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")

# adjusted-R^2 with its largest value
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(56,reg.summary$adjr2[56], col="red",cex=2,pch=20)

# Mallow's Cp with its smallest value
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(40,reg.summary$cp[40],col="red",cex=2,pch=20)

# BIC with its smallest value
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(30,reg.summary$bic[30],col="red",cex=2,pch=20)

par(mfrow=c(1,1))


par(mfrow=c(2,2))

# residual sum of squares
plot(back.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")

# adjusted-R^2 with its largest value
plot(back.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(back.summary$adjr2)
points(53,back.summary$adjr2[53], col="red",cex=2,pch=20)

# Mallow's Cp with its smallest value
plot(back.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(back.summary$cp)
points(46,back.summary$cp[46],col="red",cex=2,pch=20)

# BIC with its smallest value
plot(back.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(back.summary$bic)
points(30,back.summary$bic[30],col="red",cex=2,pch=20)

par(mfrow=c(1,1))

variables <- coef(regfit.fwd,30)
#variables <- coef(regfit.back,29)
col_names <- names(variables)
col_names
chosen_variables <- paste(col_names[2:length(col_names)], collapse = '+')
formula <- as.formula(paste("value_eur", "~", chosen_variables)) 
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
                       names=names[-c(1,random_idxs)])
ratio_df <- ratio_df[order(ratio_df$ratio,decreasing=TRUE),] # più sottovalutati in percentuale
head(ratio_df, 20)
ratio_df <- ratio_df[order(ratio_df$ratio,decreasing=FALSE),] # più overvalutati in percentuale
head(ratio_df, 20)


residuals <- best.bic$residuals
residuals_df <- data.frame(value = 10^value_eur,
                           predicted_value = 10^bic.predicted,
                           residual = 10^value_eur - 10^bic.predicted,
                           names = names[-c(1,random_idxs)])
residuals_df <- residuals_df[order(residuals_df$residual,decreasing = TRUE),]  #sopravvalutati
head(residuals_df,20)
residuals_df <- residuals_df[order(residuals_df$residual, decreasing = FALSE),] #sottovalutati
head(residuals_df,20)


log.ratio <- bic.predicted/value_eur
log.ratio.df <- data.frame(value = value_eur,
                           predicted_value = bic.predicted,
                           ratio = log.ratio, 
                           names= names[-c(1,random_idxs)])
log.ratio.df <- log.ratio.df[order(log.ratio.df$ratio,decreasing=TRUE),] # sottovalutati log
head(log.ratio.df,20)
log.ratio.df <- log.ratio.df[order(log.ratio.df$ratio,decreasing=FALSE),] # sopravalutati log
head(log.ratio.df,20)


# prediction intervals
predicted_intervals <- predict(best.bic,interval = "prediction")
comparison <- data.frame(actual_value = 10^trsf$value_eur/1e6,
                         prediction = 10^predicted_intervals[,1]/1e6,
                         lower = 10^predicted_intervals[,2]/1e6,
                         uppper = 10^predicted_intervals[,3]/1e6)
head(comparison,20)
lower_bounds <- 10^predicted_intervals[,2]
upper_bounds <- 10^predicted_intervals[,3]
sum( (10^value_eur > lower_bounds) & (10^value_eur < upper_bounds) ) / n

# if a test set has been defined
dmy <- dummyVars(" ~ .", data = test_set[,-1])
trsf2 <- data.frame(predict(dmy, newdata = test_set))
colnames(trsf2)
trsf2 <- trsf2[,-c(7,10,53,57,59,64)]
predictions <- predict(best.bic,newdata = trsf2, interval="prediction")
comp <- data.frame(actual_value = trsf2$value_eur/1e6,
                   prediction = 10^predictions[,1]/1e6,
                   lower= 10^predictions[,2]/1e6,
                   upper = 10^predictions[,3]/1e6)
sum((trsf2$value_eur/1e6 > comp$lower) & (trsf2$value_eur/1e6 < comp$upper)) / 500
head(comp)

max((abs(trsf2$value_eur/1e6-comp$prediction)/(trsf2$value_eur/1e6)))
which.min((abs(trsf2$value_eur/1e6-comp$prediction)/(trsf2$value_eur/1e6)))
test_set[497,]
comp[497,]
avg_percentage_error <- mean(abs(trsf2$value_eur/1e6-comp$prediction)/(trsf2$value_eur/1e6))
avg_percentage_error


# other residual analysis

boxplot(best.bic$residuals~trsf$age)
abline(h=0)

boxplot(best.bic$residuals~trsf$overall)
abline(h=0)

idxs <- sort(best.bic$residuals,index.return=T)$ix
trsf$age[idxs][1:100]
trsf$overall[idxs[1:100]]

df[1:100,c("short_name","value_eur")]

