
# MARIA LEONOR ZAMORA MAASS  
# DECEMBER 1ST, 2016

# ANOVA one way (one independent variable). 
# This model tests three or more groups for mean differences (continuous TARGET). 

# ANOVA two way (two independent variables).
# This models uses 2 levels for mean differences (continuous TARGET). Cross-tabulation!!!

# ANCOVA - analysis of covariance - (two independent variables).
# This model uses 2 levels: one factor variable and a continuous variable (covariate).

# Difference with other models: 
# ANOVA is special case of linear regression model but here predictors are categorical.
# Regression analysis deals with "expected changes" (effect sizes) and ANOVA with "expected differences" (group presence)
# Also Regression requires dummy code factors, while GLM (ANOVA/ANCOVA) handles dummy through "contrasts". 
# Contrasts vs dummy coding:  http://faculty.cas.usf.edu/mbrannick/regression/anova1.html



################################################################################
######## Examples of the use-case per model ####################################
################################################################################

# Linear Regression 
# Prediction: mean age of players (numerical) using team' years of experience (number)

# ANOVA - one way
# Prediction: mean age of players (numerical) using team group (more than 2 categories)

# ANOVA - two way
# Prediction: mean age of players (numerical) using team group (more than 2 categories) and team's position (categorical)

# ANCOVA 
# Prediction: mean age of players (numerical) using team group (more than 2 categories) and team' years of experience (number)

# Two-sample t-test (small datasets)
# Prediction: mean age of players (numerical) using team's status (yes/no variable)
# For large datasets try an Effect Size analysis: http://www.statisticssolutions.com/statistical-analyses-effect-size/



########################################################################################



# Read the file with all the data

path <- "C:/website_resources/scrap_docs/multivariate_Basketball/multivariate_positions_16.csv"
original_data <- read.csv(path, header=TRUE) 
original_data
names(original_data)

attach(original_data)

hist(PTS)
hist(AGE)
hist(log10(AGE))
barplot(table(Team),ylab = "Number or Teams per Division", las=2)
barplot(table(POSITION),ylab = "Number or Teams per Division", las=2)

contrasts(Team) = contr.sum(length(unique(Team)))
contrasts(POSITION) = contr.sum(length(unique(POSITION)))
original_data$log_AGE <- log10(PTS)
original_data$log_AGE <- log10(AGE)

attach(original_data)

#################################################################################################################################################################################

# DESCRIPTIVE STATISTICS

summary(original_data[,c("AGE","Team","POSITION","log_AGE")])

# Log-age in groups (per team)
split_result <- split( log_AGE,Team )
split_result

# Get another summary with this split
summary1 <- data.frame( t( sapply( split_result,summary) ) )
summary1$Std.Dev <- sapply( split_result,sd)
summary1$N <- sapply(split_result,length)
summary1
boxplot(split_result,las=2)

# Log-age in groups (per POSITION)
split_result <- split( log_AGE,POSITION )

summary1 <- data.frame( t( sapply( split_result,summary) ) )
summary1$Std.Dev <- sapply( split_result,sd)
summary1$N <- sapply(split_result,length)
summary1

boxplot(split_result,las=2)


#################################################################################################################################################################################

### Prediciton
### ANOVA  ONE WAY  (ONE CATEGORICAL) - If we change to lm(AGE ~ Team ) we can get the other model

Team
model0 <- lm(log_AGE ~ Team )
MODEL <- model0

# Model: General script
anova(MODEL)
summary(MODEL)
stdres <- rstandard(MODEL)

# Fitted values
sapply(split(fitted(MODEL),Team),mean)
sapply(split(fitted(MODEL),Team),sd)

#Graphs to see variance and normal distribution -> assumptions
plot(fitted(MODEL),stdres,xlab="Fitted values",ylab="Standardized residuals")
plot(stdres,ylab="Standardized residuals")
lines(stdres)
hist(stdres)
qqnorm(stdres)

## Getting outliers
stdres[stdres>=2.5]
stdres[stdres<=-2.5]

plot(log_AGE)
#par(new=TRUE)
#plot(fitted(MODEL),col='blue',type='l',yaxt = 'n',ylab='')


# Diagnostics
hata = hatvalues(MODEL)
cooka = cooks.distance(MODEL)
cbind(stdres,hata,cooka)



##########################################################################################################

### Comparisons

# After the " Fitted values" we sould think that
# Not all teams are necessarily different from each other. The general
# idea says that we should do an evaluation on all pair of teams: 28 comparisons 
# that the "Tukey method" (multiple comparison) does.


library(multcomp)
summary(glht(MODEL, linfct=mcp( Team ="Tukey"))) 

# What is the effect of the treatment on the target?
anova_for_tukey= aov(MODEL)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=anova_for_tukey, 'Team', conf.level=0.95)
TUKEY

# Tuckey test representation :
# margin plot in R
par(mar=c(5,7,2.5,4))
plot(TUKEY, las=2 , col="purple", pch=6)

# there are two clear groups (A and B) statistically different from each other.
# note: POR_ATL and ATL-POR


generate_label_df <- function(TUKEY, variable ){
  library(multcompView)
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  Tukey.labels
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels[variable]=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels[variable]) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset
LABELS=generate_label_df(TUKEY , "Team")


# A panel of colors to draw each group with the same color :
my_colors=c( rgb(143,199,74,maxColorValue = 255),rgb(242,104,34,maxColorValue = 255), rgb(111,145,202,maxColorValue = 255),rgb(254,188,18,maxColorValue = 255) , rgb(74,132,54,maxColorValue = 255),rgb(236,33,39,maxColorValue = 255),rgb(165,103,40,maxColorValue = 255))
# Draw the basic boxplot
a=boxplot(log_AGE ~ Team , ylim=c(min(log_AGE) , 1.1*max(log_AGE)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="avergae-target" ,las=2,main="Groups using Tukey method")
# I want to write the letter over each box. Over is how high I want to write it.
over=0.1*max( a$stats[nrow(a$stats),] )
#Add the labels
text( c(1:nlevels(Team)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )


################ Levene's test

stdres <- rstandard(MODEL)
absres <- abs(stdres)
MODELABSRES <- lm(absres ~ Team)
anova(MODELABSRES)
summary(MODELABSRES)


############### Try another model:  linear relation where team is just numerical

# REGRESSION VS ANOVA
# if we reject is because anova=SIMPLER is better (less than 0.05 rejects)
model0_linear <- lm(log_AGE ~ as.numeric(Team))

anova(model0_linear,model0)
anova(model0,model0_linear)



#################################################################################################################################################################################



### Prediction with more variables
### ANOVA  TWO WAY: Two categoricals to predict a numerical


## cross-information of the number of rows that we have
### This means, for example, one central (C) player from ATL (Atlanta)
Team
POSITION
table(POSITION,Team)

model1 <- lm(log_AGE ~ Team + POSITION )
MODEL <- model1

# Model: General script
anova(MODEL)
summary(MODEL)
stdres <- rstandard(MODEL)

# Fitted values
sapply(split(fitted(MODEL),Team),mean)
sapply(split(fitted(MODEL),Team),sd)

#Graphs to see variance and normal distribution -> assumptions
plot(fitted(MODEL),stdres,xlab="Fitted values",ylab="Standardized residuals")
plot(stdres,ylab="Standardized residuals")
lines(stdres)
hist(stdres)
qqnorm(stdres)

## Getting outliers
stdres[stdres>=2.5]
stdres[stdres<=-2.5]

plot(log_AGE)
par(new=TRUE)
plot(fitted(MODEL),col='blue',type='l',yaxt = 'n',ylab='')

# Here is easy to see "peaks""
# Then we'll be able to find them per index, 
# Or to find the points you can also use:  "  identify(log_AGE)  "

# Diagnostics
hata = hatvalues(MODEL)
cooka = cooks.distance(MODEL)
cbind(stdres,hata,cooka)



#################################################################################################################################################################################

### REMOVE OUTLIERS

# Now that we know the index... Who are the outliers?
original_data[c(30,34,40),] 

# Let's remove them and create a new dataset
data <- original_data[-c(30,34,40),]  

Team <- data$Team
POSITION <- data$POSITION
contrasts(Team) = contr.sum(length(unique(Team)))
contrasts(POSITION) = contr.sum(length(unique(POSITION)))
detach(original_data)
attach(data)

# Confirm that we have 3 points less in our dataset 
length(data[,1])
length(original_data[,1])

# DESCRIPTIVE STATISTICS

summary(data[,c("AGE","Team","POSITION","log_AGE")])

# Log-age in groups (per position)
split_result <- split( log_AGE,POSITION )
summary1 <- data.frame( t( sapply( split_result,summary) ) )
summary1$Std.Dev <- sapply( split_result,sd)
summary1$N <- sapply(split_result,length)
summary1
boxplot(split_result,las=2)

# Log-age in groups (per team)
split_result <- split( log_AGE,Team )
summary1 <- data.frame( t( sapply( split_result,summary) ) )
summary1$Std.Dev <- sapply( split_result,sd)
summary1$N <- sapply(split_result,length)
summary1
boxplot(split_result,las=2)


################################################################################


# ANOVA / ANCOVA

model2 <- lm(log_AGE ~ Team + POSITION )

## Model Script
MODEL <- model2
anova(MODEL)
summary(MODEL)
stdres <- rstandard(MODEL)

## Plots for assumptions 
plot(fitted(MODEL),stdres,xlab="Fitted values",ylab="Standardized residuals")
plot(stdres,ylab="Standardized residuals")
lines(stdres)
hist(stdres)
qqnorm(stdres)

## Getting outliers
stdres[stdres>=2.5]
stdres[stdres<=-2.5]

plot(log_AGE)
par(new=TRUE)
plot(fitted(MODEL),col='blue',type='l',yaxt = 'n',ylab='')

# Diagnostics
hata = hatvalues(MODEL)
cooka = cooks.distance(MODEL)
cbind(stdres,hata,cooka)


##############################################################################

## New outliers? let's see

data2 <- data[-c(4,5,20,26,29,30,39),]   #mention 33
Team <- data2$Team
POSITION <- data2$POSITION
contrasts(Team) = contr.sum(length(unique(Team)))
contrasts(POSITION) = contr.sum(length(unique(POSITION)))
detach(data)
attach(data2)

length(data[,1])
length(data2[,1])


model3 <- lm(log_AGE ~ Team + POSITION )

MODEL <- model3
anova(MODEL)
summary(MODEL)
stdres <- rstandard(MODEL)

plot(fitted(MODEL),stdres,xlab="Fitted values",ylab="Standardized residuals")
plot(stdres,ylab="Standardized residuals")
lines(stdres)
hist(stdres)
qqnorm(stdres)

# Diagnostics
hata = hatvalues(MODEL)
cooka = cooks.distance(MODEL)
cbind(stdres,hata,cooka)



##############################################################################################


################################ INTERACTION EFFECTS ################################
### lET'S SEE THOSE INTERACTIONS FIRST:


table(Team, POSITION)
interaction.plot( Team, POSITION, log_AGE,las=2 )
interaction.plot( POSITION, Team, log_AGE,las=2)

data3 <- data[data$POSITION!="C",]
Team <- data3$Team
POSITION <- data3$POSITION
data3$POSITION_F <- POSITION=="F"
contrasts(Team) = contr.sum(length(unique(Team)))
contrasts(POSITION) = contr.sum(length(unique(POSITION)))
detach(data2)
attach(data3)


split_result <- split( log_AGE,POSITION_F )

summary1 <- data.frame( t( sapply( split_result,summary) ) )
summary1$Std.Dev <- sapply( split_result,sd)
summary1$N <- sapply(split_result,length)
summary1
boxplot(split_result,las=2,xlab="Forward Position")


split_result <- split( log_AGE,Team )

summary1 <- data.frame( t( sapply( split_result,summary) ) )
summary1$Std.Dev <- sapply( split_result,sd)
summary1$N <- sapply(split_result,length)
summary1
boxplot(split_result,las=2)








######################################################################################################

################ Last Selection including interaction in the model: Team*POSITION_F ########################

table(Team, POSITION_F)
interaction.plot( Team, POSITION_F, log_AGE,las=2 )


model4 <- lm(log_AGE ~ Team + POSITION_F )
model4_int <- lm(log_AGE ~ Team + POSITION_F + Team*POSITION_F)

## Evaluation of the interaction model
MODEL <- model4_int
anova(MODEL)
summary(MODEL)
stdres <- rstandard(MODEL)

plot(fitted(MODEL),stdres,xlab="Fitted values",ylab="Standardized residuals")
plot(stdres,ylab="Standardized residuals")
lines(stdres)
hist(stdres)
qqnorm(stdres)

plot(log_AGE)
par(new=TRUE)
plot(fitted(MODEL),col='blue',type='l',yaxt = 'n',ylab='')

# Diagnostics
hata = hatvalues(MODEL)
cooka = cooks.distance(MODEL)
cbind(stdres,hata,cooka)


# MODEL COMPARISON !!! (WITH AND WITHOUT THE INTERACTION)
# WITH INTERACTION VS WITHOUT INTERACTION
# if we reject is because without=SIMPLER is better (reject = pvalue less than 0.05)
anova(model4_int, model4)

library(car)
Anova(model0, type=3)
Anova(model4, type=3)




##############################################################################

####### LAST tUKEY EVALUATION WITH INTERACTION

MODEL <- model4_int
summary(glht(MODEL, linfct=mcp( Team ="Tukey"))) 

# What is the effect of the treatment on the target?
anova_for_tukey=aov(MODEL)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=anova_for_tukey, 'Team', conf.level=0.95)
TUKEY

# Tuckey test representation :
par(mar=c(5,7,2.5,4))
plot(TUKEY, las=2 , col="purple", pch=6)

generate_label_df <- function(TUKEY, variable ){
  library(multcompView)
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  Tukey.labels
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels[variable]=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels[variable]) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset
LABELS=generate_label_df(TUKEY , "Team")


# A panel of colors to draw each group with the same color :
my_colors=c( rgb(143,199,74,maxColorValue = 255),rgb(242,104,34,maxColorValue = 255), rgb(111,145,202,maxColorValue = 255),rgb(254,188,18,maxColorValue = 255) , rgb(74,132,54,maxColorValue = 255),rgb(236,33,39,maxColorValue = 255),rgb(165,103,40,maxColorValue = 255))
# Draw the basic boxplot
a=boxplot(log_AGE ~ Team , ylim=c(min(log_AGE) , 1.1*max(log_AGE)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="avergae-target" ,las=2,main="Groups using Tukey method")
# I want to write the letter over each box. Over is how high I want to write it.
over=0.1*max( a$stats[nrow(a$stats),] )
#Add the labels
text( c(1:nlevels(Team)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )


################ Levene's test

stdres <- rstandard(MODEL)
absres <- abs(stdres)
MODELABSRES <- lm(absres ~ Team)
anova(MODELABSRES)
summary(MODELABSRES)






