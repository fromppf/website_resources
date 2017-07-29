
# MARIA LEONOR ZAMORA MAASS  - MZM239
# NOVEMBER 1, 2016
# MULTIPLE REGRESSION

# Read the file with all the data

path <- "C:/website_resources/scrap_docs/multivariate_Paintings/multivariate__train_paintings.csv"

original_data <- read.csv(path, header=TRUE) 

summary(original_data)

  # Data columns that will be used

data <- subset(original_data, select = c('paintinglink','subject_Nature','list_price',
                  'width','height','num_followed','grad_ed','developed_country') )

data <- subset(data,subject_Nature==1)
names(data)
attach(data)
data
target_name = 'list_price'


#################################################################################################################################################################################


# DESCRIPTIVE STATISTICS

summary(data)

c('std dev',sd(list_price))
c('std dev',sd(width))
c('std dev',sd(height))
c('std dev',sd(num_followed))
c('std dev',sd(grad_ed))
c('std dev',sd(developed_country))

target_size <- length(list_price)
target_size

# Graphs to analyze ranges
hist(list_price)

hist(width)
hist(height)
hist(num_followed)

plot(width,list_price)
plot(height,list_price)
plot(num_followed,list_price)
plot(grad_ed,list_price)
plot(developed_country,list_price)

# Using logs
hist(log10(list_price))
hist(log10(width))
hist(log10(height))
hist(log10(num_followed))

plot(log10(width),log10(list_price))
plot(log10(height),log10(list_price))
plot(log10(num_followed),log10(list_price))

identify(log10(width),log10(list_price))

# Indicators boxes
boxplot(split(log10(list_price),grad_ed), xlab="Grad Education", ylab="List Price")
boxplot(split(log10(list_price),developed_country), xlab="Developed Country", ylab="List Price")

# New summary of prices
summary(list_price)



#################################################################################################################################################################################

# DATA ERRORS

# data[c(65,142,352,722),]  width-> 
# data zero prices
# data[c(280,510,763),] height -> Mindaugas Lukauskis ..and small /art/Painting-Spring-Rhythms/175321/2381134/view 
# data[763,] big -> https://www.saatchiart.com/art/Painting-Tui-in-the-Mist/165778/2538073/view
# data[c( 280, 390 ,400 ,406, 408, 409, 471,383),]

  
data1 <- data[-c(65,142,280, 352,390 ,400 ,406, 408, 409, 471,510,559,593,722,763,786,803,386,375:397),]
data1 <- subset(data1,log10(list_price)<4)
data1 <- subset(data1,log10(list_price)>0)
data1 <- subset(data1,height>5)
data1 <- subset(data1,width>5)
data1 <- subset(data1,log10(num_followed)<=2.8)
data1 <- subset(data1,log10(num_followed)>-Inf)

attach(data1)

summary(data1)

log_list_price <- log10(list_price)
log_width <- log10(width)
log_height <- log10(height)
log_followed <- log10(num_followed)

c('std dev',sd(list_price))
c('std dev',sd(width))
c('std dev',sd(height))
c('std dev',sd(num_followed))
c('std dev',sd(grad_ed))
c('std dev',sd(developed_country))

# Using logs

summary(log_list_price)
summary(log_width)
summary(log_height)
summary(log_followed)
c('std dev',sd(log_list_price))
c('std dev',sd(log_width))
c('std dev',sd(log_height))
c('std dev',sd(log_followed))

hist(log_list_price)
plot(log_width,log_list_price)
plot(log_height,log_list_price)
plot(log_followed,log_list_price)

# Indicators boxes
boxplot(split(log10(list_price),grad_ed), xlab="Grad Education", ylab="List Price")
boxplot(split(log10(list_price),developed_country), xlab="Developed Country", ylab="List Price")


#################################################################################################################################################################################


#################################################################################################################################################################################

cor(cbind(log_list_price,log_width,log_height,log_followed,grad_ed,developed_country))
model_price <- lm(log_list_price ~ log_width+log_height+log_followed+grad_ed+developed_country)

summary(model_price)

stdres <- rstandard(model_price)
hist(stdres)
plot(fitted(model_price),stdres,xlab="Fitted values",ylab="Standardized residuals")
plot(fitted(model_price))
lines(fitted(model_price))
qqnorm(stdres)

library(car)
vif(model_price)

#remove this points !!!
stdres[stdres>=2.3]
stdres[stdres<=-2.3]

data1 <- data1[-c(78,203,403,503,613,668,8,96,97,381,669,679,37,41,110,123,186,332,571,79,261,289,388,509),]
attach(data1)
log_list_price <- log10(list_price)
log_width <- log10(width)
log_height <- log10(height)
log_followed <- log10(num_followed)

cor(cbind(log_list_price,log_width,log_height,log_followed,grad_ed,developed_country))
model_price <- lm(log_list_price ~ log_width+log_height+log_followed+grad_ed+developed_country)


summary(model_price)

stdres <- rstandard(model_price)
hist(stdres)
plot(fitted(model_price),stdres,xlab="Fitted values",ylab="Standardized residuals")
plot(fitted(model_price))
lines(fitted(model_price))
qqnorm(stdres)


################################################################################################################################################################################################

library(leaps)

leaps(cbind(log_width,log_height,log_followed,grad_ed,developed_country),log_list_price,nbest=2)
leaps(cbind(log_width,log_height,log_followed,grad_ed,developed_country),log_list_price,nbest=2,method="adjr2")
leaps(cbind(log_width,log_height,log_followed,grad_ed,developed_country),log_list_price,nbest=2,method="r2")



extractAIC(lm(log_list_price~1))
extractAIC(lm(log_list_price~log_width))
extractAIC(lm(log_list_price~log_width+log_height))
extractAIC(lm(log_list_price~log_width+log_height+developed_country))
extractAIC(lm(log_list_price~log_width+log_height+log_followed+developed_country))

n <- length(log_list_price)
extractAIC(lm(log_list_price~1))+2*2*3/(n-3)
extractAIC(lm(log_list_price~log_width))+2*3*4/(n-4)
extractAIC(lm(log_list_price~log_width+log_height))+2*4*5/(n-5)
extractAIC(lm(log_list_price~log_width+log_height+developed_country))+2*5*6/(n-6)
extractAIC(lm(log_list_price~log_width+log_height+log_followed+developed_country))+2*6*7/(n-7)


################################################################################################################################

model_price_b <- lm(log_list_price~log_width+log_height+developed_country)

summary(model_price_b)
vif(model_price_b)

stdres_b <- rstandard(model_price_b)
hist(stdres_b)
plot(fitted(model_price_b),stdres_b,xlab="Fitted values",ylab="Standardized residuals")
plot(fitted(model_price_b))
lines(fitted(model_price_b))
qqnorm(stdres_b)


################################################################################################################################


model_price_b1 <- lm(log_list_price~log_width+log_height)
model_price_b2 <- lm(log_list_price~log_width+log_height+developed_country)

anova(model_price_b1,model_price_b2)





#################################################################################################################################################################################


#
# Function for fitted line plot
#
regplot.confbands.fun <- function(x,y,confidencelevel=.95,CImean=T,PI=T,CIregline=F,legend=F){
  #### Modified from a function written by Sandra McBride, Duke University
  #### For a simple linear regression line, this function
  #### will plot the line, CI for mean response, prediction intervals, 
  #### and (optionally) a simulataneous CI for the regression line.
  xx <- x[order(x)]
  yy <- y[order(x)]
  lm1 <- lm(yy~xx)	
  plot(xx,yy,ylim=c(min(yy),(max(yy)+.2*max(yy))))
  abline(lm1$coefficients)
  #### calculation of components of intervals ####
  n <- length(yy)
  sx2 <- (var(xx))
  shat <- summary(lm1)$sigma
  s2hat <- shat^2
  SEmuhat <- shat*sqrt(1/n+ ((xx-mean(xx))^2)/((n-1)*sx2))
  SEpred <- sqrt(s2hat+SEmuhat^2)
  t.quantile <- qt(confidencelevel,lm1$df.residual)
  ####
  if (CImean==T){
    mean.up <- lm1$fitted+t.quantile*SEmuhat
    mean.down <- lm1$fitted-t.quantile*SEmuhat
    lines(xx,mean.up,lty=2)
    lines(xx,mean.down,lty=2)
  }
  if (PI==T){
    PI.up <- lm1$fitted+t.quantile*SEpred
    PI.down <- lm1$fitted-t.quantile*SEpred
    lines(xx,PI.up,lty=3)
    lines(xx,PI.down,lty=3)
  }
  if (CIregline==T){
    HW <- sqrt(2*qf(confidencelevel,n-lm1$df.residual,lm1$df.residual))*SEmuhat	
    CIreg.up <- lm1$fitted+HW
    CIreg.down <- lm1$fitted-HW
    lines(xx,CIreg.up,lty=4)
    lines(xx,CIreg.down,lty=4)
  }	
  if (legend==T){
    choices <- c(CImean,PI,CIregline)
    line.type <- c(2,3,4)
    names.line <- c("Pointwise CI for mean resp.","Prediction Int.","Simultaneous conf. region for entire reg. line")
    legend(max(xx)-.2*max(xx),max(yy)+.2*max(yy),legend=names.line[choices],lty=line.type[choices])
  }
}

regplot.confbands.fun(data_filtered$variable,data_filtered$target)


###################################################################################################################################################################################################################################################################
# 6. Assumption validations: The main interest is realted to see if expected values of the error are zero, if there is a constant variance (this was analyzed since the first steps, and that is why the new target was considered), 
# and also, if error are not correlated (independent observations coming from normal distribution).
#

plot(c(1:length(residuals(data_regression))),residuals(data_regression),type="b",xlab='observations',ylab='residuals')


# Because the order is not based on time, then we cannot think about autocorrelation but we can confirm presence of outliers here, mostly in the first half of the observations.
# Such as in the following graph where residuals are mostly around 0.


plot(fitted(data_regression),residuals(data_regression),xlab='Fitted',ylab='Residuals')

qqnorm(residuals(data_regression))
qqnorm


# This last graph reflects that the error probability comes from a normal distribution, it seems to be a defined line between -2 and 2.
#
#################################################################################################################################################################################

