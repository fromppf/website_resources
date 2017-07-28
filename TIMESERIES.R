
# MARIA LEONOR ZAMORA MAASS  - MZM239
# NOVEMBER , 2016
# TIME SERIES
# Good reference:  https://www.otexts.org/fpp/

############################################ DATASETS ############################################

## Read the file with dataset ONE (CYCLIC, NOT SEASONAL)
## A cyclic pattern exists when data rises and falls not in fixed period (each January e.g). 
## The duration of these fluctuations is usually of at least 2 years.

path <- "C:/website_resources/scrap_docs/champagne-sales.csv"
original_data <- read.csv(path, header=TRUE) 
summary(original_data)

plot(as.ts(original_data$Champagne.Sales), xaxt = "n")
axis(1, at=1:length(original_data$ï..Month), labels=FALSE)
text(seq(1, length(original_data$ï..Month), by=1), par("usr")[3] - 0.2, 
     labels = original_data$ï..Month, srt = 90, pos = 2, xpd = TRUE)

# This dataset is "multiplicative error", "no trend", "multiplicative season/cycle"



## Load dataset TWO 

library(Ecdat)
data(AirPassengers)
timeserie_air = AirPassengers
summary(original_data)

plot(as.ts(timeserie_air))


############################################  COMPONENTS #################################################### 

## COMPONENTS: RROR, TREND AND SEASONALITY
## WE CAN APPLY TO EACH OF THOSE ADJUSTMENTS:
## LINEAR (+-) ...OR... EXPONENTIAL (X) 
## ADDITIVE Time series = Seasonal + Trend + Random 
## MULTIPLICATIVE Time Series = Time series = Trend * Seasonal *Random

## Error: constant variance over time then addivitely

## OPTIONS:

## No-Trend, No-Seasonal
## No-Trend, Seasonal-Constant
## No-Trend, Seasonal-Increasing
## Trend-Linear,No-Seasonal
## Trend-Linear,Seasonal-Constant
## Trend-Linear,Seasonal-Increasing
## Trend-Exponential,No-Seasonal
## Trend-Exponential,Seasonal-Constant
## Trend-Exponential,Seasonal-Increasing



#########  ANALYZE COMPONENTS

library(forecast)

## ets() fits models with Error, Trend, and Seasonality
## forecast() takes a model already fitted and makes forecasts
## all included in "ses(x, h=3)" this runs a forecast (3 more points) on an "ets(x, model="ANN")" 
## where x is the time series and "ANN" means additive-error, no-trend, no-seasonality -> that's why is simple by definition


####  Option to get ALL components

decomposedRes <- decompose(as.ts(timeserie_air), "additive") 
plot (decomposedRes) 
decomposedRes <- decompose(as.ts(timeserie_air), "multiplicative") 
plot (decomposedRes) 

stlRes <- stl(timeserie_air, s.window = "periodic")
plot(stlRes)


####  Option to get SEASONAL components

ts.stl <- stl(as.ts(timeserie_air),"periodic")           # decompose the TS in periods
ts.sa <- seasadj(ts.stl)                                 # de-seasonalize

plot(as.ts(timeserie_air), type="l")                     # original 
plot(ts.sa, type="l")                                    # seasonal adjusted

seasonplot(ts.sa, 12, col=rainbow(12), year.labels=TRUE, main="Seasonal plot: Sales") 


####  Option to get TREND component

trend_air = ma(timeserie_air, order = 12, centre = T)
plot(as.ts(timeserie_air),col='red')
lines(trend_air)

detrend_air = timeserie_air / trend_air
plot(as.ts(detrend_air))

# quarterly seasonality: matrix of 4 rows. average  16 times 
m_air = t(matrix(data = detrend_air, nrow = 4))
seasonal_air = colMeans(m_air, na.rm = T)
plot(as.ts(rep(seasonal_air,16)))

# multiplicative with monthly seasonality: matrix of 12 rows. average  12 times
m_air = t(matrix(data = detrend_air, nrow = 12))
seasonal_air = colMeans(m_air, na.rm = T)
plot(as.ts(rep(seasonal_air,12)))


####  Option to get the NOISE (error)
random_air = timeserie_air / (trend_air * seasonal_air)
plot(as.ts(random_air),col='red')

## Reconstruct from the components
recomposed_air = trend_air*seasonal_air*random_air
plot(as.ts(recomposed_air),col = 'purple')
lines(as.ts(timeserie_air))




################################# ETS MODELS (Error, Trend, Seasonality) #####################################


## 1. Simple Exponential Smoothing (SES): To get the level of a time-series, expected for no-trend and no-season
##    assumption: forecasts should be done using decreasing weights back 
##    solution: Maximum likelihood estimation or minimum MSE
##    Yt = alpha Yt + (1-alpha) Yt-1
##    error at time t is a weighted average of the current data value and the time t-1 
##    this means: next value is equal to the current estimate

## 2. Holt's Linear Trend (double exponential smooth): Get the level, expected for (+-) linear trend (no-seasonality)
##    Forecast: Yt+h = Lt + h(Bt)     where h is the forecast length
##    Level: Lt = alpha Yt + (1-alpha) (Lt-1 + Bt-1)
##    Trend: Bt = beta (Lt - Lt-1) + (1 - beta) Bt-1

## 3. Exponential Trend Smoothing (Holt's exp): Get the level, expected for (x) exponential trend (no-seasonality)
##    Forecast: Yt+h = Lt + (phi + phi^2 + ...+ phi^h)(Bt)     where h is the forecast length
##    Level: Lt = alpha Yt + (1-alpha) (Lt-1 + phi Bt-1)
##    Trend: Bt = beta (Lt - Lt-1) + (1 - beta) phi Bt-1
##    phi should be less than 1 and it moves velocity of the trend (phi=1 is Holt and phi=0 is SES)

## 4. Holt-Winters Seasonal: Get the level, expected for trend and any type of seasonality (X) and (+-)
##    Forecast: Yt+h = Lt + h(Bt) + St+h-m    where h is the forecast length
##    Level: Lt = alpha (Yt - St-m) + (1-alpha) (Lt-1 + Bt-1)
##    Trend: Bt = beta (Lt - Lt-1) + (1 - beta) Bt-1
##    Season: St = gamma (Yt - Lt-q - Bt-1)  +  (1 - gamma) St-m


################ VALIDATION

# Look at in-sample error measures, particularly RMSE (Root-Mean-Square Error) and MASE (Mean Absolute Scaled Error)
# Minimize error selecting an ETS model with lowest AIC value
# Use best ETS model to forecast . Make sure to leave holdout sample (leave last observations for testing - most recent)
# Plot results with 80% and 95% confidence intervals (shadow)



################ Run a Simple exponential smoothing

model_ses <- ses(original_data$Champagne.Sales, h=5)
summary(model_ses)

plot(model_ses, ylab="Sales", xlab="Year-Month")
lines(fitted(model_ses), col="purple", type="l")
legend("topleft", c("observed","fitted"), lty=1, col=c(1,4))


## SAS similarity:
## proc esm data=<input-data-set> out=<output-data-set>
##   back=<n> lead=<n> ;
## id <time-ID-variable> interval=<frequency>;
## forecast <time-series-variables>;
## run;




############### Run a Holt's method and forecast for "A A N"

model_holt = holt(original_data$Champagne.Sales, h=5)
summary(model_holt)

plot(model_holt, xlab="Year-Month", ylab="Sales", type="o", pch=16)


## Forecast variable
class(model_holt) 
## Model variable
class(model_holt$model) 

AIC(model_holt$model)


## SAS implementation with data transformartion
## TITLEmy "Holt method"
## DATA datamy;
## INFILE champ;
## INPUT champ @@;
## date = INTNX('month', '01jan16'd, _N_-1);
## FORMAT date yy-mon.;
## RUN;
## TITLE2 "Holt method";
## PROC ESM DATA=champ OUT=champPrediction PRINT=(FORECASTS)
## LEAD=48 PLOT=(MODELFORECASTS);
## ID date interval = MONTH;
## FORECAST champ / MODEL=LINEAR ALPHA=0.05;
## ODS OUTPUT FORECASTS=fcChamp;
## RUN;




############## Run exponential smoothing with trend (damped)  ->  multiplicative

model_exp_damped = holt(original_data$Champagne.Sales, h=5, exponential=TRUE, damped=TRUE)
summary(model_exp_damped)

plot(model_exp_damped , xlab="Year-Month", ylab="Sales", type="o", pch=16)

AIC(model_holt$model_exp_damped)



############## RUN MULTIPLICATIVE     E R R O R

model_multiptrend = ets(original_data$Champagne.Sales, model="MMN")  
plot(model_multiptrend, xlab="Year", ylab="Sales",type="o", pch=16)




############# Run Holt-Winters exponential smoothing (seasonality)

# This one is with the second dataset

model_holtwinter = hw(as.ts(timeserie_air), seasonal="additive", h=2, damped=TRUE)
summary(model_holtwinter)

plot(model_holtwinter, xlab="Year", ylab="Sales",type="o", pch=16)





###################################### ARIMA MODELS  #########################################################











## ETS Notes: http://www.stat.cmu.edu/~hseltman/618/LNTS5.pdf
## ARIMA Notes: http://www.stat.cmu.edu/~hseltman/618/LNTS6.pdf
## http://www.stat.cmu.edu/~hseltman/618/LNTS7.pdf


