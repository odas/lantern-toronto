rm(list=ls())
install.packages(c("xts", "ggplot2", "corrplot", "viridis", 
                   "fitdistrplus","actuar", "gamlss", "gamlss.dist", "gamlss.add",
                   "sde", "aTSA"))

# 1. Download USD & CAD data from LIBOR, multiple tenors, read in data. 
df_cad=read.csv('https://raw.githubusercontent.com/odas/lantern-toronto/master/R/cad.csv', sep = ',')
df_usd=read.csv('https://raw.githubusercontent.com/odas/lantern-toronto/master/R/usd.csv', sep = ',')

# 1.1 Convert the data into xts format, order by the 'DATE' column.
library(xts)
format_fun=function(x){as.numeric(as.character(x))}
data=apply(df_cad[,-1],2,format_fun)
cad_xts=xts(x=data,order.by=as.Date(df_cad$DATE, format = '%Y-%m-%d'))
data=apply(df_usd[,-1],2,format_fun)
usd_xts=xts(x=data,order.by=as.Date(df_usd$DATE, format = '%Y-%m-%d'))

# 2. Explore dataset & data quality issues
dim(cad_xts) # 3408,4
dim(usd_xts) # 5218, 4
apply(cad_xts, 2, class) # numeric
apply(usd_xts, 2, class) # numeric
str(cad_xts) # Timerange: 1999-12-06/2012-12-26
str(usd_xts) # Timerange: 1999-12-06/2019-12-04
sum(is.na(cad_xts)) # 12 
sum(is.na(usd_xts)) # 656
which(is.na(cad_xts))
which(is.na(usd_xts)) # Weekends and a few other time points have missing values.
 

# 2.1 Remove NAs. 
# Dataset is large we can eliminate rows with NA without biasing analysis. 
cad_xts=na.omit(cad_xts)
usd_xts=na.omit(usd_xts)

# 3. Plot timeseries of IRs & 1 & 25 day increments.
# 3.1 Plot the timeseries
library(viridis)
windows(12,8)
plot.zoo(cad_xts, plot.type = 'single', 
         col=viridis(5), main="Historical CAD Interest Rates", 
         ylab="Rates (%)", xlab="Time", lwd=2)
abline(v = as.Date(c("2007-12-01","2009-05-01")), 
       col='blue', lty=2)
legend(x = "topright", legend = colnames(cad_xts),
       col = viridis(5), cex = 0.8, lwd = 5)


plot.zoo(usd_xts, plot.type = 'single', 
         col=viridis(5), main="Historical USD Interest Rates", 
         ylab="Rates (%)", xlab="Time")
abline(v = as.Date(c("2007-12-01","2009-05-01", '2011-10-01', '2015-10-01')), 
       col=c('blue','blue','red','red'), lty=2)
legend(x = "topright", legend = colnames(usd_xts),
       col = viridis(5), cex = 0.8, lwd = 5)

# 3.2 Selecting stationary data (can be modeled with fewer parameters)
# Difference the time series - makes stationary, also makes rates time independant. 
diff1_cad <- diff.xts(cad_xts, lag=1)
diff25_cad <- diff.xts(cad_xts, lag=25)
diff1_cad=na.omit(diff1_cad)
diff25_cad=na.omit(diff25_cad)

diff1_usd <- diff.xts(usd_xts, lag=1)
diff25_usd <- diff.xts(usd_xts, lag=25)
diff1_usd=na.omit(diff1_usd)
diff25_usd=na.omit(diff25_usd)

# 3.3 Plot the differenced time series
plot.zoo(x = diff1_cad, plot.type = 'multiple',
         ylim = c(-.5, +.5), cex.axis = 0.7, col = viridis(5),
         xlab="Time", main="Lag 1 Differenced CAD Interest Rates")
plot.zoo(x = diff25_cad, plot.type = 'multiple', 
         ylim = c(-.5, +.5), cex.axis = 0.7,  col = viridis(5), 
         xlab="Time", main="Lag 25 Differenced CAD Interest Rates")

plot.zoo(x = diff1_usd, plot.type = 'multiple', 
         ylim = c(-.5, +.5), cex.axis = 0.7, col = viridis(5),
         xlab="Time", main="Lag 1 Differenced USD Interest Rates")
plot.zoo(x = diff25_usd, plot.type = 'multiple', 
         ylim = c(-.5, +.5), cex.axis = 0.7, col = viridis(5),
         xlab="Time", main="Lag 25 Differenced USD Interest Rates")

# 3.4 Rate-difference values are very small. Scale values (*100) and make x axis 
# values of histogram positive for convienience of later curve fitting steps.
shift=min(min(diff1_cad), min(diff25_cad))
for (col in names(diff1_cad)) {
  diff1_cad[,col]=(diff1_cad[,col]-shift)*100
}
for (col in names(diff25_cad)) {
  diff25_cad[,col]=(diff25_cad[,col]-shift)*100
}

shift=min(min(diff1_usd), min(diff25_usd))
for (col in names(diff1_usd)) {
  diff1_usd[,col]=(diff1_usd[,col]-shift)*100
}
for (col in names(diff25_usd)) {
  diff25_usd[,col]=(diff25_usd[,col]-shift)*100
}

# 3.5 Testing stationarity of data
# Type 2 and Type 3 ADF tests show stationary data. 
library(aTSA)
for (col in names(diff1_cad)) {
  adf.test(as.numeric(diff1_cad[,col]))
}
for (col in names(diff25_cad)) {
  adf.test(as.numeric(diff25_cad[,col]))
}

library(aTSA)
for (col in names(diff1_usd)) {
  adf.test(as.numeric(diff1_usd[,col]))
}
for (col in names(diff25_usd)) {
  adf.test(as.numeric(diff25_usd[,col]))
}

# 3.6 Partition dataset into training section. Working only with USD data.
# End goal is to fit data to Vasicek model (is sensitive to dramatic events)
# The financial crisis of 2009 led to volatility in dataset, I excluded
# period of extreme volatilities and selected a stable dataset after that. 
current_diff1 <- diff1_usd['2011-10/2016-10']
current_diff25 <- diff25_usd['2011-10/2016-10']

# 3.7 Testing stationarity of subsetted data
library(aTSA)
# Type 2 and Type 3 ADF tests show stationary data. 
for (col in names(current_diff1)) {
  adf.test(as.numeric(current_diff1[,col]))
}

# Type 2 and Type 3 ADF tests show close to stationary data. 
for (col in names(current_diff25)) {
  adf.test(as.numeric(current_diff25[,col]))
}

# 4. Plot histograms and fit several distributions. 
# Histograms show that data is left skewed and has high kurtosis. 
par(mfrow = c(2, 2))
for (col in names(current_diff1)) {
  hist(as.numeric(current_diff1[,col]),breaks = 100, main=col, 
       xlab='Scaled Positive Shifted 1 Lag USD Rate Differences')
}
par(mfrow = c(2, 2))
for (col in names(current_diff25)) {
  hist(as.numeric(current_diff25[,col]),breaks = 100, main=col, 
       xlab='Scaled Positive Shifted 25 Lag USD Rate Differences')
}

#  4.1 Check for normality
# Between -2 to 1st quantile data is fairly normal.
# But large number of outliers, don't follow normal distribution. 
par(mfrow = c(2, 2))
for (col in names(current_diff1)) {
  qqnorm(as.numeric(current_diff1[,col]),main = col)
  qqline(as.numeric(current_diff1[,col]),col = "red")
}

par(mfrow = c(2, 2))
for (col in names(current_diff25)) {
  qqnorm(as.numeric(current_diff25[,col]),main = col)
  qqline(as.numeric(current_diff25[,col]),col = "red")
}

# 4.2. Normal fit test 
# The Kolmogorov-Smirnov statistic is >0.05 significance level for all curves. 
# None of the curves are normally distributed. 

library(ggplot2)
library(fitdistrplus)
for (col in names(current_diff1)) {
  cat("\n 1 Lag ", col,':', "\n")
  normal.fit <- fitdist(as.numeric(current_diff1[,col]),"norm")
  gofstat.normal <- gofstat(normal.fit)
  cat('Kolmogorov-Smirnov statistic: ', gofstat.normal$ks, "\n")
  cat('AIC:', gofstat.normal$aic, "\n")
}

for (col in names(current_diff25)) {
  cat("\n 25 Lag", col,':', "\n")
  normal.fit <- fitdist(as.numeric(current_diff25[,col]),"norm")
  gofstat.normal <- gofstat(normal.fit)
  cat('Kolmogorov-Smirnov statistic: ', gofstat.normal$ks, "\n")
  cat('AIC:', gofstat.normal$aic, "\n")
}

# 4.3 Plotting fitted normal distributions.
# 1 Day Differenced data is far from normally distributed.
x=as.numeric(current_diff1$term_1mon)
qplot(x, geom = 'blank', main='term_1mon')+
  geom_histogram(aes(y = ..density..), alpha = 0.5)+ 
  geom_line(aes(y = ..density.., linetype = 'Empirical Density'), stat = 'density', size=0.8)+
  stat_function(fun = dnorm, aes(linetype = 'Normal Density'), 
                args=list(mean=mean(x), sd=sd(x)))+ 
  scale_linetype_discrete(name = "Densities") + 
  scale_x_continuous(name="Transformed 1 Lag Rate Difference")+ 
  scale_y_continuous(name="Density")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

x=as.numeric(current_diff1$term_3mon)
qplot(x, geom = 'blank', main='term_3mon')+
  geom_histogram(aes(y = ..density..), alpha = 0.5)+ 
  geom_line(aes(y = ..density.., linetype = 'Empirical Density'), stat = 'density', size=0.8)+
  stat_function(fun = dnorm, aes(linetype = 'Normal Density'), 
                args=list(mean=mean(x), sd=sd(x)))+ 
  scale_linetype_discrete(name = "Densities") + 
  scale_x_continuous(name="Transformed 1 Lag Rate Difference")+ 
  scale_y_continuous(name="Density")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))


x=as.numeric(current_diff1$term_6mon)
qplot(x, geom = 'blank', main='term_6mon')+
  geom_histogram(aes(y = ..density..), alpha = 0.5)+ 
  geom_line(aes(y = ..density.., linetype = 'Empirical Density'), stat = 'density', size=0.8)+
  stat_function(fun = dnorm, aes(linetype = 'Normal Density'), 
                args=list(mean=mean(x), sd=sd(x)))+ 
  scale_linetype_discrete(name = "Densities") + 
  scale_x_continuous(name="Transformed 1 Lag Rate Difference")+ 
  scale_y_continuous(name="Density")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

x=as.numeric(current_diff1$term_12mon)
qplot(x, geom = 'blank', main='term_12mon')+
  geom_histogram(aes(y = ..density..), alpha = 0.5)+ 
  geom_line(aes(y = ..density.., linetype = 'Empirical Density'), stat = 'density', size=0.8)+
  stat_function(fun = dnorm, aes(linetype = 'Normal Density'), 
                args=list(mean=mean(x), sd=sd(x)))+ 
  scale_linetype_discrete(name = "Densities") + 
  scale_x_continuous(name="Transformed 1 Lag Rate Difference")+ 
  scale_y_continuous(name="Density")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

# 25 Day Differenced data is far from normally distributed.
x=as.numeric(current_diff25$term_1mon)
qplot(x, geom = 'blank', main='term_1mon')+
  geom_histogram(aes(y = ..density..), alpha = 0.5)+ 
  geom_line(aes(y = ..density.., linetype = 'Empirical Density'), stat = 'density', size=0.8)+
  stat_function(fun = dnorm, aes(linetype = 'Normal Density'), 
                args=list(mean=mean(x), sd=sd(x)))+ 
  scale_linetype_discrete(name = "Densities") + 
  scale_x_continuous(name="Transformed 25 Lag Rate Difference")+ 
  scale_y_continuous(name="Density")+ 
  theme(legend.justification=c(1,1), legend.position=c(1,1))

x=as.numeric(current_diff25$usd_3)
qplot(x, geom = 'blank', main='usd_3')+
  geom_histogram(aes(y = ..density..), alpha = 0.5)+ 
  geom_line(aes(y = ..density.., linetype = 'Empirical Density'), stat = 'density', size=0.8)+
  stat_function(fun = dnorm, aes(linetype = 'Normal Density'), 
                args=list(mean=mean(x), sd=sd(x)))+ 
  scale_linetype_discrete(name = "Densities") + 
  scale_x_continuous(name="Transformed 25 Lag Rate Difference")+ 
  scale_y_continuous(name="Density")+ 
  theme(legend.justification=c(1,1), legend.position=c(1,1))

x=as.numeric(current_diff25$term_6mon)
qplot(x, geom = 'blank', main='term_6mon')+
  geom_histogram(aes(y = ..density..), alpha = 0.5)+ 
  geom_line(aes(y = ..density.., linetype = 'Empirical Density'), stat = 'density', size=0.8)+
  stat_function(fun = dnorm, aes(linetype = 'Normal Density'), 
                args=list(mean=mean(x), sd=sd(x)))+ 
  scale_linetype_discrete(name = "Densities") + 
  scale_x_continuous(name="Transformed 25 Lag Rate Difference")+ 
  scale_y_continuous(name="Density")+ 
  theme(legend.justification=c(1,1), legend.position=c(1,1))

x=as.numeric(current_diff25$term_12mon)
qplot(x, geom = 'blank', main='term_12mon')+
  geom_histogram(aes(y = ..density..), alpha = 0.5)+ 
  geom_line(aes(y = ..density.., linetype = 'Empirical Density'), stat = 'density', size=0.8)+
  stat_function(fun = dnorm, aes(linetype = 'Normal Density'), 
                args=list(mean=mean(x), sd=sd(x)))+ 
  scale_linetype_discrete(name = "Densities") + 
  scale_x_continuous(name="Transformed 25 Lag Rate Difference")+ 
  scale_y_continuous(name="Density")+
  theme(legend.justification=c(1,1), legend.position=c(1,1))


# 4.4 Identify best fit distribution of Interest rates
library(fitdistrplus)
library(actuar)

# Cullen & Frey graph doesn't shows a fit for 1 lag data.
par(mfrow = c(2, 2))
for (col in names(current_diff1)) {
  descdist(as.numeric(current_diff1[,col]), discrete=FALSE, boot=500)
}

# Cullen & Frey graph shows that 25 lag data is PERHAPS
# usd_1: beta (but values are not in 0-1 range for original or transformed data)
# usd_3: lognormal or weibull
# usd_6: lognormal or weibull
# usd_12: lognormal or weibull
par(mfrow = c(2, 2))
for (col in names(current_diff25)) {
  descdist(as.numeric(current_diff25[,col]), discrete=FALSE, boot=500)
}

# 4.5 Further studying fit of 25 Lag data
# Further analysis shows that LOGLOGISTIC IS A FAIRLY GOOD FIT FOR many terms
# When comparing AICs between weibull, logloistic and lognormal 
# distributions, loglogistic shows the lowest AIC. 
# USD_term_12mon data with a Kolmogorov-Smirnov (KS) statistic: 0.07014078 is 
# well represented by LOGLOGISTIC distribution. Other KS statistics don't make 
# the cut. 

for (col in names(current_diff25)) {
  my_data=as.numeric(current_diff25[,col])
  fit_w  <- fitdist(my_data, "weibull")
  fit_ll <- fitdist(my_data, "llogis", start = list(shape = 1, scale = 500))
  fit_ln <- fitdist(my_data, "lnorm")
  cat('\n 25 Lag ',col, 'Kolmogorov-Smirnov statistic: \n')
  print(gofstat(list(fit_w,fit_ll,fit_ln))$ks)
  cat('\n 25 Lag ',col, 'AIC: \n')
  print(gofstat(list(fit_w,fit_ll,fit_ln))$aic)
}

# Plotting loglogistic against term_6mon and term_12mon data
my_data=as.numeric(current_diff25$term_6mon)
fit_w  <- fitdist(my_data, "weibull")
fit_ll <- fitdist(my_data, "llogis", start = list(shape = 1, scale = 500))
fit_ln <- fitdist(my_data, "lnorm")
par(mfrow=c(2,2))
plot.legend <- c("Weibull", "loglogistic", "lognormal")
denscomp(list(fit_w, fit_ll, fit_ln), legendtext = plot.legend)
cdfcomp (list(fit_w, fit_ll, fit_ln), legendtext = plot.legend)
qqcomp  (list(fit_w, fit_ll, fit_ln), legendtext = plot.legend)
ppcomp  (list(fit_w, fit_ll, fit_ln), legendtext = plot.legend)

my_data=as.numeric(current_diff25$term_6mon)
fit_w  <- fitdist(my_data, "weibull")
fit_ll <- fitdist(my_data, "llogis", start = list(shape = 1, scale = 500))
fit_ln <- fitdist(my_data, "lnorm")
par(mfrow=c(2,2))
plot.legend <- c("Weibull", "loglogistic", "lognormal")
denscomp(list(fit_w, fit_ll, fit_ln), legendtext = plot.legend)
cdfcomp (list(fit_w, fit_ll, fit_ln), legendtext = plot.legend)
qqcomp  (list(fit_w, fit_ll, fit_ln), legendtext = plot.legend)
ppcomp  (list(fit_w, fit_ll, fit_ln), legendtext = plot.legend)


# 4.6 To find a better fit USE GAMLSS package.
# (Slow step - results in comments below) 
library(gamlss)
library(gamlss.dist)
library(gamlss.add)
for (col in names(current_diff1)) {
  fit <- fitDist(as.numeric(current_diff1[,col]), k = 2, type = "realline", trace = FALSE, try.gamlss = TRUE)
  cat("\n 1 Lag ",col,'\nBest Distribution : ', fit$family,'\nAIC' ,fit$aic)
}
for (col in names(current_diff25)) {
  fit <- fitDist(as.numeric(current_diff25[,col]), k = 2, type = "realline", trace = FALSE, try.gamlss = TRUE)
  cat("\n 25 Lag ",col,'\nBest Distribution : ', fit$family,'\nAIC' ,fit$aic)
}


# Best distributions
# 1 Lag  usd_1 : 
#  Kolmogorov-Smirnov statistic:  0.1796219 
# AIC: 190.8497 

# 1 Lag  usd_3 : 
#   Kolmogorov-Smirnov statistic:  0.1727051 
# AIC: 818.3076 

# 1 Lag  usd_6 : 
  #   Kolmogorov-Smirnov statistic:  0.1643026 
# AIC: 1569.676 

# 1 Lag  usd_12 : 
  #   Kolmogorov-Smirnov statistic:  0.1431065 
# AIC: 2555.915 

#  25 Lag  usd_1 
 # Best Distribution :  ST1 Skew t (Azzalini type 1) 
# AIC 4280.552

#  25 Lag  usd_3 
 # Best Distribution :  SHASHo Sinh-Arcsinh 
# AIC 6231.468

#  25 Lag  usd_6 
 # Best Distribution :  SEP2 Skew Exponential Power type 2 or loglogistic
# AIC 7261.117

#  25 Lag  usd_12 
 # Best Distribution :  SEP3 skew exponential power type 3 or loglogistic
# AIC 7918.774 

# Multiple distributions may be modelled to interest rate differences. 
# But the Vasicek model could perhaps do a better job as it takes into 
# account the drift rate and variance rate as functions of 
# current value and time.  

# 4.7 Untransform data
shift=min(min(diff1_cad), min(diff25_cad))
for (col in names(current_diff1)) {
  current_diff1[,col]=current_diff1[,col]/100+shift
}
for (col in names(current_diff25)) {
  current_diff25[,col]=current_diff25[,col]/100+shift
}

# 5. Term structure
# 5.1 Yearly mean
diff1_annual_mean=data.frame(matrix(NA, nrow = 6, ncol = 0))
for (col in names(current_diff1)) {
  diff1_annual_mean[,col]=period.apply(current_diff1[,col],INDEX=endpoints(current_diff1[,col], on='years'), FUN=mean)
}
format_fun=function(x){as.numeric(as.character(x))}
data=apply(diff1_annual_mean,2,format_fun)
diff1_annual_mean=xts(x=data,
                       order.by=as.Date(c('2011-12-30','2012-12-31',
                                          '2013-12-31','2014-12-31',
                                          '2015-12-31','2016-10-31')))

diff25_annual_mean=data.frame(matrix(NA, nrow = 6, ncol = 0))
for (col in names(current_diff25)) {
  diff25_annual_mean[,col]=period.apply(current_diff25[,col],INDEX=endpoints(current_diff25[,col], on='years'), FUN=mean)
}
format_fun=function(x){as.numeric(as.character(x))}
data=apply(diff25_annual_mean,2,format_fun)
diff25_annual_mean=xts(x=data,
                        order.by=as.Date(c('2011-12-30','2012-12-31',
                                           '2013-12-31','2014-12-31',
                                           '2015-12-31','2016-10-31')))
# 5.2 Plot Average Term structure of LIBOR 
par(mfrow = c(3, 2))
for (row in index(diff1_annual_mean)) {
  df2=t(diff1_annual_mean[as.Date(row),])
  plot(df2, type='b', xaxt='n', xlab='Time', 
       ylab='Mean Annual Rate difference',main=as.Date(row))
  axis(1, 1:4, labels = names(diff1_annual_mean), col.axis = "blue")
}


# 6. Yearly standard deviation
diff1_annual_stdev=data.frame(matrix(NA, nrow = 6, ncol = 0))
for (col in names(current_diff1)) {
  diff1_annual_stdev[,col]=period.apply(current_diff1[,col],INDEX=endpoints(current_diff1[,col], on='years'), FUN=sd)
}
format_fun=function(x){as.numeric(as.character(x))}
data=apply(diff1_annual_stdev,2,format_fun)
diff1_annual_stdev=xts(x=data,
               order.by=as.Date(c('2011-12-30','2012-12-31',
                                  '2013-12-31','2014-12-31',
                                  '2015-12-31','2016-10-31')))

diff25_annual_stdev=data.frame(matrix(NA, nrow = 6, ncol = 0))
for (col in names(current_diff25)) {
  diff25_annual_stdev[,col]=period.apply(current_diff25[,col],INDEX=endpoints(current_diff25[,col], on='years'), FUN=sd)
}
format_fun=function(x){as.numeric(as.character(x))}
data=apply(diff25_annual_stdev,2,format_fun)
diff25_annual_stdev=xts(x=data,
                       order.by=as.Date(c('2011-12-30','2012-12-31',
                                          '2013-12-31','2014-12-31',
                                          '2015-12-31','2016-10-31')))

# 6.1 Relationship between SD, lag & tenor


# 7. Correlation matrices
library(corrplot)
corrplot(cor(current_diff1, method = c("kendall")), method='number', type = "upper")
corrplot(cor(current_diff25, method = c("kendall")), method='number', type = "upper")

# 8. PCA
pca_diff1=prcomp(current_diff1, scale = FALSE)
plot(pca_diff1, type="l")
summary(pca_diff1)

pca_diff25=prcomp(current_diff25, scale = FALSE)
plot(pca_diff25, type="l")
summary(pca_diff25)

# 9. Use sde package, fit Vasicek model to each tenor
library(sde)
# 9.1 Function for Calibration using Maximum Likelihood estimates
ouFit.ML = function(spread) {
  n = length(spread)
  delta = 1  # delta 
  Sx = sum(spread[1:n - 1])
  Sy = sum(spread[2:n])
  Sxx = sum((spread[1:n - 1])^2)
  Syy = sum((spread[2:n])^2)
  Sxy = sum((spread[1:n - 1]) * (spread[2:n]))
  mu = (Sy * Sxx - Sx * Sxy)/((n - 1) * (Sxx - Sxy) - (Sx^2 - Sx * Sy))
  theta = -log((Sxy - mu * Sx - mu * Sy + (n - 1) * mu^2)/(Sxx - 2 * mu * 
                                                             Sx + (n - 1) * mu^2))/delta
  a = exp(-theta * delta)
  sigmah2 = (Syy - 2 * a * Sxy + a^2 * Sxx - 2 * mu * (1 - a) * (Sy - a * 
                                                                   Sx) + (n - 1) * mu^2 * (1 - a)^2)/(n - 1)
  sigma = sqrt((sigmah2) * 2 * theta/(1 - a^2))
  theta = list(theta = theta, mu = mu, sigma = sigma, sigmah2 = sigmah2)
  return(theta)
}

# 9.2 Simulate the 1 Lag USD data
# X0 is the mean of training data
sim_diff1=data.frame(matrix(NA, nrow = 1285, ncol = 0))
for (col in names(current_diff1)) {
  estimate=ouFit.ML(as.numeric(current_diff1[,col]))
  thetaw <- c(estimate[[1]] * estimate[[2]], estimate[[1]], estimate[[3]])
  sim_diff1[,col] <- sde.sim(X0 = estimate[[2]], model = "OU", theta = thetaw, N = 1284, delta = 1)
}

format_fun=function(x){as.numeric(as.character(x))}
data=apply(sim_diff1,2,format_fun)
sim_diff1=xts(x=data,order.by=as.Date(index(current_diff1[1:1285,]), format = '%Y-%m-%d'))

# 9.3 Simulate the 25 Lag USD data
# X0 is the mean of training data.
sim_diff25=data.frame(matrix(NA, nrow = 1285, ncol = 0))
for (col in names(current_diff25)) {
  estimate=ouFit.ML(as.numeric(current_diff1[,col]))
  thetaw <- c(estimate[[1]] * estimate[[2]], estimate[[1]], estimate[[3]])
  sim_diff25[,col] <- sde.sim(X0 = estimate[[2]] , model = "OU", theta = thetaw, N = 1284, delta = 1)
}
format_fun=function(x){as.numeric(as.character(x))}
data=apply(sim_diff25,2,format_fun)
sim_diff25=xts(x=data,order.by=as.Date(index(current_diff25[1:1285,]), format = '%Y-%m-%d'))


# 9.4 Plot the simulated and original data

color=c('black','black','black','black','red','red','red','red')
z=cbind(current_diff1,sim_diff1)
plot.zoo(z, screen = c(1,2,3,4,1,2,3,4), col = color, 
         xlab='Time', main="Original (black) vs Simulated (red) 1 Lag Rate Differences")


zz=cbind(current_diff25,sim_diff25)
plot.zoo(zz, screen = c(1,2,3,4,1,2,3,4), col = color, 
         xlab='Time', main="Original (black) vs Simulated (red) 25 Lag Rate Differences")


# 10. Combine fitted Vasicek model by using correlated Brownian motions
# simulated using the first three principal components. 
# Leverage Jamshedian and Zhu (1997)

