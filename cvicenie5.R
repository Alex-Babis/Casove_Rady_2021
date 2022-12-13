#kniznice
library(fpp3)   
library(fpp2)  
library(ggplot2)  
library(astsa)   
library(urca) 
library(forecast)  
library(lubridate) 

# Sezonne ARIMA modely

## Modelovanie nezamestnanosti v Amerike
leisure <- us_employment %>%
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) %>%
  mutate(Employed = Employed/1000) %>%
  select(Month, Employed)

leisure <- ts(leisure$Employed, start = c(2001,1), end = c(2019,9),
              frequency = 12)

autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

# odlozime si posledny rok dat na zhodnotenie predikcii
leisure.pred <- window(leisure, start = c(2018,10))

# budeme pracovat s kratsim casovym radom
leisure <- window(leisure, end = c(2018,9))

autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

leisure.d12 <- diff(leisure, lag = 12)


autoplot(leisure.d12) +
  labs(title = "Seasonally differenced - US employment: leisure and hospitality",
       y="Number of people (millions)")+
  geom_hline(yintercept = 0, color = "green")+
  geom_hline(yintercept = mean(leisure.d12), color = "orange")

acf2(leisure.d12)

summary(ur.df(leisure.d12, lags = 12, selectlags = "BIC", type = "drift"))

leisure.d12.d1 <- diff(leisure.d12)

autoplot(leisure.d12.d1) +
  labs(title = "Seasonally and first differenced - US employment: leisure and hospitality",
       y="Number of people (millions)")+
  geom_hline(yintercept = 0, color = "green")+
  geom_hline(yintercept = mean(leisure.d12.d1), color = "orange")

summary(ur.df(leisure.d12.d1, lags = 5, selectlags = "BIC", type = "none"))

autoplot(leisure.d12.d1) +
  labs(title = "Seasonally and first differenced - US employment: leisure and hospitality",
       y="Number of people (millions)")

acf2(leisure.d12.d1)

model.y <- sarima(leisure, 2 ,1, 0, 1, 1, 0, 12)
model.y <- sarima(leisure, 0, 1, 2, 0, 1, 1, 12) # zle rezidua
model.y <- sarima(leisure, 2, 1, 0, 0, 1, 1, 12)
model.y <- sarima(leisure, 0, 1, 2, 1, 1, 0, 12) # zle rezidua
model.y <- sarima(leisure, 2, 1, 1, 1, 1, 1, 12) # zle rezidua
model.y <- sarima(leisure, 2, 1, 0, 1, 1, 1, 12)

sarima(leisure, 2 ,1, 0, 1, 1, 0, 12, details = FALSE)$BIC
sarima(leisure, 2, 1, 0, 0, 1, 1, 12, details = FALSE)$BIC
sarima(leisure, 2, 1, 0, 1, 1, 1, 12, details = FALSE)$BIC

sarima.for(leisure,12, 2, 1, 0, 0, 1, 1, 12)
points(leisure.pred, col = "blue", type = "b")

## Modelovanie price indexu na SK
HIPC.SK <- read.table("https://alex-babis.github.io/Casove_Rady_2022/HIPC_SK_96.txt",
                      header = TRUE, sep = ",")
head(HIPC.SK)
hipc.ts <- ts(HIPC.SK[,1], frequency = 12, start = 1996)

hipc.ts.pred <- window(hipc.ts , start = 2021)
hipc.ts <- window(hipc.ts, end = c(2020,12))

plot(hipc.ts, type = "l")

plot(diff(hipc.ts) , type = "l")
abline(h = 0)
abline(h = mean( diff(hipc.ts)), col = "red")

summary(ur.df(diff(hipc.ts), type = "drift",
              lags = 13, selectlags = "BIC"))

plot(diff(diff(hipc.ts)) , type = "l")
abline(h = 0)
abline(h = mean( diff(diff(hipc.ts))), col = "red")

summary(ur.df(diff(diff(hipc.ts)), type = "none",
              lags = 12, selectlags = "BIC"))

# D = 2
acf2(diff(diff(hipc.ts)), max.lag = 12*5)

# MA(1) + sAR(1)
sarima(hipc.ts,p = 0,d = 2,q = 1,P = 1,D = 0,Q = 0,S = 12)

# MA(2) + sAR(1)
sarima(hipc.ts,p = 0,d = 2,q = 2,P = 1,D = 0,Q = 0,S = 12)

# ARMA(1,1) + sAR(1)
sarima(hipc.ts,p = 1,d = 2,q = 1,P = 1,D = 0,Q = 0,S = 12)

# MA(1) + sAR(2)
sarima(hipc.ts,p = 0,d = 2,q = 1,P = 2,D = 0,Q = 0,S = 12)


sarima(hipc.ts,p = 0,d = 2,q = 2,P = 1,D = 0,Q = 0,S = 12)$BIC
sarima(hipc.ts,p = 1,d = 2,q = 1,P = 1,D = 0,Q = 0,S = 12)$BIC

sarima.for(hipc.ts,23,p = 1,d = 2,q = 1,P = 1,D = 0,Q = 0,S = 12)
points(hipc.ts.pred, col = "green", type = "l")

## Modelovanie importu elektriny SK

el.import.sk <- read.table("https://alex-babis.github.io/Casove_Rady_2022/EL_IMPORT_SK_08.txt",
                      header = TRUE, sep = ",")

el.import.ts <- ts(el.import.sk[,1], frequency = 12, start = 2008)

el.import.pred <- window(el.import.ts, start = 2018, frequency = 12)
el.import.ts <- window(el.import.ts, end = c(2017,12))

acf2(el.import.ts)

autoplot(el.import.ts)

# modelovanie trendov
# a - level
# b - slope
# s - sezonne zlozky - rozne pre rozne mesiace

# exponencialne zhladzovanie
el.import.ts.d1 <- HoltWinters(el.import.ts,beta = FALSE, gamma = FALSE)
pred <- predict(el.import.ts.d1, n.ahead= 24, prediction.interval = TRUE,
                level = 0.95)

pallete <-  c("#D55E00","#009E73","#009E73", "gray", "gray")
pred.is <- pred %>% as_tsibble(pivot_longer = FALSE)
pred.is$index <- seq(2018,2020, by = 1/12)[-25]


autoplot(el.import.ts) +
  autolayer(el.import.ts.d1$fitted[,"xhat"], series="Fitted data") +
  autolayer(el.import.pred, series="Data") +
  autolayer(pred[,"fit"], series="Forecasts")+
  autolayer(pred[,"upr"], series="UB")+
  autolayer(pred[,"lwr"], series="LB")+
  geom_ribbon(data = pred.is, aes(x = index,y = fit, ymin = lwr, ymax = upr, fill = "0.95% IS"),
              alpha = 0.2)+
  scale_colour_manual(values=pallete)+
  scale_fill_manual(values = "gray")+
  theme(axis.title.y=element_blank())

# holtwinters bez sezonnosti


hipc.ts.HW <- HoltWinters(hipc.ts, gamma = FALSE)
pred <- predict(hipc.ts.HW, n.ahead= 23, prediction.interval = TRUE,
                level = 0.95)

pallete <-  c("#D55E00","#009E73","#009E73", "gray", "gray")
pred.is <- pred %>% as_tsibble(pivot_longer = FALSE)
pred.is$index <- seq(2021,2023, by = 1/12)[-c(24,25)]

autoplot(hipc.ts) +
  autolayer(hipc.ts.HW$fitted[,"xhat"], series="Fitted data") +
  autolayer(hipc.ts.pred, series="Data") +
  autolayer(pred[,"fit"], series="Forecasts")+
  autolayer(pred[,"upr"], series="UB")+
  autolayer(pred[,"lwr"], series="LB")+
  geom_ribbon(data = pred.is, aes(x = index,y = fit, ymin = lwr, ymax = upr, fill = "0.95% IS"),  alpha = 0.2)+
  scale_colour_manual(values=pallete)+
  scale_fill_manual(values = "gray")+
  theme(axis.title.y=element_blank())


# sezonny holtwinters
leisure <- us_employment %>%
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) %>%
  mutate(Employed = Employed/1000) %>%
  select(Month, Employed)

leisure <- ts(leisure$Employed, start = c(2001,1), end = c(2019,9),
              frequency = 12)

# odlozime si posledny rok dat na zhodnotenie predikcii
leisure.pred <- window(leisure, start = c(2017,10))

# budeme pracovat s kratsim casovym radom
leisure <- window(leisure, end = c(2017,9))

autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")



auscafe.pred <- window(auscafe, start = c(2015, 10))
auscafe.ts <- window(auscafe, end = c(2015, 9))
autoplot(log(auscafe.ts)) +
  labs(title = "The total monthly expenditure on services in Australia",
       y="", x = "")


## ADITIVNY 

exp.y <- HoltWinters(leisure, seasonal = "additive")
pred <- predict(exp.y, n.ahead= 24, prediction.interval = TRUE,
                level = 0.95)

pallete <-  c("#D55E00","#009E73","#009E73", "gray", "gray")
pred.is <- pred %>% as_tsibble(pivot_longer = FALSE)

pred.is$index <- lubridate::year(pred.is$index)+
  (lubridate::month(pred.is$index) - 1)/12

autoplot(leisure) +
  autolayer(exp.y$fitted[,"xhat"], series="Fitted data") +
  autolayer(leisure.pred, series="Data") +
  autolayer(pred[,"fit"], series="Forecasts")+
  autolayer(pred[,"upr"], series="UB")+
  autolayer(pred[,"lwr"], series="LB")+
  geom_ribbon(data = pred.is, aes(x = index,y = fit, ymin = lwr, ymax = upr, fill = "0.95% IS"),  alpha = 0.2)+
  scale_colour_manual(values=pallete)+
  scale_fill_manual(values = "gray")+
  theme(axis.title.y=element_blank())


## MULTIPLIKATIVNY
exp.y <- HoltWinters(auscafe.ts, seasonal = "multiplicative")
pred <- predict(exp.y, n.ahead= 24, prediction.interval = TRUE,
                level = 0.95)

pallete <-  c("#D55E00","#009E73","#009E73", "gray", "gray")
pred.is <- pred %>% as_tsibble(pivot_longer = FALSE)

pred.is$index <- lubridate::year(pred.is$index)+
  (lubridate::month(pred.is$index) - 1)/12

autoplot(auscafe.ts) +
  autolayer(exp.y$fitted[,"xhat"], series="Fitted data") +
  autolayer(auscafe.pred, series="Data") +
  autolayer(pred[,"fit"], series="Forecasts")+
  autolayer(pred[,"upr"], series="UB")+
  autolayer(pred[,"lwr"], series="LB")+
  geom_ribbon(data = pred.is, aes(x = index,y = fit, ymin = lwr, ymax = upr, fill = "0.95% IS"),  alpha = 0.2)+
  scale_colour_manual(values=pallete)+
  scale_fill_manual(values = "gray")+
  theme(axis.title.y=element_blank())



## Modelovanie pomocou ARMA + FOURIEROVYCH FREKVENCII

# HICP

plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(hipc.ts, xreg = fourier(hipc.ts, K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(hipc.ts, K=i, h=24))) +
    autolayer(hipc.ts.pred, series="Data") 
}

gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)

# leisure
plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(leisure, xreg = fourier(leisure, K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(leisure, K=i, h=24))) +
    autolayer(leisure.pred, series="Data") 
}

gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)


# auscafe
plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(auscafe.ts, xreg = fourier(auscafe.ts, K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(auscafe.ts, K=i, h=24))) +
    autolayer(auscafe.pred, series="Data") 
}

gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)
