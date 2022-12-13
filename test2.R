library(eurostat)
toc <- get_eurostat_toc()

id <- search_eurostat("Unemployment - LFS adjusted series - historical data (1992-2020)",
                      type = "folder"
)$code[1]


write.table(as.vector(data_new1$values), "", append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)



# HIPC
dat1 <- get_eurostat("prc_hicp_midx", time_format = "num")
dat1$geo=="SK"
data_new <- dat1
data_new <- dat1[dat1$geo=="SK",]
head(data_new)
str(data_new)

apply(data_new[,1:3], 2, unique)

data_new1 <- data_new[data_new$unit == "I05" &
                        data_new$coicop == "CP00", c("values","time")]

data_new1 <- apply(data_new1 , 2, rev)

data_new1 <- data_new[data_new$indic == "BS-SFSH" &
                        data_new$s_adj == "SA" &
                        data_new$time > 2003, c("values","time")]
data_new1 <- apply(data_new1 , 2, rev)

plot(data_new1[,2], data_new1[,1] , type = "l")
abline(h = 0)
abline(h = mean( data_new1[,1]), col = "red")


plot(data_new1[-1,2], diff(data_new1[,1]) , type = "l")
abline(h = 0)
abline(h = mean( diff(data_new1[,1])), col = "red")

library(urca)

summary(ur.df(diff(data_new1[,1]), type = "drift",
              lags = 13, selectlags = "BIC"))
acf2((data_new1[,1]))
auto.arima(data_new1[,1], seasonal = TRUE)
acf2(diff(data_new1[,1]))
sarima(data_new1[,1],3,1,0)

summary(ur.df(diff(data_new1[,1]), type = "drift",
              lags = 8, selectlags = "BIC"))

acf2(diff(diff(data_new1[,1])))



write.csv(data_new1, "Financial_situation_household_2003_SK.txt", row.names = FALSE)


# data cvicenie 5 
# HIPC
dat1 <- get_eurostat("prc_hicp_midx", time_format = "num")
dat1$geo=="SK"
data_new <- dat1
data_new <- dat1[dat1$geo=="SK",]
head(data_new)
str(data_new)

apply(data_new[,1:3], 2, unique)

data_new1 <- data_new[data_new$unit == "I15" &
                        data_new$coicop == "CP00", c("values","time")]

data_new1 <- apply(data_new1 , 2, rev)

plot(data_new1[,2], data_new1[,1] , type = "l")
abline(h = 0)
abline(h = mean( data_new1[,1]), col = "red")


plot(data_new1[-1,2], diff(data_new1[,1]) , type = "l")
abline(h = 0)
abline(h = mean( diff(data_new1[,1])), col = "red")

library(urca)
library(astsa)

summary(ur.df(diff(data_new1[,1]), type = "drift",
              lags = 12, selectlags = "BIC"))
acf2((data_new1[,1]))
auto.arima(data_new1[,1], seasonal = TRUE)
acf2(diff(data_new1[,1]))
sarima(data_new1[,1],3,1,0)

summary(ur.df(diff(data_new1[,1]), type = "drift",
              lags = 8, selectlags = "BIC"))

plot(data_new1[-c(1:13),2], diff(diff(data_new1[,1]), lag = 12) , type = "l")
abline(h = 0)
abline(h = mean( diff(diff(data_new1[,1]))), col = "red")

summary(ur.df( diff(diff(data_new1[,1])), type = "none",
              lags = 12, selectlags = "BIC"))


acf2(diff(diff(data_new1[,1])), max.lag = 48)
sarima(data_new1[,1],p = 0,d = 2,q = 1,P = 2,D = 0,Q = 0,S = 12)$BIC
sarima(data_new1[,1],p = 1,d = 2,q = 1,P = 1,D = 0,Q = 1,S = 12)$BIC 


library(forecast)
data.ts <- ts(data_new1[,1], frequency = 12)
auto.arima(data_new1[,1], seasonal = TRUE )

sarima(data_new1[,1],p = 1,d = 1,q = 1)
sarima(data_new1[,1],p = 1,d = 1,q = 1,P = 0,D = 1,Q = 1,S = 12) 

#write.csv(data_new1, "HIPC_SK_96.txt", row.names = FALSE)

# fourierova metoda 
ts.data <- ts(data_new1[,1],frequency = 12, start= c(1996,1))
plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(ts.data, xreg = fourier(ts.data, K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(ts.data, K=i, h=24))) 
}

gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)

sarima(ts.data,1,2,1,xreg = fourier(ts.data, K = i))

# data 2
# irt_h_euryld_d
dat1 <- get_eurostat("lfsi_abs_w", time_format = "num")
dat1$geo=="SK"
data_new <- dat1
data_new <- dat1[dat1$geo=="SK",]
head(data_new)
str(data_new)

data_new1 <- data_new[data_new$age == "Y20-64" &
                        data_new$sex == "T" , c("values","time")]

data_new1 <- na.omit(data_new1)
data_new1 <- apply(data_new1 , 2, rev)
head(data_new1)
data_new1[,1] <- as.numeric(data_new1[,1] )
ts.data <- ts(as.numeric(data_new1[,1] ), frequency = 52, start = 1991)
plot(ts.data, type = "l" )

acf2(diff(ts.data), max.lag = 60)

auto.arima(ts.data)
sarima(ts.data,1,1,2,0,0,1,52)


plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(ts.data, xreg = fourier(ts.data, K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(ts.data, K=i, h=24))) 
}

gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)

acf2(diff(ts.data, lag = 52))
fit <- sarima(ts.data,1,0,1,0,1,1,52,xreg = fourier(ts.data, K = 20))
sarima(ts.data,3,0,3,1,1,1,52)
autoplot(sarima.for(ts.data,20,2,1,2,1,0,0,52,xreg = fourier(ts.data, K=20),
                    newxreg = fourier(ts.data, K=20, h = 20)) )



# data 3
# irt_h_euryld_d
dat1 <- get_eurostat("nrg_105m", time_format = "num")
dat1$geo=="SK"
data_new <- dat1
data_new <- dat1[dat1$geo=="SK",]
head(data_new)
str(data_new)

data_new1 <- data_new[data_new$indic_nrg == "B_100300" , c("values","time")]


data_new1 <- apply(data_new1 , 2, rev)
head(data_new1)

ts.data <- ts(data_new1[,1] , frequency = 12, start = 2008)
plot(ts.data, type = "l" )

acf2(diff(ts.data), max.lag = 60)

auto.arima(ts.data)
sarima(ts.data,2,1,0,2,0,0,12)

#write.csv(data_new1, "EL_IMPORT_SK_08.txt", row.names = FALSE)
library(fpp3)

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

acf2(diff(leisure))

sarima(leisure,2,1,0,1,1,0,12) 
sarima.for(leisure,12,3,1,0) 

sarima(leisure,2,1,0,1,1,0,12,xreg = fourier(leisure, K=1)) 
sarima.for(leisure,12,2,1,0,0,1,0,12,xreg = fourier(leisure, K=1),
           newxreg = fourier(leisure, K=1, h = 12)) 

sarima.for(leisure,12,21,1,0,0,1,0,12,xreg = fourier(leisure, K=4),
           newxreg = fourier(leisure, K=4, h = 12)) 

sarima.for(leisure,12,1,1,0,0,1,0,12,xreg = fourier(leisure, K=6),
           newxreg = fourier(leisure, K=6, h = 12)) 



