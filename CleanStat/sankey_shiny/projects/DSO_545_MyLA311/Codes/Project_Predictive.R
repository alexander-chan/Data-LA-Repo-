cycletime = datasql %>%
  select(CreatedDate,UpdatedDate,RequestType,RequestSource,CycleTime,Month,Year,Day)

cycletime_data$Month = month(as.numeric(CT_data$Month),label = T, abbr = F)
write.csv(CT_data,"/Users/pksharma/Desktop/DSO 545 Project/CycleTime.csv")

CT_data = cycletime



CT_data$Month = as.ts(CT_data$Month)


ct = CT_data %>%
  group_by(Year,Month) %>%
  summarise(CountCT = mean(CycleTime)) %>%
  arrange(Year)

fore$pred
ct = drop(ct$CountCT)

r <- ts(ct,frequency = 1)
r
plot(r)
rd <- decompose(r,type = c("additive", "multiplicative"))
plot(rd)

str(r)
fit <- arima(r,order = c(1,0,0), list(order = c(2,1,0)))
fit

fore = predict(fit, n.ahead = 12)
fore

U <- fore$pred + 1.96*fore$se
L <- fore$pred - 1.96*fore$se

ts.plot(r,fore$pred,U,L, col = c(1,2,4,4), lty = c(1,1,2,2))
title("CycleTime")
legend("topleft", col = c(1,2,4), lty = c(1,1,2),
       c("Actual","Forecast","95% CI"))


ct1 = as.array(ct1)

ts.plot(ct1)
ts.plot(fore$pred)
