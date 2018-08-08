setwd("C:/Users/micro/Documents/Tencent Files/2590755744/FileRecv")
getwd()
library(quantmod)

PFYH <- read.csv('PFYH.csv', header = TRUE)
PFYH <- xts(PFYH[, - c(1, 1)], order.by = as.Date(PFYH$Date))
head(PFYH, 3)
tail(PFYH, 3)

PFYH2017 <- PFYH["2017"]
chartSeries(PFYH2017,
            theme = 'white',
            name = '浦发银行2017年蜡烛图',
            up.col = 'red', dn.col = 'green')
addTA(Cl(PFYH2017), on = 1, col = "black", type = "l")

addTA(Cl(PFYH2017), col = "black", type = "l")

addTA(momentum(Cl(PFYH2017), n = 35, na.pad = TRUE), col = 4, type = "l")



Close <- PFYH$Close
names(Close) <- "vanke.Close"
Momen35 <- momentum(Close, n = 35, na.pad = FALSE)
names(Momen35) <- "momentum5"
head(Momen35)

signal <- ifelse(Momen35 < 0, -1, 1)
names(signal) <- "signal"
head(signal)

ret <- ROC(Close, 1)
names(ret) <- "PFYH.ret"
ret <- ret[-(1:35)]
head(ret)
Mom35Ret <- ret[-1] * lag(signal, 1, na.pad = FALSE)
names(Mom35Ret) <- "Mom35Ret"
head(Mom35Ret)

win <- Mom35Ret[Mom35Ret >= 0]
winrate <- length(win) / length(Mom35Ret)
winrate

ret <- ret[-1]
plot.zoo(merge(ret, Mom35Ret),
         col = c("black", "red"),
         main = "动量交易策略收益率时序图")

library(PerformanceAnalytics)
chart.CumReturns(merge(ret, Mom35Ret),
                 col = c("black", "red"), lty = c(1, 6),
                 main = "累计收益率")
legend("topleft", legend = c("浦发银行股票", "动量交易"),
                col = c("black", "red"), lty = c(1, 6))


win <- Mom35Ret[Mom35Ret >= 0]
loss <- Mom35Ret[Mom35Ret < 0]


par(mfrow = c(2, 1))
hist(win)
hist(loss, ylim = c(0, 80))

mean(win)
mean(loss)
summary(win)
summary(loss)
