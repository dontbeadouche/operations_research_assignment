library(arm)
library(Metrics)
icecream <- data.frame(
  temp=c(11.9, 14.2, 15.2, 16.4, 17.2, 18.1, 
         18.5, 19.4, 22.1, 22.6, 23.4, 25.1),
  units=c(185L, 215L, 332L, 325L, 408L, 421L, 
          406L, 412L, 522L, 445L, 544L, 614L)
)
basicPlot <- function(...){
  plot(units ~ temp, data=icecream, bty="n", lwd=2,
       main="Ice cream units sold", col="#00526D", 
       xlab="Temperatur (Celsius)", 
       ylab="Units sold", ...)
  axis(side = 1, col="grey")
  axis(side = 2, col="grey")
}
basicPlot()

lin.mod <- glm(units ~ temp, data=icecream, 
               family=gaussian(link="identity"))
display(lin.mod)

basicPlot()
abline(lin.mod, col="orange", lwd=2)
legend(x="topleft", bty="n", lwd=c(2,2), lty=c(NA,1),
       legend=c("observation", "linear regression"),
       col=c("#00526D","orange"),  pch=c(1,NA))

log.lin.mod <- glm(log(units) ~ temp, data=icecream, 
                   family=gaussian(link="identity"))
display(log.lin.mod)

log.lin.sig <- summary(log.lin.mod)$dispersion
log.lin.pred <- exp(predict(log.lin.mod) + log.lin.sig)
basicPlot()
lines(icecream$temp, log.lin.pred, col="red", lwd=2)
legend(x="topleft", bty="n", lwd=c(2,2), lty=c(NA,1),
       legend=c("observation", "log-transformed LM"),
       col=c("#00526D","red"),  pch=c(1,NA))

pois.mod <- glm(units ~ temp, data=icecream, 
                family=poisson(link="log"))
display(pois.mod)

pois.pred <- predict(pois.mod, type="response")
basicPlot()
lines(icecream$temp, pois.pred, col="blue", lwd=2)
legend(x="topleft", bty="n", lwd=c(2,2), lty=c(NA,1),
       legend=c("observation", "Poisson (log) GLM"),
       col=c("#00526D","blue"),  pch=c(1,NA))

market.size <- 1000
icecream$opportunity <- market.size - icecream$units
bin.glm <- glm(cbind(units, opportunity) ~ temp, data=icecream, 
               family=binomial(link = "logit"))
display(bin.glm)

bin.pred <- predict(bin.glm, type="response")*market.size
basicPlot()
lines(icecream$temp, bin.pred, col="purple", lwd=2)
legend(x="topleft", bty="n", lwd=c(2,2), lty=c(NA,1),
       legend=c("observation", "Binomial (logit) GLM"),
       col=c("#00526D","purple"),  pch=c(1,NA))

temp <- 0:35
p.lm <- predict(lin.mod, data.frame(temp=temp), type="response")
p.log.lm <- exp(predict(log.lin.mod, data.frame(temp=0:35), type="response") + 
                  0.5 * summary(log.lin.mod)$dispersion)
p.pois <- predict(pois.mod, data.frame(temp=temp), type="response")
p.bin <- predict(bin.glm, data.frame(temp=temp), type="response")*market.size 
basicPlot(xlim=range(temp), ylim=c(-20,market.size))
lines(temp, p.lm, type="l", col="orange", lwd=2)
lines(temp, p.log.lm, type="l", col="red", lwd=2)
lines(temp, p.pois, type="l", col="blue", lwd=2)
lines(temp, p.bin, type="l", col="purple", lwd=2)
legend(x="topleft", 
       legend=c("observation", 
                "linear model",
                "log-transformed LM",
                "Poisson (log) GLM",
                "Binomial (logit) GLM"),
       col=c("#00526D","orange", "red", 
             "blue", "purple"),  
       bty="n", lwd=rep(2,5), 
       lty=c(NA,rep(1,4)),
       pch=c(1,rep(NA,4)))
names(p.lm)
rss.lin <- c(crossprod(lin.mod$residuals))
rss.log <- c(crossprod(log.lin.mod$residuals))
rss.pois <- c(crossprod(pois.mod$residuals))
rss.bin <- c(crossprod(bin.glm$residuals))

mse.lin <- rss.lin / length(lin.mod$residuals)
mse.log <- rss.log / length(log.lin.mod$residuals)
mse.pois <- rss.pois / length(pois.mod$residuals)
mse.bin <- rss.bin / length(bin.glm$residuals)

rmse.lin <- sqrt(mse.lin)
rmse.log <- sqrt(mse.log)
rmse.pois <- sqrt(mse.pois)
rmse.bin <- sqrt(mse.bin)
