## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tsgarch)
suppressMessages(library(data.table))
suppressMessages(library(xts))
data(dmbp)
dmbp <- xts(dmbp, as.Date(1:nrow(dmbp), origin = '1970-01-01'))
spec <- garch_modelspec(dmbp[1:1500,1], model = 'fgarch', constant = TRUE, 
                        init = 'unconditional', distribution = 'jsu')
mod <- estimate(spec)
as_flextable(summary(mod))

## ----fig.width=7,fig.height=6-------------------------------------------------
oldpar <- par(mfrow = c(1,1))
par(mar = c(2,2,2,2))
plot(mod)
par(oldpar)

## ----fig.width=7,fig.height=4-------------------------------------------------
delta <- coef(mod)["delta"]
new_spec <- spec
new_spec$parmatrix <- copy(mod$parmatrix)
sim <- simulate(new_spec, nsim = 500, h = 10000, seed = 100, burn = 1000)
mean_sim <- mean(rowMeans(sim$sigma^delta))^(2/delta)
pred <- predict(mod, h = 1000, nsim = 0)
oldpar <- par(mfrow = c(1,1))
par(mar = c(2,2,2,2))
plot(as.numeric(pred$sigma^2), type = "l", xlab = "horizon", 
     ylab = expression(sigma^2), ylim = c(0.25, 0.41), main = "Family GARCH - JSU Prediction")
abline(h = as.numeric(unconditional(mod)), col = 2)
abline(h = mean_sim, col = 3)
legend("bottomright", c("h-step prediction","unconditional variance","simulated unconditional variance"), col = c(1,2,3), lty = 1, bty = "n")
par(oldpar)

