library(ggplot2)
library(gridExtra)

summary(ww)

# Distribution of each variable
dens <- lapply(ORIGINAL, FUN=function(var) {
  ggplot(ww, aes_string(x=var)) + geom_density()
  })
do.call(grid.arrange, args=c(dens, list(ncol=3)))

# Standardized data.frame
ww_scaled <- data.frame(scale(ww[, ORIGINAL]))
summary(ww_scaled)

# Check normality by Q-Q plot
par(mfrow=c(4,3), mar=c(2,2,2,2))
lapply(ORIGINAL, FUN=function(var) {
  qqnorm(ww_scaled[, var], main=var)
  qqline(ww_scaled[, var])
})

# Distribution of each standardized variable by table
sd.cls <- lapply(ORIGINAL, FUN=function(var) {
  cut(ww_scaled[, var], breaks=seq(floor(min(ww_scaled[, var])), 
                                   ceiling(max(ww_scaled[, var])), 1))
})
lapply(sd.cls, FUN=function(x) prop.table(table(x)))

