library(corrplot)
library(ggplot2)
library(gridExtra)

# Correlation matrix
corrplot.mixed(cor(ww[, ORIGINAL]))


showScatplt <- function(df, x, y='quality') {
  ggplot(df, aes_string(x=x, y=y)) + 
    geom_point(alpha=0.3, position='jitter')
}
scat <- lapply(INDEPENDENT, FUN=function(var) showScatplt(ww, var))
do.call(grid.arrange, args=c(scat, list(ncol=3)))


showBoxplt <- function(df, y, x='quality.f') {
  ggplot(df, aes_string(x=x, y=y, fill=x)) + 
    geom_boxplot()
}
boxp <- lapply(INDEPENDENT, FUN=function(var) showBoxplt(ww, var))
do.call(grid.arrange, args=c(boxp, list(ncol=3)))


showBoxplt <- function(df, y, x='quality.f', lower=0, upper=.99) {
  ggplot(df, aes_string(x=x, y=y, fill=x)) + 
    geom_boxplot() + 
    ylim(quantile(df[, y], prob=lower), 
         quantile(df[, y], prob=upper))
}
boxp <- lapply(INDEPENDENT, FUN=function(var) showBoxplt(ww, var))
do.call(grid.arrange, args=c(boxp, list(ncol=3)))


# [Quality conversion 1]
#   Simpler classification - good/normal/bad

boxp <- lapply(INDEPENDENT, 
               FUN=function(var) showBoxplt(ww, var, 'quality.f2'))
do.call(grid.arrange, args=c(boxp, list(ncol=3)))


showDensplt <- function(df, x, cls='quality.f2', lower=0, upper=.99) {
  ggplot(df, aes_string(x=x, fill=cls)) + 
    geom_density(alpha=0.3) +
    xlim(quantile(df[, x], prob=lower), 
         quantile(df[, x], prob=upper))
}
dens <- lapply(INDEPENDENT, FUN=function(col) showDensplt(ww, col))
do.call(grid.arrange, args=c(dens, list(ncol=3)))



# [Quality conversion 2]
#   Extreme qualities - excellent/inferior

# Distribution check by boxplot
boxp <- lapply(INDEPENDENT, 
               FUN=function(var) showBoxplt(ww, y=var, x='excellent'))
do.call(grid.arrange, args=c(boxp, list(ncol=3)))
boxp <- lapply(INDEPENDENT, 
               FUN=function(var) showBoxplt(ww, y=var, x='inferior'))
do.call(grid.arrange, args=c(boxp, list(ncol=3)))


# Distribution check by density plot
dens <- lapply(INDEPENDENT, 
               FUN=function(var) showDensplt(ww, x=var, cls='excellent'))
do.call(grid.arrange, args=c(dens, list(ncol=3)))
dens <- lapply(INDEPENDENT, 
               FUN=function(var) showDensplt(ww, x=var, cls='inferior'))
do.call(grid.arrange, args=c(dens, list(ncol=3)))



