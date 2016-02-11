library(GGally)
library(ggplot2)
library(gridExtra)

# Plot matrix by sample
get_sample <- function(ww) {
  set.seed(20160129)
  ww_sample <- subset(ww, quality == 3)
  ww_sample <- rbind(ww_sample, subset(ww, quality == 4)[sample(163, 100), ])
  ww_sample <- rbind(ww_sample, subset(ww, quality == 5)[sample(1457, 100), ])
  ww_sample <- rbind(ww_sample, subset(ww, quality == 6)[sample(2198, 100), ])
  ww_sample <- rbind(ww_sample, subset(ww, quality == 7)[sample(880, 100), ])
  ww_sample <- rbind(ww_sample, subset(ww, quality == 8)[sample(175, 100), ])
  ww_sample <- rbind(ww_sample, subset(ww, quality == 9))
  ww_sample$quality <- as.factor(ww_sample$quality)
  return(ww_sample)
}
# [Warning] This is very slow.
ggpairs(get_sample(ww[, ORIGINAL]), mapping=aes(color=quality, alpha=0.4))



showScatpltColored <- function(df, color, x='alcohol', y='quality',
                          lower=0.00, upper=0.99) {
  ggplot(df, aes_string(x=x, y=y, color=color)) +
    geom_point(alpha=0.5, position='jitter') + 
    xlim(quantile(df[, x], prob=lower), 
         quantile(df[, x], prob=upper)) +
    scale_colour_gradientn(colours=c("blue", "violet", "red"))
}
scat_wc <- lapply(INDEPENDENT, FUN=function(var) showScatpltColored(ww, var))
do.call(grid.arrange, args=c(scat_wc, list(ncol=4)))


showScatpltColored2 <- function(df, x, y, color='quality.f2',
                           lower=0.00, upper=0.99){
  ggplot(df, aes_string(x=x, y=y, color=color)) +
    geom_point(alpha=0.3, position='jitter') + 
    xlim(quantile(df[, x], prob=lower), 
         quantile(df[, x], prob=upper)) +
    ylim(quantile(df[, y], prob=lower), 
         quantile(df[, y], prob=upper))
}
scat_wc2 <- lapply(combn(INDEPENDENT, 2, simplify=F), 
                   FUN=function(var) showScatpltColored2(ww, var[1], var[2]))
do.call(grid.arrange, args=c(scat_wc2[1:11], list(ncol=4)))
do.call(grid.arrange, args=c(scat_wc2[12:22], list(ncol=4)))
do.call(grid.arrange, args=c(scat_wc2[23:33], list(ncol=4)))
do.call(grid.arrange, args=c(scat_wc2[34:44], list(ncol=4)))
do.call(grid.arrange, args=c(scat_wc2[45:55], list(ncol=4)))
# 1. The lower chlorides is, the better quality is.
# 2. Good ones are centralized to 0.3 citric.acid.
# 3. density x residual.sugar plot has a clear distinction.
# 4. free.sulfur.dioxide x total.sulfur.dioxide
# 5. free.sulfur.dioxide x density


# 1. The lower chlorides is, the better quality is.
ggplot(ww, aes(x=chlorides, y=quality.f)) + geom_point()
ggplot(ww, aes(x=chlorides, y=quality.f2)) + geom_point()
ggplot(ww, aes(x=quality.f, y=chlorides)) + geom_boxplot()
ggplot(ww, aes(x=quality.f2, y=chlorides)) + geom_boxplot()
describeBy(ww, 'quality.f2')
# -> Good ones have lower chlorides, but that does not promise.
# -> Normal ones have a skewed distribution.


# 2. Good ones are centralized to 0.3 citric.acid.
ggplot(ww, aes(x=citric.acid, y=quality.f)) + geom_point()
ggplot(ww, aes(x=citric.acid, y=quality.f2)) + geom_point()
ggplot(ww, aes(x=quality.f, y=citric.acid)) + geom_boxplot()
ggplot(ww, aes(x=quality.f2, y=citric.acid)) + geom_boxplot()
ggplot(ww, aes(x=citric.acid, fill=quality.f2)) + geom_density(alpha=0.3)
# -> The lower quality is, the wider distribution is.


# 3. density x residual.sugar plot has a clear distinction.
ggplot(ww, aes(x=density, y=residual.sugar, color=quality.f2)) +
  geom_point() + 
  xlim(min(ww$density), quantile(ww$density, prob=0.99)) +
  ylim(min(ww$residual.sugar), quantile(ww$residual.sugar, prob=0.99))
# -> Holding density, higher residual.sugar seem to have better quality
# -> High density area, this role does not seem to be applicable

# By the original quality scale
ggplot(ww, aes(x=density, y=residual.sugar, color=quality)) +
  geom_point() + 
  xlim(min(ww$density), quantile(ww$density, prob=0.99)) +
  ylim(min(ww$residual.sugar), quantile(ww$residual.sugar, prob=0.99)) +
  scale_color_gradientn(colors=rainbow(7))

# Per quality level plot
scat_per_qual <- lapply(9:3, FUN=function(n) {
  ggplot(subset(ww, quality == n), 
         aes(x=density, y=residual.sugar)) +
    geom_point(color=rainbow(7)[n-2]) + 
    ggtitle(paste("Quality", n)) + 
    xlim(min(ww$density), quantile(ww$density, prob=0.99)) +
    ylim(min(ww$residual.sugar), quantile(ww$residual.sugar, prob=0.99))
})
do.call(grid.arrange, args=c(scat_per_qual, list(ncol=3)))



# 4. free.sulfur.dioxide x total.sulfur.dioxide
ggplot(ww, aes(x=free.sulfur.dioxide, y=total.sulfur.dioxide, color=quality.f2)) +
  geom_point() + 
  xlim(min(ww$free.sulfur.dioxide), quantile(ww$free.sulfur.dioxide, prob=0.99)) +
  ylim(min(ww$total.sulfur.dioxide), quantile(ww$total.sulfur.dioxide, prob=0.99)) +
  scale_color_brewer(type = 'seq',
                     guide = guide_legend(title = 'Quality', 
                                          reverse = T))

# 5. free.sulfur.dioxide x density
ggplot(ww, aes(x=free.sulfur.dioxide, y=density, color=quality.f2)) +
  geom_point() + 
  xlim(min(ww$free.sulfur.dioxide), quantile(ww$free.sulfur.dioxide, prob=0.99)) +
  ylim(min(ww$density), quantile(ww$density, prob=0.99)) +
  scale_color_brewer(type = 'seq',
                     guide = guide_legend(title = 'Quality', 
                                          reverse = T))



# [Quality conversion 2]
#   Extreme qualities - excellent/inferior

# Check all the combination of independent variable
scat_wc2 <- lapply(combn(INDEPENDENT, 2, simplify=F), 
                   FUN=function(var) 
                     showScatpltColored2(ww, var[1], var[2], 'excellent'))
do.call(grid.arrange, args=c(scat_wc2[1:11], list(ncol=4)))
do.call(grid.arrange, args=c(scat_wc2[12:22], list(ncol=4)))
do.call(grid.arrange, args=c(scat_wc2[23:33], list(ncol=4)))
do.call(grid.arrange, args=c(scat_wc2[34:44], list(ncol=4)))
do.call(grid.arrange, args=c(scat_wc2[45:55], list(ncol=4)))

scat_wc2 <- lapply(combn(INDEPENDENT, 2, simplify=F), 
                   FUN=function(var) 
                     showScatpltColored2(ww, var[1], var[2], 'inferior'))
do.call(grid.arrange, args=c(scat_wc2[1:12], list(ncol=4)))
do.call(grid.arrange, args=c(scat_wc2[13:24], list(ncol=4)))
do.call(grid.arrange, args=c(scat_wc2[25:36], list(ncol=4)))
do.call(grid.arrange, args=c(scat_wc2[37:48], list(ncol=4)))
do.call(grid.arrange, args=c(scat_wc2[49:55], list(ncol=4)))

