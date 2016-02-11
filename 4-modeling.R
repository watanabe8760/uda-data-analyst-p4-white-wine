library(psych)
library(memisc)
library(nnet)
library(rpart)
library(e1071)
library(randomForest)

describePerformance <- function(org, pred) {
  cat("[Contingency Table]\n")
  print(table(org, pred))
  cat("\n")
  
  cat("[Contingency Table by Proportion]\n")
  print(round(prop.table(table(org, pred), 1), 3))
  cat("\n")
  
  cat("[Overall Accuracy]\n")
  cat(sum(org == pred) / length(org) * 100, '%')
  cat("\n\n")
  
  cat("[Cohen's kappa]\n")
  kp <- cohen.kappa(cbind(org, pred))
  cat("Unweighted:", kp$kappa, '\n')
  cat("  Weighted:", kp$weighted.kappa)
}

# Formulas
fml1 <- as.formula(paste("quality", "~", 
                         paste(INDEPENDENT, collapse=' + ')))
fml2 <- as.formula(paste("quality.f", "~", 
                         paste(INDEPENDENT, collapse=' + ')))
fml3 <- as.formula(paste("quality.f2", "~", 
                         paste(INDEPENDENT, collapse=' + ')))
fml.e <- as.formula(paste("excellent", "~", 
                          paste(INDEPENDENT, collapse=' + ')))
fml.i <- as.formula(paste("inferior", "~", 
                          paste(INDEPENDENT, collapse=' + ')))

# Linear Regression for numerical quality
m1 <- lm(fml1, ww)
summary(m1)
m2 <- update(m1, ~ . - citric.acid)
summary(m2)
m3 <- update(m2, ~ . - chlorides)
summary(m3)
m4 <- update(m3, ~ . - total.sulfur.dioxide)
summary(m4)
mtable(m1, m2, m3, m4)
sqrt(mean((ww$quality - m4$fitted.values)^2))
describePerformance(ww$quality, round(m4$fitted.values, 0))


# Logistic Regression
lr1 <- multinom(fml2, ww)
describePerformance(ww$quality.f, predict(lr1, ww))

lr2 <- multinom(fml3, ww)
describePerformance(ww$quality.f2, predict(lr2, ww))


# Logistic Regression by one-hot encoding
ww$q.bad <- ww$quality.f2 == 'bad'
ww$q.nom <- ww$quality.f2 == 'normal'
ww$q.god <- ww$quality.f2 == 'good'
f.b <- as.formula(paste("q.bad", "~", paste(INDEPENDENT, collapse=' + ')))
f.n <- as.formula(paste("q.nom", "~", paste(INDEPENDENT, collapse=' + ')))
f.g <- as.formula(paste("q.god", "~", paste(INDEPENDENT, collapse=' + ')))
lr.b <- glm(f.b, family=binomial, ww)
lr.n <- glm(f.n, family=binomial, ww) 
lr.g <- glm(f.g, family=binomial, ww)
pred <- data.frame(bad=predict(lr.b, ww, type='response'), 
                   normal=predict(lr.n, ww, type='response'), 
                   good=predict(lr.g, ww, type='response'))
pred$predicted <- apply(pred, 1, FUN=function(row) {
  if(which(row == max(row)) == 1) {
    return("bad")
  } else if (which(row == max(row)) == 2) {
    return("normal")
  } else {
    return("good")
  }
})
pred$predicted <- factor(pred$predicted, levels=c("bad", "normal", "good"))
describePerformance(ww$quality.f2, pred$predicted)


# Logistic Regression for extreme ones
lr.e <- glm(fml.e, family=binomial, ww)
lr.i <- glm(fml.i, family=binomial, ww)
describePerformance(ww$excellent, round(lr.e$fitted.values, 0))
describePerformance(ww$inferior, round(lr.i$fitted.values, 0))


# Descision Tree
tree1 <- rpart(fml1, ww, method='anova')
plot(tree1)
text(tree1, all=T, use.n=T)
summary(tree1)
describePerformance(ww$quality, round(predict(tree1, ww), 0))

tree2 <- rpart(fml2, ww, method='class')
plot(tree2)
text(tree2, all=T, use.n=T)
summary(tree2)
describePerformance(ww$quality.f, predict(tree2, ww, type='class'))

tree3 <- rpart(fml3, ww, method='class')
plot(tree3)
text(tree2, all=T, use.n=T)
summary(tree3)
describePerformance(ww$quality.f2, predict(tree3, ww, type='class'))

tree4 <- rpart(fml.e, ww, method='class')
plot(tree4)
text(tree4, all=T, use.n=T)
summary(tree4)
describePerformance(ww$excellent, predict(tree4, ww, type='class'))

tree5 <- rpart(fml.i, ww, method='class')
plot(tree5)
text(tree5, all=T, use.n=T)
summary(tree5)
describePerformance(ww$inferior, predict(tree5, ww, type='class'))


# SVM
svm1 <- svm(fml1, ww)
sqrt(mean((ww$quality - svm1$fitted)^2))
describePerformance(ww$quality, round(svm1$fitted, 0))

svm2 <- svm(fml2, ww)
describePerformance(ww$quality.f, svm2$fitted)

svm3 <- svm(fml3, ww)
describePerformance(ww$quality.f2, svm3$fitted)

ww$excellent <- as.factor(ww$excellent)
svm4 <- svm(fml.e, ww)
describePerformance(ww$excellent, svm4$fitted)

ww$inferior <- as.factor(ww$inferior)
svm5 <- svm(fml.i, ww)
describePerformance(ww$inferior, svm5$fitted)


# Linear Regression on PCA
pca <- prcomp(as.formula(paste("~", paste(INDEPENDENT, collapse=' + '))), 
              ww, center=T, scale=T)
wwp <- cbind(data.frame(pca$x), quality=ww$quality)

fml.pca <- 
  as.formula(paste("quality", "~", paste(names(wwp)[1:11], collapse=" + ")))
pm1 <- lm(fml.pca, wwp)
summary(pm1)
pm2 <- update(pm1, ~ . - PC6)
summary(pm2)
pm3 <- update(pm2, ~ . - PC7)
summary(pm3)
sqrt(mean((ww$quality - pm3$fitted.values)^2))


# Random Forests
rf1 <- randomForest(fml1, ww)
sqrt(mean((ww$quality - rf1$predicted)^2))
describePerformance(ww$quality, round(rf1$predicted, 0))

rf2 <- randomForest(fml2, ww)
describePerformance(ww$quality.f, rf2$predicted)

rf3 <- randomForest(fml3, ww)
describePerformance(ww$quality.f2, rf3$predicted)

ww$excellent <- as.factor(ww$excellent)
rf4 <- randomForest(fml.e, ww)
describePerformance(ww$excellent, rf4$predicted)

ww$inferior <- as.factor(ww$inferior)
rf5 <- randomForest(fml.i, ww)
describePerformance(ww$inferior, rf5$predicted)


