library(MASS)
head(shuttle)
shuttle2 <- shuttle
shuttle2$use2 <- as.numeric(shuttle2$use == 'auto')
fit1 <- glm(use2 ~ factor(wind) - 1, family = binomial, shuttle2)
summary(fit1)
exp(fit1$coefficients)

fit2 <- glm(use2 ~ factor(wind) + factor(magn) - 1, family = binomial, shuttle2)
coef2 <- exp(fit2$coefficients)
coef2[1] / coef2[2]

# question 3
fit3 <- glm(1- use2 ~ factor(wind), family = binomial, shuttle2)
summary(fit3)
fit3 <- glm(use2 ~ factor(wind), family = binomial, shuttle2)
summary(fit3)

# question 4
head(InsectSprays)
fit4 <- glm(count ~ factor(spray) - 1, family = poisson, InsectSprays)
coef4 <- exp(coef(fit4))
coef4[1] / coef4[2]

# question 6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

knots<-c(0)
splineTerms<-sapply(knots,function(knot) (x>knot)*(x-knot))
xmat<-cbind(1,x,splineTerms)
fit<-lm(y~xmat-1)
yhat<-predict(fit)

summary(fit)$coef
