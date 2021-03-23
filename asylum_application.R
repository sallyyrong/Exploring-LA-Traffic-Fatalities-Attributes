## Lecture 6 (Lecture 3.2)


rm(list=ls()) 
setwd("~/Desktop/POLI 175")

# Load and inspect data ---------------------------------------------------

dat <- read.csv("asylum_data_spain.csv")
head(dat)
summary(dat)

#AsylumHome: measure of support for increasing or decreasing number of 
#people granted asylum in home country: 
#greatly decrease (-2), decrease (-1), neither increase nor decrease (0), 
#increase (1), greatly increase (2).


# Split into training and test sets ---------------------------------------

n.total <- nrow(dat) ## store the number of observations in data into n.total
prop.train <- 0.8 #80% of the data in training data set

set.seed(54321)
k <- sample(1:n.total, size = round(n.total*prop.train), replace = FALSE)
# randomly select indices 1:n, 80%
#Note: For randomly splitting data, replace = FALSE! ## don't want to duplicate any obs
#(In contrast to bootstrap resampling, where replace = TRUE)

train.dat <- dat[k,] ## 80%
test.dat <- dat[-k,] ## 20% (everything except for k)


# Train -------------------------------------------------------------------

mod1 <- lm(AsylumHome ~ IdeoScale + Female + Age + 
             Employed + EISCED + IncomeDecile,
           data = train.dat) ## using training data to fit that linear regression
summary(mod1)


# Training Error ----------------------------------------------------------

mean((train.dat$AsylumHome - predict(mod1))^2)
## [1] 1.165982
## can start to compute MSE (mean squared error) in training data
## can apply predict function 

# Test Error --------------------------------------------------------------

mean((test.dat$AsylumHome - predict(mod1, newdata = test.dat))^2)
##[1] 1.195142 , it's a bit higher than the training data MSE, which makes sense
## form y-hat

# Comparing Different Models ----------------------------------------------

# First model (above)
#Training MSE:
mean((mod1$model$AsylumHome - predict(mod1))^2)
#Test MSE:
mean((test.dat$AsylumHome - predict(mod1, newdata = test.dat))^2)
## restraint in that this is a linear model (though you can add polynomials, interactions, etc more on the right side of the eqn)

# More flexible model

mod2 <- lm(AsylumHome ~ IdeoScale + I(IdeoScale^2) + 
             Female + Age + I(Age^2) +
             Employed + EISCED + factor(IncomeDecile),
           data = train.dat)
## includes ^2 on IdeoScale and I
## Anticipate training MSE will go down (that's guaranteed to happen mathematically). Greater degree to fit more flexibly. Minimize the 
## For the test MSE, though,

#Training MSE:
mean((mod2$model$AsylumHome - predict(mod2))^2)
## [1] 1.139773 ##decreased, a better fit; from 1.16 to 1.14
#Test MSE:
mean((test.dat$AsylumHome - predict(mod2, newdata = test.dat))^2)
## [1] 1.221664 ## increased, from 1.195 to 1.221
## does not actually reflect the true underlying process

# Even more flexible model: adding interactions

mod3 <- lm(AsylumHome ~ Female*factor(IdeoScale) +
             Female*Age + Female*I(Age^2) + Female*I(Age^3) +
             Female*I(Age^4) +
             factor(EISCED) + Employed*factor(IncomeDecile),
           data = train.dat)
## adding interactions (we're adding them sort of arbitrarily here) for more flexibility
#Training MSE:
mean((mod3$model$AsylumHome - predict(mod3))^2)
## [1] 1.09974
#Test MSE:
mean((test.dat$AsylumHome - predict(mod3, newdata = test.dat))^2)
## 1.22207 
## as you add more flexibility to model, training error will go down (you're allowing more flexibility to minize error), but comes at expense of test data fit

