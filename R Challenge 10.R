#### R CHALLENGE 10: BINOMIAL GENERALIZED MODEL ###################################
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(MuMIn)
library(agridat)
library(DHARMa)

data("jansen.carrot")
?jansen.carrot

d1 <- jansen.carrot
successes <- d1$y
failures <-d1$n - successes

# Simple GLM ####
bmod <- glm(cbind(successes, failures) ~ trt * gen, data=d1, family=binomial(link = "logit"))

plot(bmod)
hist(bmod$residuals)
plot(bmod$residuals~bmod$fitted.values)  ## residuals should be evenly dispersed around 0 across the range of x's
abline(h=0) 
qqPlot(bmod$residuals)
boxplot(bmod$residuals ~ d1$trt)  ## variances should be homogeneous for each group

sim_bmod1 <- simulateResiduals(fittedModel = bmod, n = 250)
plot(sim_bmod1)
# GLMER with block as a random effect ####
bmod2 <- glmer(cbind(successes, failures) ~ trt * gen + (1|block), 
							 data=d1, family=binomial(link = "logit"))
summary(bmod2)

## simulate and check residuals
sim_bmod2 <- simulateResiduals(fittedModel = bmod2, n = 250)
plot(sim_bmod2)

## check for overdispersion using a different method
library(RVAideMemoire)
overdisp.glmer(bmod2) # overdispersion ratio HIGH

## correct for overdispersion using observation-level random effect
d1$obs <- 1:length(d1$y)

# GLMER with block as a random effect and correcting overdispersion ####
bmod3 <- glmer(cbind(successes, failures) ~ trt * gen + (1|block) + (1|obs), 
							 data=d1, family=binomial(link = "logit"))

# Model failed to converge

# Answering question with bmod2 ####
# For which genotypes is there a treatment effect? 
summary(bmod2)
# genotypes 1, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16
# in other words, all genotypes except for 2 and 7