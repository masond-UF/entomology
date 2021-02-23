## LOAD AND PROCESSES DATA
library(tidyverse)
library(car)
data("InsectSprays")
InsectSprays$block <- as.factor(rep(c(1,2,3,4,5,6,7,8,9,10,11,12), 6)) # add blocks

d <- InsectSprays %>% filter(spray=='A'|spray=='B'|spray=='C'|spray=='F')

##############################################################
## GRAPH AND ANALYZE DATA USING COMPLETELY RANDOMIZED DESIGN

#plot data by treatment group
ggplot(d, aes(x=spray,y=count)) + geom_boxplot(outlier.shape = NA) + geom_jitter(height=0,width=.1)

#plot data by treatment and block -- note one observation per block
ggplot(d, aes(x=spray,y=count)) + geom_boxplot(outlier.shape = NA) + geom_jitter(height=0,width=.1) + facet_wrap(~block)  #12 blocks


### construct and compare linear models with and without blocking
## no block model
lm1 <- lm(count~spray, data=d)
anova(lm1)

## block as fixed effect
lm2 <- lm(count~spray+block, data=d)
anova(lm2)

#####################################################################################
## ANALYZE DATA USING RANDOMIZED COMPLETE BLOCK DESIGN WITH BLOCK AS A RANDOM EFFECT
library(lme4)
library(lmerTest)

## block as random effect
lm3 <- lmer(count~spray+(1|block), data=d)
anova(lm3)

#### compare summary() for fixed vs. random blocking effect
summary(lm2)
summary(lm3)

coef(lm3)    ## prints model coefficients

print(VarCorr(lm3), comp=c("Variance"))  ## print variance components from model

plot(lm1$residuals~lm1$fitted.values) ### check residuals of no block and block models
plot(resid(lm3)~fitted(lm3))

hist(lm1$residuals)
hist(resid(lm3))

#### compare estimated marginal means
library(emmeans)
emmeans(lm2, pairwise~spray) ## fixed effect block
emmeans(lm3, pairwise~spray) ## random effect block


###################################################################################
#### BLOCKING CHALLENGE ###########################################################
###################################################################################

## ChickWeight blocking challenge
data("ChickWeight")
ChickWeight$Diet <- as.factor(ChickWeight$Diet)

cw0 <- lm(weight ~ Time * Diet , data=ChickWeight)
emmeans(cw0, pairwise~Diet, at=list(Time=20))

## Q1. The ChickWeight dataset contains measurements on 50 chicks fed four different diets over a 21 day period
##    Last time we did an ANCOVA to compare the weights at the end of the experiment (Time=20).
##    The results seem overly strong (ie. the p-values seem artificially low). Are all observations independent?
##    What might we not have considered that was part of the experimental design but was not accounted for in our analysis?
##    Construct a more appropriate model (use random effects if justified) and compare the diets at Time=20
##    Which diets differ significantly? How do the results differ from your model last week?

cw1 <- lmer(weight ~ Time * Diet + (1|Chick), data=ChickWeight)
emmeans(cw1, pairwise~Diet, at=list(Time=20))

# Diet 1 differs from all other diets and diet 2+3 differs.
# Previously diet 2+4 and 3+4 were also significantly different. 

## Q2. How much variation is there among chicks? (ie. what is the variance component?)

print(VarCorr(cw1), comp=c("Variance"))  ## print variance components from model

# 545.72 variance associated with chick

####################################################################################
### NPK CHALLENGE ##################################################################
####################################################################################
data("npk")
head(npk)
str(npk)

ggplot(npk, aes(x=N, y=yield, fill=K))+geom_boxplot()+geom_smooth(method='lm')+theme_bw() 
ggplot(npk, aes(x=N, y=yield, fill=K))+geom_boxplot()+geom_smooth(method='lm')+theme_bw() +facet_wrap(~block)

## Q3. The npk dataset contains measures of yields of pea plants under different experimental treatments
## Nitrogen (N) and potasium (K) were factorially manipulated. Ignore Phosphorous for this example.
## There were 6 replicates, separated into distinct blocks (see image from lecture slides)
## Analyze the data using a two-way anova (ie. yield ~ N * K)
## How do the results change if you ignore block vs if you include block as a random effect?

npk.mod <- lm(yield~N*K, data=npk)
anova(npk.mod) # only N is significant

## block as random effect
npk.mod.blck <- lmer(yield~N*K+(1|block), data=npk)
Anova(npk.mod.blck, Type = 2)

## Q4. How much variation is there among blocks?

print(VarCorr(npk.mod.blck), comp=c("Variance")) 
## 3.8611

## Q5. Are the assumptions of a linear model met?
# Random effect *better satisfies assumptions of linear model*
plot(resid(npk.mod.blck)~fitted(npk.mod.blck))
hist(resid(npk.mod.blck))

# Np random effect
plot(npk.mod$residuals~npk.mod$fitted.values)
hist(npk.mod$residuals)
