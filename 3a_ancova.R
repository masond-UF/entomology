## LOAD AND PROCESSES DATA
library(tidyverse)   ### load tidyverse
library(car)         ### load car package, which is helpful for analyzing linear models
library(emmeans)     ### load emmeans package, which is helpful for getting means from linear models

##################################################################################################################
##### ANCOVA
###################################################################
#### assemble data with covariate (don't worry about the code...)##
set.seed(17)
data("InsectSprays")
d <- InsectSprays %>% filter(spray=='A'|spray=='B'|spray=='C'|spray=='F') %>% droplevels()
d$count[13:24] <- d$count[13:24]+5
d$weeds <- abs(round(rnorm(48,2*d$count,10),1))
d$weeds[25:36] <- c(55.3,46.8,30.2,62.3,24.2,33.2,18.2,12.6,39.7,41.0,46.9,42.8)
###################################################################

## plot raw data
ggplot(d, aes(x=spray,y=count)) + geom_boxplot(outlier.shape = NA) + geom_jitter(height=0,width=.1) 

anova_means <- emmeans(lm(count~spray, data=d), pairwise~spray) 
anova_means

###############################################################################################
## plot out data with weed cover
ggplot(d, aes(x=weeds,y=count)) + geom_point() + facet_wrap(~spray) + geom_smooth(method='lm')


######################################################################
## ANCOVA: multiple intercept model

lm1i <- lm(count ~ spray + weeds, data=d)

Anova(lm1i, type=2) ## Anova table with Type II sums of squares (aka Type III SS)
anova(lm1i)         ## don't use the base anova!!! Switch terms around in the model and see what happens

summary(lm1i) ## model coefficients

## calculate estimated marginal mean for each group (ie. groups means after accounting for the effect of weeds)
ancova_means <- emmeans(lm1i, pairwise~spray)  
ancova_means

## extract the emmean means (ie. group means after accounting for the effect of weeds)
lm1i_coef <- as.data.frame(emmeans(lm1i, ~spray))
## extract intercepts and add slopes into new dataframe
lm1i_coef2 <- as.data.frame(emmeans(lm1i, ~spray, at=list(weeds=0)))
lm1i_coef2$slope <- coef(lm1i)[5]


## plot the data with the fitted model
ggplot(data=d, aes(x=weeds,y=count)) + geom_point() + facet_wrap(~spray) + 
  geom_abline(data=lm1i_coef2, aes(intercept=emmean, slope=slope))+
  geom_point(data=lm1i_coef2, aes(x=0,y=emmean),color="red")+
  geom_point(data=lm1i_coef, aes(x=mean(d$weeds),y=emmean),color="blue", size=2)



##########################################################################################
## ANCOVA: multiple intercept AND slope model

lm1is <- lm(count ~ spray + weeds + spray:weeds, data=d)
## same as above: lm(count ~ spray * weeds, data=d)

Anova(lm1is, type=2)

summary(lm1is)


## calculate estimated marginal mean for each group (ie. groups means after accounting for the effect of weeds)
ancova_is_means <- emmeans(lm1is, pairwise~spray)  
ancova_is_means

## calculate the slope for each group
ancova_is_slopes <- emtrends(lm1is, pairwise~spray, var="weeds")  
ancova_is_slopes

## extract the emmean means (ie. group means after accounting for the effect of weeds)
lm1is_coef <- as.data.frame(emmeans(lm1is, ~spray))

## extract intercepts and add slopes into new dataframe
lm1is_coef2a <- as.data.frame(emmeans(lm1is, ~spray, at=list(weeds=0)))
lm1is_coef2b <- as.data.frame(emtrends(lm1is, var="weeds"))
lm1is_coef2 <- full_join(lm1is_coef2a,lm1is_coef2b,by="spray")

## plot the data with the fitted model
ggplot(data=d, aes(x=weeds,y=count)) + geom_point() + facet_wrap(~spray) + 
  geom_abline(data=lm1is_coef2, aes(intercept=emmean, slope=weeds.trend), lty=2)+
  geom_point(data=lm1is_coef2, aes(x=0,y=emmean),color="orange")+
  geom_point(data=lm1is_coef, aes(x=mean(d$weeds),y=emmean),color="purple", size=2)


###############################################################################################
## nice plot out data with weed cover
ggplot(d, aes(x=weeds,y=count)) + geom_point() + facet_wrap(~spray) + 
  geom_smooth(method='lm', color='black')+theme_bw()


##################################################################################################
###### ANCOVA CHALLENGE ##########################################################################
###################################################################################################

## load in the ChickWeight dataset. It contains weight (g) of small chickens grown on four different diets.
## Chickens were weighed every few days for 21 days. Don't worry about the 'Chick' column for this challenge
data("ChickWeight")
ChickWeight$Diet <- as.factor(ChickWeight$Diet)

## plot out data
ggplot(ChickWeight, aes(x=Time,y=weight))+geom_point()+facet_wrap(~Diet)+geom_smooth(method="lm")

coef(lmchicks)

## Q1: Conduct a regular one-way ANOVA to see if weight differs among the four diets. Which ones differ? ####
lmchicks <- lm(weight ~ Diet, data=ChickWeight)

Anova(lmchicks, type = 2) #Type

lmchicks_means <- emmeans(lmchicks, pairwise~Diet)

# 1 & 4, 1 & 3

## Q2: Do the chicks on different diets have different growth rates? Which is fastest/slowest? (ie. compare slopes) #### 

lmchicks <- lm(weight ~ Diet + Time + Diet:Time, data=ChickWeight)

## calculate the slope for each group
lmchicks_slopes <- emtrends(lmchicks, pairwise~Diet, var="Time")  
lmchicks_slopes

# Fastest is Diet 3, slowest is Diet 1

## Q3: Do the weights differ among the diets when including the time covariate? Do they differ at time point 20?
## hint: for the second part add the following to the emmeans statement: , at=list(Time=20)

lmchicks <- lm(weight ~ Diet + Time + Diet:Time, data=ChickWeight)
lmchicks_means <- emmeans(lmchicks, pairwise~Diet)
lmchicks_means_t20 <- emmeans(lmchicks, pairwise~Diet, at = list(Time = 20))

# Yes most weights differ when including time as a covariate (and at t20).

## Bonus: Plot out the emmeans with 95% CI bars for the four diets
lmchicks_plot <- as.data.frame(lmchicks_means$emmeans)

ggplot(d = lmchicks_plot, aes(x = Diet, y = emmean))+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), size = 2, width = 0,
                color = "darkgray")+
  geom_point(color = "black", size = 7)+
  ylab('Mean weight (g)')+
  xlab('Diet')+
  theme_classic()+
  theme(text = element_text(size = 20))

