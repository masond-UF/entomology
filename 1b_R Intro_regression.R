####################################################################################
#### EXAMPLE OF LINEAR REGRESSION IN R #############################################
####################################################################################
library(tidyverse)
library(car)
install.packages("esquisse")
set.seed(21)
temp <- round(runif(20,12,30), 2)            
mass <- round(rnorm(20,5*temp,25), 2)
r1 <- as.data.frame(cbind(temp,mass)) ### run lines 4-6 to generate some fake data

head(r1) ## temp is rearing temperature (C), mass is the mass of adults (mg)

ggplot(r1, aes(x=temp, y=mass))+geom_point(size=2)+theme_bw()

## construct a linear model to estimate the average adult mass per degree C of temperature increase
## for a continuous variable (temp in degree C), we are interested in estimating the slope between age and circumfrence

lm1 <- lm(mass~temp, data=r1)

Anova(lm1, type=2)  ## produces an ANOVA table
# tests the null hypothesis that the slope is different than zero
# not super useful for regressions, but can look at

summary(lm1)        ## summary() will provide the model coefficients (ie. the "guts" of the model)
# the coefficients allow you rebuild the means from the linear model equation y~B0+B1*X
# for continous variables these coefficients and p-values are very useful (unlike for categorical ANOVA)
# don't really need to look at ANOVA table or use emmeans for this type of analysis, everything of interest is in summary

coef(lm1)   ## look at model coefficients
lm1$coef    ## look at model coefficients, allows a way to extract coefficients for using in other things

### make plot with best-fit regression line (ie. the mean of circumference for each value of age) and intercept
ggplot(r1, aes(x=temp, y=mass))+geom_point(size=3)+
  geom_smooth(method="lm")+
  theme_bw()

## check assumptions of model by examining residuals
hist(lm1$residuals) ## residuals should be normally distributed
plot(lm1$residuals~lm1$fitted.values)  ## residuals should be evenly dispersed around 0 across the range of x's
abline(h=0)                             # funnel shapes or curvature is bad

qqPlot(lm1$residuals)  ## calls from car package, residuals should line up pretty closely to the blue line
                         ## points that drift from line may be outliers
leveragePlots(lm1) # codes points that may be outliers

## problems with residuals indicate assumptions of the linear model are violated and may cause problems with coefficients and p-values
## transforming the data may help
## assumptions can be slightly violated without causing problems, for example this model is seems decent

## fancy-ish plot
ggplot(r1, aes(x=temp, y=mass))+geom_point(size=3,color='blue')+geom_smooth(method='lm')+theme_bw()

#############################################################################################################
##### REGRESSION CHALLENGE ###################################################################################

## run the code above and have a look at the dataset orange

data("Orange")  ## load Orange dataset from base R
head(Orange)    ## measurements of circumference on five trees at 7 time points

## Healthy orange trees typically produce fruit at 100 cm in circumference
## A homeowner calls and says their orange tree is 3 years old (1095 days), but isn't fruiting. They didn't measure it.
## They also said their are some white spots on the leaves. 

## Build a linear model (and make plot) to answer the following questions:
## 1. What circumference should their tree be, on average? ~130 cm
## 2. Should their tree be fruiting by now? Yes.
## 3. What advice would you give the grower? See an extenstion agent
## 4. Are the model assumptions met? Yes
## 5. Make a nice figure. Change themes, etc.

lm2 <- lm(circumference~age, data= Orange)


hist(lm2$residuals)
plot(lm2)
plot(lm2$residuals$residuals)

ggplot(Orange, aes(x=age, y=circumference))+
	geom_point(size=3, alpha = 0.4)+
	ggtitle("Circumference by Tree Age")+
	xlab("Age")+
	ylab("Circumference")+
	geom_smooth(method = lm, color = "red", fill = "light blue")+
	theme_bw()+
	theme(plot.title = element_text(hjust = 0.5))+
	theme(panel.grid.minor = element_blank())
	

