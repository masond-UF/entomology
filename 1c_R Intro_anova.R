## LOAD AND PROCESSES DATA
library(tidyverse)   ### load tidyverse
library(car)         ### load car package, which is helpful for analyzing linear models
library(emmeans)     ### load emmeans package, which is helpful for getting means from linear models


########################################################################################################
##### EXAMPLE FOR CONDUCTING A ONE-WAY ANOVA IN R ######################################################

data("InsectSprays") ### load InsectSprays dataset, available from base R  

# filter to just 4 treatments
d <- InsectSprays %>% filter(spray=='A'|spray=='B'|spray=='C'|spray=='F') %>% droplevels()

## plot out data
ggplot(d, aes(x=spray,y=count)) + geom_boxplot(outlier.shape = NA) + geom_jitter(height=0,width=.1) 
                                                    ## need to suppress outliers if you jitter plot points


## construct linear model to examine the effect of the different sprays on insect counts
## for a categorical variable (spray with four levels), we are interested in comparing group means
lm1 <- lm(count~spray, data=d)  ## lm is a general function that conducts a linear model
                                 # all the "calculations" are saved in an object we called 'lm1'

Anova(lm1, type=2)  ## car::Anova will print out an ANOVA table testing 
                     # the null hypothesis that all group means are equal
                     # type = 2 provides Type II sums of squares, which is usually better than the default Type I, especially for more complicated models
                     # other functions (anova, aov, etc.) will provide similar ANOVA tables, but the Anova() is more flexible


summary(lm1)   ## summary() will provide the model coefficients (ie. the "guts" of the model)
                # the coefficients allow you rebuild the means from the linear model equation y~u+Bi
                # rebuilding the model from the coefficients is not super helpful and the p-values aren't very meaningful

emmeans(lm1, ~spray) ## emmeans::emmmeans will rebuild the model for you
                      # this code will print off the means, SE, and confidence intervals for each treatment group

emmeans(lm1, pairwise~spray)  ## adding 'pairwise' will conduct pairwise contrasts -- ie. compare each group mean to the others
                               # automattical adjusts p-values using the 'tukey' adjust. Can change this if you want using adjust=XX

## check assumptions of model by examining residuals
hist(lm1$residuals) ## residuals should be normally distributed
plot(lm1$residuals~lm1$fitted.values)  ## residuals should be evenly dispersed around 0 across the range of x's
abline(h=0)                             # funnel shapes or curvature is bad

qqPlot(lm1$residuals)  ## calls from car package, residuals should line up pretty closely to the blue line
                        # points that drift from line might be outliers
boxplot(lm1$residuals ~ d$spray)  ## variances should be homogeneous for each group

          ## problems with residuals indicate assumptions of the linear model are violated and may cause problems with coefficients and p-values
          ## transforming the data or using a different type of model may help (we will return to this example later in the course to improve it)
          ## assumptions can be slightly violated without causing problems, for example this model is seems passable but could be better.

####################################################################################
##### ANOVA CHALLENGE ##############################################################

## DUE AT THE END OF THE WEEK

## have a look at the dataset below. Baby chickens were fed different diets and they were weighed after 10 days. 
## variable 'weight' is the weight of a baby chicken (g); 'feed' is the type of type of diet the chicken was fed

d1 <- chickwts
head(d1)

Anova(lm1, type=2)
emmeans(lm1, pairwise~feed)
unique(d1$feed)
lm1 <- lm(weight~feed, data=d1)
summary(lm1)
plot(lm1)
hist(lm1$residuals)
hist(d1$weight)
# 1. Construct a linear model to analyze the data. Is there evidence at least one mean is different than another?
### Yes significant global p-values
# 2. How much variation in the data does the model explain?
### 0.5417
# 3. The feed 'casein' is the standard chicken diet. What types of feed are significantly worse than 'casein'. By how much are they worse?
### Chickens fed horsebans weighed 2.03 times less than those fed casesin;
### those fed linseed weighed 1.48 times less (51% less),
### those fed meatmeal weighed 1.17 times less (32% decrease),
### those fed soybean weighed 1.32 times less (14% decrease),
### and sunflower weighed 1.02 times more (2% increase).
# 4. Are the assumptions met?
### yes
# 5. Make a nice looking figure. show all the data.
d1_means <- emmeans(lm1, ~feed) %>% as.data.frame() ## saves emmeans as dataframe

library(plyr)
d1$feed <- revalue(d1$feed, c("casein"="Casein", "horsebean"="Horsebean",
							"linseed" = "Linseed", "meatmeal" = "Meatmeal",
							"soybean" = "Soybean", "sunflower" = "Sunflower"))

d1_means$feed <- revalue(d1_means$feed, c("casein"="Casein", "horsebean"="Horsebean",
							"linseed" = "Linseed", "meatmeal" = "Meatmeal",
							"soybean" = "Soybean", "sunflower" = "Sunflower"))

d1 <- dplyr::rename(d1,Feed = feed)

ggplot()+
	geom_jitter(data=d1, aes(x=Feed,y=weight, color=Feed),  height=0, width=.1, alpha = 0.5, size=2.75)+
	geom_errorbar(data=d1_means, aes(x=feed, y=emmean, ymin=(emmean-SE*2), ymax=(emmean+SE*2)), width=.2, color="black", lwd=0.75)+ 
	geom_point(data=d1_means, aes(x=feed, y=emmean), color="black", size=3) + 
	ggtitle("Effect of diet on chicken weight")+
	ylab("Weight (g)")+
	xlab("Feed type")+
	theme_classic()+
	theme(plot.title = element_text(hjust = 0.5))+
	theme(legend.position="none")+
	theme(text = element_text (size = 15))




