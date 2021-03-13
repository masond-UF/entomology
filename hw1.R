# HW 1—David Mason—11 March 2021 ####

library(tidyverse) # Data manipulation and visualization
library(emmeans) # Calculating and comparing means
library(DataExplorer) # Data exploration
library(lme4) # Modeling 
library(lmerTest) # Testing random effects
library(MuMIn) # R squared value

d <- read.csv("data.csv") # Load in the data

# Data exploration ####
col <- c('violet','Chocolate2')

create_report(d)
# 68 rows, 5 columns (4 discrete, 1 continuous), 340 total observations
# No missing data
# Person = factor with 17 levels, Hand & Type = factor with 2 levels
# Time is our response variable

summary(d)
# 34 rows each for primary/secondary hand
# sinisteral could be an interesting thing to explore (lots of notes)
# mean time is 14 seconds & interquartile range is ~8-16 seconds,
# median time 11 seconds, max/min is 5/58 seconds

# Histogram
hist(d$Time, col=col, xlab='Seconds',las=1, main='Histogram') 
# positively skewed with few observations of less than 5 seconds
# one potential outlier at ~58 seconds

# Boxplot of time grouped by type nested within hand
ggplot(d, aes(x=Hand, y=Time, color=Type))+ 
	geom_boxplot() + geom_jitter() + theme_classic()+
	scale_color_manual(values = col)

# Boxplot of time grouped by hand nested within type
ggplot(d, aes(x=Type, y=Time, color=Hand))+ 
	geom_boxplot() + geom_jitter() + theme_classic()+
	scale_color_manual(values = col)

# Boxplot of time by person
ggplot(d, aes(x=Person, y=Time))+ 
	geom_boxplot() + geom_jitter() + theme_classic()
# difference among observers indicates need for blocking

# Create and analyze models ####
m1 <- lmer((Time)~Type*Hand+(1|Person/Hand), d)
# Check the assumptions
hist(resid(m1)) # fairly normal
plot(m1) # not randomly distributed    
qqnorm(resid(m1))
qqline(resid(m1)) # light tailed (not normal)

# Model with log-transformed values
m2 <- lmer(log(Time)~Type*Hand+(1|Person/Hand), d) # singular warning
hist(resid(m2)) # fairly normal
plot(m2) # looks more random
qqnorm(resid(m2))
qqline(resid(m2)) # looks more random

# Model with log10-transformed values
m3 <- lmer(log10(Time)~Type*Hand+(1|Person/Hand), d) # singular warning
hist(resid(m3)) # looks more normal
plot(m3) # identical to natural log
qqnorm(resid(m3))
qqline(resid(m3)) # identical to natural log

print(VarCorr(m3), comp=c("Variance")) 
# interaction between person and hand is not explaining any variance,
# indicating that hand has the same impact on time amongst people
ranova(m3) # Hand:Person isn't significant
anova(lm(Time~Hand*Person, data = d)) # Hand:Person isn't significant

# We are explicitly exploring hand in the hypothesis, so this shouldn't
# be a random effect anyway. 

# Model with log10-transformed values without nesting hand
m4 <- (lmer(log10(Time)~Type*Hand+(1|Person), d))
print(VarCorr(m4), comp=c("Variance")) 

# Statistical test on base10
summary(m4)
anova(m4)
emmeans(m4, pairwise~Hand|Type) 
emmeans(m4, pairwise~Hand) 
r.squaredGLMM(m4) 
# Difference between foraging rates is 
# significant for penny and not for bean 

# Figures ####
d_means <- as.data.frame(emmeans(m4, pairwise~Hand|Type)) 
# turn emmeans output into data frame
d_means <- d_means[1:4,] # drop the interaction terms
d_means$dummy <- c('Bean_Primary', 'Bean_Secondary',  # create a dummy variable 
									 'Penny_Primary', 'Penny_Secondary')  # for plotting
d_means$dummy <- factor(d_means$dummy, levels = c("Bean_Primary", 
										"Penny_Primary", "Bean_Secondary", "Penny_Secondary"))

c(6, 10, 16, 25)

ggplot(d = d_means, aes(x = dummy, y = emmean, fill = Type))+
	geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), size = 2, width = 0,
                color = "gray31")+
	geom_point(size = 15, shape = 21)+
	ylab('Foraging rate (sec)')+
  xlab('Predator attack hand')+
	scale_fill_grey(start = 0.7, end = 1, name = "Prey Type")+
	theme_classic()+
	scale_x_discrete(labels=c("Bean_Primary" = "Primary", "Penny_Primary" = "",
               "Bean_Secondary" = "Secondary","Penny_Secondary" = ""))+
	scale_y_continuous(labels=c("0.8" = "6", "1.0" = "10",
                              "1.2" = "16", "1.4" = "25"))+
	theme(text = element_text(size = 25), 
				axis.text.x = element_text(hjust = -0.0),
				axis.ticks.x = element_blank())+
	theme(axis.title.x = element_text(vjust=-0.5))+
	theme(axis.title.x = element_text(hjust=6))
