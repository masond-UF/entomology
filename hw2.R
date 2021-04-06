# HW 2—David Mason
# Analyzing the simulated island biogeography 
# class experiment with a binomial model
# Set-up ####
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(MuMIn)
library(agridat)
library(DHARMa)
library(ggeffects)
library(ggpubr)
d <- read.csv("HW2.csv")
# Data exploration ####
summary(d) # we got NAs and different inputs

levels(d$Matrix)[levels(d$Matrix)=="both"] <- "Both"
levels(d$Matrix)[levels(d$Matrix)=="one"] <- "One"
d <- drop_na(d)

summary(d) # looks better

# lets check the data
plot(d$Emigration,d$Colonization) # some outliers

# is emigration > immigration?
as.logical(d$Emigration<d$Colonization)

# need to drop person six and eight
d <- filter(d, Person != "Six" & Person != "Eight")

# Create sucesses and failures ####
d <- d %>% mutate(Failures = Emigration-Colonization)
as.logical(d$Failures>=0) # Only valid values now

successes <- as.numeric(d$Colonization)
failures <- as.numeric(d$Failures)

# Create model ####
mod <- glmer(cbind(successes, failures) ~ Size_cm * Matrix + (1|Person), 
					 data=d, family=binomial(link = "logit"))

# anova and summary table
Anova(mod)
summary(mod)

## Check residuals
plot(mod)
sim_mod <- simulateResiduals(fittedModel = mod, n = 250)
plot(sim_mod) # looks good

## print out back-transformed means
emmeans(mod, pairwise ~ Matrix:Size_cm, type="response") 
emtrends(mod, pairwise~Matrix, var = 'Size_cm') 

exp(0.148) # both 
exp(0.0132) # se

exp(0.110) # single 
exp(0.0125) # se

## overall model power
									 
r.squaredGLMM(mod) 
print(VarCorr(mod), comp=c("Variance")) 

# random effect stuff
coef(mod) # intercepts vary for random effect (Person)
exp(-4.240983) # 0.01439344
exp(-1.034468) # 0.3554154

# Create figures ####
d$probability <- d$Colonization/d$Emigration

ggplot(d, aes(x = Size_cm, y = probability, color = Matrix))+
	geom_point(size = 4, alpha = 0.6)+
	geom_smooth(method=lm)+ 
	theme_bw()

size_fig <- ggpredict(mod, terms = c("Size_cm [all]")) %>% plot()
matrix_fig <- ggpredict(mod, terms = c("Matrix [all]")) %>% plot()
ggarrange(size_fig, matrix_fig)

ggpredict(mod, c("Size_cm [all]", "Matrix [all]")) %>% plot()


