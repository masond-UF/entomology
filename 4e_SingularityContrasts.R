library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(MuMIn)

### R square regression example

reg1 <- read_csv('RegR2.csv')
head(reg1)

ggplot(reg1, aes(x=Precip, y=Richness, color=Habitat)) + geom_point() + geom_smooth(method='lm') + theme_bw()

m1 <- lm(Richness ~ Precip * Habitat, data=reg1)
Anova(m1)  ## look at anova table
summary(m1) ## look at R2

m2 <- lmer(Richness ~ Precip * Habitat + (1|Site), data=reg1)
anova(m2)   ## look at anova table
summary(m2) ## look at VC. No r2???

## calculate r2 for mixed models from MuMIn package
r.squaredGLMM(m2) 

######################################################################################################
####### QUESTIONS TO WORK ON IN BREAKOUT GROUPS ######################################################
## Experiment examining how host plant and temperature influence parasite loads in an insect.
## 4 temps (20, 24, 28, 32C) crossed with three host plants (potato, tomato, physalis)
## Experiment used larvae from four clutches and was conducted in eight climate chambers
## Clutch and Chamber are random effects, but not nested

## QUESTIONS TO WORK ON
## How does parasite number differ in response to temperature for each host plant? (hint: use emmeans contrasts)
## How much variation is there within the random effects (ie. among clutches or among chambers)?
## Does the output change if you remove Clutch or Chamber random effects?

d1 <- read_csv('4e_SingularityContrasts.csv')
d1$Temp <- as.factor(d1$Temp)
head(d1)

## model and some code to get started
lm1 <- lmer(Parasites ~ Temp*Host + (1|Clutch) + (1|Chamber), data=d1)
summary(lm1)
anova(lm1)
emmeans(lm1, pairwise~Host:Temp) ## one big contrast table to get started. May need to revise.

neworder <- c("A","E","B", "F", "G", "C", "H", "D")
d1 <- arrange(transform(d1,
             Chamber=factor(Chamber,levels=neworder)),Chamber)


ggplot(lm1, aes(x = Host, y = Parasites, color = Chamber))+
	geom_boxplot()+
	facet_wrap(~Temp)+
	theme_bw()

anova(lm(Parasites~Chamber, data = d1))

ranova(lm)
