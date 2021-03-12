library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(MuMIn)
library(agridat) ## install and load package for datasets

## load rice experiment and process data
data("gomez.multilocsplitplot")
gomez.multilocsplitplot$nitro <- as.factor(gomez.multilocsplitplot$nitro)
gomez <- gomez.multilocsplitplot
head(gomez)


## plot data
ggplot(gomez, aes(x=gen, y=yield, fill=nitro))+geom_boxplot(outlier.shape = NA)+
  geom_point(position = position_jitterdodge(jitter.height=0,jitter.width=.1))+facet_wrap(~loc)

## average data by loc,nitro,gen to account for pseudo-replication
gomez_summarized <- gomez %>% group_by(loc,nitro,gen) %>% summarize(yield=mean(yield, na.rm=T))


## Regular two-way anova using summarized dataset -- no blocking
mm0 <- lm(yield ~ gen*nitro, data=gomez_summarized)
anova(mm0)

## Two-way anova with block as a random effect
mm1 <- lmer(yield ~ gen*nitro+(1|loc), data=gomez_summarized)
anova(mm1)

## Two-way anova with block and nitro nested within block as random effects
mm2 <- lmer(yield ~ gen*nitro+(1|loc/nitro), data=gomez_summarized)
anova(mm2)

## summary for split-plot model
summary(mm2)

## emmeans for just for nitro (let's not worry about genotype)
emmeans(mm2, pairwise~nitro)

## extract means and make plot of nitrogen emmeans
n1 <- emmeans(mm2, ~nitro) %>% as.data.frame()

ggplot(n1, aes(x=nitro, y=emmean)) + geom_point(size=5) + geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0, lwd=2) + 
  ylab("yield (g) +/- 95% CI") + theme_bw(base_size = 20)


#########################################################################
##### CHALLENGE #########################################################

