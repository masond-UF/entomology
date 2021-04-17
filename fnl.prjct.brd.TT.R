###### 16 APRIL 2021 ###### DAVID S. MASON ###### EXP DESIGN PROJECT
###################### SET—UP WORKSPACE ###############################
library(tidyverse)
library(lubridate)

brd <- read.csv("PRELIM CAM.csv")
###################### MUNGE DATA ##################################

# Convert the date into a format the tidyverse trucks with
brd$Date <- mdy(brd$Date)

# Round the date by week to improve figure EDA interpretation
brd$Week <- round_date(brd$Date, unit = 'week')

# Create vector of animals observed which are not birds
not.brds <- c("Bat", "Southern flying squirrel", "Unknown animal",
							"Unknown bat", "Unknown mammal", "Unknown mouse",
							"White-tailed deer", "Southern flying squirrel ")

# Filter out non-birds from the data set
brd.filt <- brd %>% filter(!Species %in% not.brds)

# Drop unknowns we can't assign a functional group to
brd.filt <- brd.filt %>% filter(!Species == "Unknown bird")

# Drop vulture because there is only one scavenger observation
brd.filt <- brd.filt %>% filter(!Species == "Unknown vulture")

# Check to make sure we only have useful values left
spec.list <- as.data.frame(unique(brd.filt$Species))

# Need to make Towhee into Eastern towhee for consistency
levels(brd.filt$Species)[levels(brd.filt$Species) == "Towhee"] <- "Eastern towhee"

# Need to make Eastern Kingbird into Eastern kingbird for consistency
levels(brd.filt$Species)[levels(brd.filt$Species) == "Eastern Kingbird"] <- "Eastern kingbird"

###################### CREATE & ASSIGN FUNCTIONAL GROUPS ####################### 

# Make vectors for each functional group
Frugivores <- c("American crow", "Blue jay", "Eastern towhee",
							 "Northern cardinal", "Pileated woodpecker",
							 "Unknown woodpecker", "Eastern bluebird", "Eastern kingbird", 
								"Eastern phoebe",	"Northern mockingbird", "Red-eyed vireo", 
								"Yellow-breasted chat", "Yellow-throated vireo")
Others <- c("Mourning dove", "Blue grosbeak", "Brown-headed cowbird", 
						"Field sparrow", "Carolina wren", "Chuck-will's-widow", "Common yellowthroat",
						"Great crested flycatcher", "Loggerhead shrike")
# Create and fill a column for functional groups
brd.filt.func <- brd.filt %>% 
	mutate(Functional = ifelse(Species %in% Frugivores, "Frugivore",
											ifelse(Species %in% Others, "Other","None")))

# Check to make sure this worked
unique(brd.filt.func$Functional)

###################### EXPLORATORY DATA ANALYSIS ########################## 
summary(brd.filt.func)

# Looking at bird observations by treatment
brd.filt.func %>% group_by(Treatment,Site) %>% summarize(Count = n()) %>% 
	ggplot(aes(x = Treatment, y = Count)) + geom_boxplot()

# Looking by site (looks like an important RV)
brd.filt.func %>% group_by(Site, Treatment) %>% summarize(Count = n()) %>% 
	ggplot(aes(x = Treatment, y = Count)) + geom_col() + facet_wrap(~Site)

# Looking by functional group (looks like a possible covariate)
brd.filt.func %>% group_by(Treatment, Functional) %>% 
	summarize(Count = n()) %>% 
	ggplot(aes(x = Treatment, y = Count, color = Functional)) + 
	geom_col(position = "Dodge") 

# Lets explore the magnet effect with Date/Week (WOAH!)
brd.filt.func %>% 
	group_by(Treatment,Week,Functional) %>% 
	summarize(Count = n()) %>% 
	ggplot(aes(x = Week, y = Count, color = Functional))+
	geom_point()+
	geom_line()+
	facet_wrap(~Treatment)

###################### SUMMARIZE THE DATA ######################################

# Data to compare all bird observations by treatment
tot.obs <- brd.filt.func %>% 
	group_by(Treatment,Site,Trap,Functional) %>% 
	summarize(Count = n())

# Data to visualize trend related to time
time.obs <- brd.filt.func %>% 
	group_by(Treatment,Site, Functional, Week) %>% 
	summarize(Count = n())

###################### CREATE THE TOTAL MODELS ########################################
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(MuMIn)
library(DHARMa)
library(RVAideMemoire)

# GLM
smpl.mod <- glm(Count~Treatment*Functional,data=tot.obs, family = 'poisson')
summary(smpl.mod)

## Diagnose GLM model fit 
plot(smpl.mod)
hist(smpl.mod$residuals) # right skewed
plot(smpl.mod$residuals~smpl.mod$fitted.values)  
abline(h=0) # look decent
qqPlot(smpl.mod$residuals) # big outlier
boxplot(smpl.mod$residuals ~ tot.obs$Treatment) # residuals look good

sim.smpl.mod <- simulateResiduals(fittedModel = smpl.mod, n = 250)
plot(sim.smpl.mod) # warnings

### Test for overdispersion (ie. more variance in the data than expected)
testDispersion(sim.smpl.mod) # overdispersion

### Test for zero-inflation (ie. more zeros in the data than expected)
testZeroInflation(sim.smpl.mod) # not zero-inflated

##### GLMM
nst.mod <- glmer(Count~Treatment*Functional+(1|Site),data=tot.obs, family = 'poisson')
summary(nst.mod)

###### Diagnose GLMM model fit 

sim.nst.mod <- simulateResiduals(fittedModel = nst.mod, n = 250)
plot(sim.nst.mod) # little better than before

overdisp.glmer(nst.mod) # ratio should be 1 (its high)

####### GLM OD
tot.obs$obs <- 1:length(tot.obs$Count) # adding dummy variable as RV

nst.OD.mod <- glmer(Count~Treatment*Functional+(1|Site)+(1|obs),data=tot.obs, family = 'poisson')
summary(nst.OD.mod)

######## Diagnose the OD GLMM
sim.nst.OD.mod <- simulateResiduals(fittedModel = nst.OD.mod, n = 250)
plot(sim.nst.mod) # still doesn't look good

testUniformity(sim.nst.mod) # Failed
testDispersion(sim.nst.mod) # Failed
testZeroInflation(sim.nst.mod) # Failed

######### Negative binomial
nst.nb.mod <- glmer.nb(Count~Treatment*Functional+(1|Site), data=tot.obs)
summary(nst.nb.mod)

########## Diagnose the nb
sim.nst.nb.mod <- simulateResiduals(fittedModel = nst.nb.mod, n = 250)
plot(sim.nst.nb.mod)

########### Poisson with Zero Inflation (note no overdispersion). Is it enough to account for zero inflation without correcting for overdispersion? 
library(glmmTMB) ## new package. similar to lme4 but more flexibility and options

nst.ZI.mod <- glmmTMB(Count~Treatment*Functional+(1|Site), 
                      family=poisson, 
                      zi=~Treatment*Functional, 
                      data=tot.obs)

############ Diagnose the OD GLMM
sim.nst.ZI.mod <- simulateResiduals(fittedModel = nst.ZI.mod, n = 250)
plot(sim.nst.ZI.mod) # still doesn't look good

############# Poisson corrected for zero-inflation AND overdispersion
nst.ZI.OD.mod <- glmmTMB(Count~Treatment*Functional+(1|Site)+(1|obs), 
                     family=poisson, 
                     zi=~Treatment*Functional, 
                     data=tot.obs)

############## Diagnose the ZI and OD GLMM 
sim.nst.ZI.OD.mod <- simulateResiduals(fittedModel = nst.ZI.OD.mod, n = 250)
plot(sim.nst.ZI.OD.mod) # Looks good

##############  ZI negative binomial
nst.ZI.nb.mod <- glmmTMB(Count~Treatment*Functional+(1|Site),
                     family=nbinom2, 
                     zi=~Treatment*Functional, 
                     data=tot.obs)

############### Diagnose the model
sim.nst.ZI.nb.mod <- simulateResiduals(fittedModel = nst.ZI.nb.mod, n = 250)
plot(sim.nst.ZI.nb.mod) # Looks good

###################### COMPARE THE TOTAL MODELS ######################################
summary(smpl.mod) ## summary for simple poisson model
Anova(smpl.mod)  

summary(nst.mod) ## summary for nested glmm
Anova(nst.mod)  

summary(nst.OD.mod)  ## summary for overdispered nested glmm
Anova(nst.OD.mod)  

summary(nst.nb.mod)  ## summary for nested negative binomial
Anova(nst.nb.mod)

summary(nst.ZI.mod)  ## summary for zero-inflated poisson
Anova(nst.ZI.mod)

summary(nst.ZI.OD.mod)  ## summary Poisson corrected for ZI AND OD
Anova(nst.ZI.OD.mod)

summary(nst.ZI.nb.mod)  ## summary forzero-inflated nb
Anova(nst.ZI.nb.mod)

## which model is best?
AIC(smpl.mod,nst.mod,nst.OD.mod,nst.nb.mod,nst.ZI.mod,
		nst.ZI.OD.mod,nst.ZI.nb.mod)

### examine SD associated with random effects 
options (scipen = 999)
VarCorr(nst.ZI.OD.mod)
SD <- matrix(c("Site","obs","total",0.0000012401,0.8552376549, sum(0.0000012401+0.8552376549)),nrow=3,ncol=2)
colnames(SD) <- c("Groups", "SD")
SD <- as.data.frame(SD)

means <- as.data.frame(emmeans(nst.ZI.OD.mod, ~Treatment*Functional, type = 'response', bias.adj = T,
				sigma = 0.8552389))
###################### MAKE TOTAL FIGURE ######################################
library("ggpubr")
levels(means$Treatment)[levels(means$Treatment)=="B"] <- "Recent burn"
levels(means$Treatment)[levels(means$Treatment)=="C"] <- "One-year rough"


ggplot(means, aes(x = Functional, y = rate))+
	geom_linerange(aes(ymin=lower.CL,ymax=upper.CL, col=Functional), 
								 size=1.5)+
	geom_point(aes(col = Functional),size = 8)+
	theme_classic()+
	theme(text = element_text(size = 22),
				legend.title = element_blank(),
				legend.position = "none",
				plot.title = element_text(hjust = 0.5),
				strip.background = element_blank(),
   			strip.text.y = element_blank(),
				axis.title.x = element_text(face='bold', vjust=-2.5),
				axis.title.y = element_text(face='bold', vjust=3),
				strip.text.x = element_text(size = 24,face ='bold'),
				legend.spacing.x = unit(0.5, 'cm'),
				plot.margin = unit(c(1,1.2,0.9,1.2),"cm"),
				legend.box.spacing = unit(1.2,'cm'),
				axis.ticks.x = element_blank(),
				axis.ticks.y = element_line(size=1.2))+
	scale_color_manual(values = c("#00AFBB", "#E7B800"))+
	scale_y_continuous(breaks=c(0,2,4,6,8,10), limits = c(0,11))+
	ylab("Mean total observations")+
	xlab("Feeding guild")+
	facet_wrap(~Treatment)
	


