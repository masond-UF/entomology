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
Omnivores <- c("American crow", "Blue jay", "Eastern towhee",
							 "Northern cardinal", "Pileated woodpecker",
							 "Unknown woodpecker")
Insectivore.Granivore <- c("Blue grosbeak", "Brown-headed cowbird", "Field sparrow")
Insectivore <- c("Carolina wren", "Chuck-will's-widow", "Common yellowthroat",
								"Great crested flycatcher", "Loggerhead shrike")
Frugivore.Insectivore <- c("Eastern bluebird", "Eastern kingbird", "Eastern phoebe",
													"Northern mockingbird", "Red-eyed vireo", 
													"Yellow-breasted chat", "Yellow-throated vireo")
Granivore <- c("Mourning dove")

# Create and fill a column for functional groups
brd.filt.func <- brd.filt %>% 
	mutate(Functional = ifelse(Species %in% Omnivores, "Omnivore",
											ifelse(Species %in% Insectivore.Granivore, "Insectivore.Granivore",
											ifelse(Species %in% Insectivore, "Insectivore",
											ifelse(Species %in% Frugivore.Insectivore, "Frugivore.Insectivore",
											ifelse(Species %in% Granivore, "Granivore", "None"))))))

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

# Create models

###################### CREATE THE MODELS ##################################
