###############################################
##### LOAD AND VIEW FISHER'S IRIS DATA USING BASE R #################
data(iris) # load data (already exists in base R)
head(iris) # print first 6 lines of dataset
tail(iris) # print last 6 lines of dataset

str(iris) # print 'structure' of dataset giving you info about each column
 

#################################################################################
######## making and modifying variables ###################################################

### make a new column that is a unique number
iris$Plant <- 1:length(iris$Species)

### make a new column that is total petal and sepal length
iris$PetSep.Length <- iris$Petal.Length+iris$Sepal.Length

### make a new column that log-transforms PetSep.Length
iris$lnPS.Len <- log(iris$PetSep.Length)

### make a new column for 'genus'. The only values you want is "Iris"
iris$Genus <- 'Iris'

### combine two columns
?paste
iris$GenSpp <- paste(iris$Genus, iris$Species, sep="_")

### change Species=='versicolor' to 'versi' in the GenSpp column
# sub() can be used for replacement but will only do 1 replacement
# gsub() can also be used for replacement but will all matching instances
?gsub

iris$GenSpp <- gsub('versicolor', 'versi', iris$GenSpp )  ## looks for 'versicolor' and replaces it with 'versi' in the column iris$Species
                                                           ## use caution when overwriting a column!! 
                                                           ## (it will remain unchanged in your original file though)

### use gsub() to add genus name to species column (alternative to making new column and then pasting together)
iris$GenSpp1 <- gsub('.*^', 'Iris_', iris$Species)

#########################################################################################
#### LOAD AND VIEW FISHER'S IRIS DATA USING TIDYVERSE ######################################################
library(tidyverse)      # load package tidyverse (install if needed)

data(iris)               # reload iris to clear changes from above
iris1 <- as_tibble(iris) # load iris and convert to tibble

str(iris1)         ## same as str() above
glimpse(iris1)     ## similar to str(), just glimpses data


##########################################################################################
######## making and modifying variables ###################################################

## mutate() will allow you create and modify variables
iris1 <- iris1 %>% mutate(Plant=1:length(iris1$Species), PetSep.Length=Petal.Length+Sepal.Length, lnPS.Len=log(PetSep.Length), 
                          Genus='Iris', GenSpp=gsub('.*^', 'Iris_', .$Species)) 
          ## note that I am overwriting iris1. Use with caution


### summarize() calculates means, sd, min, max, etc. on a dataset
### summarize() summarise() 

iris1 %>% summarize(mean(Petal.Length))  ## mean of Petal.Length in dplyr

mean(iris1$Petal.Length) ## mean of Petal.Length in base R

### summarize lnPS.Len by Species
means_PetLen1 <- iris1 %>% group_by(Species) %>% summarize(Petal.Length_mean=mean(Petal.Length))

means_PetLen2 <- aggregate(Petal.Length~Species, FUN="mean", data=iris1)


### summarize multiple variables by species use summarize_all()
means1 <- iris1 %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, lnPS.Len, Species) %>%  group_by(Species) %>% 
  summarize_all(list(mean=mean,sd=sd,n=length))


###############################################################################################################################
### reshape data for better usability #########################################################################################

### reshape data from wide to long
iris_long <- iris1 %>% group_by(Species) %>% 
  pivot_longer(cols=c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, lnPS.Len), names_to = 'Trait', values_to = 'value')

### calculate mean, sd, and n for each Species X trait combo and then calculate SE
means2 <- iris_long %>% group_by(Species,Trait) %>% summarize(mean=mean(value), sd=sd(value), n=length(value)) %>% 
  mutate(se=sd/sqrt(n)) %>% filter(Trait!='lnPS.Len')

### note that lines 83-84 and 87-88 could all be done in one long piped command
means2a <- iris1 %>% group_by(Species) %>% 
  pivot_longer(cols=c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, lnPS.Len), names_to = 'Trait', values_to = 'value') %>% 
  group_by(Species,Trait) %>% summarize(mean=mean(value), sd=sd(value), n=length(value)) %>% 
  mutate(se=sd/sqrt(n)) %>% filter(Trait!='lnPS.Len')

#################################################################################################################################
### make plot, below are two plots to start with. One is ineffective and one is more effective.

ggplot(data=means2, aes(x=Species, y=mean, fill=Trait)) + 
  geom_point(size=5, position=position_dodge(width=0.25), pch=22) + labs(y="Floral part measurement (mm)") +
  geom_errorbar(aes(ymin=(mean-sd), ymax=(mean+sd)), width=.2, position=position_dodge(width=0.25), lwd=1.5) +
  scale_fill_manual(values=c("purple", "forestgreen","blue","firebrick"), labels=c("Petal Length","Petal Width", "Sepal Length", "Sepal Width")) +
  theme(panel.border=element_rect(color="black",size=2, fill=NA))+xlab("Species")

ggplot(data=iris_long %>% filter(Trait!='lnPS.Len'), aes(x=Species, y=value)) + 
  geom_boxplot() + 
  facet_wrap(~Trait, scales = 'free_y')+
  labs(y="Floral part measurement (mm)") +
  scale_color_manual(values=c("purple", "forestgreen","blue","firebrick")) +
  theme_bw()

