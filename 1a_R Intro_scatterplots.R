###############################################
##### LOAD FISHER'S IRIS DATA #################
data(iris) # load data (already exists in base R)

plot(Sepal.Length~Species, data=iris, col=c("red","blue","purple")) #make boxplot

plot(Sepal.Length~Sepal.Width, data=iris, col="blue")

plot(Sepal.Length~Sepal.Width, data=iris, 
     pch=16, col=c("red","blue","purple")[iris$Species] ) # seperate color for each species

### plot only the data for Iris virginica
plot(Sepal.Length~Sepal.Width, data=iris[iris$Species=='virginica',]) ## use brackets to select the columns you want
## read '==' as 'exactly equals', one '=' dont work here


###########################################################################################
#### SUBSETTING DATA USING TIDYVERSE ######################################################
library(tidyverse)      # load package tidyverse (install if needed)

## FILTER TO SELECT ONLY IRIS VIRGINICA
vir <- filter(iris, Species=="virginica") ## dplyr::filter filters the data
head(vir)

plot(Sepal.Length~Sepal.Width, col="red", data=vir)  ## plot from the subsetted dataframe
plot(Sepal.Length~Sepal.Width, col="red", data=iris[iris$Species=='virginica',])  ## identical to above 

## filter within plot function so you don't have to create a new dataframe
plot(Sepal.Length~Sepal.Width, col="red", 
     data=iris %>% filter(Species=="virginica")) ## identical to above using tidyverse-dplyr
## the ' %>% ' is a pipe command, read as 'and then do'
## nice bc you don't need to make the subsetted dataset

### what is more intuitive?
data1 <- iris[iris$Species=='virginica',]  # use iris, within iris select species that are viriginica
data2 <- iris %>% filter(Species=="virginica") # use iris and then filter to species that are viriginica

hist(vir$Sepal.Length)

### remove plants with small sepal lengths less than 5mm
vir_sl5 <- iris %>% filter(Sepal.Length>5)

hist(vir_sl5$Sepal.Length)

### filter to exclude things
no_vir <- iris %>% filter(Species!='virginica')  ## read '!=' as 'not equal'
                                                  # this will remove viriginica from dataset

### select certain columns
petal_dat <- iris %>% filter(Species!='virginica') %>%   ## excludes virigina, pipe at the end of the line will continue reading onto the next
  select(Species,Petal.Length,Petal.Width)               ## selects only the columns you choose
                                                                                                          

###############################################################################################################
### VISUALIZING DATA in GGPLOT2 -- SCATTERPLOTS ###############################################################
###############################################################################################################

############################################################################
### Scatterplots in base R #################################################

## plot Sepal.Length by Sepal.Width only the data for Iris virginica
plot(Sepal.Length~Sepal.Width, data=iris %>% filter(Species=='virginica'))

## plot data and add trendline
plot(Sepal.Length~Sepal.Width, data=iris %>% filter(Species=='virginica'))
abline(lm(Sepal.Length~Sepal.Width, data=iris %>% filter(Species=='virginica')))  ## adds line from lm

### plot Sepal.Length by Sepal.Width for all three species
plot(Sepal.Length~Sepal.Width, data=iris, 
     pch=16, col=c("red","blue","purple")[iris$Species] ) # separate color for each species


############################################################################
### Scatterplots in ggplot2 #################################################

## plot Sepal.Length by Sepal.Width only the data for Iris virginica
ggplot(data=iris %>% filter(Species=='virginica'), aes(x=Sepal.Width, y=Sepal.Length)) +                                               
  geom_point()

## plot data and add trendline
ggplot(data=iris %>% filter(Species=='virginica'), aes(x=Sepal.Width, y=Sepal.Length)) +                                               
  geom_point() +
  geom_smooth(method='lm')    ## geom_smooth() adds a regression line and method='lm' says use a linear model. Can fit other types of lines.

## change color of points
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length)) +          ## color= outside aes() changes color of all points on graph
  geom_point(color="blue")

## plot data and change color by species
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) +          ## color= in aes() changes point color mapped to a column in iris
  geom_point()

## add trendlines for each species
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) +          ## color= in aes() changes point color mapped to a column in iris
  geom_point() + geom_smooth(method='lm')
  
## plot each species on a separate panel                                                                  
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length)) + 
  geom_point(color="blue") +   ## color outside of aes() changes color of all points (ie. not mapped to a column)
  facet_wrap(~Species)

## add trendline                                                                   
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length)) + 
  geom_point(color="blue") +   ## color outside of aes() changes color of all points (ie. not mapped to a column)
  facet_wrap(~Species) +
  geom_smooth(method='lm')


##############################################################################
### Make fancy scatterplots #################################################

## default plot for Sepal.Length by Sepal.Width for all three species
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) +          ## color= in aes() changes point color mapped to a column in iris
  geom_point() + geom_smooth(method='lm')

## change theme to new default for better looking plot
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) +          ## color= in aes() changes point color mapped to a column in iris
  geom_point() + geom_smooth(method='lm') +
  theme_bw()

## change themes manually to make it super fancy
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) +          ## color= in aes() changes point color mapped to a column in iris
  geom_point(size=3) + geom_smooth(method='lm') +                               ## change point size to make them bigger
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73")) +               ## change points to a color-blind friendly palette
  theme(panel.background = element_blank(),                              
      panel.border = element_rect(color="black", fill=NA, size=2)) +            ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=20)) +                                            ## make font larger
  theme(legend.position = "top", legend.title=element_blank(),
        legend.background = element_rect(fill="white", size=1.25, linetype="solid", color="black"))   ## change position of legend


## same plot as above, but plot species with different shaped points
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, shape=Species)) +          ## color= in aes() changes point shape mapped to a column in iris
  geom_point(size=3) + geom_smooth(method='lm') +                               ## change point size to make them bigger
  scale_shape_manual(values=c("circle","square","triangle")) +                  ## change shapes
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +            ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=20)) +                                            ## make font larger
  theme(legend.position = "top", legend.title=element_blank(),
        legend.background = element_rect(fill="white", size=1.25, linetype="solid", color="black"))   ## change position of legend

## same plot as above, but plot species by both shape and color
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, shape=Species, color=Species)) +          ## color= in aes() changes point shape mapped to a column in iris
  geom_point(size=3) + geom_smooth(method='lm') +                               ## change point size to make them bigger
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73")) +               ## change points to a color-blind friendly palette
  scale_shape_manual(values=c("circle","square","triangle")) +                  ## change shapes
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +            ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=20)) +                                            ## make font larger
  theme(legend.position = "top", legend.title=element_blank(),
        legend.background = element_rect(fill="white", size=1.25, linetype="solid", color="black"))   ## change position of legend


#######################################################################################################################################
#### Use the iris dataset to make some plots between Petal.Length and Sepal.Length
 # 1. use the facet_Wrap to make three panels, one for each species. Also code species to have different shapes, but make all points 'red'
     # Do not include trendlines. Make the background white. theme_bw is okay, if time try playing around with other themes.



