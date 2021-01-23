library(tidyverse)
library(emmeans)
data(iris)
head(iris)

## plot Sepal.Length by Species
plot(Sepal.Length~Species, data=iris) #make boxplot

###########################################################################################
#### PLOTTING IN GGPLOT ###################################################################

## boxplot of Sepal.Length by Species
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_boxplot()

## boxplot w/ points for Sepal.Length by Species
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_boxplot() + geom_point()

## boxplot w/ jittered points for Sepal.Length by Species
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_boxplot() + geom_jitter()

## boxplot w/ jittered points for Sepal.Length by Species (fix jittering)
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_boxplot(outlier.shape=NA) + geom_jitter(height=0, width=.15)

## dotplot for Sepal.Length by Species (fix jittering)
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_dotplot(binaxis = "y", stackdir = "center") 


## violin plot for Sepal.Length by Species
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_violin()  

## violin plot (w/ dotplot) for Sepal.Length by Species
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_violin() + geom_dotplot(binaxis = "y", stackdir = "center") 

## violin plot (w/ boxplot) for Sepal.Length by Species
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_violin() + geom_boxplot(width=.1, fill="grey")


## change color of boxes
ggplot(iris, aes(x=Species, y=Sepal.Length, fill=Species)) + geom_boxplot(outlier.shape=NA) + 
  geom_jitter(height=0, width=.15) + 
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73"))               ## change points to a color-blind friendly palette
  

################################################################################################
#### ADDING MEANS PLOTS ###############################################################################

## boxplot of Sepal.Length by Species w/ mean
ggplot(iris, aes(x=Species, y=Sepal.Length, fill=Species)) + geom_boxplot(outlier.shape=NA) + 
  geom_jitter(height=0, width=.15) + 
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73"))  +             ## change points to a color-blind friendly palette
  stat_summary(fun=mean, geom="point", size=5, color="red")

## use linear model to estimate means and SE for plotting
sl1 <- lm(Sepal.Length~Species, data=iris)
anova(sl1)

sl_means <- emmeans(sl1, ~Species) %>% as.data.frame() ## saves emmeans as dataframe

###############################################
## make basic barplot w/ SE bars
ggplot(sl_means, aes(x=Species, y=emmean)) + geom_bar(stat="identity", color="black", fill='grey') + 
  geom_errorbar(aes(ymin=(emmean-SE), ymax=(emmean+SE)), width=.2)

## spruce up barplot
ggplot(sl_means, aes(x=Species, y=emmean)) + geom_bar(stat="identity", color="black", fill='grey', width=.5) + 
  geom_errorbar(aes(ymin=(emmean-SE), ymax=(emmean+SE)), width=.2) + ## make bars thinner
  geom_hline(yintercept = 0) + 
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +            ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=20)) 

## add points to barplot [MORE ADVANCED...]
ggplot() + geom_bar(data=sl_means, aes(x=Species, y=emmean), stat="identity", color="black", fill='grey', width=.5) + 
  geom_errorbar(data=sl_means ,aes(x=Species, y=emmean, ymin=(emmean-SE), ymax=(emmean+SE)), width=.2) + ## make bars thinner
  geom_jitter(data=iris, aes(x=Species, y=Sepal.Length), height=0, width=.15) +
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +            ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=20)) 

###############################################
## make dotplot w/ SE bars
ggplot() + 
  geom_jitter(data=iris, aes(x=Species, y=Sepal.Length), height=0, width=.1) +
  geom_point(data=sl_means, aes(x=Species, y=emmean), color="red", size=5) + 
  geom_errorbar(data=sl_means ,aes(x=Species, y=emmean, ymin=(emmean-SE), ymax=(emmean+SE)), width=.2, color="red", lwd=2) + ## make bars thinner
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +            ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=20)) 

## save as high-res .tif file
## save as .tiff file
plot1 <- ggplot() + 
  geom_jitter(data=iris, aes(x=Species, y=Sepal.Length), height=0, width=.1) +
  geom_point(data=sl_means, aes(x=Species, y=emmean), color="red", size=3) + 
  geom_errorbar(data=sl_means ,aes(x=Species, y=emmean, ymin=(emmean-SE), ymax=(emmean+SE)), width=.2, color="red", lwd=1.25) + ## make bars thinner
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +            ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=20)) 

tiff("ExamplePlot.tiff", width=6, height=4, units='in',
     res=600, compression='lzw')
plot1
dev.off()


###########################################################################################################################################
######## R CHALLENGE #####################################################################################################################

### From R file 1c_R Intro_anova #5. Make a nice looking plot that includes the mean and SE of chick weight for the six feeds.
### Try making a boxplot with jittered points and then overlay the mean +/- SE in a large dot of a different color.
### Try changing the color of each box. Customize the colors, themes, etc. to make it look nice and readable.




