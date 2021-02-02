library(tidyverse)
library(agridat) ## package with lots of agricultural datasets. Install if needed.

#### R CHALLENGE DUE FRIDAY JAN 29, 2021

## Examine the ortiz.tomato.yield dataset. The help gives decent metadata (but it's not great).
## The study grew several tomato genotypes at 18 sites around the world
## There are two datasets, the ortiz.tomato.cov contains site-level information.
## The ortiz.tomato.yield dataset contains data on yield for a bunch of plants grown at each site.
?ortiz.tomato


## Part 1: Examine the yield dataset
data("ortiz.tomato.yield")
yield <- as_tibble(ortiz.tomato.yield)
  
head(yield)
str(yield)
glimpse(yield)

##################################################################################################################################
## Q1. Look at the structure of the data. How many environments are in the dataset? How many genotypes are in the dataset?

str(yield)
# 18 environments
# 15 genotypes


##################################################################################################################################
## Q2. Use mutate() and pipes ( %>% ) to calculate a new variable called 'frt_wt_ha' that is fruit weight per hectare from 
##      yield (number of tomatoes per hectare) and weight (weight per fruit in g). Convert it to kg. 
##      Also, genotype 'OP10' was mislabel. It should be 'OP5'.
##      In the same line of code, use gsub() to replace 'OP10' with 'OP5'.
##      Note that if you rewrite over the original dataset and make a mistake, you can reload it (line 15) to clear your errors.

yield <- yield %>% 
						mutate(frt_wt_ha = (yield*weight)/1000, 
						gen = gsub('OP10', 'OP5', gen))

##################################################################################################################################
## Q3. Make a boxplot of each of  your three measured variables by environment. Plot each variable in a separate panel (see example).
##     Hint: you may need to reshape the data. For the plot, try add scales="free" and nrow=3 to facet_wrap.

?pivot_longer()


yield_long <- yield %>% 
	pivot_longer(cols = c(yield, weight, frt_wt_ha),
							 names_to = "variable",
							 values_to = "value")

variable_names <- c('frt_wt_ha' = "Fruit weight (kg/ha)",
										'weight' = "Fruit weight (g)",
										'yield' = 'Fruit yield (tomatoes per ha)')

ggplot(yield_long, aes(x = env, y= value))+
	geom_boxplot()+
	facet_wrap(~variable, scales = "free_y", ncol = 3,
						 labeller = as_labeller(variable_names))+
	xlab("Environment")+
	ylab("Variable measured")+
	theme_classic()
	

## BONUS: Do Q2 and Q3 all in one code block
yield %>% 
	pivot_longer(cols = c(yield, weight, frt_wt_ha),
							 names_to = "variable",
							 values_to = "value") %>% 
	ggplot(aes(x = env, y= value))+
	geom_boxplot()+
	facet_wrap(~variable, scales = "free_y", ncol = 3,
						 labeller = as_labeller(variable_names))+
	xlab("Environment")+
	ylab("Variable measured")+
	theme_classic()

##################################################################################################################################
## Q4. Make the same plot above but filter out any environment that has 1 for Driv.
##     Hint: You'll need to look in the covs datasets to which environments have Driving present.
   
data("ortiz.tomato.covs")
covs <- as_tibble(ortiz.tomato.covs)

head(covs)
str(covs)

##     Another hint: you'll need to merge the datasets together in order to filter out the ones that have no Driving.
##     Hint 3: use the full_join command below to merge the datasets. They are relational.

## code to join: replace the dataset if you called your summarized yield dataset something other than 'yield1'. 
##  Replace the XXXX with ID column for linking.
tomato <- full_join(yield,covs, by='env')

tomato_filtered <- tomato %>% 
	filter(Driv!=1) 

tomato_filtered_long <- tomato_filtered %>% 
	select(env, gen, weight, yield, frt_wt_ha) %>% 
	pivot_longer(cols = c(yield, weight, frt_wt_ha),
							 names_to = "variable",
							 values_to = "value")

ggplot(tomato_filtered_long, aes(x = env, y= value))+
	geom_boxplot()+
	facet_wrap(~variable, scales = "free_y", ncol = 3,
						 labeller = as_labeller(variable_names))+
	xlab("Environment")+
	ylab("Variable measured")+
	theme_classic()

##################################################################################################################################
## Q5. What are the means and standard deviation of 'fruit_ha' for each environment? Use summarize to make a table.
##     Protip: If you run into problems with na's, you can add 'na.rm=T' to your summarize function like this: (frt_wt_ha, na.rm=T). 


tomato_means <- tomato_filtered_long %>% 
	filter(variable == 'frt_wt_ha') %>% 
	group_by(env) %>% 
	summarize(mean = mean(value, na.rm = TRUE),
						sd = sd(value, na.rm = TRUE))
	

## export as .csv file 
?write.csv
write.csv(tomato_means, "tomato_means.csv")


p1 <- ggplot(tomato_filtered_long, aes(x = env, y = value, fill = env))+
	geom_boxplot()+
	facet_wrap(~variable, scales = "free", nrow = 1, strip.position = "left",
						 labeller = as_labeller(variable_names))+
	ylab(NULL)+
	theme(legend.position = "top",
				axis.text.x = element_text(angle = 45),
				strip.background = element_blank(),
				text = element_text(size = 16),
				strip.placement = "outside")
