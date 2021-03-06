library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(plyr)

d1 <- read.csv("ttseedremoval.csv")

d1$TREATMENT <- revalue(d1$TREATMENT, c("B"="BURN", "C"="CONTROL"))
d1$SPECIES <- revalue(d1$SPECIES, c("SIAL"="MUSTARD", 
																		"HEAN"="SUNFLOWER",
																		"CAAN"="PEPPER"))


lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

means <- d1 %>%
 	dplyr::group_by(DATE, TREATMENT, SPECIES) %>%
  dplyr::summarise(smean = mean(REMOVAL, na.rm = TRUE),
            ssd = sd(REMOVAL, na.rm = TRUE),
  					count = n()) %>%
  dplyr::mutate(se = ssd / sqrt(count),
         lower_ci = lower_ci(smean, se, count),
         upper_ci = upper_ci(smean, se, count))



colors <- c("red", "black")
ggplot(means, aes(x = DATE, y = smean))+
	geom_point()+
	facet_wrap(~TREATMENT+SPECIES, dir = "v")+
	geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci))+
	theme_bw()
