#' R code for data exploration
library(tidyverse) 
library(agridat)
library(corrplot) #new package
library(EnvStats) #new package
library(nycflights13) #new package

#' Here we use the same dataset as last week on tomota yields from different environments and different 
#' genotypes.
## Part 1: Examine the yield dataset
data("ortiz.tomato.yield")
data("ortiz.tomato.covs")

yield <- as_tibble(ortiz.tomato.yield)
covs <- as_tibble(ortiz.tomato.covs)

tomato <- full_join(yield,covs, by='env') 

tomato

#Reviewing ways to look at structure in the dataset
str(tomato)
summary(tomato)

#'Central tendency measures
#'Here we measure and compare how central tendency measures fluctuate between different distributions
#'We first calculate the mean, and median. Note we use na.rm=T because we have NAs in the data. 
#'When we use this argument R will remove NAs when calculating your metrics

#calculate the mean and median for weight
mean_weight<-mean(tomato$weight, na.rm = T)
median_weight<-median(tomato$weight,na.rm = T)

#Get a histogram of the distribution of weight
ggplot(tomato) + geom_histogram(aes(x = weight), color = "white") + theme_classic()

#Looks relatively normally distributed
#Let's re-plot but with the mean and the median as vertical lines
#mean will be colored in blue, median in orange
ggplot(tomato) +
  geom_histogram(aes(x = weight), color = "white") +
  geom_vline(aes(xintercept= mean_weight), color = "blue", size = 2)+
  geom_vline(aes(xintercept= median_weight), color = "orange", size = 2) +
  theme_classic()
#They're right on top of each other which is characteristic of a normally distributed variable

#calculate the mean and median for yield
mean_yield<-mean(tomato$yield, na.rm = T)
median_yield<-median(tomato$yield,na.rm = T)

mean_yield
median_yield

#Get a histogram of the distribution of yield
ggplot(tomato) + 
  geom_histogram(aes(x = yield), color = "white") +
  theme_classic()
#Looks very right (positive) skewed

#Again re-plot with the mean and the median
ggplot(tomato) +
  geom_histogram(aes(x = yield), color = "white") +
  geom_vline(aes(xintercept= mean_yield), color = "blue", size = 2)+
  geom_vline(aes(xintercept= median_yield), color = "orange", size = 2) +
  theme_classic()
#'This time there is a spread. Median is less than the mean. The median is actually more
#'closer to the mode of the yield (taller bars) than the mean.

###OUTLIERS###
#'Let's scan for outliers in this data with the numeric variables
boxplot(tomato$weight) #nicely distributed, no warning signs here
boxplot(tomato$yield) #Lot's of warning signs here

#How many points above the upper limit of the boxplot?
outliers<-rosnerTest(yield$yield, k =10)
outliers$all.stats

#From the test we see that three values are deemed as outliers (138.8, 136, and 134.6)

#Assessing normality
#quick transformations that may work:
hist(tomato$yield)
hist(log(tomato$yield))
hist(sqrt(tomato$yield))



#'Correlations, here we use a more flexible dataset to look at relationships
#'between different variables
#'
#'We can first use the GGally package. The ggpairs() code provides us with scatter plots 
#'that plot variables against one another in a pairwise fashion. We also see the distribution of the data
#'and the correlation coefficients between a pair of vairbales

library(GGally)
ggpairs(tomato %>% dplyr::select(-env,-gen))


#'let's use the corrplot package to identify which variables
#'are correlated with one another. First we need to make a correlation matrix of 
#'the data

#'Here we calculate a correlation matrix using a pearson's correlation coefficicent method.
#'Keep in mind that the pearson's method often assumes nromality between variables which we did not 
#'check. Other methods such as spearman's correlation coefficent can be used for non-normal data.

cor_tomatos<-cor(tomato %>% select(-env,-gen) %>% na.omit(), method = c("pearson"))

#'Let's visualize this matrix better with the corrplot package. This line of code
#'will generate a plot showing the relationships between all variables in pairwise manner
#'using the correlation matrix we made previously. The colors represent the value 
#'of the correlation coefficent. Blues indicate positive correlation while reds refelct 
#'negative correlations. The size of the circles reflect the correlation coefficient (e.g,
#'bigger circles represent larger correlation coefficeint values).

corrplot(cor_tomatos, method = "circle", type = "upper")

#If you want both the circles and the correlation coefficients you can run this line:
corrplot.mixed(cor_tomatos)

### TOMATO DISCUSSION QUESTIONS
## Use the ggpairs() and/or corrplot() functions from above to answer the following questions
# Q1. What variable is most strongly correlated with yield?
# Q2. What variable is most strongly correlated with weight?
# Q3. Might there be an issue of colinearity with any of the soil or climate variables? 

# Bonus- Below is code to that sets each categorical variable, which were coded as 0/1 as a factor and colors environments by Irrigation treatment
# What sites tend to irrigate more than others? Does irrigation affect yield? Does Driv affect yield?
ggpairs(tomato %>% mutate(Irr=as.factor(Irr),Driv=as.factor(Driv),Trim=as.factor(Trim)) %>% dplyr::select(-env,-gen), mapping=aes(color=Irr))




######################################################################################################################
####Data exploration approaches with the tidyverse - Part 2 ##########################################################
######################################################################################################################
#'Here we use a dataset named 'flights' from the nycflights12 package. This is data showing every flight
#'to come out of three New York Airports in the year 2013 (domestic only). It comes with the main data named
#''flights' but also comes with extra datasets (or metadata files) names 'airlines' and 
#'airports'. 'Airlines' provides the airline name for each carrier code (e.g. UA is United Airlines)
#''Airports provides an airport name for each destination code (MCO is Orlando)


#let's load up the flights dataset
data("flights")
?flights
#we see that this dataset is rather large w/ 336,776 rows
str(flights)

#Here are the carrier codes and the corresponding airline names
data("airlines")
airlines

#the seed allows us to get reproducible results
set.seed(1234) 

#'Let's grab a random subsample to make it easier to work with
#'the sample_n function allows us to randomly sample rows from
#'a dataset. Here we sample just 20,000 rows. Your computer will thank you
#'later when you are plotting these data

ny_flights<-sample_n(flights, 20000)
ny_flights

#'Let's see how many flights total came out of each airport during the year.
#'We can use the n() function to count up the total number of observations. In this case we are
#'counting the total number of observations in each group (origin/airport)/

ny_flights %>% 
  group_by(origin) %>%
  summarise(total_flights = n())

#'We see that EWR (Newark Liberty Airport has the most outgoing flights). But more importantly it tells 
#'us that the flights are more or less evenly distributed between the three airports. No single airport is 
#'taking up most of the data


#' Let's see which carriers from which airports had the most amount of flight delays?
#' Note: I filter out negative departure delay times because those represent planes that 
#' departed ahead of schedule. 
#' Note: I use the na.rm =T argument here because there are NAs in the datset for dep_delay

delayed_planes<-ny_flights %>% 
  filter(dep_delay > 0) %>%
  group_by(origin, carrier) %>%
  summarise(total_delay = sum(dep_delay, na.rm = T)) %>%
  arrange(desc(total_delay))
delayed_planes
airlines

#'looks like carrier EV (ExpressJet Airlines Inc.) was delayed the most coming out of EWR (Newark Liberty)
#' followed by UA (United Airlines). EV had a total of 60336 delayed minutes which translate to 
#' roughly a total delay of 41.9 hours! Keep in mind we are only using 20000 rows here (6 % of the data)!


#'Let's plot some of this data. Here we use the geom_tile() in ggplot which is useful for plotting
#'categorical variables on both axes

ggplot(delayed_planes) +
  geom_tile(aes(x = origin, y = carrier, fill = total_delay), alpha = 0.9,color = "Grey") +
  theme_classic() 

#' If you want to make a cool color gradient you can use the 'wesanderson'

library(viridis)
ggplot(delayed_planes) +
  geom_tile(aes(x = origin, y = carrier, fill = total_delay), alpha = 0.9,color = "Grey") +
  theme_minimal() +
  scale_fill_viridis_c(option = "D")

#'Let's finally explore trends over time. More specifically, 
#'are there more departure delays in certain months? Let's focus of United Airlines coming out of 
#'Newark because we know it's one of the more busier airports from our previous code

delay_months<-ny_flights %>%
  filter(carrier == "UA", origin == "EWR" , dep_delay>0) %>%
  group_by(month) %>%
  summarise(delay_min = sum(dep_delay, na.rm = T))


ggplot(data = delay_months) + 
  geom_line(aes(x = month, y = delay_min)) +
  geom_point(aes(x=month, y = delay_min), size = 4) + 
  labs(x = "Month", y = "Total departure delays (minutes)")

#basically don't fly on United Airlines out of Newark in  July or December

#' Final thing. What are the top 5 airlines/carrier in terms of outgoing flight volume from 
#' all the airports and where are these airlines primarily flying too?
#' Note: Here we use two chunks of dplyr code. The first identifies the the top 5 airlines
#' the next uses the first chunk to filter for those top 5 using the "%in%" operator

top_airlines<-ny_flights %>% 
  group_by(carrier) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>% #not arrange(desc()) basically arranges the column (total) in descending order
  top_n(n = 5)

top_airlines # Here we see the top carriers are UA, B6, EV, DL, AA

ny_flights %>%
  filter(carrier %in% c("UA", "B6", "EV", "DL", "AA")) %>%
  group_by(carrier,dest) %>%
  summarise(total= n()) %>%
  arrange(desc(total))

## alternative to above using | as "or"
ny_flights %>%
  filter(carrier=="UA"| carrier=="B6"| carrier=="EV"| carrier=="DL"| carrier=="AA") %>%
  group_by(carrier,dest) %>%
  summarise(total= n()) %>%
  arrange(desc(total))

#'looks like Delta  (DL) flew 629 total flights to Atlanta (ATL). Atlanta must be a very popular spot!
#'But why?! This is where data exploration becomes limited. We have no data to support why Atlanta is 
#'popular. However thankfully we know that Hartsfield Jackson (Atlanta's main airport) is a primary hub for
#'Delta Airlines. That's why the airline tries to keep all their flights moving through ATL. This showcases
#'how powerful data exploration is but how sometimes it can leads to wrong assumptions. 




######################################################################
#### R CHALLENGE Data Exploration DUE 02/05/2021
#'For this challenge we will use the flights dataset (subset to 20000 obs)
data("flights")
set.seed(1234) 

ny_flights<-sample_n(flights, 20000)
ny_flights
str(ny_flights)


#'Question 1: Two parts. First, which months had the most flights departing each of the three airports?
#' Second, show a line plot of all three airports and the number of flights departing them across 
#' every month (Hint: x-axis would be months, y would be the number of flights).

q1 <- ny_flights %>% 
      group_by(origin, month) %>%
      summarise(total_flights = n()) 

q1 %>% 
  ggplot(aes(x = month, y = total_flights, color = origin))+
  geom_point()+
  geom_line()+
  theme_classic()+
  facet_wrap(~origin)

ggplot(data = q1)+ 
  geom_line(aes(x = month, y = total_flights)) + 
  geom_point(aes(x=month, y = total_flights), size = 4) + 
  labs(x = "Month", y = "Total departure delays (minutes)") +
  facet_wrap("origin")

# July for EWR and JFJ, October LGA

#' Question2: Which airline/carrier flew the most miles, the most number of flights? 
#' Also which carrier had the lowest miles to flights ratio?
#' (Lower miles to fligh ratio would mean an airline flew less miles per flight)

q2 <- ny_flights %>% 
      group_by(carrier) %>%
      summarise(total_distance = sum(distance)) %>% 
      arrange(desc(total_distance))

print(q2[1,]) # UA flew the most miles

q2.b <- ny_flights %>% 
      group_by(carrier) %>%
      summarise(total_flights = n()) %>% 
      arrange(desc(total_flights))

print(q2.b[1,]) # UA flew the most flights

q2.c <- ny_flights %>% 
      group_by(carrier) %>%
      summarise(total_distance = sum(distance), 
                total_flights = n(),
                ratio = total_distance/total_flights) %>% 
      arrange(desc(ratio))

print(q2.c[15,]) # YV had the lowest miles to flight ratio


#'Question 3:Identify the 5 most popular destinations from all three airports.
#'Then filter the dataset for these 10 destinations. 
#'For every month, which destination received the most flights and from which airports? 
#' How would you plot this? 

q3 <- ny_flights %>%
      group_by(origin,dest) %>%
      summarise(total= n())

EWR <- filter(q3, origin == "EWR")
EWR <- EWR %>% arrange(desc(total))
EWR <- EWR[1:5,2]

JFK <- filter(q3, origin == "JFK")
JFK <- JFK %>% arrange(desc(total))
JFK <- JFK[1:5,2]

LGA <- filter(q3, origin == "LGA")
LGA <- LGA %>% arrange(desc(total))
LGA <- LGA[1:5,2]

all <- rbind(EWR, JFK, LGA)
pop_dest <- unique(all)
pop_dest <- as.vector(pop_dest$dest)

top10 <- ny_flights %>% 
          filter(dest %in% pop_dest)

top10_month <- top10 %>% 
               group_by(month, dest, origin) %>%
               summarise(total_flights = n())

month_label <- c("1" = "January", "2" = "February", "3" = "March",
                 "4" = "April", "5" = "May", "6" = "June", "7" = "July",
                 "8" = "August", "9" = "September", "10" = "October",
                 "11" = "November", "12" = "December")

top10_month <- rename(top10_month, Origin = origin)

top10_month %>% 
    ggplot(aes(x = dest, y = total_flights, fill = Origin))+
    geom_col()+
    scale_fill_viridis_d(option = 'D')+
    xlab("Destination")+
    ylab("Total Flights")+
    facet_wrap(~month,labeller = as_labeller(month_label))+
    theme_classic()+
    theme(strip.background = element_blank(),
    panel.border = element_blank())+
    theme(axis.text.x = element_text(angle = 45, hjust=1))

# Jan = Boston / EWR, Feb = Boston / JFK, March = Atlanta / LGA, 
# April = Boston / EWR, May = Atlanta / LGA, June = Atlanta / LGA, 
# July = Boston / JFK, August = Atlanta / LGA, September = Atlanta / LGA, 
# October = Chicago / LGA, November = Chicago, LGA, December = Atlanta / LGA

#Optional: CHALLENGE CHALLENGE
#' Which carriers fly to which of the top ten destinations at what time of the day?
#' Play around with the full dataset 'flights'! See how much faster or slower it might take to 
#' run certain plots and operators!
#' Hint: you can use the "hours" variable as time of day


