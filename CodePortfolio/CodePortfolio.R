##Week 2 03/26
## Setting ther working directory
setwd("~/RKAR/Harrisburg Univ/Study Doc/ANLY 506- Exploratory Data Analysis/Code Portfolio")
install.packages("lazyeval",dependencies = T)
library("lazyeval")
install.packages("tidyverse",dependencies = T)
library("tidyverse")
# > library("tidyverse")
# ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
# ✔ ggplot2 3.1.0       ✔ purrr   0.3.2  
# ✔ tibble  2.1.1       ✔ dplyr   0.8.0.1
# ✔ tidyr   0.8.3       ✔ stringr 1.4.0  
# ✔ readr   1.3.1       ✔ forcats 0.4.0  
# ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
# ✖ dplyr::filter()     masks stats::filter()
# ✖ purrr::is_atomic()  masks lazyeval::is_atomic()
# ✖ purrr::is_formula() masks lazyeval::is_formula()
# ✖ dplyr::lag()        masks stats::lag()

# library("readr") <-- library("tidyverse") already installed  
library("dplyr")

##import csv file as ozone
ozone <- read_csv("US EPA data 2017.csv")
str(ozone)
###Renaming columns of the dataframe as r-standard i.e. rplacing spaces with ".'
names(ozone)
# test<-make.names(names(ozone))
names(ozone)<-make.names(names(ozone))
names(ozone)

### Number of Row/ column in dataframe Ozone
total.row<-nrow(ozone)
total.row
total.col<-ncol(ozone)
total.col
dim(ozone)
###Mean of the values of a Column
mean(ozone$Observation.Count)
### First 10 rows of Ozone
head(ozone, n=10L)

# Last 10 rows of ozone
tail(ozone, n=10)

# Last 10 rows of ozone with restricted columns e.g 5
tail(ozone[,c(1:4,55)], n=10)
### Data type of column Datum in dataframe Ozone
class(ozone$Datum)
# Distinct states
length(unique(ozone$State.Name))

str(ozone)

# subsetting dataframe
library("dplyr")
subset<-select(ozone,names(ozone)[1:3])
dim(subset)  
               
#Week3 04/02
long_name <- function(a = "a long argument" ,
                      b = "another argument",
                      c = "another long argument") {
                      #  some code here
}

#Week4 04/09
install.packages("rmarkdown")
library("rmarkdown")

str(ChickWeight)


#Week5 04/16

install.packages("jpeg")
install.packages("circlize")
install.packages("BayesFactor")
install.packages("coda")
install.packages("Matrix")

library("jpeg")
library("circlize")
library("coda")
library("Matrix")
library("BayesFactor")
library("yarrr")

install.packages("yarrr", dependencies = T)

summary(pirates)
pirates.female.height = subset(pirates, sex == "female", select = c(height))
mean(pirates.female.height$height)
pirates.male = subset(pirates, sex=="male")
summary(pirates.male)

table(pirates$sex)
prop.table(table(pirates$sex))

library(tidyverse)
table1

# Compute rate per 10,000
table1 %>% mutate(rate = cases/ population * 10000)

# Compute cases per year
table1 %>% count(year, wt = cases)

# Visualise changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

# WK 6 04/23
setwd("~/RKAR/Harrisburg Univ/Study Doc/ANLY 506- Exploratory Data Analysis/Code Portfolio")
#2
install.packages("nycflights13")
library(nycflights13)
library(tidyverse)

dec30 <- filter(flights, month == 12, day == 30)
summary(dec30)
dec30.dep_delays.na <- is.na(dec30$dep_delay)
sum(dec30.dep_delays.na)
#3
mean(dec30$dep_delay,na.rm=TRUE)
#4
income.df <- read_csv("income.csv")
summary(income.df)
mean(income.df$M_weekly, na.rm=T)
mean(income.df$M_weekly, na.rm=T,trim=0.1)
median(income.df$M_weekly,na.rm=T)

#6
install.packages("matrixStats")
library(matrixStats)

weighted_median <- function(x, w, ..., na.rm = FALSE){
  if(na.rm){
    df_omit <- na.omit(data.frame(x, w))
    return(weightedMedian(df_omit$x, df_omit$w, ...))
  } 
  weightedMedian(x, w, ...)
}
# weightedMedian(income.df$M_weekly,income.df$Industry)
# weightedMedian(income.df$M_weekly,income.df$Industry, na.rm = T)
# weighted_median(income.df$M_weekly,income.df$Industry)
weighted_median(income.df$M_weekly,income.df$Industry,na.rm = T)
#7
sd(income.df$F_workers)
IQR(income.df$F_workers)
mad(income.df$F_workers)
#8
library(dplyr)
library(tibble)

income <- as_tibble(income.df)
summary(income)
head(income)

M_weekly.gt2000<-filter(income,M_weekly>2000)
M_weekly.gt2000
#10
income<-mutate(income,diff=M_weekly-F_weekly)
income
income%>%glimpse()
income %>% count(M_workers)

sd_output<-sd(income$diff, na.rm = T)
round(sd_output,2)

#Exercise
str(flights)
flight.w.arrivaldelay.twoormore.hours<-filter(flights,arr_delay>=2)
print(flight.w.arrivaldelay.twoormore.hours)
delayed.flight.to.Houstan<- filter(flight.w.arrivaldelay.twoormore.hours,dest %in% c("IAH","HOU"))
print(delayed.flight.to.Houstan)
table(delayed.flight.to.Houstan$carrier)
delayed.flight.to.Houstan.by.AAUA<-filter(delayed.flight.to.Houstan, carrier %in% c("UA","AA"))
print(delayed.flight.to.Houstan.by.AAUA)

# Wk 7 
# Data visualisation ggplot2
library(tidyverse)
summary(mpg)
# To plot mpg, run this code to put displ on the x-axis and hwy on the y-axis:
# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
#Aesthetic: color
ggplot(data=mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
#Aesthetic: size
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
#> Warning: Using size for a discrete variable is not advised.

# Aesthetic: transparency
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

#Aesthetic: shape
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

#Aesthetic: shape + color
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class, color = class))

#Set the aesthetic properties of the geom manually. 
#For example, we can make all of the points in our plot blue:
  
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 3)

#To facet your plot on the combination of two variables, add facet_grid()
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

##################
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )


par(mar = c(3, 1.5, 2, 1.5))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 1.0/2)



Age <- c(22, 25, 18, 20)
Name <- c("James", "Mathew", "Olivia", "Stella")
Gender <- c("M", "M", "F", "F")

DataFrame = data.frame(Age,Name,Gender)
subset(DataFrame,Gender=="M")

p <- c (3, 5, 6)
q <- c (3, 3, 3)
p+q


data(AirPassengers)
c[[1]]
AirPassengers[AirPassengers >= 1949 & AirPassengers < 1950, 12]
AirPassengers[time(AirPassengers) >= 1949 & time(AirPassengers) < 1950]




a <- list ("x"=5, "y"=10, "z"=15)
sum(list(a))
sum(a)
sum(unlist(a))

y <- list("a", "b", "c")
y <- y+list("one","two","three")
y


aggregate(airquality, list(airquality$Month),mean, na.rm=T) 



income<- read_csv("income.csv")
income%>%glimpse()



Mean weekly wage difference between male and female across the industry

library(knitr)
