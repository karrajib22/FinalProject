# Importing, saving and managing data 

## Setting the working directory
setwd("~/RKAR/Harrisburg Univ/Study Doc/ANLY 506- Exploratory Data Analysis/Code Portfolio")
## Returning the current working directory
getwd()

## Workspace management

### get all objects in current workspace
ls() 
### remove object x,y from workspace
rm(x,y)

### remove all objects from workspace
rm(list = ls())

## Importing data
### read text file "test.txt" from working directory into object "tbl",
### declare if the file is with/without header (=TRUE/FALSE), with seperator / delimetr[" " or "," or "\t"] 
### if string will be converted to factor or not (stringsAsFactors = TRUE/ FALSE)
tbl<- read.table("test.txt", sep = " ", header=TRUE, stringsAsFactors = FALSE)

### write object "tbl" into text file "test.txt" with seperator [" " or "," or "\t"], to be saved into working directory
write.table(tbl, "test.txt", sep=" ")

### reading a csv from working directory to an object
#### import csv file as ozone
ozone <- read_csv("US EPA data 2017.csv")


## dataframe manipulation
### installing "tidyverse" package for which "lazyeval" package is pre-requisite
### subsequently ggplot2, tibble, tidyr, readr, dplyr are installed
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

### Rmarkdown package to install
install.packages("rmarkdown")
library("rmarkdown")

### structure of ozone dataframe
str(ozone)

###Renaming columns of the dataframe as r-standard i.e. rplacing spaces with ".'
#### first retrieving the as-is names of the columns of Ozone dataframe
names(ozone)
#### making the conversion
names(ozone)<-make.names(names(ozone))
####retrieving the result after change
names(ozone)

### Number of Row/ column in dataframe Ozone
total.row<-nrow(ozone)
total.row
total.col<-ncol(ozone)
total.col
dim(ozone)

### Mean of the values of a Column
mean(ozone$Observation.Count)

### First 10 rows of Ozone
head(ozone, n=10L)

### Last 10 rows of ozone
tail(ozone, n=10)

### Last 10 rows of ozone with restricted columns e.g 5
tail(ozone[,c(1:4,55)], n=10)

### Data type of column Datum in dataframe Ozone
class(ozone$Datum)

### Distinct values of a column e.g. State.name
length(unique(ozone$State.Name))


### subsetting dataframe ozone by using its first 3 variables
library("dplyr")
subset<-select(ozone,names(ozone)[1:3])
dim(subset)  
names(subset)
               
### sample function structre
long_name <- function(a = "a long argument" ,
                      b = "another argument",
                      c = "another long argument") {
                      #  some code here
}


### Installing relevant packages to install "yarr" package and working with dataframe "pirates"

install.packages("jpeg")
install.packages("circlize")
install.packages("BayesFactor")
install.packages("coda")
install.packages("Matrix")
install.packages("yarrr", dependencies = T)
library("jpeg")
library("circlize")
library("coda")
library("Matrix")
library("BayesFactor")
library("yarrr")

### summary of dataframe "pirates"
summary(pirates)

### creating a subset of pirates dataframe only with hight variable and only for female
pirates.female.height = subset(pirates, sex == "female", select = c(height))

### calculating mean of heights
mean(pirates.female.height$height)

### creating a subset of dataframe "pirates" for only male
pirates.male = subset(pirates, sex=="male")
summary(pirates.male)

### tabulate sex on its categories
table(pirates$sex)

### tabulate sex on its categories by fractions
prop.table(table(pirates$sex))

### load tibble table1 using package "tidyverse"
library(tidyverse)
table1

### Compute rate per 10,000 and add this as a new variable in t"he same tibble "table1"
table1 %>% mutate(rate = cases/ population * 10000)

### Compute cases per year
table1 %>% count(year, wt = cases)

### Visualise changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

### using package "nycflights13"
install.packages("nycflights13")
library(nycflights13)
library(tidyverse)

### subsetting dataframe "flights" on date 30th Dec
dec30 <- filter(flights, month == 12, day == 30)
summary(dec30)

### subsetting dataframe "dec30" where no deperture delay is recorded
dec30.dep_delays.na <- is.na(dec30$dep_delay)
sum(dec30.dep_delays.na)

### calculating mean delay in deperture ignoring the records with missing values
mean(dec30$dep_delay,na.rm=TRUE)

### calculating Mean, Trimmed Mean, Median
income.df <- read_csv("income.csv")
summary(income.df)
mean(income.df$M_weekly, na.rm=T)
mean(income.df$M_weekly, na.rm=T,trim=0.1)
median(income.df$M_weekly,na.rm=T)

### Calculating weighted median using package "matrixStats"
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

### Calculating Std Dev, IQR, and MAD
sd(income.df$F_workers)
IQR(income.df$F_workers)
mad(income.df$F_workers)

### Dataframe to tibble and apply filter
library(dplyr)
library(tibble)

income <- as_tibble(income.df)
summary(income)
head(income)

M_weekly.gt2000<-filter(income,M_weekly>2000)
M_weekly.gt2000

### Adding a calculated Variable to tibble
income<-mutate(income,diff=M_weekly-F_weekly)
income
### Summary of Tibble
income%>%glimpse()
income %>% count(M_workers)

sd_output<-sd(income$diff, na.rm = T)
round(sd_output,2)

### Tibble with filtered condition
str(flights)
flight.w.arrivaldelay.twoormore.hours<-filter(flights,arr_delay>=2)
print(flight.w.arrivaldelay.twoormore.hours)
delayed.flight.to.Houstan<- filter(flight.w.arrivaldelay.twoormore.hours,dest %in% c("IAH","HOU"))
print(delayed.flight.to.Houstan)
### Tabulate
table(delayed.flight.to.Houstan$carrier)

### Tibble with filtered condition
delayed.flight.to.Houstan.by.AAUA<-filter(delayed.flight.to.Houstan, carrier %in% c("UA","AA"))
print(delayed.flight.to.Houstan.by.AAUA)
#
#
#


# Data visualisation ggplot2
library(tidyverse)
summary(mpg)

### To plot mpg, run this code to put displ on the x-axis and hwy on the y-axis:
### ggplot(data = <DATA>) + 
###   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

#### Aesthetic: color
ggplot(data=mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

#### Aesthetic: size
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
#### Warning: Using size for a discrete variable is not advised.

#### Aesthetic: transparency
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

#### Aesthetic: shape
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

#### Aesthetic: shape + color
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class, color = class))

#### Set the aesthetic properties of the geom manually. 
#### For example, we can make all of the points in our plot blue:
  
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 3)

#### To facet your plot on the combination of two variables, add facet_grid()
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)


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
#
#
#



# Exploratory Data Analysis

## Dataframe form vectores
Age <- c(22, 25, 18, 20)
Name <- c("James", "Mathew", "Olivia", "Stella")
Gender <- c("M", "M", "F", "F")

## Dataframe and Subsetting
DataFrame = data.frame(Age,Name,Gender)
subset(DataFrame,Gender=="M")

p <- c (3, 5, 6)
q <- c (3, 3, 3)
p+q


data(AirPassengers)
c[[1]]
AirPassengers[AirPassengers >= 1949 & AirPassengers < 1950, 12]
AirPassengers[time(AirPassengers) >= 1949 & time(AirPassengers) < 1950]


## Misclenious List operation

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

## Building Matrix from vectors
fa=c(3, 4, 7, 8)
fb=c(5, 7, 7, 6)
fm <- matrix(c(fa,fb), byrow=T, nrow=2)
fm

### Calculating Manhattan and Euclidean distance
dist(fm, method = "manhattan")
dist(fm, method = "euclidean")

# Clustering (Hiererchical and K-means)

##  Required packages
install.packages("tidyverse",dependencies = T)
install.packages("NbClust")
install.packages("cluster")
install.packages("clustertend")
install.packages("factoextra")
install.packages("dendextend") # for comparing two dendrograms for Hierarchical Clustering

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(clustertend)
library(factoextra) # clustering algorithms & visualization
library(NbClust)
library(dendextend) # for comparing two dendrograms for Hierarchical Clustering
library(stats)

## Example of a Agglomerative Hierarchical Clustering Problem using dataset USArrests
data(USArrests)
head(USArrests)
### scaling/standardizing the dataframe after removing any missing value that might be present
data<- scale(na.omit(USArrests))
head(data)
### Dissimilarity matrix
d <- dist(data, method = "euclidean")
### hclust() using Complete Linkage method. 
### Other methods are "ward.D", "ward.D2", "single", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
hc1 <- hclust(d, method = "ward.D2" ) 
#hc1 <- hclust(d, method = "complete" )
#### Ploting the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
#### Cutting the tree into 4 groups and visualizing
sub_grp <- cutree(hc1, k = 4)
table(sub_grp)
rect.hclust(hc1, k = 4, border = 2:5)
fviz_cluster(list(data = data, cluster = sub_grp))

### Alternatively we can use agnes() which also get the agglomerative coefficient, which measures the amount of clustering structure found 
### (values closer to 1 suggest strong clustering structure).
### method can be one of "average", "single", "complete", "ward"
# hc2<- agnes(data, method = "single")
hc2<- agnes(data, method = "ward")
#Cut agnes() tree into 4 groups
cutree(as.hclust(hc2), k = 4)
### visualize the dendrogram:
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 
#### Agglomerative coefficient
hc2$ac #or
coef(hc2) #or
coef(as.hclust(hc2))
#
#


## Example of a Divisive Hierarchical Clustering Problem using dataset USArrests
data(USArrests)
head(USArrests)
### scaling/standardizing the dataframe after removing any missing value that might be present
data<- scale(na.omit(USArrests))
head(data)
### compute divisive hierarchical clustering
hc3 <- diana(data)
# Cut diana() tree into 4 groups
cutree(as.hclust(hc3), k = 4)
### plot dendrogram
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of diana")
### Divise coefficient; amount of clustering structure found
hc3$dc#or
coef(hc3) #or
coef(as.hclust(hc3))
#
#
### Determining Optimal Clusters for Hierarchical Clustering
#### Elbow Method
fviz_nbclust(data, FUN = hcut, method = "wss") 
#### Average Silhouette Method
fviz_nbclust(data, FUN = hcut, method = "silhouette") 
#### Gap Statistic Method
gap_stat <- clusGap(data, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
#
#


## Example of a K-Means clustering Problem using dataset USArrests
data(USArrests)
head(USArrests)
### scaling/standardizing the dataframe after removing any missing value that might be present
data<- scale(na.omit(USArrests))
head(data)
### Clustering distance measure
#### A distance matrix between the rows of a data matrix. The default distance computed  method is the Euclidean.
#### However other methods are "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" or "kendall".
distance <- get_dist(data, method = "euclidean")
#### Visualizing a distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
### Computing k-means clustering with following componenets:
#### cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
#### centers: A matrix of cluster centers.
#### totss: The total sum of squares.
#### withinss: Vector of within-cluster sum of squares, one component per cluster.
#### tot.withinss: sum(withinss).
#### betweenss: The between-cluster sum of squares, i.e. (totss-tot.withinss).
#### size: The number of points in each cluster.
k2<-kmeans(data, centers = 2, nstart = 25)
str(k2)
fviz_cluster(k2, data )
#
#


## Anothe example of K-means clustering

#### read_table2() is like read.table(), 
#### it allows any number of whitespace characters between columns, and the lines can be of different lengths.
d1<- read_table2("QuizCluster1", col_names = FALSE)
d2<- read_table2("QuizCluster2", col_names = FALSE)
d3<- read_table2("QuizCluster3", col_names = FALSE)
#### Creating new dataframe using cbind command to bind multiple columns of different dataframe sources
clusters <- cbind(d1$X2,d2$X2,d3$X2)
#### Creating names of the newly created dataframe
colnames(clusters) <- c("c1", "c2", "c3")

#### The gap statistic generated by the function clusGap() [in cluster package].
gap_stat = clusGap(clusters,FUN = kmeans,K.max=10,nstart=2)

#### Visualize the gap statistic generated by the function clusGap() [in cluster package]. 
#### The optimal number of clusters is specified using the "firstmax" method (?cluster::clustGap).
fviz_gap_stat(gap_stat)
#### Dertemines and visualize the optimal number of clusters using different methods: within cluster sums of squares, 
#### average silhouette and gap statistics.
fviz_nbclust(clusters, kmeans, method = "gap_stat")+ theme_classic()
#
#

## Determining Optimal Clusters for Hierarchical Clustering
#### Elbow Method
fviz_nbclust(data, kmeans, method = "wss") 
fviz_nbclust(clusters, kmeans, method = "wss") 
#### Average Silhouette Method
fviz_nbclust(data, kmeans, method = "silhouette") 
fviz_nbclust(clusters, kmeans, method = "silhouette") 
#### Gap Statistic Method
fviz_nbclust(data, kmeans, method = "gap_stat") 
fviz_nbclust(clusters, kmeans, method = "gap_stat") 

fviz_gap_stat(clusGap(data, FUN = kmeans, nstart = 25, K.max = 10, B = 50))
fviz_gap_stat(clusGap(data, FUN = kmeans, nstart = 2, K.max = 10))
#
#

## Advanced example combining principles learned till date
## pairwise scatter plots to compare the clusters vs the original variables.
data %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()
#
## xtract the clusters and add to our initial data to do some descriptive statistics at the cluster level:
USArrests %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
