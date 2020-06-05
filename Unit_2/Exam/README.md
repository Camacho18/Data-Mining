Instructions
Develop the following problem with R and RStudio for knowledge extraction
that the problem requires.
The directors of the movie review website are very happy with their
previous delivery and now they have a new requirement for you.
The previous consultant had created a chart for them that is illustrated in the following
image.

However, the R code used to create the graph has been lost and cannot
be recovered.

Your task is to create the code that will re-create the same table making it show what
as close to the original as possible.
You will be provided with a new dataset.
Track
Please note that not all Genres and Studio are used.
You will need to filter your dataframe after importing the csv file.

``` R
getwd()#get the working directory that R will take
setwd("C:/Users/deyanira/Downloads/Examen U2") #change the working directory to the path specified inside
getwd() #get the new path R will work on

movies <- read.csv("Project-Data.csv") ## It is responsible for reading the data corresponding to the established route to read the data that will be used according to the given specifications
head(movies) #Displays the first 6 data found within the called data array

colnames(movies) <- c("Day_Week","Director","Genre","Title","Release_Date","Studio","Adjusted Gross","Budget","Gross","IMDb_Rating","MovieLens_Rating","Overseas","Overseas%","Profit","Profit%","Runtime","US","GrossPor") #Change the columns name of the data.
filter_movies <- (movies$Genre=="action" | movies$Genre=="adventure" | movies$Genre=="animation" | movies$Genre=="comedy" | movies$Genre=="drama") & (movies$Studio=="Buena Vista Studios" | movies$Studio=="Fox" | movies$Studio=="Paramount Pictures" | movies$Studio=="Sony" | movies$Studio=="Universal" | movies$Studio=="WB") # Filter the data according to the categories of films and the studies that produce them, this in order to obtain the data to be used.

filter_movies #shows the previously made filter, each data shows it as true or false, depending on whether said data met the filter

movies <- movies[filter_movies,] #At this point is when filtering is applied, it compares the data that was loaded with the matrix that was made previously, this way it only leaves the data that is true.

head(movies)#Show the first 6 records

install.packages ("ggplot2")
library(ggplot2)#system to declaratively create charts

u <- ggplot(movies, aes(x=Genre, y=GrossPor,color=Studio, size=Budget)) # Declaration of the input data frame for a graph, specifying the data and columns that will take the values ​​on the axes (X and Y)

u + geom_jitter() + geom_boxplot(aes(color=Budget,alpha=0.5, size=0.5)) # Fluctuations (geom_jitter) are added to the graph to handle
u

##geom_jitter: paint the dots with a small offset
##geom_boxplot: Make a box diagram, which allows you to see through the quartiles, how the distribution is, its degree of asymmetry, extreme values, the position of the median, etc.
```
