# Exam Unit 2

**Instructions**  
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
**Track**  
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


# Exam Unit 3

**Instructions**  

Develop the following problem with R and RStudio for knowledge extraction that the problem requires.  

Implement the Naive Bayes classification model with the dataset
Social_Network_Ads.csv and using the e1071 library with the naiveBayes () function. Once the classifier is obtained, do the data visualization analysis correspondent.  

At the end of development, explain in detail the consistency of the Naive Bayes classification and also the detailed explanation to data visualization.

```R
#Instalar Paquetes necesarios 
install.packages("e1071")
install.packages("naivebayes")

library("e1071")
library("naivebayes")

#Indicamos la ruta de nuestro archivo
getwd()
setwd("C:\Users\Dell\Desktop\Semestre_X\MineriaDatos\Exam_2")
getwd()


# Importalos los datos desde un artchivo tipo csv a una variable
dataset = read.csv('Social_Network_Ads.csv')
summary(dataset)
head(dataset)

# Elegimos las columnas que vamos a utilizar
dataset = dataset[3:5]

# Visualizamos el DataSet
summary(dataset)
head(dataset)

# codificamos la columna categorica que nos indica si se realizaron compras
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
summary(dataset)
str(dataset)
#lo volvemos a visualizar
summary(dataset)

#Importamos la libreria `caTools`
library(caTools)

#Dividimos el conjunto de datos para su training y generamos una regla del 75%
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Tranformamos las columnas aun mejor significado para el que el algorito
# maneje mejor los datos.
# el [-3] significa que va agarrar las primeras dos columnas 
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

#visualizamos los datos del entrenamiento
head(training_set)
head(test_set)

# Ahora entrenamos el algoritmo de naive bayes
# x	Una matriz numérica, o un marco de datos de variables categóricas y / o numéricas.
# y	Vector de clase
classifier = naiveBayes(x = training_set[-3],
                        y = training_set$Purchased)
classifier

training_set[-3]
training_set$Purchased

y_pred = predict(classifier, newdata = test_set[-3])
y_pred

test_set[-3]

cm = table(test_set[, 3], y_pred, dnn = c("Actual", "Predicha"))
cm

# En este puntos graficamos los datos con el modelo 
# Visualizar los datos de entrenamiento 
set = training_set

# Visualizar los datos de prueba
set = test_set


X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')

y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3], main = 'Test Naive Bayes Results',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'palegoldenrod', 'aquamarine'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'red', 'blue'))









```
