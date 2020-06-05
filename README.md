# Data-Mining

## Index
- [Practice 1](#Practice_1)
- [Practice 2](#Practice_2)
- [Practice 3](#Practice_3)
- [Practice 4](#Practice_4)
- [Practice 5](#Practice_5)
- [Practice 5](#Practice_5)
- [Practice 6](#Practice_6)
- [HomeWork 1](#HomeWork_1)
- [HomeWork 2](#HomeWork_2)
- [HomeWork 3](#HomeWork_3)
- [HomeWork 4](#HomeWork_4)
- [HomeWork 5](#HomeWork_5)


## Practice_1

Use a data set with real data and implement it in the practice of simple linear regression.

``` R
getwd()
setwd("C:/Users/Dell/Desktop/Semestre_X/Practica_1")
getwd()

# Importing the dataset
dataset <- read.csv('Salary_Data_Practice.csv')

# Splitting the dataset into the Training set and Test set
# Install.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(dataset$Salary, SplitRatio = 2/3)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Salary ~ YearsExperience,
               data = dataset)
summary(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x=training_set$YearsExperience, y=training_set$Salary),
             color = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'blue') +
  ggtitle('Salary vs Experience (Training Set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Visualising the Test set results
ggplot() +
  geom_point(aes(x=test_set$YearsExperience, y=test_set$Salary),
             color = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'blue') +
  ggtitle('Salary vs Experience (Test Set)') +
  xlab('Years of experience') +
  ylab('Salary')
```

## Practice_2
Generate the analysis with data visualization using the multiple linear regression code

```R
getwd()
setwd("C:/Users/Dell/Desktop/Semestre_X/Practica_2")
getwd()

# Importing the dataset
dataset <- read.csv('50_Startups.csv')

# Encoding categorical data 
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1,2,3))

dataset
# Splitting the dataset into the Training set and Test set
# Install.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(dataset$Profit, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
#regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State)
regressor = lm(formula = Profit ~ .,
               data = training_set )

summary(regressor)

# Prediction the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred


# Assigment: visualize the siple liner regression model with R.D.Spend

regressor = lm(formula = Profit ~ R.D.Spend,
               data = training_set )

summary(regressor)

y_pred = predict(regressor, newdata = test_set)
y_pred

# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x=training_set$R.D.Spend, y=training_set$Profit),
             color = 'red') +
  geom_line(aes(x = training_set$R.D.Spend, y = predict(regressor, newdata = training_set)),
            color = 'blue') 


# Visualising the Test set results
ggplot() +
  geom_point(aes(x=test_set$R.D.Spend, y=test_set$Profit),
             color = 'red') +
  geom_line(aes(x = training_set$R.D.Spend, y = predict(regressor, newdata = training_set)),
            color = 'blue')
```

## Practice_3 

Explain step by step what is the function "Backward Elimination"

```R
getwd()
setwd("C:/Users/Dell/Desktop/Semestre_X/Practice_3")
getwd()

# Importing the dataset
dataset <- read.csv('50_Startups.csv')

# Encoding categorical data 
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1,2,3))

dataset
# Splitting the dataset into the Training set and Test set
# Install.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(dataset$Profit, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
#regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State)
regressor = lm(formula = Profit ~ .,
               data = training_set )

summary(regressor)

# Prediction the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred


# Homework analise the follow atomation backwardElimination function 
# Resive el dataset y el valor SL
backwardElimination <- function(x, sl) {
  # Determina la longitud del dataset
  numVars = length(x)
  # hace el recorrido por el dataset
  for (i in c(1:numVars)){
    # Ajuste de la regresión lineal multiple por eso tiene un . para que recorra todas las columnas
    regressor = lm(formula = Profit ~ ., data = x)
    # Determina el maximo coeficiente de del modelo modelo 
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){ # si es mayor con which devolverá la posición de los elementos cuando  "Pr(>|t|)"] == maxVar
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j] # reasignara la data que debe ser analizada
    }
    numVars = numVars - 1 # siguiente valor
  }
  return(summary(regressor))# al terminar retorna la regresión que satisface lo indicado
}

SL = 0.5
#dataset = dataset[, c(1,2,3,4,5)]
training_set
backwardElimination(training_set, SL)
```

## Practice_4

```R
library(ElemStatLearn)
library(caTools)
library(ggplot2)

getwd()
setwd("/Users/Dell/Desktop/DataMining-master/MachineLearning/LogisticRegression")
getwd()

# Se importa el conjunto de datos con el que trabajará.

dataset <- read.csv('Social_Network_Ads.csv')

# Se seleccionan los columnas con los que trabajaremos.
dataset <- dataset[, 3:5]

set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

training_set[, 1:2] <- scale(training_set[, 1:2])
test_set[, 1:2] <- scale(test_set[, 1:2])

classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

# Predecir los resultados del conjunto de prueba
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
prob_pred
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred

# se crea la Matriz de Confusión 
cm = table(test_set[, 3], y_pred)
cm

#se asignan los valores a utulizar en elgrafico
ggplot(training_set, aes(x=EstimatedSalary, y=Purchased)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

ggplot(training_set, aes(x=Age, y=Purchased)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

ggplot(test_set, aes(x=EstimatedSalary, y=Purchased)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

ggplot(test_set, aes(x=Age, y=Purchased)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)


install.packages(path_to_source, repos = NULL, type="source")
install.packages("~/Downloads/ElemStatLearn_2015.6.26.2.tar", repos=NULL, type="source")

set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

#observar el resultado de los datos de prueba
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
```

## Practice_5

```R
getwd()
setwd("Users/deyanira/Downloads/DataMining-master/MachineLearning/LogisticRegression")
getwd()

dataset <- read.csv('Social_Network_Ads.csv')

dataset <- dataset[, 3:5]

library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

training_set[, 1:2] <- scale(training_set[, 1:2])
test_set[, 1:2] <- scale(test_set[, 1:2])

classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
prob_pred
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred

cm = table(test_set[, 3], y_pred)
cm

library(ggplot2)

ggplot(training_set, aes(x=EstimatedSalary, y=Purchased)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

ggplot(training_set, aes(x=Age, y=Purchased)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

ggplot(test_set, aes(x=EstimatedSalary, y=Purchased)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

ggplot(test_set, aes(x=Age, y=Purchased)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

install.packages(path_to_source, repos = NULL, type="source")
install.packages("~/Downloads/ElemStatLearn_2015.6.26.2.tar", repos=NULL, type="source")

library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
```

## Practice_6 

```R
getwd()
setwd("/Users/deyanira/Downloads/DataMining-master/MachineLearning/SVM") #se carga el directorio o carpeta donde se encuentran los archivos que utilizaremos
getwd()

dataset = read.csv('datos.csv') #se cargan los datos del archivo que estamos llamando
dataset = dataset[3:4]

dataset$cl = factor(dataset$cl, levels = c(0, 1))

library(caTools) #se utilizara la libreria catools 
set.seed(123) #para crear simulaciones u objetos aleatorios que se pueden reproducir.
split = sample.split(dataset$cl, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_set[-2] = scale(training_set[-2])
test_set[-2] = scale(test_set[-2])

library(e1071)
classifier = svm(formula = cl ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'sigmoid')

y_pred = predict(classifier, newdata = test_set[-2])
y_pred

cm = table(test_set[, 2], y_pred)
cm

library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x', 'y')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'x', ylab = 'y',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 2] == 1, 'green4', 'red3'))

library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x', 'y')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'x', ylab = 'y',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 2] == 1, 'green4', 'red3'))
```
## Homework_2

Geom_jitter

To understand this topic we will start by creating a graph of points

```R
ggplot (data = miles) +
  geom_point (mapping = aes (x = displacement, y = highway))
```
![Image](Unit_2/Homework/images/image2.1.png)

The values of thevariables highway and displacement are rounded so that the points appear on a grid and many overlap each other. This problem is known as overlap (overplotting).This arrangement makes it difficult to see where the mass of data is. Are the data points evenly distributed along the graph, or is there a special combination of highway and displacement containing 109 values?

You can avoid this by setting the position adjustment to “jitter”. position = "jitter" adds a small amount of random noise to each point. This disperses the points, since two points are unlikely to receive the same amount of random noise.

```R 
ggplot (data = miles) +
  geom_point (mapping = aes (x = displacement, y = highway), position = "jitter")
```
![Image](Unit_2/Homework/images/image2.2.png)

Adding randomness to points may seem like a strange way to improve your graph. While it makes it less accurate at small scales, it makes it more telling on a large scale. Since this is such a useful operation, ggplot2 includes an abbreviation for geom_point (position = "jitter"): geom_jitter ().


## Homework_3
<p align="center">¿P - value?</p>
<p align="justify">
The p-value is a direct measure of how plausible it is to obtain a sample like the current one if it is true H0. Small values ​​indicate that it is very infrequent to obtain a sample like the current one, while high values ​​are frequent. The p-value is used to indicate how much (or how little) the alternative hypothesis contradicts the current sample.

</p>
<p align="justify"> 
Reporting what the p-value is has the advantage of allowing anyone to decide which hypothesis to accept based on their own level of risk α. This is not possible when reporting, as has been traditional, indicating only the result of the decision, that is, whether His accepted or rejected0 with a fixed α.
By providing the p-value obtained with the current sample, the decision will be made according to the following rule:
  
</p>
<p align="center">
if pv ≤ α, accept H1
</p>
<p align="center">
if pv > α, accept H0 
</p>
<p align="justify"> 
Entering the practical field, some statistical packages They provided in their listings the significancelevel,literally meaning significance levelwhen often actually refer to the p-value (p-value).
</p>

## Homework_4


Split Ratio () and lm ()

Generally, though The data that you want to work with is given in tables, they are not organized in the way you intend to work on them. Thefunction split ()allows you to classify the data, typically given as a vector, or as a data frame. 

Splitting ratio:
if (0<= splitratio <1) <code =""> then SplitRatio fraction of points from Y will be set to True

if (SplitRatio ==1) then one random point from Y will be set to True

if ( SplitRatio>1) then SplitRatio number of points from Y will be set to True



lm () 
Used to fit linear models can be used to perform regression, single-stratum analysis of variance and covariance analysis.
Models for lm are specified symbolically. 

lm returns an object of class "lm" or for multiple responses of class c ("mlm", "lm").
An object of class  "lm" is a list containing at least the following components:
coefficients
a named vector of coefficients
residual rights copyrights
the residuals, that is, the answer minus the fitted values.
fitted values ​​the adjusted
mean values
range
The numerical range of the fitted linear model.
weights
(for weighted adjustments only) the specified weights.
df.residual
The residual degrees of freedom.
call
the matching call
conditions
The terms object used.
contrasts
(only where relevant) the contrasts used.
xlevels
(only where relevant) a record of the levels of the factors used in the adjustment.
offset
offset used (missing if none were used).
and
if requested, the response used.
X
if requested, the model matrix used.
model
if requested (the default), the frame of the model used.
na.action
(where applicable) information returned by model.frame about special handling of NAs.

In addition, nonzero adjustments will have assigned components, effects, and (unless requested) qr related to linear adjustment, for use by extractor functions such as summary and effects .

## Homework_5

What does GLM?
glm is used to fit generalized linear models, specified by a symbolic description of the linear predictor and a description of the error distribution.
glm returns a class object inherited from "glm" which inherits from the class "lm".

glm.fit is the workhorse function: it is not normally called directly, but it can be more efficient when the response vector, design matrix, and family have already been calculated.

Thefunction summary(that is, summary.glm) can be used to get or print a summary of the results, and thefunction anova(that is, anova.glm) to produce an analysis of variance table.
Generic access functions coefficients, effects, fitted.values and residuals can be used to extract various useful features from the value returned by glm.
An object of class "glm" is a list containing at least the following components:
coefficients
a named vector of coefficients
copyright residuals
the residuals of work , that is, the residuals in the final iteration of the IWLS setting. Since zero weight cases are omitted, your work residuals are NA.
fitted values ​​the fitted
mean values, obtained by transforming the linear predictors by the inverse of the link function.
range
The numerical range of the fitted linear model.
family
The family object used.

