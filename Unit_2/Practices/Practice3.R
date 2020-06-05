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

     