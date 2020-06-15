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








