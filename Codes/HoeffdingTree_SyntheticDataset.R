library(foreign)
library(mlpack)
library(caret)
library(rpart)


# Creazione dataset sintetico
num_rows=10000000 #per possibili test decrementare il numero di esempi a causa del tempo di allenamento dei modelli  
set.seed(123)
dati <- data.frame(
  Col1 = rnorm(num_rows, mean = 10, sd = 2),
  Col2 = runif(num_rows, min = 0, max = 100),
  Col3 = rpois(num_rows, lambda = 5),
  Col4 = rchisq(num_rows, df = 3),
  Col5 = rgamma(num_rows, shape = 2, scale = 3),
  Col6 = rnorm(num_rows, mean = 5, sd = 1),
  Col7 = runif(num_rows, min = 0, max = 50),
  Col8 = rpois(num_rows, lambda = 3),
  Col9 = rchisq(num_rows, df = 2),
  Classe = factor(sample(c(0, 1), num_rows, replace = TRUE))
)
View(dati)

# Ripartizione dei dati in train e test
train_indices <- createDataPartition(dati$Classe, p = 0.8, list = FALSE)
train_data <- dati[train_indices, ] 
test_data <- dati[-train_indices, ] 

# Alleno modello bsato su algoritmo Hoeffding Tree, usando Gini Index come metrica standard per il calcolo dell'impurità 
output <- hoeffding_tree(training=train_data)
tree <- output$output_model 

# Instanzio il modello per fare previsione su dati non osservati  
output <- hoeffding_tree(input_model=tree, test=test_data) 
predictions <- output$predictions 

# N.B. L'algoritmo HoeffdingTree incrementa il valore delle predizioni binarie memorizzate nel campo "predictions", la modifica dei valori è necessaria per fare il comparison sulle etichette vere.
predictions[predictions == 2] <- 0 
predictions[predictions == 3] <- 1

#cat(predictions)

# Definizione funzione per il calcolo dell'accuracy(HoeffdingTree restituisce le predizioni in una matrice)
calculate_accuracy=function(predictions, actual) {
  sum(predictions == actual) / length(predictions)
}

accuracy=calculate_accuracy(predictions, test_data$Classe)
cat("Accuracy del Modello:", accuracy, "\n")

conf_matrix<-confusionMatrix(data=factor(predictions), reference = test_data$Classe, positive="1")
conf_matrix
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- conf_matrix$byClass["F1"]
performance_table <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score"),
  Value = c(accuracy, precision, recall, f1_score)
)
print(performance_table)
#Instanziazione algoritmo di Hoeffding usando l'Entropia anzichè il Gini Index
output2 <- hoeffding_tree(training=train_data,info_gain = TRUE)
tree2 <- output2$output_model

output2 <- hoeffding_tree(input_model=tree2, test=test_data) 
predictions2 <- output2$predictions
predictions2[predictions2 == 2] <- 0 
predictions2[predictions2 == 3] <- 1

accuracy2=calculate_accuracy(predictions2, test_data$Classe)
cat("Accuracy del Modello:", accuracy2, "\n")

conf_matrix<-confusionMatrix(data=factor(predictions2), reference = test_data$Classe, positive="1")
conf_matrix
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- conf_matrix$byClass["F1"]
performance_table <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score"),
  Value = c(accuracy, precision, recall, f1_score)
)
print(performance_table)

#Allenamento modello Decision Tree standard

standard_tree <- rpart(Classe ~ ., data = train_data)
standard_predictions <- predict(standard_tree, test_data, type = "class")
standard_accuracy <- calculate_accuracy(standard_predictions, test_data$Classe)
cat("Accuracy del Decision Tree standard:", standard_accuracy, "\n")



conf_matrix<-confusionMatrix(data=standard_predictions, reference = test_data$Classe, positive="1")
conf_matrix
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- conf_matrix$byClass["F1"]
performance_table <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score"),
  Value = c(accuracy, precision, recall, f1_score)
)
print(performance_table)


