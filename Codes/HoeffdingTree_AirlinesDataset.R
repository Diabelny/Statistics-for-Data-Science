#Il Seguente dataset è disponibile sulla piattaforma MOA. Nel codice viene utilizzato l'algoritmo Hoeffding_tree usando Gini Index & Entropia come misure di impurità"

library(foreign)
library(mlpack)
library(caret)
library(rpart)



dati <- read.arff("airlines.arff")
View(dati)
dati$Delay <- as.numeric(dati$Delay) -1


set.seed(123)  

# Divisione dataset in 80% train e 20% test
train_indices <- createDataPartition(dati$Delay, p = 0.8, list = FALSE)
train_data <- dati[train_indices, ] 
test_data <- dati[-train_indices, ] 


output <- hoeffding_tree(training=train_data)
tree <- output$output_model 

output <- hoeffding_tree(input_model=tree, test=test_data) 
predictions <- output$predictions 
predictions[predictions == 1] <- 0 
predictions[predictions == 2] <- 1

#cat(predictions)

calculate_accuracy=function(predictions, actual) {
  sum(predictions == actual) / length(predictions)
}
accuracy=calculate_accuracy(predictions, test_data$Delay)
cat("Accuracy del Modello:", accuracy, "\n")

conf_matrix<-confusionMatrix(data=factor(predictions), reference =factor(test_data$Delay), positive="1")
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
predictions2[predictions2 == 1] <- 0 
predictions2[predictions2 == 2] <- 1

accuracy2=calculate_accuracy(predictions2, test_data$Delay)
cat("Accuracy del Modello:", accuracy2, "\n")

conf_matrix<-confusionMatrix(data=factor(predictions2), reference =factor(test_data$Delay), positive="1")
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


#CONFRONTO CON STANDARD DECISION TREE
train_data$Delay=factor(train_data$Delay)
test_data$Delay=factor(test_data$Delay)
standard_tree <- rpart(Delay ~ ., data = train_data)
standard_predictions <- predict(standard_tree, test_data, type = "class")
standard_accuracy <- calculate_accuracy(standard_predictions, test_data$Delay)
cat("Accuracy del Decision Tree standard:", standard_accuracy, "\n")

conf_matrix<-confusionMatrix(data=standard_predictions, reference = test_data$Delay, positive="1")
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
