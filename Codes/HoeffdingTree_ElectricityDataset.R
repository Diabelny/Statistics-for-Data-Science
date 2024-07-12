#Il Seguente dataset è disponibile sulla piattaforma MOA. Nel codice viene utilizzato l'algoritmo Hoeffding_tree usando Gini Index & Entropia come misure di impurità"

library(foreign)
library(mlpack)
library(caret)
library(rpart)

dati <- read.arff("elecNormNew.arff")
View(dati)

#PER ADATTARE IL CONTENUTO DELLA VARIABILE TARGET ALL'ALGORITMO la variabile target è stata trasformata in numerica


dati$class <- as.character(dati$class) 
dati$class <- ifelse(dati$class == "UP", 0, 1)
set.seed(123)  

train_indices <- createDataPartition(dati$class, p = 0.8, list = FALSE)
train_data <- dati[train_indices, ] 
test_data <- dati[-train_indices, ] 


output <- hoeffding_tree(training=train_data)
tree <- output$output_model


output <- hoeffding_tree(input_model=tree, test=test_data) 
predictions <- output$predictions
predictions[predictions == 1] <- 0 
predictions[predictions == 2] <- 1

calculate_accuracy=function(predictions, actual) {
  sum(predictions == actual) / length(predictions)
}

standard_accuracy=calculate_accuracy(predictions, test_data$class)
cat("Accuracy del Modello:", standard_accuracy, "\n")


conf_matrix<-confusionMatrix(data=factor(predictions), reference = factor(test_data$class), positive="1")
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



output2 <- hoeffding_tree(training=train_data,info_gain = TRUE)
tree2 <- output2$output_model 

output2 <- hoeffding_tree(input_model=tree2, test=test_data)
predictions2 <- output2$predictions 
predictions2[predictions2 == 1] <- 0 
predictions2[predictions2 == 2] <- 1

standard_accuracy2=calculate_accuracy(predictions2, test_data$class)
cat("Accuracy del Modello:", standard_accuracy2, "\n")

conf_matrix<-confusionMatrix(data=factor(predictions2), reference = factor(test_data$class), positive="1")
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
train_data$class=factor(train_data$class)
test_data$class=factor(test_data$class)
standard_tree <- rpart(class ~ ., data = train_data)
standard_predictions <- predict(standard_tree, test_data, type = "class")
standard_accuracy <- calculate_accuracy(standard_predictions, test_data$class)
cat("Accuracy del Decision Tree standard:", standard_accuracy, "\n")

conf_matrix<-confusionMatrix(data=factor(standard_predictions), reference = factor(test_data$class), positive="1")
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

