abalone =  read.csv("abalone.csv")

library(tidyverse)
library(randomForest)
library(caret)
library(corrplot)

# Histogram of Rings
ggplot(abalone, aes(x = Rings)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Rings")

# Boxplot for Rings by Sex
ggplot(abalone, aes(x = Sex, y = Rings)) + 
  geom_boxplot(fill = "cyan", color = "black") +
  labs(title = "Rings Distribution by Sex")

# Pair Plot to visualize relationships between variables
pairs(~Length + Diameter + Height + Whole_weight, data = abalone, 
      main = "Pairwise Relationships", pch = 20)

anova_result <- aov(Rings ~ Sex, data = abalone)
summary(anova_result)

abalone$Sex <- as.factor(abalone$Sex)
correlations <- cor(abalone[,-1])
correlations

# Train a Random Forest to get variable importance
set.seed(12)
model <- randomForest(Rings ~ ., data = abalone, importance = TRUE)
varImpPlot(model)


abalone_no_sex <- abalone[, !names(abalone) %in% c("Sex")]
index <- createDataPartition(abalone_no_sex$Rings, p = 0.8, list = TRUE)
train_data <- abalone_no_sex[index$Resample1,]
test_data <- abalone_no_sex[-index$Resample1,]
train_control <- trainControl(
  method = "cv",  # using cross-validation
  number = 10     # number of folds in cross-validation
)
models <- list()
models$rf <- train(Rings ~ ., data = abalone_no_sex, method = "rf", trControl = train_control)

# SVM
models$svm <- train(Rings ~ ., data = abalone_no_sex, method = "svmRadial", trControl = train_control, preProcess = "scale")


# GBM
models$gbm <- train(Rings ~ ., data = abalone_no_sex, method = "gbm", trControl = train_control, verbose = FALSE)

# Linear Regression
models$lm <- train(Rings ~ ., data = abalone_no_sex, method = "lm", trControl = train_control)


predictions_rf <- predict(models$rf, test_data)
predictions_svm <- predict(models$svm, test_data)
predictions_gbm <- predict(models$gbm, test_data)
predictions_lm <- predict(models$lm, test_data)

rmse_rf <- RMSE(predictions_rf, test_data$Rings)
r2_rf <- R2(predictions_rf, test_data$Rings)

rmse_svm <- RMSE(predictions_svm, test_data$Rings)
r2_svm <- R2(predictions_svm, test_data$Rings)

rmse_gbm <- RMSE(predictions_gbm, test_data$Rings)
r2_gbm <- R2(predictions_gbm, test_data$Rings)

rmse_lm <- RMSE(predictions_lm, test_data$Rings)
r2_lm <- R2(predictions_lm, test_data$Rings)

cat("RF RMSE:", rmse_rf, "R2:", r2_rf, "\n")
cat("SVM RMSE:", rmse_svm, "R2:", r2_svm, "\n")
cat("GBM RMSE:", rmse_gbm, "R2:", r2_gbm, "\n")
cat("LM RMSE:", rmse_lm, "R2:", r2_lm, "\n")

# Set up training control
train_control <- trainControl(method = "cv", number = 10, search = "grid")

tune_grid <- expand.grid(
  mtry = seq(2, ncol(abalone_no_sex) - 1, by = 1), # adjust the range for mtry based on the reduced number of features
  splitrule = c("variance"),
  min.node.size = c(5, 10)
)

set.seed(12)
tuned_model <- train(Rings ~ ., data = abalone_no_sex, method = "ranger",
                     trControl = train_control, tuneGrid = tune_grid,
                     metric = "RMSE")

library(caret)
library(FactoMineR)

abalone_pca = abalone_no_sex[, !names(abalone) %in% c("Rings")]
# Assuming abalone_no_sex is your dataset
data <- abalone_pca  # make sure this dataset does not include the response variable 'Rings'

# Scale the data
data_scaled <- scale(data)

pca_result <- PCA(data_scaled, graph = FALSE)

plot(pca_result, choice = "eigen")

num_components <- which(cumsum(pca_result$eig[,2]) > 85)[1]
data_pca <- pca_result$ind$coord[, 1:num_components]
num_components <- which(cumsum(pca_result$eig[,2]) > 85)[1]
data_pca <- pca_result$ind$coord[, 1:num_components]

# Prepare the final dataset for modeling
final_data <- data.frame(Rings = abalone_pca$Rings, data_pca)

# Splitting the data into training and testing sets
set.seed(12)
index <- createDataPartition(final_data$Rings, p = 0.8, list = TRUE)
train_data_pca <- final_data[index$Resample1,]
test_data_pca <- final_data[-index$Resample1,]

# Train models using the PCA-transformed data
train_control_pca <- trainControl(method = "cv", number = 10)
models_pca <- list()
models_pca$rf <- train(Rings ~ ., data = train_data_pca, method = "rf", trControl = train_control_pca)
models_pca$svm <- train(Rings ~ ., data = train_data_pca, method = "svmRadial", trControl = train_control_pca)
models_pca$gbm <- train(Rings ~ ., data = train_data_pca, method = "gbm", trControl = train_control_pca, verbose = FALSE)

# Linear Regression
models_pca$lm <- train(Rings ~ ., data = train_data_pca, method = "lm", trControl = train_control_pca)

predictions_rf_pca <- predict(models_pca$rf, test_data_pca)
predictions_svm_pca <- predict(models_pca$svm, test_data_pca)
predictions_gbm_pca <- predict(models_pca$gbm, test_data_pca)
predictions_lm_pca <- predict(models_pca$lm, test_data_pca)

rmse_rf_pca <- RMSE(predictions_rf_pca, test_data_pca$Rings)
r2_rf_pca <- R2(predictions_rf_pca, test_data_pca$Rings)

rmse_svm_pca <- RMSE(predictions_svm_pca, test_data_pca$Rings)
r2_svm_pca <- R2(predictions_svm_pca, test_data_pca$Rings)

rmse_gbm_pca <- RMSE(predictions_gbm_pca, test_data_pca$Rings)
r2_gbm_pca <- R2(predictions_gbm_pca, test_data_pca$Rings)

rmse_lm_pca <- RMSE(predictions_lm_pca, test_data_pca$Rings)
r2_lm_pca <- R2(predictions_lm_pca, test_data_pca$Rings)

cat("RF RMSE:", rmse_rf_pca, "R2:", r2_rf_pca, "\n")
cat("SVM RMSE:", rmse_svm_pca, "R2:", r2_svm_pca, "\n")
cat("GBM RMSE:", rmse_gbm_pca, "R2:", r2_gbm_pca, "\n")
cat("LM RMSE:", rmse_lm_pca, "R2:", r2_lm_pca, "\n")

# Load Shiny library
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Abalone Age Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("length", "Length",value = 0.5),
      numericInput("diameter", "Diameter",value = 0.4),
      numericInput("height", "Height",value = 0.4),
      numericInput("whole_weight", "Whole Weight",value = 0.4),
      numericInput("shucked_weight", "Shucked Weight",value = 0.5),
      numericInput("viscera_weight", "Viscera Weight",value = 0.4),
      numericInput("shell_weight", "Shell Weight",value = 0.34),
      actionButton("predict", "Predict Age",value=0.5)
    ),
    mainPanel(
      textOutput("age_prediction")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$predict, {
    new_data <- data.frame(
      Length = input$length,
      Diameter = input$diameter,
      Height = input$height,
      Whole_weight = input$whole_weight,
      Shucked_weight = input$shucked_weight,
      Viscera_weight = input$viscera_weight,
      Shell_weight = input$shell_weight
    )
    
    # Predict using the model
    predicted_age <- predict(models_pca$rf, new_data)
    
    # Output the prediction
    output$age_prediction <- renderText({
      paste("Predicted age:", round(predicted_age))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)