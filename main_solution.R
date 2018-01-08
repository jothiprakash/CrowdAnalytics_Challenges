# Import necessary packages
library(randomForest)
library(caret)
library(ade4)

# Import the datasets
mens_train <- read.csv("data/mens_train_file.csv")
mens_test <- read.csv("data/mens_test_file.csv")
womens_train <- read.csv("data/womens_train_file.csv")
womens_test <- read.csv("data/womens_test_file.csv")

# Combine Men's and Women's train data
mens_womens_train <- rbind(mens_train, womens_train)

# Combine Men's and Women's test data
mens_womens_test <- rbind(mens_test, womens_test)

# Quick peek into the data
View(head(mens_train))

# Check for NA values in the dataset
colSums(is.na(mens_train)) # Every values are available
colSums(is.na(mens_test)) # Except "outcome" column every values are available
colSums(is.na(womens_train)) # Every values are available
colSums(is.na(womens_test)) # Except "outcome" column every values are available

# Splitting the train data
split_index <- sample(1:nrow(mens_womens_train),
                      nrow(mens_womens_train) * 0.8)
train <- mens_womens_train[split_index, ]
test <- mens_womens_train[-split_index, ]
test_labels <- test[, 27]

# Create unnecessary columns vector
unnecessary_columns <- c(25, 26, 28) # Columns "id", "train", "gender" are ignored

# RandomForest model
classifier <- randomForest(outcome ~ .,
                           data = train[, -unnecessary_columns],
                           norm.votes = T,
                           proximity = T)

# Predicting the results
pred <- predict(classifier,
                newdata = test[, -c(27, unnecessary_columns)],
                type = "prob")

# Confusion matrix
confusionMatrix(test_labels, pred)

# Importance of features
varImpPlot(classifier)
View(classifier$importance)

# Predicting the results for test data
classifier <- randomForest(outcome ~ .,
                           data = mens_womens_train[, -unnecessary_columns],
                           norm.votes = T,
                           proximity = T)

pred <- predict(classifier,
                newdata = mens_womens_test[, -c(27, unnecessary_columns)],
                type = "prob")

# Columns preparation for output format
submission_id <- paste(mens_womens_test$id, 
                       "_",
                       mens_womens_test$gender,
                       sep = "")

train_column <- mens_womens_test$train

# output_columns <- acm.disjonctif(data.frame(pred))

# Final data to export
final_output <- data.frame(submission_id,
                           train_column,
                           pred[, c(2, 1, 3)])

# Some tweeks before exporting the result
sample_submission <- read.csv("data/AUS_SubmissionFormat.csv")

names(final_output) <- names(sample_submission)

row_order <- match(sample_submission$submission_id,
                   final_output$submission_id)

# Export the data
write.csv(x = final_output[row_order, ],
          file = "final_output.csv",
          row.names = F)
