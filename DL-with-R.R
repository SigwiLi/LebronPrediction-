library(keras)
library(tensorflow)
library(caTools)
library(data.table)
data <- fread("E:\\Rmodel-Test\\nba_data.csv")

pts <- scan()
reb <- scan()
stl <- scan()
blk <- scan()

data$pts_over <- ifelse(data$PTS > pts, 1, 0)
data$reb_over <- ifelse(data$REB > reb, 1, 0)
data$stl_over <- ifelse(data$STL > stl, 1 ,0)
data$blk_over <- ifelse(data$BLK > blk, 1, 0)
data$WL <- ifelse(data$WL == "W", 1,0 )


data_ <- subset(data, select = c("PTS", "REB", "STL", "BLK", "WL", "pts_over", "reb_over", "stl_over", "blk_over"))
colSums(is.na(data_))
data_ <- na.omit(data_)



split <- sample.split(1:nrow(data_), 0.7 * nrow(data_))
train_set <- data_[split, ]
test_set <- data_[-split, ]

train_x <- as.matrix(train_set[, 1:4])
test_x <- as.matrix(test_set[, 1:4])

train_y_pts <- as.matrix(train_set$pts_over)
test_y_pts <- as.matrix(test_set$pts_over)

train_y_reb <- as.matrix(train_set$reb_over)
test_y_reb <- as.matrix(test_set$reb_over)

train_y_stl <- as.matrix(train_set$stl_over)
test_y_stl <- as.matrix(test_set$stl_over)

train_y_blk <- as.matrix(train_set$blk_over)
test_y_blk <- as.matrix(test_set$blk_over)

train_y_wl <- as.matrix(train_set$WL)
test_y_wl <- as.matrix(test_set$WL)


train_model <- function(train_x, train_y, test_x, test_y, target_name) {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 64, activation = 'relu', input_shape = c(4)) %>%
    layer_dense(units = 64, activation = 'relu') %>% 
    layer_dense(units = 1, activation = 'sigmoid')

  model %>%  compile(
    optimzer = 'rmsprop',
    loss = 'binary_crossentropy',
    metrics = keras.metrics.BinaryAccuracy()
  )
  
  history <- model %>% fit(
    train_x, train_y,
    epochs = 10,
    batch_size = 128,
    validation_date = list(test_x, test_y)
  )
  
  val_scr = model %>% evaluate(test_x, test_y)
  print(val_scr)
  
  return(model)
}

model_pts <- train_model(train_x, train_y_pts, test_x, test_y_pts, "pts_over")
model_reb <- train_model(train_x, train_y_reb, test_x, test_y_reb, "reb_over")
model_stl <- train_model(train_x, train_y_stl, test_x, test_y_stl, "stl_over")
model_blk <- train_model(train_x, train_y_blk, test_x, test_y_blk, "blk_over")
model_wl <- train_model(train_x, train_y_wl, test_x, test_y_wl, "WL")




predict_pts <- model_pts %>% predict(test_x)
predict_reb <- model_reb %>% predict(test_x)
predict_stl <- model_stl %>% predict(test_x)
predict_blk <- model_blk %>% predict(test_x)
predict_wl <- model_wl %>% predict(test_x)


predict_pts_class <- ifelse(predict_pts > 0.5, 1, 0)
predict_reb_class <- ifelse(predict_reb > 0.5, 1, 0)
predict_stl_class <- ifelse(predict_stl > 0.5, 1, 0)
predict_blk_class <- ifelse(predict_blk > 0.5, 1, 0)
predict_wl_class <- ifelse(predict_wl > 0.5, 1, 0)


head(predict_pts_class)
head(predict_reb_class)
head(predict_stl_class)
head(predict_blk_class)
head(predict_wl_class)


table(Predicted = predict_pts_class, Actual = test_y_pts)

table(Predicted = predict_reb_class, Actual = test_y_reb)

table(Predicted = predict_stl_class, Actual = test_y_stl)

table(Predicted = predict_blk_class, Actual = test_y_blk)

table(Predicted = predict_wl_class, Actual = test_y_wl)








