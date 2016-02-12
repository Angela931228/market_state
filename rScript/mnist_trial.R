library(h2o)
h2o.init(nthreads = -1)

train_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/train.csv.gz"
test_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/test.csv.gz"
train <- h2o.importFile(train_file)
test <- h2o.importFile(test_file)
summary(train)
summary(test)

y <- "C785"
x <- setdiff(names(train), y)

train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

model <- h2o.deeplearning(x=x,y=y, training_frame = train, validation_frame = test, distribution = "multinomial",
                          activation = "RectifierWithDropout", hidden = c(200,200,200), input_dropout_ratio = 0.2, l1=1e-5,epochs = 10)

model_cx <- h2o.deeplearning(x=x,y=y, training_frame = train, validation_frame = test, distribution = "multinomial",
                          activation = "RectifierWithDropout", hidden = c(200,200,200), input_dropout_ratio = 0.2, l1=1e-5,epochs = 10, nfolds=5)

model@parameters
model
h2o.performance(model,train =TRUE)
h2o.performance(model,valid=TRUE)

h2o.mse(model,valid = TRUE)
h2o.mse(model_cx,xval = TRUE)

hidden_opt <- list( c(100,300,100), c(500,500,500))
l1_opt <- c(1e-5,1e-7)
hyper_params <- list(hidden = hidden_opt, l1 = l1_opt)
model_grid <- h2o.grid("deeplearning",hyper_params = hyper_params,x = x,y = y,distribution = "multinomial",training_frame = train,validation_frame = test)

for(model_id in model_grid@model_ids){
  model <- h2o.getModel(model_id)
  mse <- h2o.mse(model,valid =TRUE)
  print(sprintf("Test set MSE: %f", mse))
}



