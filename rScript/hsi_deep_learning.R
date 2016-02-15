library(h2o)
h2o.init(nthreads = -1)
#with lag
hsi_data<- h2o.importFile("hsi_4factor.csv")
hsi_train <- hsi_data[c(1:70),]
hsi_test <- hsi_data[c(71:93),]

model_hsi<- h2o.deeplearning(x=colnames(hsi_4factor)[c(3,4,5,6)],y=colnames(hsi_4factor)[2], hidden = c(350,350), training_frame = hsi_train, validation_frame = hsi_test)
print(model_hsi)
h2o.performance(model_hsi,train =TRUE)
h2o.performance(model_hsi,valid=TRUE)
m<-svm(hsi~., data=hsi_4factor[-1,],cross=10,gamma = 0.1)
summary(m)

sink("hsi_dl.txt", append=FALSE, split=FALSE)
print(model_hsi)
sink()

#without lag
hsi_data_nolag<- h2o.importFile("hsi_4factor_nolag.csv")
hsi_train_nolag <- hsi_data_nolag[c(1:70),]
hsi_test_nolag <- hsi_data_nolag[c(71:93),]

model_hsi_nolag<- h2o.deeplearning(x=colnames(hsi_4factor_nolag)[c(3,4,5,6)],y=colnames(hsi_4factor_nolag)[2], training_frame = hsi_train_nolag, validation_frame = hsi_test_nolag, distribution = "multinomial",
                             activation = "RectifierWithDropout", hidden = c(200,200,200), input_dropout_ratio = 0.2, l1=1e-5,epochs = 10)
h2o.performance(model_hsi_nolag,train =TRUE)
h2o.performance(model_hsi_nolag,valid=TRUE)


