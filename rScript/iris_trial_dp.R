library("h2o")
library("mlbench")
## Start a local cluster with 2GB RAM
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, 
                    Xmx = '2g')
iris.hex <- as.h2o(iris)
iris.dl <- h2o.deeplearning(x = 1:4, y = 5, training_frame = iris.hex)
# now make a prediction
predictions <- h2o.predict(iris.dl, iris.hex)
summary(iris.hex)
#demo(h2o.glm)
print(iris.dl)
sink("iris_dl.txt", append=FALSE, split=FALSE)