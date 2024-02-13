simple_data = read.csv(noquote(file.choose()))
multiple_data = read.csv(noquote(file.choose()))
SLR <- lm(y~x,data=simple_data)
target <- multiple_data[, ncol(multiple_data)]
features <- multiple_data[, -ncol(multiple_data)]
MLR <- lm(target ~ ., data = data.frame(target, features))
summary(MLR)
summary(SLR)
confint(SLR)
confint(MLR)
