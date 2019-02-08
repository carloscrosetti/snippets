library("rpart")
library("ggplot2")

d <- data.frame(x=1:100, y=1:100)
model <- rpart(y~x, data=d)

print(model)

pred <- predict(model, newdata= d)
ggplot(data=d, mapping=aes(x=pred, y=y)) 
geom_point() 
geom_abline(color='blue')
ggtitle("actual value as a function of predicted value")