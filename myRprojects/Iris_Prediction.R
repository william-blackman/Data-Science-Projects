data(iris)
set.seed(42)

#generating list of random numbers to create the samples
indexes = sample(
  x = 1:150,
  size = 100)

#assigning the random rows of iris data to train & test variables
train = iris[indexes, ]
test = iris[-indexes, ]

head(train)

library(tree)
#use a decision tree model with 2 parameters tod determine Iris species
model = tree(
  formula = Species ~ Petal.Length + Petal.Width,
  data = train)

summary(model)
plot(model)
text(model)

library(RColorBrewer)
palette = brewer.pal(3, "Set2")

#create a scatterplot colored by species
plot(
  x = iris$Petal.Length,
  y = iris$Petal.Width,
  pch = 19,
  col = palette[as.numeric(iris$Species)],
  main = "Iris Petal Length vs. Petal Width",
  xlab = "Petal Length",
  ylab = "Petal Width")

#plot the decision tree boundaries
partition.tree(
  tree = model,
  label = "Species",
  add = TRUE)

predictions = predict(
  object = model,
  newdata = test,
  type = "class")

table(
  x = predictions,
  y = test$Species)

library(caret)

confusionMatrix(
  data = predictions,
  reference = test$Species)

save(model, file = "Tree.RData")
