setwd('C:/Users/willj/Documents/GitHub/Data Science Projects/myRprojects/data_viz_with_R')

movies = read.csv("Movies.csv")
head(movies)

plot(
  x = movies$Rating,
  main = "Count of Movies by Rating",
  xlab = 'Rating',
  ylab = 'Count of Movies',
  )

?ylim

dotchart(
  x =table(movies$Rating),
  pch = 16,
  main = "Count of Movies by Rating",
  ylab = 'Rating',
  xlab = 'Count of Movies',
)

library(lattice)
table = table(movies$Rating)
ratings = as.data.frame(table)
names(ratings)[1] = "Rating"
names(ratings)[2] = "Count"
print(ratings)

barchart(
  x = Count ~ Rating,
  data = ratings,
  main = "Count of Movies by Rating",
  xlab = "Rating")

barchart(
  x = Rating ~ Count,
  data = ratings,
  main = "Count of Movies by Rating",
  ylab = "Rating")

histogram(
    x = ~rating,
    data = movies,
    main = "Percent of Movies by Rating")

library(ggplot2)

ggplot(
    data = movies,
    aes(x = Rating))+
    geom_bar() +
    coord_flip() +
    ggtitle("Count of Movies by Rating")

ggplot(
  data = movies,
  aes(x = Rating))+
  geom_point(stat = "count") +
  coord_flip() +
  ggtitle("Count of Movies by Rating")

plot(
  x = movies$Runtime,
  y = jitter(rep(0, nrow(movies))),
  ylab = "",
  yaxt = "n")

boxplot(
  x = movies$Runtime,
  horizontal = T,
  main = "Dist",
  xlab = "Runtime"
)

stripplot(
  x = ~Runtime,
  data = movies,
  main = "Dist",
  xlab = "Runtime",
  jitter = TRUE,
  amount = 0.3
)

average = tapply(
  movies$Box.Office,
  movies$Rating,
  mean
)

average

barplot(
    height = average,
    main = "Average Box Office Revenue by Rating",
    xlab = "Rating",
    ylab = "Average Revenue ($M)",
    col = "light blue")

boxplot(
    movies$Box.Office ~ movies$Rating,
    main = "box office revenue by rating",
    xlab = "rating",
    ylab = "revenue")

library(dplyr)
average = movies %>%
  select(Rating, Box.Office) %>%
  group_by(Rating) %>%
  summarize(Box.Office = mean(Box.Office)) %>%
  as.data.frame()

average

barchart(
  x = Box.Office ~ Rating,
  data = average,
  main = "Average Box Office Revenue by Rating",
  xlab = "Rating",
  ylab = "Box Office ($M)")

bwplot(
  x = Box.Office ~ Rating,
  data = movies,
  notch = TRUE,
  main = "Average Box Office Revenue by Rating",
  xlab = "Rating",
  ylab = "Box Office ($M)")

ggplot(
  data = average,
  aes(x=Rating, y = Box.Office)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Box Office Revenue by Rating") +
  xlab("Rating") +
  ylab("Box Office Revenue ($M)")

ggplot(
  data = movies,
  aes(x=Rating, y = Box.Office)) +
  geom_boxplot() +
  ggtitle("Average Box Office Revenue by Rating") +
  xlab("Rating") +
  ylab("Box Office Revenue ($M)")
)

ggplot(
  data = movies,
  aes(x=Rating, y = Box.Office)) +
  geom_boxplot(notch = TRUE) +
  ggtitle("Average Box Office Revenue by Rating") +
  xlab("Rating") +
  ylab("Box Office Revenue ($M)")


ggplot(
  data = iris,
  aes(x=Species, y = Petal.Length)) +
  geom_violin() +
  ggtitle("Distribution of Petal Length") +
  xlab("Species") +
  ylab("Petal Length")


install.packages("shiny")
library(shiny)
runExample("01_hello")

data(iris)
ggplot(
  data = iris,
  aes(x = Species)) +
  geom_bar() +
  geom_tile("Count of Iris Species in the Dataset")+
  xlab("Species") +
  ylab("Count")

library(dplyr)

iris["Petal.Ratio"] = iris$Petal.Length / iris$Petal.Width

  ggplot(
    data = iris,
    aes(x = Species, y=Petal.Ratio)) +
    geom_() +
    ggtitle("Ratio of Petal Length to Petal Width") +
    xlab("Species") +
    ylab("Petal Length/Petal Width")
