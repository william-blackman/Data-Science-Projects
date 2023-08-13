housing = read.csv("housing.csv")
head(housing)
library(ggplot2)
library(dplyr)

household_location = housing %>% group_by(ocean_proximity) %>%
  summarise(sum_households=sum(households))

ttl_households = household_location %>% as.data.frame()

ggplot(
  data = ttl_households,
  aes(x = reorder(ocean_proximity, -sum_households), y = sum_households/1000000))+
  geom_col(fill = "deepskyblue3")+
  ggtitle("Household Count by Ocean Proximity")+
  xlab("Location")+
  ylab("Number of Houses (millions)")

ggplot(
  data = housing,
  aes(x = population, y = households))+
  geom_point()+
  ggtitle("Region Population VS Households")+
  xlab("Population")+
  ylab("Households")

ggplot(
  data = housing,
  aes(x = total_rooms, y = total_bedrooms))+
  geom_point()+
  ggtitle("Total Rooms vs. Total Bedrooms")+
  xlab("Total Rooms")+
  ylab("Total Bedrooms")



