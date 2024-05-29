
testfun <- function() { #space where we can define what the function should do
  
}

class(testfun)

testfun()

testfun <- function() {
  print("this function does nothing")
}

testfun()
# not function is printing what we set there

#usually we want to use it for inputs

testfun <- function(something) {
  print(something)
}

testfun("this function does sth more but still not much")

my_birthday <- "2001-01-07"
today <-Sys.Date()

difftime(today, my_birthday)

get_age <- function(my_birthday, my_units = "days") {
  today <- Sys.Date()
  age <- difftime(today, my_birthday, units = my_units)
  age
}


get_age("1984-11-11")



get_age <- function(my_birthday, my_units) {
  browser()
  today <- Sys.Date()
  age <- difftime(today, my_birthday, units = my_units)
  age
}


get_age("1984-11-11", "secs")


get_age <- function(my_birthday, my_units = "days") {
  browser()
  today <- Sys.Date()
  age <- difftime(today, my_birthday, units = my_units)
  age
}

#my_units doesnt exist, no interface

get_age("1984-11-11")


#Task 1. Write your own functions
#1 A function which calculates a persons BMI based on their 
#height and weight (BMI = weight (kg)/height(m)^2)



BMI <- function(weight, hight) {
  weight/(hight^2)
}

BMI(60, 1.7)


#2 A function that converts degrees from C to F (F= C*9/5+32)

Farenheight <- function(Celcius) {
  Celcius*9/5+32
}

Farenheight(30)

#3 A function which calculates the Euclidean distance between two sets of coordinates (x1, y1 
#and x2, y2) based on the formula below: Euclidean distance = sqrt((x2-x1)^2 + (y2-y1)^2)

eucl <- function(x1, y1, x2, y2) {
  sqrt((x2-x1)^2 + (y2-y1)^2)
}

eucl(0,0,1,1)

#Task 2 Prepare Analysis
library(readr)
library(ggplot2)
library(dplyr)

wildboar <- read_csv("wildschwein_BE_2056.csv")

from <- as.POSIXct("2015-04-01", tz = "UTC")
to <- as.POSIXct("2015-04-15*", tz ="UTC")

wildboar <- wildboar |> 
  filter(DatetimeUTC > from, DatetimeUTC < to) |>
  filter(TierName %in% c("Rosa", "Sabi"))


ggplot(wildboar, aes (E,N, color = TierName))+
  geom_point() + 
  coord_equal()
install.packages("lubridate")
library(lubridate)
wildboar <- wildboar |>
  mutate(
    Datetime_round = round_date(DatetimeUTC, "15 minutes")
  )

sabi <- filter(wildboar, TierName == "Sabi")
rosa <- filter (wildboar, TierName == "Rosa")

sabi
rosa

joined <- full_join(sabi, rosa, by = "Datetime_round", suffix = c("_sabi", "_rosa")) #|> View()


joined <- joined |>
  mutate(
    distance = eucl (E_sabi, N_sabi, E_rosa, N_sabi),
    meet = distance < 100
  )


p <- ggplot(joined) + 
  geom_point(aes(E_sabi, N_sabi), color = "blue") +
  geom_point(aes(E_rosa, N_rosa), color = "red") +
  geom_segment(data = filter(joined, meet),
               aes(x = E_rosa, y = N_rosa, xend = E_sabi, yend = N_sabi), color ="black") +
  coord_equal()

meetingpoints <- joined |>
  filter(meet) |>
  mutate(
   E.mean = (E_rosa + E_sabi) / 2,
   N.mean = (N_rosa + N_sabi) / 2
)

library (plotly)
ggplotly(p)  

joined |>
  filter(DatetimeRound < "2015-04-04") |>
  plot_ly (x=~E_rosa, y=~N_rosa, z=~DatetimeRound, type ="scatter3d", mode="lines" ) |>
  add_trace(joined, x = x=~E_sabi, y=~N_sabi, z=~DatetimeRound) |>
  add_markers(data=meetingpoints, x= ~E.mean, y= ~N.mean, z= ~DatetimeRound) |>
  layout(scene=list(
    xaxis = list(title = "E"),
    yaxis = list(title = "N"),
    zaxis = list(title = "Time")

  ))
  
