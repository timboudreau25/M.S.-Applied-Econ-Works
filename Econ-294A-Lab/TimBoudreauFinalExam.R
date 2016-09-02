## Tim Boudreau
## Econ 294A
## FINAL EXAM
## March 16 2016

rm(list = ls())

library(nycflights13)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(class)

normalize <- function(x) {
  num <- x - min(x, na.rm = T)
  denom <- max(x, na.rm = T) - min(x, na.rm = T)
  return (num/denom)
}

my.data <- nycflights13_sqlite()

flights <- tbl(my.data, "flights") %>% 
  collect() %>%
  mutate(canceled = is.na(arr_time))
flights$canceled <- flights$canceled + 0

weather <- tbl(my.data, "weather") %>% collect()
airlines <- tbl(my.data, "airlines") %>% collect()
airports <- tbl(my.data, "airports") %>% collect()
planes <- tbl(my.data, "planes") %>% collect()
# sqlite_stat1 <- tbl(my.data, "sqlite_stat1") %>% collect()

#######################################
## Data Creation ##
#######################################

###############
### Weather ###
###############

flights.w <- flights %>%
  group_by(year, month, day, hour) %>%
  summarise(
    delay = mean(dep_delay, na.rm = T),
    cancel = sum(canceled)
  )

weather.w <- weather %>%
  arrange(year, month, day, hour)

data.w <- inner_join(flights.w, weather.w, by = c("year", "month", "day", "hour"))

###############
### Airport ###
###############

flights.ap <- flights %>%
  group_by(year, month, day, hour, dest) %>%
  summarise(
    delay = mean(dep_delay, na.rm = T),
    cancel = sum(canceled)
  ) 

airports.ap <- airports[,1:2]

data.ap <- inner_join(flights.ap, airports.ap, by = c("dest" = "faa"))

#############
### Plane ###
#############

flights.p <- flights %>%
  group_by(year, month, day, hour, tailnum) %>%
  summarise(
    delay = mean(dep_delay, na.rm = T),
    cancel = sum(canceled)
  ) 

data.p <- inner_join(flights.p, planes, by = c("tailnum"))

#######################################
## Data Analysis ##
#######################################


###############
### Weather ###
###############

summary(glm(cancel~delay, data = data.w))

cat("\n\nHourly delay has a significant effect at the 99% level on 
    the total number of canceled flights per hour.\n\n")

#### Normalized cancel, probit model ####

summary(glm(normalize(cancel)~temp+dewp+humid+wind_dir+wind_speed+
              wind_gust+precip+pressure+visib, 
            family=binomial(link="probit"),data = data.w))

cat("\n\nNo variable has a statistically significant effect on 
    normalized hourly canceled flights.\n\n")

#### Hourly delay ####

summary(glm(delay~temp+humid+wind_dir+wind_speed+precip+
              pressure+visib, data = data.w))

summary(glm(delay~temp+humid+wind_dir+pressure+visib, data = data.w))

summary(glm(delay~temp+humid+pressure+visib, data = data.w))

ggplot(data = data.w, aes(x = temp, y = delay, colour = cancel, size = cancel)) + 
  scale_x_log10()  + geom_point( alpha = 1) + 
  scale_size(range = c(1,10))

cat("\n\nHere, temperature, humidity, pressure and visibility have 
    99% confident, statistically significant effects on
    delay time. Wind direction, though, is statistically
    significant at the 90% level.\n\n")

#### Hourly canceled flights ####

summary(glm(cancel~temp+humid+wind_dir+wind_speed+precip+pressure
            +visib,data = data.w))

summary(glm(cancel~temp+wind_dir+pressure+visib,data = data.w))

summary(glm(cancel~temp+pressure+visib,data = data.w))

cat("\n\nTemperature, pressure and visibility have a significant 
    effect on the amount of canceled hourly flights, at 
    confidence levels of 99%, 95% and 95% respectively. 
    Wind direction is also significant, though only at 
    a 90% confidence level and when we include other 
    non-significant variables.\n\n")

###############
### Time ###
###############

data.t <- flights %>%
  group_by(year, month, day, hour, minute) %>%
  summarise(
    delay = mean(dep_delay, na.rm=T),
    cancel = sum(canceled)
  )

summary(glm(delay~month+day+hour+minute, data = data.t))

summary(glm(delay~month+hour+minute, data = data.t))

cat("\n\nOut of the time variables, month, hour and minute
    are statistically significant at the 99% confidence
    level. Day isn't statistically significant.\n\n")

summary(glm(cancel~month+day+hour+minute, data = data.t))

cat("\n\nThe variables that significantly impact number of flights
    canceled are hour, at the 99% level, and minutes, 
    at the 95% level.\n\n")

ggplot(data = data.t, aes(x = delay, y = hour, colour = hour)) + 
  scale_x_log10()  + geom_point( alpha = 1) + 
  scale_size(range = c(1,10))

###############
### Airport ###
###############

reg.ap <- summary(glm(delay~dest, data = data.ap))

sig <- 0
j <- 1

for (i in 2:(nrow(coef(reg.ap)))) {
  if (coef(reg.ap)[i,4] < .05) {
    sig[j] <- i
    j <- j + 1
  }
}

all.dest <- sort(unique(data.ap$dest))

all.sig.dest <- all.dest[sig]

sig.dest.rows <- match(all.sig.dest, data.ap$dest)[1:length(all.sig.dest)]

all.sig.names <- data.ap$name[sig.dest.rows]

apdelay <- aggregate(data.ap[,6], list(data.ap$dest), mean, na.rm = T)

cat("\n\nNot every airport destination has a significant 
    relationship with delayed flights. These ones do:\n\n", 
    all.sig.dest[1:13], "\n", all.sig.dest[14:25], "\n\n",
    all.sig.names[1:3], "\n", all.sig.names[4:6], "\n", 
    all.sig.names[7:9], "\n", all.sig.names[10:12], "\n", 
    all.sig.names[13:15], "\n", all.sig.names[16:18], "\n", 
    all.sig.names[19:21], "\n", all.sig.names[22:24], "\n", 
    all.sig.names[25], "\n\n")

reg.ap <- summary(glm(cancel~dest, data = data.ap))

sig <- 0
j <- 1

for (i in 2:(nrow(coef(reg.ap)))) {
  if (coef(reg.ap)[i,4] < .05) {
    sig[j] <- i
    j <- j + 1
  }
}

all.dest <- sort(unique(data.ap$dest))

all.sig.dest <- all.dest[sig]

sig.dest.rows <- match(all.sig.dest, data.ap$dest)[1:length(all.sig.dest)]

all.sig.names <- data.ap$name[sig.dest.rows]

cat("\n\nNot every airport destination has a significant 
    relationship with canceled flights. These ones do:\n\n", 
    all.sig.dest[1:10], "\n", all.sig.dest[11:20], "\n",
    all.sig.dest[21:30], "\n\n",
    all.sig.names[1:3], "\n", all.sig.names[4:6], "\n", 
    all.sig.names[7:9], "\n", all.sig.names[10:12], "\n", 
    all.sig.names[13:15], "\n", all.sig.names[16:18], "\n", 
    all.sig.names[19:21], "\n", all.sig.names[22:24], "\n", 
    all.sig.names[25:27], "\n", all.sig.names[28:30], "\n")



data.sub <- data.ap %>%
  group_by(dest) %>%
  summarize(
    delaymean = mean(delay, na.rm = TRUE)
  )

data.su <- data.sub[ which(data.sub$dest == all.sig.dest)]

ggplot(data = data.sub, aes(x = delaymean, y = ..density.., color = dest), 
       main = "Density of Mean Delay Times, Colored by Destination Airport", 
       xlab = "Delay Mean", ylab = "Density") +
  geom_histogram() + guides(fill = FALSE)


###############
### Airplane ###
###############

summary(glm(delay~type+manufacturer+model, data = data.p))

cat("\n\nNone of the engine types are significant by themselves, 
    but various models and plane types (also seen as tail numbers) 
    have a significant effect on delay.\n\n")

ggplot(data = data.p, aes(x = engines, y = delay, colour = type), 
       main = "Number of Engines and Flight Delay Lengths", 
       xlab = "Engines", ylab = "Delay") +
  geom_point( alpha = .2) +
  scale_size(range = c(4,10)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 6)))