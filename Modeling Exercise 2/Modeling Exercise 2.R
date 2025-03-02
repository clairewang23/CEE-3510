setwd("C:/Users/rabbi/OneDrive/Documents/GitHub/CEE-3510")

#================================================================
# assignment #2 - BOD modeling
# CEE 3510
# March 2, 2025
# units, concentration = mg/L; rate constant = 1/day
# ===============================================================

# report names of objects present in working directory,
# then clears all objects in the working directory
ls()
rm(list=ls())
ls()

# install library to solve initial value problems
library(deSolve)
library(ggplot2)

# create model
model_BOD <- function (time, y, parms) {
  with(as.list(c(y, parms)), {
    dBOD <- -1 * (k1 + k3) * BOD
    list(c(dBOD))
  })
}

# set parameters and initial conditions
y <- c(BOD = 12)
parms <- c(k1 = 0.34, k3 = 0.04) 
times <- seq(0, 10, 1)

# solve
out_BOD <- ode(y, times, model_BOD, parms)

# add distance downstream to model
velocity <- 5 #km/d
distance <- velocity * times

BOD_distance <- cbind(out_BOD, distance)

# plot
ggplot(data = BOD_distance, mapping = aes(x = distance, y = BOD)) + geom_line(color = 'blue') + labs(title = "BOD Decay", x = "Distance (km)", y = "BOD (mg/L)") + theme_light() + ylim(0,14)
# plot(out_BOD, type="l", ylim=c(0,14),xlab="time (d)", ylab="BOD (mg/L)", main="BOD Decay")
