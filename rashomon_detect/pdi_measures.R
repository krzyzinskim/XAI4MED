library(fda.usc)
library(doremi)

derivative_fraction_sign_difference <- function(profile1, profile2, xargs){
  profile1der <- calculate.gold(time = xargs,
                                signal = profile1,
                                embedding = 4,
                                n = 1)
  profile2der <- calculate.gold(time = xargs,
                                signal = profile2,
                                embedding = 4,
                                n = 1)
  spline1 <- spline(profile1der$dsignal[,2] ~ profile1der$dtime)
  spline2 <- spline(profile2der$dsignal[,2] ~ profile2der$dtime)
  mean(sign(spline1$y) != sign(spline2$y))
}

derivative_fraction_sign_difference_simplified <- function(profile1, profile2, xargs){
  derivative1 <- fdata.deriv(fdata(profile1, argvals = xargs))$data[1,]
  derivative2 <- fdata.deriv(fdata(profile2, argvals = xargs))$data[1,]
  mean(sign(derivative1) != sign(derivative2))
}

euclidean_distance <- function(profile1, profile2, xargs){
  as.numeric(metric.lp(
    fdata(profile1, argvals = xargs), 
    fdata(profile2, argvals = xargs)
  ))
}

derivative_euclidean_distance <- function(profile1, profile2, xargs){
  as.numeric(semimetric.deriv(
    fdata(profile1, argvals = xargs), 
    fdata(profile2, argvals = xargs)
  ))
}

categorical_distance <- function(profile1, profile2, xargs){
  dist(rbind(profile1, profile2))[1]
}