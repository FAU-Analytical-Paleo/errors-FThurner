
# Height of a building --------------------------------------------------

radians <- function(d, m, s){
  rad <- pi*(d+m/60+s/3600)/180
  return(rad)
}

height <- tan(radians(1,21,0))*(2550)
uncertainty_height <- (tan(radians(0,1,0))/tan(radians(1,21,0))+25/2550)*height

paste("The height of the building is", height, "in meters with an uncertainty of", uncertainty_height, "in meters.")



# Duration of volcanic activity -------------------------------------------

duration <- 29.66-25.53
uncertainty_duration <- 0.1+0.2

paste("The likely duration of volcanic activity is", duration, "Ma", "+/-", uncertainty_duration, "Ma.")



# Earthquake size ---------------------------------------------------------

colnames <- c("X", "r", "Mo")

earthquakes <- read.table("ex3_eqscals.txt", header=F, sep="", col.names=colnames)

mean(earthquakes$r)
median(earthquakes$r)
sd(earthquakes$r)
mad(earthquakes$r)

mean(earthquakes$Mo)
median(earthquakes$Mo)
sd(earthquakes$Mo)
mad(earthquakes$Mo)



x11()

par(mfrow=c(2, 1))

measurements <- c(1:19)
plot(0,0, main="Estimated fault radius", xlab="Measure points", ylab="Fault Radius", xlim=c(1, 19), ylim=c(100, 1700))
lines(measurements, earthquakes$r)

plot(0,0, main="Seismic Moment", xlab="Measure points", ylab="Mo", xlim=c(1, 19), ylim=range(earthquakes$Mo))
lines(measurements, earthquakes$Mo)

boxplot(earthquakes$Mo)

z <- 0

for(n in 1:19){
  a <- (earthquakes[n, 3]-mean(earthquakes$Mo))^2
  z <- z+a
  print(z)
}

error_earthquake <- sqrt(z/19)

Mw <- log10(mean(earthquakes$Mo))/1.5-6.0

uncertainty_Mw <- log10(error_earthquake)/1.5-6.0

paste()