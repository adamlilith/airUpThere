library(terra)
library(omnibus)


# CHELSA
srcs <- listFiles('C:/ecology/Drive/R/airUpThere/R', pattern='.r')
for (src in srcs) source(src)

srcs <- listFiles('C:/ecology/Drive/R/airUpThere/data', pattern='.rda')
for (src in srcs) load(src)


