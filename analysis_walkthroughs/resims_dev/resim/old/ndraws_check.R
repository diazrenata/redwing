source(here::here("resim", "resim_fxns.R"))
library(BBSsize)

g <- granby

g_5 <- construct_sampling_gmm(granby, 5)
g_25 <- construct_sampling_gmm(granby, 25)

plot(g_5$begin$density, g_25$begin$density)
plot(g_5$end$density, g_25$end$density)
library(ggplot2)

ggplot(g_5$begin, aes(mass, density)) + geom_line() + geom_line(data = g_25$begin, color = "green")

ggplot(g_5$end, aes(mass, density)) + geom_line() + geom_line(data = g_25$end, color = "green")

system.time(replicate(5,  construct_sampling_gmm(granby, 5)))
#   user  system elapsed
# 77.171   1.568  78.516

system.time(construct_sampling_gmm(granby, 25))
# user  system elapsed
# 85.486   1.025  86.310

system.time(replicate(5, construct_sampling_gmm(granby, 25)))
# 433.627   5.744 438.320
