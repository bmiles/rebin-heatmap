m <- ggplot(liq, aes(x=value))
m + geom_histogram(aes(y = ..density..)) + geom_density() +
 labs(title="Histogram of thickness map of Micrux Au Electrode on pyrex, 19-07-2014",
     x = "thickness [nm]",
     y = "Density")

ggsave("Histogram of thickness map of Micrux Au Electrode on pyrex, 19-07-2014.pdf")
