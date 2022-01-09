# A collection of plots of raw fjord data from British Columbia and New Zealand

library(ggplot2)

d <- read.csv(file = "data/fjords.csv", header = TRUE)

dfNZ <- d[d$region == "NZ", ]
dfBC <- d[d$region == 'BC', ]

## British Columbia (BC) Raw Data Plots

# scatter graph - length vs width, & area as size of dot 
p <- ggplot(data = dfBC, aes(x = length, y = width, size = area))
p + geom_point(color = "deepskyblue4")

# scatter graph with colors and labels
p <-
  ggplot(data = dfBC, aes(
    x = length,
    y = width,
    color = area,
    size = area
  )) + geom_point()
p + scale_color_gradient(low = "cyan", high = "red") +
  xlab("Length (km)") +
  ylab("Width (km)") +
  ggtitle("British Columbia Fjords") +
  scale_fill_manual(name = "Area") +
  guides(size = "none") +
  labs(color = expression ("Area" ~ (km ^ 2)),
       caption = paste0("Figure 2 (above) shows an overview of the relationship", 
                       "that valley characteristics, \n length and width have ",
                       "with catchment area of fjords in British Columbia."))

# scatter plot with smooth line, length vs area
p <- ggplot(data = dfBC, aes(x = length, y = area)) + geom_point()
p + geom_point(color = "green4") +
  labs(x = "Length", y = "Area", title = "British Columbia - Length vs Area") +
  geom_smooth(color = "black")

# scatter plot with smooth line, width vs area
p <- ggplot(data = dfBC, aes(x = width, y = area)) + geom_point()
p + geom_point(color = "green4") +
  labs(x = "Width", y = "Area", title = "British Columbia - Width vs Area") +
  geom_smooth(color = "black")


## New Zealand (NZ) Raw Data Plots

# scatter graph - length vs width, & area as size of dot 
p <-
  ggplot(data = dfNZ, aes(
    x = length,
    y = width,
    color = area,
    size = area
  )) + geom_point()
p + scale_color_gradient(low = "cyan", high = "red") +
  xlab("Length (km)") +
  ylab("Width (km)") +
  ggtitle("New Zealand Fjords") +
  scale_fill_manual(name = "Area") +
  guides(size = "none") +
  labs(color = expression ("Area" ~ (km ^ 2)),
       caption = paste0("Figure 1 (above) shows an overview of the relationship", 
                       "that valley characteristics,\n length and width have ",
                       "with catchment area of fjords in New Zealand."))

# scatter plot with smooth line, length vs area
p <- ggplot(data = dfNZ, aes(x = length, y = area)) + geom_point()
p + geom_point(color = "green4") +
  labs(x = "Length", y = "Area", title = "New Zealand - Length vs Area") +
  geom_smooth(color = "black")

# scatter plot with smooth line, width vs area
p <- ggplot(data = dfNZ, aes(x = width, y = area)) + geom_point()
p + geom_point(color = "green4") +
  labs(x = "Width", y = "Area", title = "New Zealand - Width vs Area") +
  geom_smooth(color = "black")
