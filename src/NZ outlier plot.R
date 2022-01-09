library(ggplot2)

d <- read.csv(file = "data/fjords.csv")

a <- d[d$region == "NZ", ]

highlight.gene <- "62"

a
a$highlight <-
  ifelse(rownames(a) == highlight.gene, "highlight", "normal")
textdf <- a[a$GeneName == highlight.gene,]
mycolours <- c("highlight" = "red", "normal" = "grey50")

a
textdf

p <- ggplot(data = a, aes(x = width, y = area))
p + geom_point(size = 3, aes(colour = highlight)) +
  xlab("Width (km)") +
  theme(legend.position = "none") +
  labs(y = expression ("Area" ~ (km ^ 2)), 
       caption = paste0("Figure 3 (above) shows how the outlier (shown in red)", 
                       "in the \nNZ Fjord data relates to other data points in", 
                       "the data set"))
