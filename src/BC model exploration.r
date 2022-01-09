# Exploration of linear models to predict catchment area of fjords in British 
# Columbia using their length and width

# create a title for a residual diagnostics plot
make_title_string <-
  function(region,
           area_trans,
           X_trans,
           X_var,
           extras = '') {
    rtn <- paste(
      region,
      "-Area vs ",
      X_var,
      " - Transfrom: Area_",
      area_trans,
      " ",
      paste(X_var, X_trans, sep = '_'),
      sep = ''
    )
  }

make_title_of_residual_diagnostics_plot <-
  function(region,
           area_trans,
           X_trans,
           X_var,
           extras = '') {
    title <- make_title_string
    path_name <- paste("./Diagnostic_Plots", region, sep = '/')
    file_name <- paste(paste(
      paste(X_var, X_trans, sep = '_'),
      paste("Area", area_trans, sep = '_'),
      sep = '-'
    ), ".png", sep = '')
    
    result <- list()
    
    return (c(paste(
      path_name,
      paste(extras, file_name, sep = ''),
      sep = '/'
    ),
    title))
  }

# get the data
data <- read.csv(file = "data/fjords.csv")

dfBC <- data[data$region == 'BC', ]

BC_fit_length <- lm(area ~ length, data = dfBC)
BC_fit_width  <- lm(area ~ width,  data = dfBC)

summary(dfBC$width)
summary(dfBC$length)
sd(dfBC$length)
summary(BC_fit_length)
summary(BC_fit_width)

# length seems much better
## lets do a res diag plot of area ~ length
fit <- lm(area ~ length, data = dfBC)
plot_name  <- make_title_of_residual_diagnostics_plot(
  reg = "BC",
  area_trans = '1',
  X_trans = '1',
  X_var = "Length",
  extras = 'rnd2-'
)

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()

## the diagnostics actually don't look great.
## lets do a boxcox
library(MASS)
boxcox(fit)
png("BC-Boxcox-1-1.png", width = 275, height = 275)
boxcox(fit, lambda = seq(-0.5, 0.8, 0.02))
dev.off()
# suggests a log transform of response variable Y

fit <- lm(log(area) ~ length, data = dfBC)
summary(fit)
plot_name <-
  make_title_of_residual_diagnostics_plot(
    reg = "BC",
    area_trans = '0',
    X_trans = '1',
    X_var = "Length",
    extras = 'rnd2-'
  )

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()
# the diagnostics are much worse.

## lets try boxcox without outliers 8 & 5
clean_dfBC <- dfBC[!rownames(dfBC) == 5 & !rownames(dfBC) == 8 , ]
fit <- lm(area ~ length, data = clean_dfBC)
boxcox(fit)



# doesn't make a difference
## let's try the log transform of y and then see what R^2 we get

fit <- lm(log(area) ~ length, data = clean_dfBC)
summary(fit)

plot_name <-
  make_title_of_residual_diagnostics_plot(
    reg = "BC",
    area_trans = '0',
    X_trans = '1',
    X_var = "Length",
    extras = 'rnd3-5-8'
  )

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()

## no better
# back to all data
fit <- lm(log(area) ~ log(length), data = dfBC)
summary(fit)
plot_name <-
  make_title_of_residual_diagnostics_plot(
    reg = "BC",
    area_trans = '0',
    X_trans = '0',
    X_var = "Length",
    extras = 'rnd3-'
  )

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()

# After transforming there seems to be no problem. Lets have a look at just 8? That seems to be the worse one
clean_dfBC <- dfBC[!rownames(dfBC) == 8 , ]
fit <- lm(area ~ length, data = clean_dfBC)
boxcox(fit)
summary(fit)

plot_name <-
  make_title_of_residual_diagnostics_plot(
    reg = "BC",
    area_trans = '1',
    X_trans = '1',
    X_var = "Length",
    extras = 'rnd3-Take out 8-'
  )

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()

opar <- par(mfrow = c(1, 1))
plot(fit, main = "BC-Point '8' Removed")

# It looks like 18 is bad now
clean_dfBC <- dfBC[!rownames(dfBC) == 8 & !rownames(dfBC) == 18, ]
fit <- lm(area ~ length, data = clean_dfBC)
boxcox(fit)
summary(fit)

plot_name <-
  make_title_of_residual_diagnostics_plot(
    reg = "BC",
    area_trans = '1',
    X_trans = '1',
    X_var = "Length",
    extras = 'rnd2-Take out 8 and 18-'
  )

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()

opar <- par(mfrow = c(1, 1))
plot(fit, main = "BC-Point '8' and '18' Removed")

# 15 and 20 are outliers now
clean_dfBC <-
  dfBC[!rownames(dfBC) == 8 &
         !rownames(dfBC) == 18 & !rownames(dfBC) == 15 &
         !rownames(dfBC) == 20, ]
fit <- lm(area ~ length, data = clean_dfBC)
boxcox(fit)
summary(fit)

plot_name <-
  make_title_of_residual_diagnostics_plot(
    reg = "BC",
    area_trans = '1',
    X_trans = '1',
    X_var = "Length",
    extras = 'rnd2-Take out 8_15_18_20-'
  )

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()

#no more outliers, so lets do a boxcox
boxcox(fit)

# it looks like a log transform
# lets do it!!!! mutha f*cka :P yeeaaah
clean_dfBC <-
  dfBC[!rownames(dfBC) == 8 &
         !rownames(dfBC) == 18 & !rownames(dfBC) == 15 &
         !rownames(dfBC) == 20, ]
fit <- lm(log(area) ~ length, data = clean_dfBC)
boxcox(fit)
summary(fit)

plot_name <-
  make_title_of_residual_diagnostics_plot(
    reg = "BC",
    area_trans = '0',
    X_trans = '1',
    X_var = "Length",
    extras = 'rnd2-Take out 8_15_18_20-'
  )

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()

# now 5 is being shit
clean_dfBC <-
  dfBC[!rownames(dfBC) == 8 &
         !rownames(dfBC) == 18 &
         !rownames(dfBC) == 15 & !rownames(dfBC) == 20 &
         !rownames(dfBC) == 5, ]
fit <- lm(log(area) ~ length, data = clean_dfBC)
boxcox(fit)
summary(fit)

plot_name <-
  make_title_of_residual_diagnostics_plot(
    reg = "BC",
    area_trans = '0',
    X_trans = '1',
    X_var = "Length",
    extras = 'rnd2-Take out 8_15_18_20_5-'
  )

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()

# the boxcox says we should square it now?...
# lets take a step back and try taking the log of X
clean_dfBC <-
  dfBC[!rownames(dfBC) == 8 &
         !rownames(dfBC) == 18 &
         !rownames(dfBC) == 15 & !rownames(dfBC) == 20 &
         !rownames(dfBC) == 5, ]
fit <- lm(log(area) ~ log(length), data = clean_dfBC)
boxcox(fit)
summary(fit)

plot_name <-
  make_title_of_residual_diagnostics_plot(
    reg = "BC",
    area_trans = '0',
    X_trans = '0',
    X_var = "Length",
    extras = 'rnd2-Take out 8_15_18_20_5-'
  )

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()

# RDs look good. Lets put 5 back in
clean_dfBC <-
  dfBC[!rownames(dfBC) == 8 &
         !rownames(dfBC) == 18 & !rownames(dfBC) == 15 &
         !rownames(dfBC) == 20, ]
fit <- lm(log(area) ~ log(length), data = clean_dfBC)
boxcox(fit)
summary(fit)

plot_name <-
  make_title_of_residual_diagnostics_plot(
    reg = "BC",
    area_trans = '0',
    X_trans = '0',
    X_var = "Length",
    extras = 'rnd2-Take out 8_15_18_20-'
  )

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()

# lets put 20 & 15 back in now
clean_dfBC <- dfBC[!rownames(dfBC) == 8 & !rownames(dfBC) == 18, ]
fit <- lm(log(area) ~ log(length), data = clean_dfBC)
boxcox(fit)
summary(fit)

plot_name <-
  make_title_of_residual_diagnostics_plot(
    reg = "BC",
    area_trans = '0',
    X_trans = '0',
    X_var = "Length",
    extras = 'rnd2-Take out 8_18-'
  )

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()

# lets put 18 back in now- only 8 is missing now
clean_dfBC <- dfBC[!rownames(dfBC) == 8, ]
fit <- lm(log(area) ~ log(length), data = clean_dfBC)
boxcox(fit)
summary(fit)

plot_name <-
  make_title_of_residual_diagnostics_plot(
    reg = "BC",
    area_trans = '0',
    X_trans = '0',
    X_var = "Length",
    extras = 'rnd2-Take out 8-'
  )

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()

# lets run lm on all the data now
fit <- lm(log(area) ~ log(length), data = dfBC)
boxcox(fit)
summary(fit)

plot_name <-
  make_title_of_residual_diagnostics_plot(
    reg = "BC",
    area_trans = '0',
    X_trans = '0',
    X_var = "Length",
    extras = 'rnd2-all data-'
  )

png(plot_name[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = plot_name[2])
dev.off()

# it is the same as before.... we're done!
# Make lets plot the final model
fit <- lm(log(area) ~ log(length), data = dfBC)
pred.int <- predict(fit, interval = "prediction")
data <- cbind(dfBC, pred.int)

p <- ggplot(data, aes(x = width, y = area)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(y = expression ("Area" ~ (km ^ 2))) +
  xlab("Log(Length (km))")  +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed")
p

summary(fit)
summary(dfBC$length)
confint(fit)
