# Exploration of linear models to predict catchment area of fjords in New 
# Zealand using their length and width

# create a title for a residual diagnostics plot
setNameOfRD <- function(reg, area_trans, X_trans, X_var, extras = '') {
  title <-
    paste(
      reg,
      "-Area vs ",
      X_var,
      " - Transfrom: Area_",
      area_trans,
      " ",
      paste(X_var, X_trans, sep = '_'),
      sep = ''
    )
  path_name <- paste("./Diagnostic_Plots", reg, sep = '/')
  file_name <-
    paste(paste(
      paste(X_var, X_trans, sep = '_'),
      paste("Area", area_trans, sep = '_'),
      sep = '-'
    ), ".png", sep = '')
  
  result <- list()
  
  return (c(paste(
    path_name, paste(extras, file_name, sep = ''), sep = '/'
  ), title))
}

### Setup here for exporting plots###
reg <- "NZ"
area_trans = "0.5"
X_trans = "0.5"
X_var <- "Width"
##
### Code to make file name ###
title <-
  paste(
    reg,
    "-Area vs ",
    X_var,
    " - Transfrom: Area_",
    area_trans,
    " ",
    paste(X_var, X_trans, sep = '_'),
    sep = ''
  )
path_name <- paste("./Diagnostic_Plots", reg, sep = '/')
file_name <-
  paste(paste(
    paste(X_var, X_trans, sep = '_'),
    paste("Area", area_trans, sep = '_'),
    sep = '-'
  ), ".png", sep = '')

d <- read.csv(file = "fjords.csv")

dfNZ <- d[d$region == "NZ", ]

NZ_fit_length <- lm(area ~ length, data = dfNZ)
NZ_fit_width <- lm(area ~ width, data = dfNZ)

summary(NZ_fit_length)
summary(NZ_fit_width)

# NZ area's seem to better predicted with width
opar <- par(mfrow = c(2, 2))
plot(NZ_fit_width)

library(MASS)

boxcox(NZ_fit_width, plotit = T)

# it looks like sqrt is the best option
NZ_fit_width <- lm(sqrt(area) ~ width, data = dfNZ)

png(paste(path_name, file_name, sep = '/'),
    width = 750,
    height = 600)
opar <- par(mfrow = c(2, 2))
plot(NZ_fit_width, main = title)
dev.off()

summary(NZ_fit_width)

### Diags look good, lets see if they're even better with a X transform
NZ_fit_width <- lm(sqrt(area) ~ log(width), data = dfNZ)

png(paste(path_name, file_name, sep = '/'),
    width = 750,
    height = 600)
opar <- par(mfrow = c(2, 2))
plot(NZ_fit_width, main = title)
dev.off()

summary(NZ_fit_width)


### Let's try to log transform as suggested by the researchers
NZ_fit_width <- lm(log(area) ~ log(width), data = dfNZ)

png(paste(path_name, file_name, sep = '/'),
    width = 750,
    height = 600)
opar <- par(mfrow = c(2, 2))
plot(NZ_fit_width, main = title)
dev.off()

summary(NZ_fit_width)

## It looks like sqrt and log are best.
## Let's go back to that and then try without pt 62

dfNZ_new <- dfNZ[!rownames(dfNZ) == '62', ]

NZ_fit_width <- lm(sqrt(area) ~ log(width), data = dfNZ_new)

png(paste(path_name, paste('62-', file_name, sep = ''), sep = '/'),
    width = 750,
    height = 600)
opar <- par(mfrow = c(2, 2))
plot(NZ_fit_width, main = title)
dev.off()

summary(NZ_fit_width)

# Removing the outlier does not improve predictions under the transformation
## Let's try without a transform

dfNZ_new <- dfNZ[!rownames(dfNZ) == '62', ]

NZ_fit_width <- lm(area ~ width, data = dfNZ_new)

png(paste(path_name, paste('62-', file_name, sep = ''), sep = '/'),
    width = 750,
    height = 600)
opar <- par(mfrow = c(2, 2))
plot(NZ_fit_width, main = title)
dev.off()

summary(NZ_fit_width)

## it turns out the removing the outlier gives us a higher R^2
### Let's do BOXCOX with this 'new' dataset

library(MASS)
boxcox(NZ_fit_width)
png("NZ-Boxcox-1-1-62.png", width = 250, height = 225)
boxcox(NZ_fit_width, lambda = seq(-0.8, 1.8, 0.02))
dev.off()

# still suggests sqrt
NZ_fit_width <- lm(sqrt(area) ~ width, data = dfNZ_new)
summary(NZ_fit_width)

png(paste(path_name, paste('62-', file_name, sep = ''), sep = '/'),
    width = 750,
    height = 600)
opar <- par(mfrow = c(2, 2))
plot(NZ_fit_width, main = title)
dev.off()

# R^2 isn't better than no transformed
## residual diagnostics aren't better either

boxcox(NZ_fit_width)
# lets try transforming X
NZ_fit_width <- lm(sqrt(area) ~ sqrt(width), data = dfNZ_new)
summary(NZ_fit_width)
boxcox(NZ_fit_width)

png(paste(path_name, paste('62-', file_name, sep = ''), sep = '/'),
    width = 750,
    height = 600)
opar <- par(mfrow = c(2, 2))
plot(NZ_fit_width, main = title)
dev.off()

# where is pt 62
par(mfrow = c(1, 1))
plot(area ~ width, data = dfNZ)
identify(x = dfNZ$width,
         y = dfNZ$area,
         label = row.names(dfNZ))

# I forgot to try just a log transform on the response
fit <- lm(log(area) ~ width, data = dfNZ)
fn <-
  setNameOfRD(
    reg = "NZ",
    area_trans = '0',
    X_trans = '1',
    X_var = "Width",
    extras = 'rnd2-'
  )

png(fn[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = fn[2])
dev.off()

summary(fit)
boxcox(fit)

# try just a log transform on the response with 62 removed
fit <- lm(log(area) ~ width, data = dfNZ_new)
fn <-
  setNameOfRD(
    reg = "NZ",
    area_trans = '0',
    X_trans = '1',
    X_var = "Width",
    extras = 'rnd2-sans_62'
  )

png(fn[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = fn[2])
dev.off()

summary(fit)
boxcox(fit)

# try just a log transform on the response with 62 removed
fit <- lm(sqrt(area) ~ log(width), data = dfNZ_new)
fn <-
  setNameOfRD(
    reg = "NZ",
    area_trans = '0.5',
    X_trans = '0',
    X_var = "Width",
    extras = 'rnd2-sans_62'
  )

png(fn[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = fn[2])
dev.off()

summary(fit)
boxcox(fit)

# log trans wasn't so great :)

library(ggplot2)

# Make lets plot the final model
fit <- lm(area ~ width, data = dfNZ_new)
pred.int <- predict(fit, interval = "prediction")
data <- cbind(dfNZ_new, pred.int)

p <- ggplot(data, aes(x = width, y = area)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(y = expression ("Area" ~ (km ^ 2))) +
  xlab("Width (km)")
p

# How many SDs is pt 62 away from the mean
mean(dfNZ$width)
sd(dfNZ$width)
summary(dfNZ$width)
(3.13 - mean(dfNZ$width)) / sd(dfNZ$width)

# try No transform on the response with 62 removed
fit <- lm(area ~ width, data = dfNZ_new)
fn <-
  setNameOfRD(
    reg = "NZ",
    area_trans = '1',
    X_trans = '1',
    X_var = "Width",
    extras = 'rnd2-sans_62'
  )

png(fn[1], width = 750, height = 600)
opar <- par(mfrow = c(2, 2))
plot(fit, main = fn[2])
dev.off()

summary(fit)
boxcox(fit)

confint(fit)
summary(dfNZ_new$width)
