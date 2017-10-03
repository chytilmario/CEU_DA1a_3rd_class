hotels <- read.csv('http://bit.ly/CEU-R-hotels-2017')
install.packages("data.table")
library(data.table)
hotels <- data.table(hotels)

hotels[, price_EUR := price_HUF / 310]
hotels[, pricecat := cut(price_EUR, 3, dig.lab = 8)]
str(hotels)
hotels[, .N, by = pricecat]

hotels[, pricecat := cut(price_EUR,
                         c(30, 300), 
                         dig.lab = 8)]
hotels[, .N, by = pricecat]

hotels[, pricecat := cut(price_EUR,
                         c(0, 30, 300, Inf), 
                         dig.lab = 8)]
hotels[, .N, by = pricecat][order(pricecat)]

hist(hotels$price_EUR)

avg_price <- mean(hotels$price_EUR)
avg_price <- hotels[, mean(price_EUR)]
sd_price <- hotels[, sd(price_EUR)]

hotels[, pricecat := cut(price_EUR,
                         c(0,
                           avg_price - sd_price,
                           avg_price + sd_price,
                           Inf),
                         dig.lab = 8)]
hotels[, .N, by = pricecat][order(pricecat)]

hotels[, mean(price_EUR), by = city]

hotels[, list(avg_price = mean(price_EUR)), by = city]

hotels[, price_avg := mean(price_EUR), by = city]
hotels

hotels[, price_sd := sd(price_EUR), by = city]
hotels

hotels[, pricecat := cut(price_EUR,
                         c(0,
                           price_avg[1] - price_sd[1],
                           price_avg[1] + price_sd[1],
                           Inf),
                         labels = c('below avg', 'avg', 'above avg'),
                         dig.lab = 8),
       by = city]
hotels[, .N, by = pricecat][order(pricecat)]

hotels[, size_category := cut(hotels[, .N, by = city],
                         c(30, 300), 
                         dig.lab = 8)]
hotels[, .N, by = pricecat]
str(hotels)

hotels[, .N, by = list(pricecat, citytype)]

hotels <- read.csv('http://bit.ly/CEU-R-hotels-2017-v2')
str(hotels)

install.packages("data.table")
library("data.table")
hotels <- data.table(hotels)

install.packages("ggplot2")
library(ggplot2)
ggplot(hotels, aes(pricecat)) + geom_bar()

ggplot(hotels, aes(pricecat)) + geom_bar() + theme_bw()

p <- ggplot(hotels, aes(pricecat)) + geom_bar()
str(p)
p

ggplot(hotels, aes(pricecat)) + 
  geom_bar(color = 'orange', fill = 'yellow')

ggplot(hotels, aes(pricecat)) + geom_bar() +
  coord_flip()

p + xlab('') + ylab('N') + ggtitle("Number of hotels by pricecut")

ggplot(hotels, aes(citytype)) + geom_bar()

ggplot(hotels, aes(rating)) + geom_histogram(binwidth = 0.1)

ggplot(hotels, aes(stars, rating)) + geom_point()
ggplot(hotels, aes(stars, rating)) + geom_point(alpha = 0.2)
ggplot(hotels, aes(stars, rating)) + geom_boxplot()
ggplot(hotels, aes(factor(stars), rating)) + geom_boxplot()

install.packages("hexbin")
library(hexbin)
ggplot(hotels, aes(factor(stars), rating)) + geom_hex()


p <- ggplot(hotels, aes(factor(stars), rating)) + geom_boxplot()
p + facet_wrap(~ citytype)

ggplot(hotels, aes(pricecat)) + geom_bar(fill = "orange")
ggplot(hotels, aes(pricecat, fill = citytype)) + geom_bar()
ggplot(hotels, aes(pricecat)) + geom_bar(fill = citytype)

ggplot(hotels, aes(pricecat, fill = citytype)) + geom_bar(position = "dodge")

ggplot(hotels, aes(price_EUR)) + geom_density()
ggplot(hotels, aes(price_EUR, fill = pricecat)) + geom_density(alpha = 0.25)

install.packages("ggthemes")
library(ggthemes)

p <- ggplot(hotels, aes(rating, price_EUR, color = citytype)) +
  geom_point()
p + theme_wsj() + scale_color_wsj()

ggplot(hotels, aes(price_EUR)) + geom_histogram()
z <- ggplot(hotels, aes(price_EUR)) + geom_histogram()
z + facet_wrap(~ citytype)

z <- ggplot(hotels, aes(price_EUR)) + geom_boxplot()
z + facet_wrap(~ citytype)

ggplot(hotels, aes(price_EUR, dist_center_km)) + geom_point() 
str(hotels)
