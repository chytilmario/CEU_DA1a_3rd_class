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
