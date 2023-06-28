price <- c(190000, 210000, 143500, 90000, 90000)
ward <- c(7, 10, 3, 1, 1)
type <- c('A', 'H', 'H', 'A', 'H')

house_data <- data.frame(price, ward, type)

gap_data <- read.csv('http://go.gwu.edu/gapminder')

library(dplyr)

gap_data2 <- gap_data %>% select(country, gdpPercap, year, pop) %>%
  filter(year == 2007) %>% mutate(gdpPercapEuro = gdpPercap * 0.88) %>%
  mutate(bigCountry = (pop > 200000000)) %>% arrange(-gdpPercap)

gap_data3 <- gap_data %>% select(country, gdpPercap, year, pop)
gap_data3 <- gap_data3 %>% filter(year == 2007)
gap_data3 <- gap_data3 %>% mutate(gdpPercapEuro = gdpPercap * 0.88)

write.csv(gap_data,'C:/Users/jimmy/OneDrive/Documents/R/STATS/gap_data.csv')

install.packages('readxl')
library(readxl)
