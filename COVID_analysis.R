library(tidyverse)
library(forecast)
library(zoo)
library(xts)
library(gridExtra)
library(gghighlight)

# read data

COVID_confirmed <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") 
COVID_deaths <-  read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv") 
COVID_recovered <-  read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") 


# reshape data

# pivot_longer

COVID_confirmed <- COVID_confirmed %>% 
  pivot_longer(cols      = c('1/22/20':'3/18/20'),
               names_to  = "date",
               values_to = "n_cases")

COVID_deaths <- COVID_deaths %>% 
  pivot_longer(cols      = c('1/22/20':'3/18/20'),
               names_to  = "date",
               values_to = "n_cases")

COVID_recovered <- COVID_recovered %>% 
  pivot_longer(cols      = c('1/22/20':'3/18/20'),
               names_to  = "date",
               values_to = "n_cases")

# rename columns

colnames(COVID_confirmed) <-  c('state', 'country', 'lat', 'long','date', 'n_cases')
colnames(COVID_deaths) <-  c('state', 'country', 'lat', 'long','date', 'n_cases')
colnames(COVID_recovered) <-  c('state', 'country', 'lat', 'long','date', 'n_cases')


COVID_confirmed <- COVID_confirmed %>% group_by(country, date) %>% summarise(n_cases=sum(n_cases))
COVID_deaths <- COVID_deaths %>% group_by(country, date) %>% summarise(n_cases=sum(n_cases))
COVID_recovered <- COVID_recovered %>% group_by(country, date) %>% summarise(n_cases=sum(n_cases))

COVID_confirmed$date <- as.Date(COVID_confirmed$date, format = '%m/%d/%Y')
COVID_deaths$date <- as.Date(COVID_confirmed$date, format = '%m/%d/%Y')
COVID_recovered$date <- as.Date(COVID_confirmed$date, format = '%m/%d/%Y')


# Viz
COVID_confirmed_viz <- COVID_confirmed %>% 
  dplyr::filter(country %in% c("Iran", "US", "Italy", "Spain")) %>% 
  group_by(country) %>% 
  arrange(country, date) %>% 
  dplyr::filter(date >= "0020-02-15")

COVID_deaths_viz <- COVID_deaths %>% 
  dplyr::filter(country %in% c("Iran", "US", "Italy", "Spain")) %>% 
  group_by(country) %>% 
  arrange(country, date) %>% 
  dplyr::filter(date >= "0020-02-15")

COVID_recovered_viz <- COVID_recovered %>% 
  dplyr::filter(country %in% c("Iran", "US", "Italy", "Spain")) %>% 
  group_by(country) %>% 
  arrange(country, date) %>% 
  dplyr::filter(date >= "0020-02-20")

COVID_confirmed_viz1 <- COVID_confirmed %>% 
  dplyr::filter(country == "Korea, South") %>% 
  group_by(country) %>% 
  arrange(country, date) %>% 
  dplyr::filter(date >= "0020-02-20")

COVID_deaths_viz1 <- COVID_deaths %>% 
  dplyr::filter(country == "Korea, South") %>% 
  group_by(country) %>% 
  arrange(country, date) %>% 
  dplyr::filter(date >= "0020-02-20")

COVID_recovered_viz1 <- COVID_recovered %>% 
  dplyr::filter(country == "Korea, South") %>% 
  group_by(country) %>% 
  arrange(country, date) %>% 
  dplyr::filter(date >= "0020-02-20")

p1 <- ggplot()+
  geom_line(data = COVID_confirmed_viz, aes(date, n_cases, group = country, color = country), size = 1)+
  geom_line(data = COVID_confirmed_viz1, aes(date, n_cases, group = country, color = country), size = 3, linetype = "dashed")+
  ylab("n")+
  theme_bw()+
  ggtitle("Confirmed cases")

p2 <- ggplot()+
  geom_line(data = COVID_deaths_viz, aes(date, n_cases, group = country, color = country), size = 1)+
  geom_line(data = COVID_deaths_viz1, aes(date, n_cases, group = country, color = country), size = 3, linetype = "dashed")+
  ylab("n")+
  theme_bw()+
  ggtitle("Deaths")

p3 <- ggplot()+
  geom_line(data = COVID_recovered_viz, aes(date, n_cases, group = country, color = country), size = 1)+
  geom_line(data = COVID_recovered_viz1, aes(date, n_cases, group = country, color = country), size = 3, linetype = "dashed")+
  ylab("n")+
  theme_bw()+
  ggtitle("Recovered")


grid.arrange(p1,p2,p3)

# pivot_wider


COVID_confirmed <- COVID_confirmed %>% 
    pivot_wider(id_cols = date,
               names_from  = country,
               values_from = n_cases)

COVID_deaths <- COVID_deaths %>% 
  pivot_wider(id_cols = date,
              names_from  = country,
              values_from = n_cases)

COVID_recovered <- COVID_recovered %>% 
  pivot_wider(id_cols = date,
              names_from  = country,
              values_from = n_cases)





# countries

COVID_confirmed_China <- xts(x = COVID_confirmed$China, 
                             order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))

COVID_confirmed_Italy <- xts(x = COVID_confirmed$Italy, 
                             order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))

COVID_confirmed_Iran <- xts(x = COVID_confirmed$Iran, 
                             order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))

COVID_confirmed_Spain <- xts(x = COVID_confirmed$Spain, 
                             order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))

COVID_confirmed_Germany <- xts(x = COVID_confirmed$Germany, 
                             order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))

COVID_confirmed_US <- xts(x = COVID_confirmed$US, 
                             order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))

COVID_confirmed_France <- xts(x = COVID_confirmed$France, 
                             order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))

COVID_confirmed_SouthKorea <- xts(x = COVID_confirmed$`Korea, South`, 
                             order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))

COVID_confirmed_Switzerland <- xts(x = COVID_confirmed$Switzerland, 
                             order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))

COVID_confirmed_UK <- xts(x = COVID_confirmed$`United Kingdom`, 
                             order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))

COVID_confirmed_Canada <- xts(x = COVID_confirmed$Canada, 
                          order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))



COVID_confirmed_Egypt <- xts(x = COVID_confirmed$Egypt, 
                             order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))






ggplot()+
  # geom_line(data = fortify(COVID_confirmed_China),aes(Index,COVID_confirmed_China), color = 'red')+
  # geom_line(data = fortify(COVID_confirmed_Italy),aes(Index,COVID_confirmed_Italy), color = "blue")+
  # geom_line(data = fortify(COVID_confirmed_Iran),aes(Index,COVID_confirmed_Iran), color ="orange")+
  # geom_line(data = fortify(COVID_confirmed_Spain),aes(Index,COVID_confirmed_Spain), color = "cyan")+
  # geom_line(data = fortify(COVID_confirmed_Germany),aes(Index,COVID_confirmed_Germany), color = "green")+
  geom_line(data = fortify(COVID_confirmed_US),aes(Index,COVID_confirmed_US), color = "black")+
  geom_line(data = fortify(COVID_confirmed_France),aes(Index,COVID_confirmed_France), color = "pink")+
  geom_line(data = fortify(COVID_confirmed_SouthKorea),aes(Index,COVID_confirmed_SouthKorea))+
  geom_line(data = fortify(COVID_confirmed_Switzerland),aes(Index,COVID_confirmed_Switzerland))+
  geom_line(data = fortify(COVID_confirmed_UK),aes(Index,COVID_confirmed_UK))+
  geom_line(data = fortify(COVID_confirmed_Canada),aes(Index,COVID_confirmed_Canada))+
  geom_line(data = fortify(COVID_confirmed_Egypt),aes(Index,COVID_confirmed_Egypt))

# Create dates
dates <- as.Date("2016-01-01") + 0:4

# Create ts_a
ts_a <- xts(x = 1:5, order.by = dates)

# Create ts_b
ts_b <- xts(x = 1:5, order.by = as.POSIXct(dates))

# Extract the rows of ts_a using the index of ts_b
ts_a[index(ts_a)]

# Extract the rows of ts_b using the index of ts_a
ts_a[index(ts_b)]




