library(tidyverse)
library(forecast)
library(zoo)
library(xts)

# read data

COVID_confirmed <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") 
COVID_deaths <-  read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv") 
COVID_recovered <-  read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") 


# reshape data

# pivot_longer

COVID_confirmed <- COVID_confirmed %>% 
  pivot_longer(cols      = c('1/22/20':'3/17/20'),
               names_to  = "date",
               values_to = "n_cases")

COVID_deaths <- COVID_deaths %>% 
  pivot_longer(cols      = c('1/22/20':'3/17/20'),
               names_to  = "date",
               values_to = "n_cases")

COVID_recovered <- COVID_recovered %>% 
  pivot_longer(cols      = c('1/22/20':'3/17/20'),
               names_to  = "date",
               values_to = "n_cases")

# rename columns

colnames(COVID_confirmed) <-  c('state', 'country', 'lat', 'long','date', 'n_cases')
colnames(COVID_deaths) <-  c('state', 'country', 'lat', 'long','date', 'n_cases')
colnames(COVID_recovered) <-  c('state', 'country', 'lat', 'long','date', 'n_cases')


COVID_confirmed <- COVID_confirmed %>% group_by(country, date) %>% summarise(n_cases=sum(n_cases))
COVID_deaths <- COVID_deaths %>% group_by(country, date) %>% summarise(n_cases=sum(n_cases))
COVID_recovered <- COVID_recovered %>% group_by(country, date) %>% summarise(n_cases=sum(n_cases))

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




COVID_confirmed_US <- xts(x = COVID_confirmed$US, 
                                    order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))

COVID_confirmed_China <- xts(x = COVID_confirmed$China, 
                        order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))

COVID_confirmed_Canada <- xts(x = COVID_confirmed$Canada, 
                           order.by = as.Date(COVID_confirmed$date,format = '%m/%d/%Y'))

plot(COVID_confirmed_Canada)

Acf(COVID_confirmed_US)

Acf(VID_confirmed_China)



a

ggplot(fortify(COVID_confirmed_ts),aes(Index,COVID_confirmed_USA_ts))+geom_line()

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




