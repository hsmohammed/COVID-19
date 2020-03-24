library(tidyverse)
library(forecast)
library(zoo)
library(xts)
library(gridExtra)
library(gghighlight)

# read data

COVID_confirmed_raw <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") 
COVID_deaths_raw <-  read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv") 
COVID_recovered_raw <-  read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") 


# reshape data

# pivot_longer

COVID_confirmed_states <- COVID_confirmed_raw %>% 
  pivot_longer(cols      = c('1/22/20':names(COVID_confirmed_raw)[ncol(COVID_confirmed_raw)]),
               names_to  = "date",
               values_to = "n_cases")

COVID_deaths_states <- COVID_deaths_raw %>% 
  pivot_longer(cols      = c('1/22/20':names(COVID_deaths_raw)[ncol(COVID_deaths_raw)]),
               names_to  = "date",
               values_to = "n_cases")

COVID_recovered_states <- COVID_recovered_raw %>% 
  pivot_longer(cols      = c('1/22/20':names(COVID_recovered_raw)[ncol(COVID_recovered_raw)]),
               names_to  = "date",
               values_to = "n_cases")

# rename columns

colnames(COVID_confirmed_states) <-  c('state', 'country', 'lat', 'long','date', 'n_cases')
colnames(COVID_deaths_states) <-  c('state', 'country', 'lat', 'long','date', 'n_cases')
colnames(COVID_recovered_states) <-  c('state', 'country', 'lat', 'long','date', 'n_cases')


COVID_confirmed <- COVID_confirmed_states %>% group_by(country, date) %>% summarise(n_cases=sum(n_cases))
COVID_deaths <- COVID_deaths_states %>% group_by(country, date) %>% summarise(n_cases=sum(n_cases))
COVID_recovered <- COVID_recovered_states %>% group_by(country, date) %>% summarise(n_cases=sum(n_cases))

COVID_confirmed$date <- as.Date(COVID_confirmed$date, format = '%m/%d/%Y')
COVID_deaths$date <- as.Date(COVID_deaths$date, format = '%m/%d/%Y')
COVID_recovered$date <- as.Date(COVID_recovered$date, format = '%m/%d/%Y')


# n_cases

country_case <- function(country1) {
  
  df1 <- COVID_confirmed %>% group_by(country) %>% dplyr::filter(country==country1) %>%  summarize(n_cases_today = max(n_cases))
  df2 <- COVID_deaths %>% group_by(country)%>% dplyr::filter(country==country1) %>% summarize(n_cases_today = max(n_cases))
  df3 <- COVID_recovered %>% group_by(country)%>% dplyr::filter(country==country1) %>% summarize(n_cases_today = max(n_cases))
  #                                                            
  print(paste0("number of confirmed cases today:  ", df1$n_cases_today))
  # df1$n_cases_today

  print(paste0("number of Deaths today:  ",df2$n_cases_today))
  # df2$n_cases_today

  print(paste0("number of Recovered cases today:  ", df3$n_cases_today))
  # df3$n_cases_today
  
}

country_case("Canada")

world_case <- function() {
   
  df1 <- COVID_confirmed %>% group_by(country) %>% summarize(n_cases_today = max(n_cases)) %>% summarize(n_cases_total = sum(n_cases_today))
  df2 <- COVID_deaths %>% group_by(country) %>% summarize(n_cases_today = max(n_cases)) %>% summarize(n_cases_total = sum(n_cases_today))
  df3 <- COVID_recovered %>% group_by(country) %>% summarize(n_cases_today = max(n_cases)) %>% summarize(n_cases_total = sum(n_cases_today))
  
  print(paste0("number of total confirmed cases in the world:  ", df1$n_cases_total))
  print(paste0("number of total deaths in the world:  ", df2$n_cases_total))
  print(paste0("number of total recovered cases in the world:  ", df3$n_cases_total))
  
}

world_case()

# pivot_wider


COVID_confirmed_wider <- COVID_confirmed %>% 
  pivot_wider(id_cols = date,
              names_from  = country,
              values_from = n_cases)

COVID_deaths_wider <- COVID_deaths %>% 
  pivot_wider(id_cols = date,
              names_from  = country,
              values_from = n_cases)

COVID_recovered_wider <- COVID_recovered %>% 
  pivot_wider(id_cols = date,
              names_from  = country,
              values_from = n_cases)


# pivot_wider


COVID_confirmed_wider <- COVID_confirmed %>% 
  pivot_wider(id_cols = date,
              names_from  = country,
              values_from = n_cases)

COVID_deaths_wider <- COVID_deaths %>% 
  pivot_wider(id_cols = date,
              names_from  = country,
              values_from = n_cases)

COVID_recovered_wider <- COVID_recovered %>% 
  pivot_wider(id_cols = date,
              names_from  = country,
              values_from = n_cases)

COVID_confirmed <- COVID_confirmed %>% dplyr::group_by(country) %>% 
    arrange(country, date) %>% 
    mutate(new_cases = n_cases-lag(n_cases, default = 0)) 
COVID_deaths <- COVID_deaths %>% dplyr::group_by(country) %>% 
  mutate(new_cases = n_cases-lag(n_cases, default = 0)) %>% 
  arrange(country, date)
COVID_recovered <- COVID_recovered %>% dplyr::group_by(country) %>% 
  arrange(country, date) %>% 
  mutate(new_cases = n_cases-lag(n_cases, default = 0))
COVID_confirmed_wider <- COVID_confirmed_wider %>% 
  arrange(date)
COVID_deaths_wider <- COVID_deaths_wider %>% 
  arrange(date)
COVID_recovered_wider <- COVID_recovered_wider %>%
  arrange(date)

date_vec <- as.Date(COVID_confirmed_wider$date,format = '%m/%d/%Y')

plot_country <- function(df, country, type){
  


  p1 <- ggplot()+
    geom_line(data = df,aes(date,df[[country]]))+
     theme_bw()+
     ylab("number of cases")+
    
    scale_x_date(date_breaks = "3 days")+
      scale_y_continuous(breaks = round(seq(min(df[[country]]), max(df[[country]]), length.out = 15),.10))+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle(type)
  
  return(p1)
  
}

table_country <- function(df, country1) {
  
  df1 <- df %>% dplyr::filter(country == country1)
  return(df1)
  
}

plot_new_cases <- function(df, country1, type){
  
  df1 <- df %>% dplyr::filter(country==country1)
  
  p1 <- ggplot()+
    geom_line(data = df1,aes(date,new_cases))+
    theme_bw()+
    ylab("number of cases")+
    
    scale_x_date(date_breaks = "3 days")+
    # scale_y_continuous(breaks = round(seq(min(df[[country]]), max(df[[country]]), length.out = 15),.10))+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle(type)
  
  return(p1)
  
}

world_case()
country_case("Canada")

plot_country(COVID_confirmed_wider, "Canada", "Confirmed")
plot_new_cases(COVID_confirmed_BC, "Canada", "Confirmed")


tail(table_country(COVID_confirmed, "Canada"))



COVID_confirmed_tot <- COVID_confirmed %>%
  dplyr::ungroup() %>% 
  group_by(date) %>% 
  summarize(n_cases_cum = sum(n_cases)) %>% 
  mutate(n_cases_new = n_cases_cum-lag(n_cases_cum, default = 0))
            


COVID_deaths_tot <- COVID_deaths %>%
  dplyr::ungroup() %>% 
  group_by(date) %>% 
  summarize(n_cases_cum = sum(n_cases)) %>% 
  mutate(n_cases_new = n_cases_cum-lag(n_cases_cum, default = 0))



COVID_recovered_tot <- COVID_recovered %>%
  dplyr::ungroup() %>% 
  group_by(date) %>% 
  summarize(n_cases_cum = sum(n_cases)) %>% 
              mutate(n_cases_new = n_cases_cum-lag(n_cases_cum, default = 0))
            



COVID_confirmed_tot %>% ggplot()+geom_line(aes(date, n_cases_cum))
COVID_confirmed_tot %>% ggplot()+geom_line(aes(date, n_cases_new))

COVID_confirmed_tot_zoo <- zoo(COVID_confirmed_tot$n_cases_new, order.by = COVID_confirmed_tot$date)
Acf(COVID_confirmed_tot_zoo)
pacf(COVID_confirmed_tot_zoo)


COVID_confirmed_US_Canada <- COVID_confirmed_states %>% dplyr::filter(country %in% c("US", "Canada"))
COVID_confirmed_US_Canada$date <- as.Date(COVID_confirmed_US_Canada$date, format = '%m/%d/%Y')
COVID_confirmed_US_Canada <- COVID_confirmed_US_Canada %>% dplyr::group_by(state) %>% 
  arrange(state, date) %>% 
  mutate(new_cases = n_cases-lag(n_cases, default = 0))


COVID_confirmed_US_Canada$state <- as.factor(COVID_confirmed_US_Canada$state)
levels(COVID_confirmed_US_Canada$state)

COVID_confirmed_state <- function(state1) {
  COVID_confirmed_state <- COVID_confirmed_US_Canada %>% dplyr::filter(state == state1)
  return(COVID_confirmed_state)
  
}

tail(COVID_confirmed_state("British Columbia"))
