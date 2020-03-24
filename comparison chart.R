

COVID_confirmed_viz <- COVID_confirmed %>%
  dplyr::filter(country %in% c("Iran", "US", "Italy", "Spain")) %>%
  group_by(country) %>%
  arrange(country, date) %>%
  dplyr::filter(date >= "0020-02-15")
#
COVID_deaths_viz <- COVID_deaths %>%
  dplyr::filter(country %in% c("Iran", "US", "Italy", "Spain")) %>%
  group_by(country) %>%
  arrange(country, date) %>%
  dplyr::filter(date >= "0020-02-15")
#
COVID_recovered_viz <- COVID_recovered %>%
  dplyr::filter(country %in% c("Iran", "US", "Italy", "Spain")) %>%
  group_by(country) %>%
  arrange(country, date) %>%
  dplyr::filter(date >= "0020-02-20")
#
COVID_confirmed_viz1 <- COVID_confirmed %>%
  dplyr::filter(country == "Korea, South") %>%
  group_by(country) %>%
  arrange(country, date) %>%
  dplyr::filter(date >= "0020-02-20")
#
COVID_deaths_viz1 <- COVID_deaths %>%
  dplyr::filter(country == "Korea, South") %>%
  group_by(country) %>%
  arrange(country, date) %>%
  dplyr::filter(date >= "0020-02-20")
#
COVID_recovered_viz1 <- COVID_recovered %>%
  dplyr::filter(country == "Korea, South") %>%
  group_by(country) %>%
  arrange(country, date) %>%
  dplyr::filter(date >= "0020-02-20")
#
p1 <- ggplot()+
  geom_line(data = COVID_confirmed_viz, aes(date, n_cases, group = country, color = country), size = 1)+
  geom_line(data = COVID_confirmed_viz1, aes(date, n_cases, group = country, color = country), size = 3, linetype = "dashed")+
  ylab("n")+
  theme_bw()+
  ggtitle("Confirmed cases")
#
p2 <- ggplot()+
  geom_line(data = COVID_deaths_viz, aes(date, n_cases, group = country, color = country), size = 1)+
  geom_line(data = COVID_deaths_viz1, aes(date, n_cases, group = country, color = country), size = 3, linetype = "dashed")+
  ylab("n")+
  theme_bw()+
  ggtitle("Deaths")
#
p3 <- ggplot()+
  geom_line(data = COVID_recovered_viz, aes(date, n_cases, group = country, color = country), size = 1)+
  geom_line(data = COVID_recovered_viz1, aes(date, n_cases, group = country, color = country), size = 3, linetype = "dashed")+
  ylab("n")+
  theme_bw()+
  ggtitle("Recovered")
#
#
grid.arrange(p1,p2,p3)

