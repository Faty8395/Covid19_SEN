## Opérations sur les données temporelles
   # Le débit de la rivière Chaudière : est mesuré depuis 1915
install.packages("tidyverse")
library("tidyverse")
hydro <- read_csv("data/023402_Q.csv")
# ..............................................
data("airquality")
   # La fonction read_csv() détecte automatiquement que 
   # la colonne Date est une date
## le nombre d'observations, le nombre de variables et le 
   # type de chaque variable de la base de données
glimpse(hydro)
# ......................................
glimpse(airquality)

## Explorer graphiquement le débit de la rivière chaudière
library(ggplot2)
hydro %>%
  ggplot(aes(x = Date, y = `Débit`)) +
  geom_line()
# ..........................................
airquality%>%
  ggplot(aes(x = Day, y = Wind)) + 
  geom_line()

## Visualiser la structure du débit en fonction du temps
   # si le débit suit des cycles réguliers
   # isoler les données depuis 2014
hydro %>%
  filter(Date >= as.Date("2014-01-01")) %>%
  ggplot(aes(x = Date, y = `Débit`)) +
  geom_line()
# .........................................
airquality %>%
  filter(Day >= as.integer("14")) %>%
  ggplot(aes(x = Day, y = `Wind`)) +
  geom_line()

## La fonction as.Date et l'argument format dicrivent la 
   # à laquelle la date est expriùée
as.Date(x = "1999/03/29", format = "%Y/%m/%d")
# ............................................
as.integer(x = "20")
format(airquality)

## Le module lubridate rend possible l'extraction de la date 
   # ( date() ), l'année ( year() ), le mois ( month() ), le 
   # jour de la semaine ( wday() ), le jour julien ( yday() ), etc
date_1 <- ymd_hms("2019-03-14 09:14:00")
date_1 %>% date()

date_1 %>% month()

date_1 %>% yday()

date_1 %>% wday()

date_1 %>% seconds()

## Obtenir le débit mensuel moyen de la rivière Chaudière depuis 1990
   # créer une nouvelle colonne Year et une autre Month avec la fonction mutate()
   # effectuer un filtre sur l'année, regrouper par mois pour obtenir le sommaire
   # en terme de
   # moyenne, puis lancer le graphique
hydro_month <- hydro %>%
  mutate(Year = Date %>% year(),
         Month = Date %>% month()) %>%
  filter(Year >= 1990) %>%
  group_by(Month) %>%
  dplyr::summarise(MeanFlow = mean(`Débit`, na.rm = TRUE))
hydro_month %>%
  ggplot(aes(x=Month, y=MeanFlow)) +
  geom_line() +
  scale_x_continuous(breaks = 1:12) +
  expand_limits(y = 0)
# ...................................
airquality_month1 <- airquality %>%
  mutate(Year = Day %>% year(),
         Month1 = Day %>% month1()) %>%
  filter(Year >= 20) %>%
  group_by(Month1) %>%
  dplyr::summarise(MeanFlow = mean(`Wind`, na.rm = TRUE))
airquality_month1 %>%
  ggplot(aes(x=Month1, y=MeanFlow)) +
  geom_line() +
  scale_x_continuous(breaks = 1:12) +
  expand_limits(y = 0)
