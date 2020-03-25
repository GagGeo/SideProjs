---
title: "SARS_COV02"
subtitle: "Stats for the ongoing SARS-CoV-2 situation"
author: "Geoffroy Gagliardi"
date: '`r Sys.Date()`'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

# Preamble

```{r pkgs, warning=FALSE, message=FALSE}
# Packages

## Data Wrangling
library(tidyverse)

## Visualization
library(plotly)
library(viridis)
library(maps)
library(ggalt)
library(gganimate)

## Analysis
library(growthrates)

## Look 
library(reactable)
library(htmltools)
library(knitr)
library(kableExtra)
dslabs::ds_theme_set()

# remotes::install_github("GuangchuangYu/nCov2019")
```

```{r data_import, message=FALSE, warning=FALSE}
confirmed <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
deaths <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
recovered <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
```

```{r total}
confirmed <- confirmed %>% gather(key = date, value = confirmed, 5:ncol(.)) %>% mutate(date = as.Date(date, "%m/%d/%y")) 
death <- deaths  %>% gather(key = date, value = deaths, 5:ncol(.)) %>% mutate(date = as.Date(date, "%m/%d/%y"))
recovered <- recovered  %>% gather(key = date, value = recovered, 5:ncol(.)) %>% mutate(date = as.Date(date, "%m/%d/%y"))

total <- left_join(x = confirmed, y = death, by = c("Province/State", "Country/Region", "Lat", "Long", "date")) %>%
  left_join(x = ., y = recovered, by = c("Province/State", "Country/Region", "Lat", "Long", "date")) 

total <- total %>% group_by(country = `Country/Region`) %>% 
  mutate(days_since_100 = as.numeric(date) - min(as.numeric(date[confirmed>=100])),
         ddc10 = as.numeric(date) - min(as.numeric(date[deaths>=10]))) %>%
  ungroup()
```

# Raw Datas 

## Data Summary

```{r top15}
top15 <- total %>% filter(date == max(as.numeric(date))) %>%
  select(country, confirmed, deaths, recovered) %>%
  group_by(country) %>%
  summarise(cases = sum(confirmed),
            deaths = sum(deaths),
            recovered = sum(recovered)) %>%
  arrange(desc(cases)) %>% head(15) %>%
  mutate(rank = seq(from = 1, to = nrow(.), by = 1),
         deathrate = paste(round((deaths/cases)*100, 2), "%"),
         recoveryrate = paste(round((recovered/cases)*100, 2), "%")) %>%
  select(rank, everything())

kable(top15, caption = "Top 15 higher number of cases") %>%
  kable_styling()  
```

```{r current_tot}
current_tot <- total %>% filter(date == max(as.numeric(date))) %>%
  select(confirmed, deaths, recovered) %>% 
  summarise(Cases = sum(confirmed, na.rm = TRUE),
            Deaths = sum(deaths, na.rm = TRUE),
            Recovered = sum(recovered, na.rm = TRUE)) %>%
  mutate(DeathRate = paste(round((Deaths/Cases)*100, 2), "%"),
         RecoveryRate = paste(round((Recovered/Cases)*100, 2), "%"))
kable(current_tot, caption = "Global Summary") %>% kable_styling()
```


## Visualization

```{r fig.width=20, fig.height=10}
evol <-  total %>% group_by(country, date) %>% 
  summarise(cases = sum(confirmed)) %>% ungroup 

p.allcases <- evol %>%
  ggplot(aes(x = date, y = cases, col = country)) +
  geom_line() +
  geom_text(data = subset(x = evol, subset = date == max(as.numeric(date))), aes(label = country, col = country, x = date + 0.5, y = cases)) +
  theme(legend.position = "none") + ggforce::facet_zoom(xlim = c(18330, max(as.numeric(total$date))), ylim = c(0,35000))

p.allcases 
```


```{r p.d100, fig.width=20, fig.height=20}
p.d100 <- total %>% 
  filter(is.finite(days_since_100), days_since_100 >= 0) %>%
  # filter(country != "China") %>%
  group_by(country, days_since_100) %>%
  summarise(cases = sum(confirmed)) %>% 
  rename(`N days since 100th case` = days_since_100) %>%
  ggplot(aes(x = `N days since 100th case`, y = cases, col = country)) +
  geom_line() + geom_point(size = 3, alpha = 0.5) +
  labs(title = "Evolution since the 100th case",
       x = "Number of day since the 100th case", 
       y = "Number of cases",
       subtitle = "Novel Coronavirus (COVID-19) Cases, provided by JHU CSSE") +
  theme(legend.position = "none", plot.caption = element_text(face = "bold.italic"))

ggplotly(p.d100)
```

```{r p.dc10, fig.width=20, fig.height=20}
p.dc10 <- total %>% 
  filter(is.finite(ddc10), ddc10 >= 0) %>%
  group_by(country, ddc10) %>%
  summarise(deaths = sum(deaths)) %>% 
  rename(`N days since 10th death` = ddc10) %>%
  ggplot(aes(x = `N days since 10th death`, y = deaths, col = country)) +
  geom_line() + geom_point(size = 3, alpha = 0.5) +
  labs(title = "Evolution since the 10th death",
       x = "Number of day since the 10th death", 
       y = "Number of deaths",
       subtitle = "Novel Coronavirus (COVID-19) Cases, provided by JHU CSSE") +
  theme(legend.position = "none", plot.caption = element_text(face = "bold.italic"))

ggplotly(p.dc10)
```


```{r mapplot, fig.width=20, fig.height=10}
map.df <- total %>% mutate(confirmed = ifelse(confirmed == 0, NA, log(confirmed, 10)))
mapplot <- ggplot() +
  geom_map(data = map_data("world"), map = map_data("world"), aes(x = long, y = lat, map_id = region)) +
  geom_point(data = map.df, aes(x = Long, y = Lat, col = country, size = confirmed), alpha = 0.6) +
               transition_states(states = date) +
  theme(panel.border = element_rect(color = "grey20", fill = NA),
        legend.position = "none", panel.grid.major = element_line(size = 0.15,color = "grey40")) +
  labs(x = NULL, y = NULL, title = "Evolution of cases", caption = "Novel Coronavirus (COVID-19) Cases, provided by JHU CSSE \n By: G. Gagliardi (@GagGeo)")

animate(plot = mapplot)
anim_save(filename = "Imgs/mapplot.gif", animation = mapplot)
```

<!-- # Growth Model -->

<!-- ```{r} -->

<!-- ## initial parameters and box constraints -->
<!-- p   <- c(y0 = 0.03, mumax = .1, K = 0.1, h0 = 1) -->

<!-- lower   <- c(y0 = 0.001, mumax = 1e-2, K = 0.005, h0 = 0) -->
<!-- upper   <- c(y0 = 0.1,   mumax = 1,    K = 0.5,   h0 = 10) -->

<!-- ## fit growth models to all data using log transformed residuals -->
<!-- growth <- all_growthmodels( -->
<!--                    confirmed ~ date | country, -->
<!--                    data = total, -->
<!--                    p = p, lower = lower, upper = upper, -->
<!--                    log = "y", ncores = 2) -->

<!-- ``` -->


