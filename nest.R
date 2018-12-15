library(pacman)
p_load(gapminder)
p_load(tidyverse)
p_load(broom)

gapminder <- gapminder %>% mutate(year1950 = year - 1950)

# nested data
by_country <- gapminder %>%
    group_by(continent, country) %>%
    nest()
 
by_country$data[[1]]
 
# fit a model
 
country_mdl <- function(df){
    lm(lifeExp ~ year1950, data = df)
    }

names(by_country) 

models <- by_country %>%
    mutate(mod = map(data, country_mdl))
 
models <- models %>%
    mutate(glance = mod %>% map(broom::glance),
           rsq = glance %>% map_dbl("r.squared"),
           tidy = mod %>% map(broom::tidy),
           augment = mod %>% map(broom::augment))

# use the models data to create plots

models %>%
    ggplot(aes(rsq, reorder(country, rsq))) +
    geom_point(aes(color = continent))

# unnest the data to return long dataframe

unnest(models, data)
unnest(models, rsq, .drop = T)
unnest(models, tidy, .drop = T)
