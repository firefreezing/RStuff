

# References:
# Lionel Henry & Hadley Wickham's online book on Tidy evaluation: https://tidyeval.tidyverse.org/
# nice blog: https://edwinth.github.io/blog/dplyr-recipes/
# Tidy eval cheat sheet: https://psych252.github.io/psych252book/figures/cheatsheets/tidyeval.pdf

library(tidyverse)


data(mtcars)
glimpse(mtcars)

mtcars %>%
  group_by(cyl) %>%
  summarise_at(vars(mpg, wt, vs:carb), mean)

# vars is the quoting function - it returns its arguments as "blueprints" to be interpreted later on by appropriate verbs
vars(mpg, wt, vs:carb)
