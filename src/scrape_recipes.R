
# prelims -----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(stringr)


# scrape recipes ----------------------------------------------------------


nyt_2020_20 <- 'https://www.nytimes.com/article/most-popular-recipes-2020.html'
nyt_2020_50 <- 'https://cooking.nytimes.com/68861692-nyt-cooking/29031379-our-50-most-popular-recipes-of-2020'
fnw_2020 <- 'https://www.foodandwine.com/cooking-techniques/best-recipes-2020-food-wine'

# bon appetit monthly lists
ba_sept <- 'https://www.bonappetit.com/gallery/most-popular-recipes-september-2020'

# epicurios
epi_oct <- 'https://www.epicurious.com/recipes-menus/most-popular-recipes-october-2020-gallery'

food52_monthly <-'https://food52.com/blog/25050-most-popular-recipes-2020'
