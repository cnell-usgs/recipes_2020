
# prelims -----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(stringr)


# best recipe lists  ----------------------------------------------------------

# new york times
nyt_2020_50 <- 'https://cooking.nytimes.com/68861692-nyt-cooking/29031379-our-50-most-popular-recipes-of-2020'
nyt_2020_loved <- 'https://cooking.nytimes.com/68861692-nyt-cooking/29030943-50-recipes-our-readers-loved-most-in-2020?action=click&module=Global%20Search%20Recipe%20Card&pgType=search&rank=2'

# food and wine
fnw_2020 <- 'https://www.foodandwine.com/cooking-techniques/best-recipes-2020-food-wine'

# bon appetit monthly lists
ba_sept <- 'https://www.bonappetit.com/gallery/most-popular-recipes-september-2020'

# epicurios
epi_oct <- 'https://www.epicurious.com/recipes-menus/most-popular-recipes-october-2020-gallery'

food52_monthly <-'https://food52.com/blog/25050-most-popular-recipes-2020'



# scrape recipes ----------------------------------------------------------

## NYT

# extract recipe names
nyt_recipes <- read_html(nyt_2020_50) %>%
  html_nodes('article.recipe-card h3.name') %>%
  html_attr('data-name')

# extract recipe urls
nyt_urls <- read_html(nyt_2020_50) %>%
  html_nodes('article.recipe-card') %>%
  html_attr('data-url')

url_list <- paste0('https://cooking.nytimes.com',nyt_urls)

# pull ingredients and ingredient quantities for each recipe

page <- read_html(url_list[1])

nyt_recipes[[2]]

# yield and time
recipe_yield <- url_list %>%
  map(function(x) read_html(x) %>%
        html_nodes('.recipe-yield-value') %>%
        html_text())
recipe_yield[[]][2] # time
recipe_yield[[2]][1] # yield

# ingredients
recipe_ingredients <- url_list %>%
  map(function(x) read_html(x) %>%
        html_nodes('.recipe-ingredients span.ingredient-name')%>%
        html_text(trim = TRUE))
recipe_ingredients[[2]]

# ingredient quantities
recipe_quantity <- url_list %>%
  map(function(x) read_html(x) %>%
        html_nodes('.recipe-ingredients span.quantity')%>%
        html_text(trim = TRUE))
recipe_quantity[[2]]

recipe_nyt <- data.frame(name = unlist(nyt_recipes),
                         url = unlist(nyt_urls))%>%
  mutate(yield = map(url, function(x) read_html(paste0('https://cooking.nytimes.com',x)) %>%
                       html_nodes('.recipe-yield-value') %>%
                       html_text()))

recipe_nyt <- tibble(name = unlist(nyt_recipes),
                     url = unlist(nyt_urls),
                     yield = unlist(recipe_yield)[1:50],
                     time = unlist(recipe_yield)[51:100],
                     ingredients = recipe_ingredients,
                      quantity = recipe_quantity)
saveRDS(recipe_nyt, 'data/recipe_nyt.rds')

## food and wine
fnw <- fnw_2020 %>%
  read_html() 

# names
fnw_names <- fnw %>%
  html_nodes('.articleContainer__content h3') %>%
  html_text(trim = TRUE)

# urls
fnw_urls <- fnw %>%
  html_nodes('.paragraph em a') %>%
  html_attr("href")

page <- read_html(fnw_urls[4])

# time and yield
fnw_yield_time <- fnw_urls %>%
  map(function(x) read_html(x) %>%
    html_nodes('.recipe-meta-item-body') %>%
    html_text(trim=TRUE))
fnw_yield_time%>%unlist

# ingredients
fnw_ingredients <- fnw_urls %>%
  map(function(x) read_html(x) %>%
  html_nodes('.ingredients-item-name') %>%
  html_text(trim=TRUE))

recipes_fnw <- tibble(name = unlist(fnw_names),
                      url = unlist(fnw_urls),
                      yield_time = fnw_yield_time,
                      quantity_ingredients = fnw_ingredients)
str(recipes_fnw)

# bon appetit
months <- c('january','february','march','april','may','june','july','august','september','october','november')

ba_urls <- sprintf('https://www.bonappetit.com/gallery/most-popular-recipes-%s-2020', months)

#names
ba_names <- lapply(ba_urls, function(x) 
  read_html(x) %>%
    html_nodes('.gallery-slide-caption__hed-text')%>%
    html_text(trim=TRUE))
unlist(ba_names)

ba_urls <- lapply(ba_urls, function(x)
  read_html(x) %>%
    html_nodes('.gallery-slide-caption .external-link')%>%
    html_attr('href'))

# pull list of all recipes to search
ba_list <- unlist(ba_urls)

ba_ingredients <- ba_list %>%
  map(function(x) read_html(x) %>%
    html_nodes(".recipe__ingredient-list .dlbFmD") %>%
    html_text())
unlist(ba_ingredients)

# quantity
ba_quantity <- ba_list %>%
  map(function(x) read_html(x) %>%
        html_nodes(".recipe__ingredient-list .cGNXar") %>%
        html_text())

'https://www.epicurious.com/recipes-menus/most-popular-recipes-of-september-2020-gallery'
# epicurious
epi_urls <- sprintf('https://www.epicurious.com/recipes-menus/most-popular-recipes-of-%s-2020-gallery', months)
# epicurious
epi_url <- sprintf('https://www.epicurious.com/recipes-menus/most-popular-recipes-%s-2020-gallery', months)

#nov
epi_name_nov <- epi_urls[11] %>%
  read_html() %>%
  html_nodes(".gallery-slide-caption__hed-text") %>%
  html_text()

epi_url_nov <- lapply(epi_urls[11], function(x)
  read_html(x) %>%
    html_nodes('.gallery-slide-caption .external-link')%>%
    html_attr('href'))

#oct
epi_name_oct <- epi_url[10] %>%
  read_html() %>%
  html_nodes(".gallery-slide-caption__hed-text") %>%
  html_text()
epi_name_oct

epi_url_oct <- lapply(epi_url[10], function(x)
  read_html(x) %>%
    html_nodes('.gallery-slide-caption .external-link')%>%
    html_attr('href'))
epi_url_oct


# sept
epi_name_sept <- epi_urls[9] %>%
  read_html() %>%
  html_nodes(".gallery-slide-caption__hed-text") %>%
  html_text()
epi_name_oct

epi_url_sept <- lapply(epi_urls[9], function(x)
  read_html(x) %>%
    html_nodes('.gallery-slide-caption .external-link')%>%
    html_attr('href'))
epi_url_sept

# august
epi_name_aug <- read_html('https://www.epicurious.com/recipes-menus/most-popular-august-recipes-gallery')%>%
  html_nodes(".gallery-slide-caption__hed-text") %>%
  html_text()
epi_name_aug

epi_url_aug <- read_html('https://www.epicurious.com/recipes-menus/most-popular-august-recipes-gallery')%>%
  html_nodes('.gallery-slide-caption .external-link')%>%
  html_attr('href')

# scrape monthly lists
epi_lists<-c(epi_url_aug, epi_url_sept, unlist(epi_url_oct), unlist(epi_url_nov))
epi_lists

epi_ingredients <- epi_lists %>%
  map(function(x) read_html(x)%>%
  html_nodes("li.ingredient")%>%
  html_text())
epi_ingredients

epi_names<-c(epi_name_aug, epi_name_sept, epi_name_oct, epi_name_nov)

## eat this

eatthis_url <- 'https://www.eatthis.com/best-recipes-2020/'

eat_names <- read_html(eatthis_url) %>%
  html_nodes(".header-mod h2.title") %>%
  html_text()
eat_names

eat_urls<-read_html(eatthis_url) %>%
  html_nodes("strong a") %>%
  html_attr('href')

eat_urls[1] %>%
  read_html() %>%
  html_nodes(".main-content .content")
  
# spoonacular API ---------------------------------------------------------

key <- 'fe28fe96998e4eb09be43a62443e5839'


# clean ingredients -------------------------------------------------------

library(quanteda)
require(readtext)
quanteda_options("threads" = 10)
library(tidytext)
library(spacyr)
library(tokenizers)

recipe_nyt <- readRDS('data/recipe_nyt.rds')

unlist(ba_ingredients)

# test ingredient list
food_list <- data.frame(ingredients = recipe_nyt$ingredients[[1]])
rbind(food_list, unlist(ba_ingredients))
unlist(recipe_nyt$ingredients)

# convert to one token per row

# remove stop words
data(stop_words)

food_list %>%
  unnest_tokens(word, ingredients) %>%
  anti_join(stop_words)

food_data <- rbind(recipe_nyt %>% select(name, ingredients) %>% mutate(source = 'nyt'),
      recipes_fnw %>% select(name, ingredients=quantity_ingredients)%>% mutate(source = 'fnw'),
      tibble(name = unlist(ba_names), ingredients = c(ba_ingredients), source = 'ba'),
      tibble(name = unlist(epi_names), ingredients = c(epi_ingredients), source = 'epi'))
saveRDS(food_data, 'data/recipe_ingredients.rds')

## most common words across all recipes

length(food_data$ingredients) #234 recipes
length(unique(unlist(food_data$ingredients))) # 2006 ingredients

length(unique(ing_list$ingredients)) # 2006
saveRDS(ing_list, 'data/ingredient_list_raw.rds')

ing_stop <- ing_list %>%
  unnest_tokens(word, ingredients) %>%
  anti_join(stop_words) 
saveRDS(ing_stop, 'data/ingredient_list_raw.rds')

# rank of all words
ing_stop %>%
  count(word, sort=TRUE)

list.files('data')
foods<-read.csv('data/top-1k-ingredients.csv', sep=';', col.names=c('word','code'))
str(foods)

str(ing_stop)
common_ingredients <-ing_stop %>%
  semi_join(foods)%>%
  count(word, sort=TRUE)
str(stop_words)


common_ingredients %>%
  ggplot() +
  