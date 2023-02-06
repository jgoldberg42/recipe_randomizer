# Run recipe_parser.r on all urls from url_list.r
library(ids)

all_recipes = data.frame(all_recipes)
all_recipes$id = random_id(length(all_recipes$all_recipes), 6)

recipes = data.frame(title = character(), 
                     id = character(), 
                     servings = character(),
                     cooktime = character(),
                     link = character(),
                     category = character())

ingredients = data.frame(recipe_id = character(),
                         amount = character(), 
                         unit = character(),
                         name = character())

j = 1
for(i in 1:length(all_recipes$all_recipes)){
  temp = recipe_parser(all_recipes$all_recipes[i])
  
  recipes[i,] = c(temp[[1]],
                 id = all_recipes$id[i],
                 temp[[2]],
                 temp[[3]],
                 all_recipes$all_recipes[i],
                 'main')
  
  ingredients = rbind(ingredients, 
                      cbind(all_recipes$id[i],
                      temp$amount, 
                      temp$unit, 
                      temp$name))
}

colnames(ingredients) = c('recipe_id',
                          'amount',
                          'unit',
                          'name')