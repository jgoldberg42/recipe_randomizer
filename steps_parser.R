
steps_parser = function(url){
  library(rvest)
  library(stringr)
  steps = read_html(url) %>% 
    html_nodes(".wprm-recipe-instructions li") %>% 
    html_text()
  return(data.frame(steps))
}

temp = steps_parser('https://www.budgetbytes.com/classic-homemade-meatloaf/')


recipe_steps = data.frame(recipe_id = character(),
                         step = integer(), 
                         instructions = character())

for(i in 1:length(all_recipes$all_recipes)){
  temp = steps_parser(all_recipes$all_recipes[i])
  recipe_steps = rbind(recipe_steps,
                       data.frame(recipe_id = all_recipes$id[i], 
                                  step = 1:length(temp$steps),
                                  instructions = temp$steps)
                       )
}

colnames(recipe_steps) = c('recipe_id',
                           'step',
                           'instructions')