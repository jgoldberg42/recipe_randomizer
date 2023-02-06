

recipe_generator = function(desired_servings){
  library(dplyr)
  selected = recipes$id[sample.int(length(recipes$id), 1)]
  
  running_servings = as.numeric(recipes$servings[recipes$id == selected])
  
  while(running_servings < desired_servings){
    base_ingredients = ingredients %>%
      filter(recipe_id %in% selected,
             unit %in% c('lb.', 'lb', 'lbs.', 'lbs',
                         'cup', 'cups', ''),
             name != 'water') %>%
      select(name)
    
    similar_recipes = ingredients %>% 
      filter(name %in% base_ingredients$name) %>% 
      select(recipe_id) %>% 
      unique()
    
    sub_selected = similar_recipes$recipe_id[sample.int(length(similar_recipes$recipe_id), 1)]
    
    running_servings = running_servings + as.numeric(gsub("([0-9]+).*$", "\\1", 
                                                          recipes$servings[recipes$id == sub_selected]))
    selected = append(selected, sub_selected)
  }
  
  return(c(recipes %>% filter(id %in% selected),
           ingredients %>% filter(recipe_id %in% selected),
           recipe_steps %>% filter(recipe_id %in% selected)))
}

rm(a)
a = recipe_generator(12)
a$title

b = data.frame(amount = a$amount,
               unit = a$unit,
               name = a$name) %>% 
  arrange(name)
rownames(b) = NULL
write.csv(b, 'this_week.csv', row.names = FALSE)
a$link



