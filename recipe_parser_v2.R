# Take in recipe url, and return data components for recipe characteristics,
# ingredients, and steps.
#
# Currently, the function only parses recipes from budgetbytes.com
recipe_parser = function(url){
  library(rvest)
  library(stringr)
  # Read in the html of the recipe
  recipe_page = read_html(url)
  # Select recipe characteristics
  title = recipe_page %>%
    html_nodes(".wprm-block-text-bold") %>% 
    html_text()
  
  servings = recipe_page %>%
    html_nodes(".wprm-recipe-servings") %>% 
    html_text()
  # Keep prep, cook, and total times
  cooktime = paste(recipe_page %>% 
                     html_nodes(".wprm-recipe-prep-time-container") %>% 
                     html_text(),
                   recipe_page %>% 
                     html_nodes(".wprm-recipe-cook-time-container") %>% 
                     html_text(),
                   recipe_page %>% 
                     html_nodes(".wprm-recipe-total-time-container") %>% 
                     html_text(),
                   sep = ', ')
  # Turn individual ingredients into rows for an ingredients table
  # Initialize ingredients data frame
  ingredients = data.frame(amount = character(), 
                           unit = character(),
                           name = character())

  full_ingredients = recipe_page %>%
    html_nodes(".wprm-recipe-ingredient") %>% 
    html_text()
  
  full_amounts = recipe_page %>%
    html_nodes(".wprm-recipe-ingredient-amount") %>% 
    html_text()
  
  full_units = recipe_page %>%
    html_nodes(".wprm-recipe-ingredient-unit") %>% 
    html_text()
  
  full_names = recipe_page %>%
    html_nodes(".wprm-recipe-ingredient-name") %>% 
    html_text()
  
  if(length(full_amounts) == length(full_units) & 
     length(full_units) == length(full_names)){
    ingredients = data.frame(tolower(cbind(full_amounts, 
                                           full_units, 
                                           full_names)))
    colnames(ingredients) = c('amount', 'unit', 'name')
  } else{for(i in 1:max(length(full_amounts), 
                   length(full_units), 
                   length(full_names))){
      # Ingredients minus price and amount
      temp = str_trim(unlist(
        strsplit(str_trim(
          str_replace_all(full_ingredients[i],
                          '\\(\\s*\\$[:digit:]+.[:digit:]+\\)', 
                          '')), 
          full_amounts[i], fixed=TRUE)))[2]
      
      unit = str_trim(unlist(strsplit(temp, 
                                      full_names[i], fixed=TRUE)))
      
      ingredients[i,] = data.frame(tolower(cbind(full_amounts[i], 
                                                unit, 
                                                full_names[i])))
  }
    
    colnames(ingredients) = c('amount', 'unit', 'name')
  }
  
  return(c(c(title, servings, cooktime), ingredients))
}

a = recipe_parser('https://www.budgetbytes.com/classic-homemade-meatloaf/')