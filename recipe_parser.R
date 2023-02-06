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
  # Parse list of ingredients by ingredient group. Since ingredients are nested
  # by group, start with the groups and later separate each individual ingredient
  groups = recipe_page %>% 
            html_nodes(".wprm-recipe-ingredient-group") %>% 
            html_text()
  # Remove prices from ingredient lists
  cleaned_groups = str_replace_all(groups, 
                                   '\\(\\s*\\$[:digit:]+.[:digit:]+\\)', 
                                   '')
  # Split at each ingredient amount, including the amount in the following
  # chunk
  grouped_ingredients = strsplit(cleaned_groups, 
                                 '(?<=.)(?=[\\d])', 
                                 perl=TRUE)
  # Turn individual ingredients into rows for an ingredients table
  # Initialize ingredients data frame
  ingredients = data.frame(group = character(), 
                           amount = character(), 
                           unit = character(),
                           name = character())
  # Initialize ingredient number
  k = 1
  # Loop over ingredient groups
  for(i in 1:length(grouped_ingredients)){
    # If only one group at start, no sub-groups exist and just ingredient list
    if(i == 1 & length(grouped_ingredients) == 1){
      # No groups so leave null
      group = ''
      # Same steps as below
      while(length(grouped_ingredients[[i]]) > 0){
        if(str_count(grouped_ingredients[[i]][1], "\\S+") == 1 &
           length(grouped_ingredients[[i]]) > 1){
          grouped_ingredients[[i]][1] = paste0(grouped_ingredients[[i]][1],
                                               grouped_ingredients[[i]][2])
          grouped_ingredients[[i]] = grouped_ingredients[[i]][-2]
          
        } else if(str_count(grouped_ingredients[[i]][1], "\\S+") == 1 &
                  length(grouped_ingredients[[i]]) == 1){
          grouped_ingredients[[i]] = grouped_ingredients[[i]][-1]
          
        } else if(str_count(grouped_ingredients[[i]][1], "\\S+") == 2){
          amount = strsplit(grouped_ingredients[[i]][1], ' ')[[1]][1]
          unit = ''
          name = strsplit(grouped_ingredients[[i]][1], ' ')[[1]][2]
          
          ingredients[k,] = c(group, amount, unit, name)
          k = k + 1
          
          grouped_ingredients[[i]] = grouped_ingredients[[i]][-1]
        } else{
          amount = strsplit(grouped_ingredients[[i]][1], ' ')[[1]][1]
          unit = strsplit(grouped_ingredients[[i]][1], ' ')[[1]][2]
          name = paste(strsplit(grouped_ingredients[[i]][1], ' ')[[1]][-c(1,2)], 
                       collapse=' ')
          
          ingredients[k,] = c(group, amount, unit, name)
          k = k + 1
          
          grouped_ingredients[[i]] = grouped_ingredients[[i]][-1]
        }
      }
    } else{
    # Separate ingredient group to be added as the group column in the data frame
    group = tolower(grouped_ingredients[[i]][1])
    # Remove the group name so that only the individual ingredients remain
    # Convert to lower case and remove excess spaces
    grouped_ingredients[[i]] = tolower(str_trim(grouped_ingredients[[i]][-1]))
    # Loop over individual ingredients within group i
    while(length(grouped_ingredients[[i]]) > 0){
      # If the ingredient has just 1 piece, then it's the first part of a fraction.
      # If there is only one item left in the ingredients list, it's likely a special
      # case where a specification of the previous ingredient wasn't included, and
      # remains on its own. In this case, we just drop it
      if(str_count(grouped_ingredients[[i]][1], "\\S+") == 1 &
         length(grouped_ingredients[[i]]) > 1){
        # In this case, concatenate it with the second item in the list, since
        # The first is "1/" and the second contains the denominator along with
        # the unit and/or name
        grouped_ingredients[[i]][1] = paste0(grouped_ingredients[[i]][1],
                                             grouped_ingredients[[i]][2])
        # Drop the second component since it's been appended to the first
        # Then step out and return back to the if-else tree
        grouped_ingredients[[i]] = grouped_ingredients[[i]][-2]
        
      } else if(str_count(grouped_ingredients[[i]][1], "\\S+") == 1 &
                length(grouped_ingredients[[i]]) == 1){
        grouped_ingredients[[i]] = grouped_ingredients[[i]][-1]
        # If there are two pieces there is no unit, just an amount of an ingredient
        # and the ingredient name
      } else if(str_count(grouped_ingredients[[i]][1], "\\S+") == 2){
        amount = strsplit(grouped_ingredients[[i]][1], ' ')[[1]][1]
        # Make unit blank so it conforms with the data frame
        unit = ''
        name = strsplit(grouped_ingredients[[i]][1], ' ')[[1]][2]
        # Add the ingredient to the data frame, and increase the index for the next
        # one
        ingredients[k,] = c(group, amount, unit, name)
        k = k + 1
        # Drop the ingredient from the list, and return to the while-loop if
        # additional ingredients exist
        grouped_ingredients[[i]] = grouped_ingredients[[i]][-1]
      } else{
        # If there are more than 2 components, there is an amount and a unit
        # followed by a multi-part ingredient name
        amount = strsplit(grouped_ingredients[[i]][1], ' ')[[1]][1]
        unit = strsplit(grouped_ingredients[[i]][1], ' ')[[1]][2]
        # Paste all but the first two
        name = paste(strsplit(grouped_ingredients[[i]][1], ' ')[[1]][-c(1,2)], 
                     collapse=' ')
        # Add the ingredient to the data frame, and increase the index for the next
        # one
        ingredients[k,] = c(group, amount, unit, name)
        k = k + 1
        # Drop the ingredient from the list, and return to the while-loop if
        # additional ingredients exist
        grouped_ingredients[[i]] = grouped_ingredients[[i]][-1]
      }
    }
  }}
return(c(c(title, servings, cooktime), ingredients))
}

a = recipe_parser('https://www.budgetbytes.com/classic-homemade-meatloaf/')