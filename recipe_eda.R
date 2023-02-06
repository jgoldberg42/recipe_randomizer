library(rvest)
library(stringr)
# Recipe basics
test = read_html('https://www.budgetbytes.com/cider-roasted-turkey-breast/')

recipe_title = test %>% html_nodes(".wprm-block-text-bold") %>% html_text()

servings = test %>% html_nodes(".wprm-recipe-servings") %>% html_text()

cooktime = paste(test %>% html_nodes(".wprm-recipe-prep-time-container") %>% 
                   html_text(),
                 test %>% html_nodes(".wprm-recipe-cook-time-container") %>% 
                   html_text(),
                 test %>% html_nodes(".wprm-recipe-total-time-container") %>% 
                   html_text(),
                 sep = ', ')

# Ingredients
# can't split like this if units not in all ingredients
amount = test %>% html_nodes(".wprm-recipe-ingredient-amount") %>% 
        html_text()
unit = test %>% html_nodes(".wprm-recipe-ingredient-unit") %>% 
        html_text()
name = test %>% html_nodes(".wprm-recipe-ingredient-name") %>% 
        html_text()

# instead, get groups and split strings
groups = test %>% html_nodes(".wprm-recipe-ingredient-group") %>% html_text()

cleaned_groups = str_replace_all(groups, 
                                 '\\(\\s*\\$[:digit:]+.[:digit:]+\\)', 
                                 '')

grouped_ingredients = strsplit(cleaned_groups, '(?<=.)(?=[\\d])', perl=TRUE)

ingredients = data.frame(group = character(), 
                         amount = character(), 
                         unit = character(),
                         name = character())

k = 1
for(i in 1:length(grouped_ingredients)){
  if(i == 1 & length(grouped_ingredients) == 1){
    group = ''
    while(length(grouped_ingredients[[i]]) > 0){
      if(str_count(grouped_ingredients[[i]][1], "\\S+") == 1){
        grouped_ingredients[[i]][1] = paste0(grouped_ingredients[[i]][1],
                                             grouped_ingredients[[i]][2])
        grouped_ingredients[[i]] = grouped_ingredients[[i]][-2]
        
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
  group = tolower(grouped_ingredients[[i]][1])
  grouped_ingredients[[i]] = tolower(str_trim(grouped_ingredients[[i]][-1]))
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
  }
}








'https://www.budgetbytes.com/recipe-catalog/?fwp_by_course=main-dish'

protein_list = c('beef',
                 'chicken',
                 'pork',
                 'sausage',
                 'seafood',
                 'tofu',
                 'turkey')

pages = ceiling(c(51, 131, 65, 42, 25, 9, 19)/12)

protein = 'beef'

url = paste0('https://www.budgetbytes.com/recipe-catalog/page/',
             2,
             '/?fwp_by_course=main-dish&fwp_protein=',
       protein)

links = read_html(url)

links %>% html_nodes(".archive-post-listing a") %>% html_attr('href')




a = test %>% html_nodes(".wprm-recipe-instructions li") %>% html_text()

a = test %>% html_nodes(".wprm-recipe-ingredient") %>% html_text()

b = test %>% html_nodes(".wprm-recipe-ingredient-amount") %>% html_text()
c = test %>% html_nodes(".wprm-recipe-ingredient-name") %>% html_text()

temp = unlist(strsplit(a[1], b[1]))[2]

unlist(strsplit(str_trim(str_replace_all(temp, 
                '\\(\\s*\\$[:digit:]+.[:digit:]+\\)', 
                '')), c[1], fixed=TRUE))

a[1] = str_replace_all(a[1], 'lb. ', '')