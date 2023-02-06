# Create list of all main dish links on Budget Byte

# List all protein options
protein_list = c('beef',
                 'chicken',
                 'pork',
                 'sausage',
                 'seafood',
                 'tofu',
                 'turkey')
# Number of pages for each (total recipes/12 recipes per page)
pages = ceiling(c(51, 131, 65, 42, 25, 9, 19)/12)
# Initialize list of urls
all_recipes = c()
# Loop over all proteins
for(i in 1:7){
  protein = protein_list[i]
  # For this protein, loop over all pages
  for(j in 1:pages[i]){
    # Generate url for specific page
    url = paste0('https://www.budgetbytes.com/recipe-catalog/page/',
                 j,
                 '/?fwp_by_course=main-dish&fwp_protein=',
                 protein)
    # Read html for specified page
    page = read_html(url)
    # Pull all recipe links from page
    recipe_links = page %>% 
                    html_nodes(".archive-post-listing a") %>% 
                    html_attr('href')
    # Append to existing list of links
    all_recipes = append(all_recipes, recipe_links)
  }
}

all_recipes = unique(all_recipes)

# Add vegetarian recipes
for(j in 1:11){
  url = paste0('https://www.budgetbytes.com/recipe-catalog/page/',
               j,
               '/?fwp_by_course=main-dish&fwp_by_diet=vegetarian')
  # Read html for specified page
  page = read_html(url)
  # Pull all recipe links from page
  recipe_links = page %>% 
    html_nodes(".archive-post-listing a") %>% 
    html_attr('href')
  # Append to existing list of links
  all_recipes = append(all_recipes, recipe_links)
}

