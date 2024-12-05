library(readxl)

# Function to perform iterative merging of categories based on t-test
merge_categories <- function(df, var_name, response_var) {
  
  # Ensure the selected variable is a factor
  df[[var_name]] <- as.factor(df[[var_name]])
  post_categories <- list()
  
  # Loop over each pair of categories
  for (n in 1:500) {
    # Get categories
    categories <- levels(df[[var_name]])
    categories <- categories[sample(length(categories))]
    
    df_temp <- df
    
    i <- 1
    while (i <= length(categories)) {
      j <- i + 1
      while (j <= length(categories)) {
        # Subset data for the two categories
        category_i <- categories[i]
        category_j <- categories[j]
        
        # Perform t-test between the two categories
        t_test <- t.test(
          as.formula(paste(response_var, "~", var_name)), 
          data = df_temp[df_temp[[var_name]] %in% c(category_i, category_j), ]
        )
        
        # If p-value is less than 0.05, merge the two categories
        if (t_test$p.value > 0.05) {
          new_level <- paste(sort(c(category_i, category_j)), collapse = "+")  # Alphabetical order
          
          levels(df_temp[[var_name]]) <- c(levels(df_temp[[var_name]]), new_level)  # Add new level
          
          # Merge the two categories
          df_temp[[var_name]][df_temp[[var_name]] %in% c(category_i, category_j)] <- new_level
          
          # Remove old levels
          df_temp[[var_name]] <- factor(df_temp[[var_name]], levels = setdiff(levels(df_temp[[var_name]]), c(category_i, category_j)))
          
          # Update the category list after merging
          categories <- levels(df_temp[[var_name]])
          
          # Reset indices
          i <- 1
          j <- i + 1
        } else {
          j <- j + 1
        }
      }
      i <- i + 1
    }
    post_categories[[n]] <- categories  # Save the post categories for this iteration
  }
  
  
  # Flatten post_categories into a single vector
  all_categories <- unlist(post_categories)
  
  # Count the occurrences of each category
  category_counts <- table(all_categories)
  category_counts_sorted <- sort(category_counts, decreasing = TRUE)
  categories_old <- levels(df[[var_name]])
  
  categorization = list()
  
  print("Select go")
  print(categories_old)
  
  for( i in 1:length(category_counts_sorted)){
    names = names(category_counts_sorted)[i]
    names_split = strsplit(names(category_counts_sorted)[i], "\\+")[[1]]
    
    category = ""
    added = FALSE
    
    for (name in names_split){
      print(name)
      # Remove specific names
      if (name %in% categories_old){
        
        categories_old <- categories_old[!categories_old %in% c(name)]
        print(categories_old)
        
        category = paste0(category,"+",name)
        added = TRUE
      }
    }
    if (added){
      categorization = c(categorization, category)
    }
  }
  return(categorization)
}




### run 

# load data
data <- read_excel("Data/data_train.xlsx")

#reprocess
data$tipo.casa[data$tipo.casa %in% c("Otros")] = "piso"

data$distrito <- as.factor(data$distrito)
data$estado <- as.factor(data$estado)
data$banos <- as.factor(data$banos)
data$dorm <- as.factor(data$dorm)
data$tipo.casa <- as.factor(data$tipo.casa)

# call function
categorization <- merge_categories(data, var_name = "estado", response_var = "precio.house.m2")

# print results
print(categorization)


# t.test(
#   formula = as.formula(paste("precio.house.m2", "~", "estado")), 
#   data = data[data$estado %in% c("buen_estado", "segunda_mano"), ]
# )





