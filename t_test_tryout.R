library(readxl)


data <- read_excel("Data/data_train.xlsx")

df = data

# Ensure `number_of_rooms` is a factor
df$distrito <- as.factor(df$distrito)
post_categories <- list()


# Loop over each pair of categories
for (n in 1:100) {
  print(n)
  # get categories
  categories <- levels(df$distrito)
  categories <- categories[sample(length(categories))]
  
  print(paste("pre",categories))
  df_temp = df
  
  i = 1
  while (i <= length(categories)) {
    j = i+1
    while (j <= length(categories)) {
      # Subset data for the two categories
      category_i <- categories[i]
      category_j <- categories[j]
    
      # Perform t-test between the two categories
      t_test <- t.test(precio.house.m2 ~ distrito, 
                       data = df_temp[df_temp$distrito %in% c(category_i, category_j), ])
      
      print(paste(category_i,category_j,t_test$p.value))
      
      # If p-value is less than 0.05, merge the two categories
      if (t_test$p.value > 0.05) {
        new_level <- paste0(category_i, "+", category_j)  # Create new merged level name
        new_level <- paste(sort(c(category_i, category_j)), collapse = "+")  # Create new merged level name in alphabetical order
        
        
        levels(df_temp$distrito) <- c(levels(df_temp$distrito), new_level)  # Add the new level
        # Merge the two categories
        df_temp$distrito[df_temp$distrito %in% c(category_i, category_j)] <- new_level
        
        df_temp$distrito <- factor(df_temp$distrito, levels = setdiff(levels(df_temp$distrito), category_i))
        df_temp$distrito <- factor(df_temp$distrito, levels = setdiff(levels(df_temp$distrito), category_j))
        
        # Update the category list after merging
        categories <- levels(df_temp$distrito)
      
        # reset
        i = 1
        j = i+1
  
      }else {
        j = j+1
      }
    }
    i = i+1
  }
  print(paste("post",categories))
  post_categories[[n]] <- categories  # Save the post categories for this iteration
  
}

# Flatten post_categories into a single vector
all_categories <- unlist(post_categories)

# Count the occurrences of each category
category_counts <- table(all_categories)
category_counts_sorted <- sort(category_counts, decreasing = TRUE)
print(category_counts_sorted)

# Plot the histogram
par(mar = c(5, 4, 4, 2) + 15)  # Increase the bottom margin (first value)
barplot(category_counts,
        main = "Histogram of Post Categories",
        xlab = "Categories",
        ylab = "Frequency",
        col = "skyblue",
        las = 2)  # Rotate axis labels for readability




