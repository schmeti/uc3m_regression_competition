
library(RcmdrMisc)
library(rpart)
library(MASS)
library(ggplot2)

### Categorical predictor
## Prior assumptions
# Normality 
normalityTest(response ~ group, test="shapiro.test", data=data)
# Homocedasticity
bartlett.test(response ~ group, data = df)  # Parametric
leveneTest(response ~ group, data = df)  # From `car` package
## Test
# Binary
t.test(response ~ group, data = df) # Equal variances
wilcox.test(response ~ group, data = df) # Non-parametric
# Various categories 
anova_model <- aov(response ~ group, data = df)
summary(anova_model)                      # Parametric
kruskal.test(response ~ group, data = df) # Non-parametric

### Numerical predictors
cor.test(df$response, df$predictor, method = "pearson") # Normal variables and linearity
cor.test(df$response, df$predictor, method = "spearman") #Non-parametric


############################################
data <- read_excel("Data/data_train.xlsx")
data$log.precio.house.m2 <- log(data$precio.house.m2)
data$precio.house.m2 <- NULL
shapiro.test(data$log.precio.house.m2) # 0.0816 > 0.005 ---> We assume normality

Relevance_test = function(data, response, group){
  if (is.factor(data$group) && length(names(data$group)) == 2){
    # Normality 
    norm <- normalityTest(response ~ group, test="shapiro.test", data=data)$p_val
    # Homocedasticity
    homoced1 <- bartlett.test(response ~ group, data = df)$p_val
    homoced2 <- leveneTest(response ~ group, data = df)$p_val
    
    if (sum(c(norm, homoced1, homoced2) > 0.05)==3){
      print("Under normality and homocedasticity:")
      print(t.test(response ~ group, data = df)) # Equal variances
    } else{
      print("Non-parametric Test:")
      print(wilcox.test(response ~ group, data = df)) # Non-parametric
    }
  } else if (is.factor(data$group)){
    # Normality 
    norm <- normalityTest(response ~ group, test="shapiro.test", data=data)$p_val
    # Homocedasticity
    homoced1 <- bartlett.test(response ~ group, data = df)$p_val
    homoced2 <- leveneTest(response ~ group, data = df)$p_val
    
    if (sum(c(norm, homoced1, homoced2) > 0.05)==3){
      print("Under normality and homocedasticity:")
      anova_model <- aov(response ~ group, data = df)
      print(summary(anova_model))                      # Parametric
    } else{
      print("Non-parametric Test:")
      print(kruskal.test(response ~ group, data = df)) # Non-parametric
  }
  } else{
    print("Assuming normality and linearity:")
    print(cor.test(df$response, df$predictor, method = "pearson")) # Normal variables and linearity
    print("Non-parametric test:")
    print(cor.test(df$response, df$predictor, method = "spearman")) # Non-parametric
  }
  return(NULL)
}

################################################################################

# Function to evaluate assumptions of normality and homoscedasticity
evaluate_assumptions <- function(data, response, group) {
  # Shapiro-Wilk test for normality
  shapiro_p <- shapiro.test(data[[response]])$p.value 
  
  # Anderson-Darling test for normality
  library(nortest)
  ad_p <- ad.test(data[[response]])$p.value 
  
  # Levene's test for homoscedasticity
  library(car)
  levene_p <- leveneTest(data[[response]] ~ data[[group]])$p.value 
  
  return(list(
    shapiro_normality = shapiro_p > 0.05, 
    ad_normality = ad_p > 0.05, 
    homoscedasticity = levene_p > 0.05
  ))
}

# Main function
Relevance_test <- function(data, response, group) {
  if (is.factor(data[[group]])) {
    # Categorical predictor
    if (nlevels(data[[group]]) == 2) { # Binary predictor
      assumptions <- evaluate_assumptions(data, response, group)
      if (assumptions$shapiro_normality & assumptions$homoscedasticity) {
        print("T-test for groups with satisfied assumptions:")
        print(t.test(data[[response]] ~ data[[group]])) # Parametric
      } else {
        print("Mann-Whitney U test for groups without satisfied assumptions:")
        print(wilcox.test(data[[response]] ~ data[[group]])) # Non-parametric
      }
    } else { # Categorical predictor with more than 2 levels
      assumptions <- evaluate_assumptions(data, response, group)
      if (assumptions$shapiro_normality & assumptions$homoscedasticity) {
        print("ANOVA for groups with satisfied assumptions:")
        anova_model <- aov(data[[response]] ~ data[[group]])
        print(summary(anova_model)) # Parametric
      } else {
        print("Kruskal-Wallis test for groups without satisfied assumptions:")
        print(kruskal.test(data[[response]] ~ data[[group]])) # Non-parametric
      }
    }
  } else { # Numerical predictor
    print("Correlation analysis:")
    print("Pearson correlation (normality and linearity):")
    print(cor.test(data[[response]], data[[group]], method = "pearson")) # Parametric
    print("Spearman correlation (non-parametric):")
    print(cor.test(data[[response]], data[[group]], method = "spearman")) # Non-parametric
  }
}

# Example usage
library(readxl)
data <- read_excel("Data/data_train.xlsx")
data$log.precio.house.m2 <- log(data$precio.house.m2)
data$precio.house.m2 <- NULL

Relevance_test(data, "log.precio.house.m2", "grupo_categ")






