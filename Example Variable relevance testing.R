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






