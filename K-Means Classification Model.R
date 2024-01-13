
attach(Olympic)

######################
## Data Exploration ##
######################
install.packages("ggrepel")

library(tidyverse)
library(caret)
library(randomForest)
library(MASS)
library(plotly)
library(ggrepel)
library(dplyr)
library(tidyr)

## Data pre-processing
# Impute missing values with median
Olympic$Height = ifelse(is.na(Olympic$Height), median(Olympic$Height, na.rm = TRUE), Olympic$Height)
Olympic$Weight = ifelse(is.na(Olympic$Weight), median(Olympic$Weight, na.rm = TRUE), Olympic$Weight)
Olympic$Age = ifelse(is.na(Olympic$Age), median(Olympic$Age, na.rm = TRUE), Olympic$Age)

# Some feature engineering
# 1. Calculate BMI and categories
Olympic$BMI = Olympic$Weight / ((Olympic$Height / 100) ^ 2)
Olympic$BMI_Category = cut(Olympic$BMI, breaks = c(0, 18.5, 25, 30, Inf), labels = c("Underweight", "Normal Weight", "Overweight", "Obese"))
# 2. Create age groups
Olympic$Age_Group = cut(Olympic$Age, breaks = c(0, 18, 25, 35, 50, Inf), labels = c("0-18", "19-25", "26-35", "36-50", "50+"))

# Encoding Categorical Variable
# 1. Convert 'Sex' to a binary numeric variable
Olympic$Sex = as.factor(Olympic$Sex)
Olympic$Sex = as.numeric(Olympic$Sex) - 1  # 0 for Female, 1 for Male
# 2. Covert 'Medal' to whether won a medal or not
Olympic$Medal = ifelse(!is.na(Olympic$Medal) & Olympic$Medal != "NA", 1, 0) # 0 for NA, 1 for any medal


## EDA
# Summary Statistics for Numeric Variables
summary(Olympic[, c("Age", "Height", "Weight", "BMI")])

# Distributions
# 1. Age
ggplot(Olympic, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")
# 2. Weight
ggplot(Olympic, aes(x = Weight)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Weight", x = "Weight", y = "Frequency")
# 3. Height
ggplot(Olympic, aes(x = Height)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Height", x = "Height", y = "Frequency")
# 4. BMI
ggplot(Olympic, aes(x = BMI)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of BMI", x = "BMI", y = "Frequency")

# Correlation matrix
cor(Olympic[, c("Age", "Height", "Weight", "BMI")], use = "complete.obs")
cor(Olympic[, c("Age", "Height", "BMI")], use = "complete.obs")

# Boxplots
# 1. Age by Sport
ggplot(Olympic, aes(x = Sport, y = Age)) +
  geom_boxplot(fill = "blue", color = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Box plot of Age by Sport", x = "Sport", y = "Age")
# 2. Height by Sport
ggplot(Olympic, aes(x = Sport, y = Height)) +
  geom_boxplot(fill = "blue", color = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Box plot of Height by Sport", x = "Sport", y = "Height")
# 3. Weight by Sport
ggplot(Olympic, aes(x = Sport, y = Weight)) +
  geom_boxplot(fill = "blue", color = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Box plot of Weight by Sport", x = "Sport", y = "Weight")
# 4. BMI by Sport
ggplot(Olympic, aes(x = Sport, y = BMI)) +
  geom_boxplot(fill = "blue", color = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Box plot of BMI by Sport", x = "Sport", y = "BMI")



######################
## Model Selections ##
######################

## K-means Clustering (Weight)
# Select relevant physical attributes for clustering
cluster_data <- Olympic[, c("Age", "Height", "Weight")]

# Handle missing values
cluster_data <- na.omit(cluster_data)

# Standardize the data (important for k-means)
scaled_data <- scale(cluster_data)

# Determine the optimal number of clusters (k) using the elbow method
wss <- numeric(20)
for (i in 1:20) {
  wss[i] <- sum(kmeans(scaled_data, centers = i)$withinss)
}

# Plot the elbow method to choose k
plot(1:20, wss, type = "b", xlab = "Number of Clusters (k)", ylab = "Within Sum of Squares")

# Based on the plot, choose an appropriate value for k (the "elbow" point) and run k-means clustering
k <- 4
kmeans_result <- kmeans(scaled_data, centers = k)

# Add the cluster assignment to the original dataset
Olympic$Cluster <- as.factor(kmeans_result$cluster)

# Analyze the distribution of sports within each cluster
cluster_sport_distribution <- Olympic %>%
  group_by(Cluster, Sport) %>%
  summarise(Athlete_Count = n())

# Display characteristics of attributes in one table for each cluster
result_tables <- list()
for (i in 1:k) {
  cluster_sport_attributes <- Olympic %>%
    filter(Cluster == i) %>%
    group_by(Sport) %>%
    summarise(
      Mean_Age = mean(Age, na.rm = TRUE),
      Mean_Height = mean(Height, na.rm = TRUE),
      Mean_Weight = mean(Weight, na.rm = TRUE),
      Athlete_Count = n()
    ) %>%
    arrange(desc(Athlete_Count))  # Arrange by descending Athlete_Count
  result_tables[[i]] <- as.data.frame(cluster_sport_attributes)
}
for (i in 1:k) {
  cat("Cluster", i, ":\n")
  print(result_tables[[i]])
  cat("\n")
}

# Plot of clusters
ggplot(Olympic, aes(x = Height, y = Weight, color = Cluster)) +
  geom_point() +
  labs(title = "Clusters of Athletes Based on Physical Attributes",
       x = "Height",
       y = "Weight",
       color = "Cluster") +
  theme_minimal()


## K-means Clustering (BMI)
# Select relevant physical attributes for clustering
cluster_data <- Olympic[, c("Age", "Height", "BMI")]

# Handle missing values
cluster_data <- na.omit(cluster_data)

# Standardize the data (important for k-means)
scaled_data <- scale(cluster_data)

# Determine the optimal number of clusters (k) using the elbow method
wss <- numeric(20)
for (i in 1:20) {
  wss[i] <- sum(kmeans(scaled_data, centers = i)$withinss)
}

# Plot the elbow method to choose k
plot(1:20, wss, type = "b", xlab = "Number of Clusters (k)", ylab = "Within Sum of Squares")

# Based on the plot, choose an appropriate value for k (the "elbow" point) and run k-means clustering
k <- 4
kmeans_result <- kmeans(scaled_data, centers = k)

# Add the cluster assignment to the original dataset
Olympic$Cluster <- as.factor(kmeans_result$cluster)

# Analyze the distribution of sports within each cluster
cluster_sport_distribution <- Olympic %>%
  group_by(Cluster, Sport) %>%
  summarise(Athlete_Count = n())

# Display characteristics of attributes in one table for each cluster
result_tables <- list()
for (i in 1:k) {
  cluster_sport_attributes <- Olympic %>%
    filter(Cluster == i) %>%
    group_by(Sport) %>%
    summarise(
      Mean_Age = mean(Age, na.rm = TRUE),
      Mean_Height = mean(Height, na.rm = TRUE),
      Mean_BMI = mean(BMI, na.rm = TRUE),
      Athlete_Count = n()
    ) %>%
    arrange(desc(Athlete_Count))  # Arrange by descending Athlete_Count
  result_tables[[i]] <- as.data.frame(cluster_sport_attributes)
}
for (i in 1:k) {
  cat("Cluster", i, ":\n")
  print(result_tables[[i]])
  cat("\n")
}

# Plot of clusters
ggplot(Olympic, aes(x = Height, y = BMI, color = Cluster)) +
  geom_point() +
  labs(title = "Clusters of Athletes Based on Physical Attributes",
       x = "Height",
       y = "BMI",
       color = "Cluster") +
  theme_minimal()


## Random Forest (Weight)
# Top counted from Cluster 4 (low Height & Weight): Gymnastics (much higher count), Athletics, Swimming
# Cluster 3 (medium): Athletics, Swimming, Gymnastics
# Cluster 2 (overlapping points): Shooting, Equestrianism, Fencing
# Cluster 1 (high Height & Weight): Athletics, Swimming, Rowing, Basketball, Ice Hockey

# Assumptions on Height & Weight
# 1. Gymnastics (high importance)
sport_data <- Olympic %>% filter(Sport == 'Gymnastics')
rf_model <- randomForest(factor(Medal) ~ Age + Height + Weight, ntree = 500, data = sport_data, importance=TRUE, na.action = na.omit)
importance(rf_model)
# Statistical tests
t.test(Height ~ Medal, data = sport_data)
t.test(Weight ~ Medal, data = sport_data)

# 2. Athletics & Swimming (low importance)
sport_data <- Olympic %>% filter(Sport == 'Athletics')
rf_model <- randomForest(factor(Medal) ~ Age + Height + Weight, ntree = 500, data = sport_data, importance=TRUE, na.action = na.omit)
importance(rf_model)
t.test(Height ~ Medal, data = sport_data)
t.test(Weight ~ Medal, data = sport_data)

sport_data <- Olympic %>% filter(Sport == 'Swimming')
rf_model <- randomForest(factor(Medal) ~ Age + Height + Weight, ntree = 500, data = sport_data, importance=TRUE, na.action = na.omit)
importance(rf_model)
t.test(Height ~ Medal, data = sport_data)
t.test(Weight ~ Medal, data = sport_data)

# 3. Rowing & Basketball & Ice Hockey (high importance)
sport_data <- Olympic %>% filter(Sport == 'Rowing')
rf_model <- randomForest(factor(Medal) ~ Age + Height + Weight, ntree = 500, data = sport_data, importance=TRUE, na.action = na.omit)
importance(rf_model)
t.test(Height ~ Medal, data = sport_data)
t.test(Weight ~ Medal, data = sport_data)

sport_data <- Olympic %>% filter(Sport == 'Basketball')
rf_model <- randomForest(factor(Medal) ~ Age + Height + Weight, ntree = 500, data = sport_data, importance=TRUE, na.action = na.omit)
importance(rf_model)
t.test(Height ~ Medal, data = sport_data)
t.test(Weight ~ Medal, data = sport_data)

sport_data <- Olympic %>% filter(Sport == 'Ice Hockey')
rf_model <- randomForest(factor(Medal) ~ Age + Height + Weight, ntree = 500, data = sport_data, importance=TRUE, na.action = na.omit)
importance(rf_model)
t.test(Height ~ Medal, data = sport_data)
t.test(Weight ~ Medal, data = sport_data)


## Random Forest (BMI)
# Top counted from Cluster 1 (overlapping points): Shooting, Equestrianism, Athletics
# Cluster 2 (low Height & BMI): Gymnastics, Athletics, Swimming
# Cluster 3 (high Height & BMI): Athletics, Rowing, Ice Hockey
# Cluster 4 (medium): Athletics, Swimming, Gymnastics, Cycling

# Assumptions on Height & BMI
# 1. Shooting & Equestrianism (low importance)
sport_data <- Olympic %>% filter(Sport == 'Shooting')
rf_model <- randomForest(factor(Medal) ~ Age + Height + BMI, ntree = 500, data = sport_data, importance=TRUE, na.action = na.omit)
importance(rf_model)
t.test(Height ~ Medal, data = sport_data)
t.test(BMI ~ Medal, data = sport_data)

sport_data <- Olympic %>% filter(Sport == 'Equestrianism')
rf_model <- randomForest(factor(Medal) ~ Age + Height + BMI, ntree = 500, data = sport_data, importance=TRUE, na.action = na.omit)
importance(rf_model)
t.test(Height ~ Medal, data = sport_data)
t.test(BMI ~ Medal, data = sport_data)

# 2. Rowing & Ice Hockey (high importance)
sport_data <- Olympic %>% filter(Sport == 'Rowing')
rf_model <- randomForest(factor(Medal) ~ Age + Height + BMI, ntree = 500, data = sport_data, importance=TRUE, na.action = na.omit)
importance(rf_model)
t.test(Height ~ Medal, data = sport_data)
t.test(BMI ~ Medal, data = sport_data)

sport_data <- Olympic %>% filter(Sport == 'Ice Hockey')
rf_model <- randomForest(factor(Medal) ~ Age + Height + BMI, ntree = 500, data = sport_data, importance=TRUE, na.action = na.omit)
importance(rf_model)
t.test(Height ~ Medal, data = sport_data)
t.test(BMI ~ Medal, data = sport_data)


