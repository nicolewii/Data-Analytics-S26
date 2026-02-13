library("caret")
library("class")
library("cluster")
library(GGally)
library(psych)

## read data
abalone <- read.csv("/Users/nicolelee/Documents/GitHub/Data-Analytics-S26/Lab 3/abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

## derive age group based in number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]

## convert class labels to strings
abalone.sub$age.group <- as.character(abalone.sub$age.group)

## convert back to factor
abalone.sub$age.group <- as.factor(abalone.sub$age.group)

## split train/test
train.indexes <- sample(4177,0.7*4177)

train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]

##EXERCISE 1 - KNN MODELS##

## MODEL 1 ~ length and height
features1 <- c("length", "height")
## MODEL 2 ~ whole weight and shell weight
features2 <- c("whole_weight", "shell_weight")

# Training and testing for model 1
train1 <- train[, features1]
test1  <- test[, features1]

# Training and testing for model 2
train2 <- train[, features2]
test2  <- test[, features2]

# Labels
train_labels <- train$age.group
test_labels  <- test$age.group

# Normalize Model 1 features
preproc1 <- preProcess(train1, method=c("center", "scale"))
train1_norm <- predict(preproc1, train1)
test1_norm <- predict(preproc1, test1)

# Model 1 - K = 10, length and height
m1_pred <- knn(train = train1_norm, test = test1_norm, cl = train_labels, k = 10)

# Print table
m1_table <- table(Predicted = m1_pred, Actual = test_labels)
print(m1_table)

# Accuracy calculations
m1_accuracy <- sum(diag(m1_table)) / sum(m1_table)
cat("Model 1 Accuracy (length, height):", round(m1_accuracy * 100, 2), "%\n")

# Normalize Model 2 features
preproc2 <- preProcess(train2, method=c("center", "scale"))
train2_norm <- predict(preproc2, train2)
test2_norm <- predict(preproc2, test2)

# Model 2 - K = 10, whole weight and shell weight
m2_pred <- knn(train = train2_norm, test = test2_norm, cl = train_labels, k = 10)

# Table
m2_table <- table(Predicted = m2_pred, Actual = test_labels)
print(m2_table)

# Accuracy calculations
m2_accuracy <- sum(diag(m2_table)) / sum(m2_table)
cat("Model 2 Accuracy (whole_weight, shell_weight):", round(m2_accuracy * 100, 2), "%\n")

# Comparing 
if (m1_accuracy > m2_accuracy) {
  better_features <- train1_norm
  better_test_features <- test1_norm
  better_feature_names <- features1
  cat("Better Model: Model 1 (length, height)\n")
} else {
  better_features <- train2_norm
  better_test_features <- test2_norm
  better_feature_names <- features2
  cat("Better Model: Model 2 (whole_weight, shell_weight)\n")
}

# Find optimal k value in range of 1-100
k_values <- 1:100
accuracies <- numeric(100)

for (i in 1:100) {
  pred <- knn(train = better_features,
              test = better_test_features,
              cl = train_labels,
              k = k_values[i])
  
  o_table <- table(Predicted = pred, Actual = test_labels)
  accuracies[i] <- sum(diag(o_table)) / sum(o_table)
}

# Find optimal k
optimal_k <- k_values[which.max(accuracies)]
optimal_accuracy <- max(accuracies)

cat("Optimal k:", optimal_k, "\n")
cat("Optimal Accuracy:", round(optimal_accuracy * 100, 2), "%\n")

##EXERCISE 2 - CLUSTERING ##

# Determine optimal data set based on better model
if(m1_accuracy > m2_accuracy){
  cluster_data <- abalone.sub[, features1]
}else{
  cluster_data <- abalone.sub[, features2]
}

# Normalize
cluster_data_norm <- scale(cluster_data)


#USING K MEANS
#clustering testing from k to be 10-20

k_range <- 10:20
kmeans_silhouettes <- numeric(11)

for (i in 1:11) {
  k <- k_range[i]
  kmeans_model <- kmeans(cluster_data_norm, centers=k, nstart=20)
  sil <- silhouette(kmeans_model$cluster, dist(cluster_data_norm))
  kmeans_silhouettes[i] <- mean(sil[, 3])
}

# find optimal k
optimal_k_kmeans <- k_range[which.max(kmeans_silhouettes)]
cat("Optimal k for k-means:", optimal_k_kmeans, "\n")

# plot Kmeans
plot(k_range, kmeans_silhouettes, type="b", pch=19, col="darkgreen",
     xlab="Number of clusters (k)", ylab="Silhouette Score",
     main="k-means: Silhouette Score vs k")
abline(v=optimal_k_kmeans, col="red", lty=2)
grid()

kmeans_final <- kmeans(cluster_data_norm, centers=optimal_k_kmeans, nstart=25)

# silhouetting
sil_kmeans <- silhouette(kmeans_final$cluster, dist(cluster_data_norm))
plot(sil_kmeans, col=1:optimal_k_kmeans, border=NA,
     main=paste("k-means Silhouette Plot (k =", optimal_k_kmeans, ")"))

#USING PAM
pam_silhouettes <- numeric(11)
for (i in 1:11) {
  k <- k_range[i]
  pam_model <- pam(cluster_data_norm, k=k)
  pam_silhouettes[i] <- pam_model$silinfo$avg.width
}

# find optimal k
optimal_k_pam <- k_range[which.max(pam_silhouettes)]
cat("Optimal k for PAM:", optimal_k_pam, "\n")

# plot PAM
plot(k_range, pam_silhouettes, type="b", pch=19, col="darkblue",
     xlab="Number of clusters (k)", ylab="Silhouette Score",
     main="PAM: Silhouette Score vs k")
abline(v=optimal_k_pam, col="red", lty=2)
grid()

# optimal PAM silhouette model
pam_final <- pam(cluster_data_norm, k=optimal_k_pam)
sil_pam <- silhouette(pam_final$clustering, dist(cluster_data_norm))
plot(sil_pam, col=1:optimal_k_pam, border=NA,
     main=paste("PAM Silhouette Plot (k =", optimal_k_pam, ")"))


#COMPARING BOTH 

cat("\nk-means: k =", optimal_k_kmeans, 
    ", Silhouette =", round(max(kmeans_silhouettes), 4), "\n")
cat("PAM: k =", optimal_k_pam, 
    ", Silhouette =", round(max(pam_silhouettes), 4), "\n")

par(mfrow=c(1,2))
plot(k_range, kmeans_silhouettes, type="b", pch=19, col="darkgreen",
     xlab="k", ylab="Silhouette", main="k-means")
plot(k_range, pam_silhouettes, type="b", pch=19, col="darkblue",
     xlab="k", ylab="Silhouette", main="PAM")
par(mfrow=c(1,1))