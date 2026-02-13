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

## separate x (features) & y (class labels)
X_train <- train[,1:7] 
Y_train <- train[,8]
X_test <- test[,1:7]
Y_test <- test[,8]

##EXERCISE 1 - KNN MODELS##
#normalize data (using preProcess)
preproc <- preProcess(X_train, method=c("center", "scale"))
X_train_norm <- predict(preproc, X_train)
X_test_norm <- predict(preproc, X_test)

# Model 1 - K = 10, all features
m1_pred <- knn(train = X_train_norm, test = X_test_norm, cl = Y_train, k = 10)
#print table
m1_table <- table(Predicted = m1_pred, Actual = Y_test)
print(m1_table)
#accuracy calculations
m1_accuracy <- sum(diag(m1_table)) / sum(m1_table)
cat("Model 1 Accuracy (all features):", round(m1_accuracy * 100, 2), "%\n")

# Model 2 - K = 10, subsets
X_train_subset <- X_train_norm[, 4:7]
X_test_subset <- X_test_norm[, 4:7]

m2_pred <- knn(train = X_train_subset, test = X_test_subset, cl = Y_train, k = 10)

# table
m2_table <- table(Predicted = m2_pred, Actual = Y_test)
print(m2_table)

# accuracy calculations
m2_accuracy <- sum(diag(m2_table)) / sum(m2_table)
cat("Model 2 Accuracy:", round(m2_accuracy * 100, 2), "%\n")

#comparing 
if (m1_accuracy > m2_accuracy) {
  better_features <- X_train_norm
  better_test_features <- X_test_norm
  cat("Better Model: Model 1\n")
} else {
  better_features <- X_train_subset
  better_test_features <- X_test_subset
  cat("Better Model: Model 2\n")
}

#find optimal k value in range of 1-100
k_values <- 1:100
accuracies <- numeric(100)

for (i in 1:100) {
  pred <- knn(train = better_features,
              test = better_test_features,
              cl = Y_train,
              k = k_values[i])
  
  o_table <- table(Predicted = pred, Actual = Y_test)
  accuracies[i] <- sum(diag(o_table)) / sum(o_table)
}

# find optimal k
optimal_k <- k_values[which.max(accuracies)]
optimal_accuracy <- max(accuracies)

cat("Optimal k:", optimal_k, "\n")
cat("Optimal Accuracy:", round(optimal_accuracy * 100, 2), "%\n")

##EXERCISE 2 - CLUSTERING ##

# determine optimal data set
if(m1_accuracy > m2_accuracy){
  cluster_data <-abalone.sub[,1:7]
}else{
  cluster_data <-abalone.sub[,4:7]
}
#normalize
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

