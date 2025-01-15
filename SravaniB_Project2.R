library(igraph)
library(sna)
library(psych)
library(factoextra)
library(plotly)
library(ggplot2)
library(corrplot)
library(lattice)
library(pairs)
library(class) 
library(plyr)
library(gmodels)
directory<-"C:\\Users\\Sravani\\OneDrive\\Documents\\2024_SPRING\\CS6444 - BIG DATA\\Projects\\Project_2"
obesity_data_file<- file.path(directory,"ObesityDataSet_raw_and_data_sinthetic.csv")
obesity<-read.table(file = obesity_data_file, stringsAsFactors = FALSE,sep = ',', header = TRUE)
str(obesity)
dim(obesity)
obesity[1:10,]
describe(obesity)
summary(obesity)
plot(obesity)


library(dplyr)

directory <- "C:\\Users\\Sravani\\OneDrive\\Documents\\2024_SPRING\\CS6444 - BIG DATA\\Projects\\Project_2"
obesity_data_file <- file.path(directory, "ObesityDataSet_raw_and_data_sinthetic.csv")
ObesityDataSet <- read.table(file = obesity_data_file, stringsAsFactors = FALSE, sep = ',', header = TRUE)

ObesityDataSet_num <- ObesityDataSet

ObesityDataSet_num <- ObesityDataSet_num %>%
  mutate(
    CAEC = ifelse(CAEC == "no", 0,
                  ifelse(CAEC == "Sometimes", 1,
                         ifelse(CAEC == "Frequently", 2, 3))),
    CALC = ifelse(CALC == "no", 0,
                  ifelse(CALC == "Sometimes", 1,
                         ifelse(CALC == "Frequently", 2, 3))),
    MTRANS = ifelse(MTRANS == "Automobile", 3,
                    ifelse(MTRANS == "Bike", 1,
                           ifelse(MTRANS == "Motorbike", 2, 
                                  ifelse(MTRANS == "Public_Trans", 4,0)))),
    NObeyesdad = ifelse(NObeyesdad == "Insufficient_Weight", 0,
                        ifelse(NObeyesdad == "Normal_Weight", 1,
                               ifelse(NObeyesdad == "Overweight_Level_I", 2,
                                      ifelse(NObeyesdad == "Overweight_Level_II", 3,
                                             ifelse(NObeyesdad == "Obesity_Type_I", 4,
                                                    ifelse(NObeyesdad == "Obesity_Type_II", 5, 6)))))),
    Gender = ifelse(Gender == "Male", 1, 0),
    family_history_with_overweight = ifelse(family_history_with_overweight == "no", 0, 1),
    FAVC = ifelse(FAVC == "no", 0, 1),
    SMOKE = ifelse(SMOKE == "no", 0, 1),
    SCC = ifelse(SCC == "no", 0, 1)
  )


obesity_encoded <- ObesityDataSet_num
obesity[11:30,]
obesity_encoded[11:30,]

describe(obesity_encoded)
summary(obesity_encoded)


# Select only the numeric columns from obesity_encoded
numeric_cols <- sapply(obesity_encoded, is.numeric)
print(numeric_cols)

# Plot pairwise scatterplot of numeric variables
pairs(obesity_encoded[, numeric_cols], 
      main = "Pairwise Scatterplot of Numeric Variables")

plot(obesity_encoded$family_history_with_overweight, obesity_encoded$NObeyesdad  ,
     xlab = "family_history_with_overweight ", ylab = "NObeyesdad ",
     main = "Scatter Plot of family_history_with_overweight  vs NObeyesdad")


#Scatter Plot between height and weight 
library(ggplot2)

ggplot(obesity_encoded, aes(x = Height, y = Weight, color = NObeyesdad)) +
  geom_point() +
  scale_color_viridis_b()+
  labs(x = "Height", y = "Weight", color = "Obesity") +
  ggtitle("Scatter Plot of Height vs Weight with Obesity Color Encoded")

#

# Create a table of counts for each combination of family_history_with_overweight and NObeyesdad
counts <- table(obesity_encoded$family_history_with_overweight, obesity_encoded$NObeyesdad)

# Create a bar plot
barplot(counts, 
        main = "Bar Chart of family_history_with_overweight vs Obesity levels", 
        xlab = "family_history_with_overweight", 
        ylab = "Obesity levels",
        legend.text = TRUE,
        col = rainbow(nrow(counts)))



# Create a bar plot
counts <- table(obesity_encoded$CALC, obesity_encoded$NObeyesdad)
barplot(counts, 
        main = "Bar Chart of Rate of Consumption of alcohol vs Obesity levels", 
        xlab = "Obesity levels", 
        ylab = "CALC",
        legend.text = TRUE,
        col = rainbow(nrow(counts)))
# Create a table of counts for each combination of family_history_with_overweight and NObeyesdad
counts <- table(obesity_encoded$Gender, obesity_encoded$NObeyesdad)

# Create a bar plot
barplot(counts, 
        main = "Bar Chart of Gender vs Obesity levels", 
        xlab = "Gender", 
        ylab = "Obesity levels",
        legend.text = TRUE,
        col = rainbow(nrow(counts)))


# Define colors for Gender (assuming 0 and 1)
gender_colors <- c("pink", "blue")  # Assign pink to 0 and blue to 1

# Create a 3D scatter plot using plotly
plot_ly(data = obesity_encoded, 
        x = ~Age, 
        y = ~Gender, 
        z = ~NObeyesdad,
        color = factor(obesity_encoded$Gender),
        colors = gender_colors,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 5))
layout(scene = list(xaxis = list(title = "Age"),
                    yaxis = list(title = "Gender"),
                    zaxis = list(title = "NObeyesdad"),
                    camera = list(eye = list(x = -1.25, y = -1.25, z = 1.25))))




# Create a 3D scatter plot using plotly
plot_ly(data = obesity_encoded, 
        x = ~Gender, 
        y = ~NObeyesdad, 
        z = ~family_history_with_overweight,
        color = factor(obesity_encoded$Gender),
        colors = gender_colors,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "Gender"),
                      yaxis = list(title = "NObeyesdad"),
                      zaxis = list(title = "family_history_with_overweight"),
                      camera = list(eye = list(x = -1.25, y = -1.25, z = 1.25))))




# Define colors for Gender (assuming 0 and 1)
gender_colors <- c("red", "blue")  # Assign pink to 0 and blue to 1

# Create a 3D scatter plot using plotly
plot_ly(data = obesity_encoded, 
        x = ~Height, 
        y = ~Weight, 
        z = ~family_history_with_overweight,
        color = factor(obesity_encoded$family_history_with_overweight),
        colors = gender_colors,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "Height"),
                      yaxis = list(title = "Weight"),
                      zaxis = list(title = "family_history_with_overweight"),
                      camera = list(eye = list(x = -1.25, y = -1.25, z = 1.25))))


smoke_colors <- c("red", "blue")  # Assign pink to 0 and blue to 1
# Create a 3D scatter plot using plotly
plot_ly(data = obesity_encoded, 
        x = ~Height, 
        y = ~Weight, 
        z = ~SMOKE,
        color = factor(obesity_encoded$SMOKE),
        colors = smoke_colors,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "Height"),
                      yaxis = list(title = "Weight"),
                      zaxis = list(title = "SMOKE"),
                      camera = list(eye = list(x = -1.25, y = -1.25, z = 1.25))))


#Correlation without Normalization
obesity_encoded_corr <- cor(obesity_encoded)
obesity_encoded_corr
corrplot::corrplot(obesity_encoded_corr)


#Correlation with Normalization
obesity_encoded_norm <- scale(obesity_encoded[,-16])
pca_res <- prcomp(obesity_encoded_norm)
cor_w <- cor(obesity_encoded_norm, method="spearman")
corrplot(cor_w, type = "lower", order = "hclust", tl.col = "black", tl.cex = 0.5)


#Eliminating values using principle component Analysis, contribution and correlation
obesity_encoded_norm <- scale(obesity_encoded[,-16])
pca_res <- prcomp(obesity_encoded_norm)
fviz_contrib(pca_res, "var", axes = 1:6, fill = "beige", color = "lightgreen")

#remove weight , NObeyesdad as they are dependent
obesity_encoded_norm <- scale(ObesityDataSet_num[,c(-4,-16,-17)])
pca_res <- prcomp(obesity_encoded_norm)

#the percentage of variances explained by each principal component
fviz_eig(pca_res, addlabels = TRUE, ncp = 15, barfill = "beige", barcolor = "lightblue", linecolor = "lightgreen")

#Eigen Plot 
fviz_eig(pca_res, choice = "eigenvalue", ncp = 15, barfill = "beige", barcolor = "lightblue", linecolor = "lightgreen",  addlabels = TRUE,   main = "Eigenvalues")


#Contribution Plot
fviz_contrib(pca_res, "var", axes = 1:6, fill = "beige", color = "lightgreen")
elim_subset_data <- subset(obesity_encoded, select = c("Height","Gender","CH2O", "CALC", "FCVC", "Age", "TUE"))
elim_subset_data

#for the training-test split: 70-30%, 60-40%, 50-50%. 
subset_data_70_30 <- obesity_encoded
subset_data_70_30.rowsnum = nrow(subset_data_70_30)
subset_data_70_30.sample = 0.7
subset_data_70_30.rows = subset_data_70_30.sample * subset_data_70_30.rowsnum
subset_data_70_30.rows
subset_data_70_30.train.index = sample(subset_data_70_30.rowsnum,subset_data_70_30.rows)
length(subset_data_70_30.train.index)
subset_data_70_30.train = subset_data_70_30[subset_data_70_30.train.index,]
subset_data_70_30.train[1:20,]
subset_data_70_30.test = subset_data_70_30[-subset_data_70_30.train.index,]
subset_data_70_30.test[1:20,]


subset_data_60_40 <- obesity_encoded
subset_data_60_40.rowsnum = nrow(subset_data_60_40)
subset_data_60_40.sample = 0.6
subset_data_60_40.rows = subset_data_60_40.sample * subset_data_60_40.rowsnum
subset_data_60_40.rows
subset_data_60_40.train.index = sample(subset_data_60_40.rowsnum,subset_data_60_40.rows)
length(subset_data_60_40.train.index)
subset_data_60_40.train = subset_data_60_40[subset_data_60_40.train.index,]
subset_data_60_40.train[1:20,]
subset_data_60_40.test = subset_data_60_40[-subset_data_60_40.train.index,]
subset_data_60_40.test[1:20,]


subset_data_50_50 <- obesity_encoded
subset_data_50_50.rowsnum = nrow(subset_data_50_50)
subset_data_50_50.sample = 0.5
subset_data_50_50.rows = subset_data_50_50.sample * subset_data_50_50.rowsnum
subset_data_50_50.rows
subset_data_50_50.train.index = sample(subset_data_50_50.rowsnum,subset_data_50_50.rows)
length(subset_data_50_50.train.index)
subset_data_50_50.train = subset_data_50_50[subset_data_50_50.train.index,]
subset_data_50_50.train[1:20,]
subset_data_50_50.test = subset_data_50_50[-subset_data_50_50.train.index,]
subset_data_50_50.test[1:20,]



#-----------------------------------NORMALIZATION---------------------------------------------------------------
#Min-Max Normalization 
normalize <- function(x) {((x - min(x)) / (max(x) - min(x)))}
normalize
obesity_encoded.norm <- as.data.frame(lapply(obesity_encoded,normalize))
obesity_encoded.norm[1:17]

obesity_encoded_norm_corr <- cor(obesity_encoded.norm)
obesity_encoded_norm_corr
corrplot::corrplot(obesity_encoded_norm_corr)
#Z-score normalization:

# Store the means and standard deviations before normalization
feature_means <- apply(obesity_encoded, 2, mean)
feature_stds <- apply(obesity_encoded, 2, sd)


zscore<- function(x){(x-mean(x))/sd(x)}
obesity_encoded.znorm<- as.data.frame(lapply(obesity_encoded,scale))
obesity_encoded.znorm[1:17]

obesity_encoded_znorm_corr <- cor(obesity_encoded.znorm)
obesity_encoded_znorm_corr
corrplot::corrplot(obesity_encoded_znorm_corr)

# Function to retrieve original values from Z-score normalized values
retrieve_original <- function(znorm_value, mean_value, sd_value) {
  original_value <- (znorm_value * sd_value) + mean_value
  return(original_value)
}
retrieved_original_values <- as.data.frame(mapply(retrieve_original, 
                                                  obesity_encoded.znorm, 
                                                  feature_means, 
                                                  feature_stds))

# Compare the original and retrieved values returns TRUE if the original and retrieved values are the same
all.equal(obesity_encoded, retrieved_original_values)
obesity_encoded
retrieved_original_values

#------------------------------------------------------------------------------------------------------------------
standardized_data <- as.data.frame(scale(obesity_encoded))
plot(standardized_data)

log_transformed_data <- as.data.frame(lapply(obesity_encoded, function(x) log(x+1)))  # Adding 1 to avoid log(0)
plot(log_transformed_data)

#K MEANS FOR WHOLE DATA
obesity_cluster <- obesity_encoded
obesity_cluster.norm <- obesity_encoded.norm
k <- 2  # Assuming you want to create 2 clusters; adjust this based on your specific needs
obesity_cluster.k2 <- kmeans(obesity_cluster, centers = k)
obesity_cluster.k2
factoextra::fviz_cluster(obesity_cluster.k2,obesity_encoded.norm)

library(NbClust)
# Compute the NbClust results for a range of cluster numbers (e.g., 2 to 10)
nbclust_result <- NbClust(data = obesity_cluster, min.nc = 2, max.nc = 10, method = "kmeans")
# Visualize the NbClust results
fviz_nbclust(nbclust_result)


k <- 3  # Assuming you want to create 3 clusters; adjust this based on your specific needs
obesity_cluster.k3 <- kmeans(obesity_cluster, centers = k)
obesity_cluster.k3
factoextra::fviz_cluster(obesity_cluster.k3,obesity_encoded.norm)

k <- 4  # Assuming you want to create 4 clusters; adjust this based on your specific needs
obesity_cluster.k4 <- kmeans(obesity_cluster, centers = k)
obesity_cluster.k4
factoextra::fviz_cluster(obesity_cluster.k4,obesity_cluster.norm)

k <- 5  # Assuming you want to create 5 clusters; adjust this based on your specific needs
obesity_cluster.k5 <- kmeans(obesity_cluster, centers = k)
obesity_cluster.k5
factoextra::fviz_cluster(obesity_cluster.k5,obesity_cluster.norm)

k <- 6  # Assuming you want to create 6 clusters; adjust this based on your specific needs
obesity_cluster.k6 <- kmeans(obesity_cluster, centers = k)
obesity_cluster.k6
factoextra::fviz_cluster(obesity_cluster.k6,obesity_cluster.norm)

k <- 7  # Assuming you want to create 7 clusters; adjust this based on your specific needs
obesity_cluster.k7 <- kmeans(obesity_cluster, centers = k)
obesity_cluster.k7
factoextra::fviz_cluster(obesity_cluster.k7,obesity_cluster.norm)

k <- 8  # Assuming you want to create 8 clusters; adjust this based on your specific needs
obesity_cluster.k8 <- kmeans(obesity_cluster, centers = k)
obesity_cluster.k8
factoextra::fviz_cluster(obesity_cluster.k8,obesity_cluster.norm)

k <- 9  # Assuming you want to create 9 clusters; adjust this based on your specific needs
obesity_cluster.k9 <- kmeans(obesity_cluster, centers = k)
obesity_cluster.k9
factoextra::fviz_cluster(obesity_cluster.k9,obesity_cluster.norm)

k <- 10  # Assuming you want to create 10 clusters; adjust this based on your specific needs
obesity_cluster.k10 <- kmeans(obesity_cluster, centers = k)
obesity_cluster.k10
factoextra::fviz_cluster(obesity_cluster.k10,obesity_cluster.norm)

#K MEANS without features with very little correlation with obesity levels

obesity_essential <- elim_subset_data
obesity_essential.norm <- as.data.frame(lapply(obesity_essential,normalize))
k <- 2  # Assuming you want to create 2 clusters; adjust this based on your specific needs
obesity_essential.k2 <- kmeans(obesity_essential, centers = k)
obesity_essential.k2
factoextra::fviz_cluster(obesity_essential.k2,obesity_essential.norm)

k <- 3  # Assuming you want to create 3 clusters; adjust this based on your specific needs
obesity_essential.k3 <- kmeans(obesity_essential, centers = k)
obesity_essential.k3
factoextra::fviz_cluster(obesity_essential.k3,obesity_essential.norm)

k <- 4  # Assuming you want to create 4 clusters; adjust this based on your specific needs
obesity_essential.k4 <- kmeans(obesity_essential, centers = k)
obesity_essential.k4
factoextra::fviz_cluster(obesity_essential.k4,obesity_essential.norm)

k <- 5  # Assuming you want to create 5 clusters; adjust this based on your specific needs
obesity_essential.k5 <- kmeans(obesity_essential, centers = k)
obesity_essential.k5
factoextra::fviz_cluster(obesity_essential.k5,obesity_essential.norm)

k <- 6  # Assuming you want to create 6 clusters; adjust this based on your specific needs
obesity_essential.k6 <- kmeans(obesity_essential, centers = k)
obesity_essential.k6
factoextra::fviz_cluster(obesity_essential.k6,obesity_essential.norm)

k <- 7  # Assuming you want to create 7 clusters; adjust this based on your specific needs
obesity_essential.k7 <- kmeans(obesity_essential, centers = k)
obesity_essential.k7
factoextra::fviz_cluster(obesity_essential.k7,obesity_essential.norm)

k <- 8  # Assuming you want to create 8 clusters; adjust this based on your specific needs
obesity_essential.k8 <- kmeans(obesity_essential, centers = k)
obesity_essential.k8
factoextra::fviz_cluster(obesity_essential.k8,obesity_essential.norm)

k <- 9  # Assuming you want to create 9 clusters; adjust this based on your specific needs
obesity_essential.k9 <- kmeans(obesity_essential, centers = k)
obesity_essential.k9
factoextra::fviz_cluster(obesity_essential.k9,obesity_essential.norm)

k <- 10  # Assuming you want to create 10 clusters; adjust this based on your specific needs
obesity_essential.k10 <- kmeans(obesity_essential, centers = k)
obesity_essential.k10
factoextra::fviz_cluster(obesity_essential.k10,obesity_essential.norm)


# 4 PREDICTION 70-30
subset_data_70_30.train.k8 = kmeans(subset_data_70_30.train,centers = 8)
subset_data_70_30.train.k8

subset_data_70_30.test.k8 = knn(subset_data_70_30.train,subset_data_70_30.test,subset_data_70_30.train.k8$cluster, k=8)
subset_data_70_30.test.k8
length(subset_data_70_30.test.k8)

subset_data_70_30.test.kmeans.k8 = kmeans(subset_data_70_30.test,centers = 8)
subset_data_70_30.test.kmeans.k8
subset_data_70_30.test.k8.labels = subset_data_70_30.test.kmeans.k8$cluster
subset_data_70_30.test.k8.labels
length(subset_data_70_30.test.k8.labels)

#Linear Modelling
subset_data_70_30.train.glm = glm(NObeyesdad~Gender+Height+Age+FCVC+CH2O+CALC+TUE,family = gaussian,data = subset_data_70_30.train)
summary(subset_data_70_30.train.glm)
#plot(subset_data_70_30.train.glm)

subset_data_70_30.train.glm.anova = anova(subset_data_70_30.train.glm,test="Chisq")
subset_data_70_30.train.glm.anova

#Predict
subset_data_70_30.test.pred = predict(subset_data_70_30.train.glm,newdata = subset_data_70_30.test)
subset_data_70_30.test.pred
summary(subset_data_70_30.test.pred)

#Confidence Intervals:
confint(subset_data_70_30.train.glm)

#Compare Actual Vs Prediction
subset_data_70_30.test.pred.k8 = kmeans(subset_data_70_30.test.pred,centers = 8)
subset_data_70_30.test.pred.k8
subset_data_70_30.test.ct.k8 = CrossTable(subset_data_70_30.test.pred.k8$cluster,subset_data_70_30.test.kmeans.k8$cluster,prop.chisq = TRUE)

install.packages("caret")
library(caret)
conf_matrix_70_30 <- table(subset_data_70_30.test.pred.k8$cluster, subset_data_70_30.test.kmeans.k8$cluster)

# Calculate True Positives (TP)
TP <- sum(diag(conf_matrix_70_30))

# Calculate False Positives (FP)
FP <- sum(conf_matrix_70_30) - TP

# Calculate False Negatives (FN)
FN <- sum(conf_matrix_70_30) - TP

# Calculate True Negatives (TN)
TN <- sum(conf_matrix_70_30) - FP

# Calculate Precision
precision <- TP / (TP + FP)

# Calculate Recall (Sensitivity)
recall <- TP / (TP + FN)

# Calculate Specificity
specificity <- TN / (TN + FP)

# Calculate F1 Score
f1_score <- 2 * precision * recall / (precision + recall)

# Calculate Error
error <- 1 - (TP + TN) / sum(conf_matrix_70_30)

# Calculate Accuracy
accuracy <- (TP + TN) / sum(conf_matrix_70_30)

# View the results
precision
recall
specificity
f1_score
error
accuracy
TP
FP

# 4 PREDICTION 60-40
subset_data_60_40.train.k8 = kmeans(subset_data_60_40.train,centers = 8)
subset_data_60_40.train.k8

subset_data_60_40.test.k8 = knn(subset_data_60_40.train,subset_data_60_40.test,subset_data_60_40.train.k8$cluster, k=8)
subset_data_60_40.test.k8
length(subset_data_60_40.test.k8)

subset_data_60_40.test.kmeans.k8 = kmeans(subset_data_60_40.test,centers = 8)
subset_data_60_40.test.kmeans.k8
subset_data_60_40.test.k8.labels = subset_data_60_40.test.kmeans.k8$cluster
subset_data_60_40.test.k8.labels
length(subset_data_60_40.test.k8.labels)

#Linear Modelling
subset_data_60_40.train.glm = glm(NObeyesdad~Gender+Height+Age+FCVC+CH2O+CALC+TUE,family = gaussian,data = subset_data_60_40.train)
summary(subset_data_60_40.train.glm)
#plot(subset_data_60_40.train.glm)
subset_data_60_40.train.glm.anova = anova(subset_data_60_40.train.glm,test="Chisq")
subset_data_60_40.train.glm.anova

#Predict
subset_data_60_40.test.pred = predict(subset_data_60_40.train.glm,newdata = subset_data_60_40.test)
subset_data_60_40.test.pred
summary(subset_data_60_40.test.pred)

#Confidence Intervals:
confint(subset_data_60_40.train.glm)

#Compare Actual Vs Prediction
subset_data_60_40.test.pred.k8 = kmeans(subset_data_60_40.test.pred,centers = 8)
subset_data_60_40.test.pred.k8
subset_data_60_40.test.ct.k8 = CrossTable(subset_data_60_40.test.pred.k8$cluster,subset_data_60_40.test.kmeans.k8$cluster,prop.chisq = TRUE)

# Calculate confusion matrix for 60-40 split
conf_matrix_60_40 <- table(subset_data_60_40.test.pred.k8$cluster, subset_data_60_40.test.kmeans.k8$cluster)

# Calculate True Positives (TP)
TP_60_40 <- sum(diag(conf_matrix_60_40))

# Calculate False Positives (FP)
FP_60_40 <- sum(conf_matrix_60_40) - TP_60_40

# Calculate False Negatives (FN)
FN_60_40 <- sum(conf_matrix_60_40) - TP_60_40

# Calculate True Negatives (TN)
TN_60_40 <- sum(conf_matrix_60_40) - FP_60_40

# Calculate Precision
precision_60_40 <- TP_60_40 / (TP_60_40 + FP_60_40)

# Calculate Recall (Sensitivity)
recall_60_40 <- TP_60_40 / (TP_60_40 + FN_60_40)

# Calculate Specificity
specificity_60_40 <- TN_60_40 / (TN_60_40 + FP_60_40)

# Calculate F1 Score
f1_score_60_40 <- 2 * precision_60_40 * recall_60_40 / (precision_60_40 + recall_60_40)

# Calculate Error
error_60_40 <- 1 - (TP_60_40 + TN_60_40) / sum(conf_matrix_60_40)

# Calculate Accuracy
accuracy_60_40 <- (TP_60_40 + TN_60_40) / sum(conf_matrix_60_40)

# View the results
precision_60_40
recall_60_40
specificity_60_40
f1_score_60_40
error_60_40
accuracy_60_40
TP_60_40
FP_60_40

# 4 PREDICTION 50-50
subset_data_50_50.train.k8 = kmeans(subset_data_50_50.train,centers = 8)
subset_data_50_50.train.k8

subset_data_50_50.test.k8 = knn(subset_data_50_50.train,subset_data_50_50.test,subset_data_50_50.train.k8$cluster, k=8)
subset_data_50_50.test.k8
length(subset_data_50_50.test.k8)

subset_data_50_50.test.kmeans.k8 = kmeans(subset_data_50_50.test,centers = 8)
subset_data_50_50.test.kmeans.k8
subset_data_50_50.test.k8.labels = subset_data_50_50.test.kmeans.k8$cluster
subset_data_50_50.test.k8.labels
length(subset_data_50_50.test.k8.labels)

#Linear Modelling
subset_data_50_50.train.glm = glm(NObeyesdad~Gender+Height+Age+FCVC+CH2O+CALC+TUE,family = gaussian,data = subset_data_50_50.train)
summary(subset_data_50_50.train.glm)
#plot(subset_data_50_50.train.glm)
subset_data_50_50.train.glm.anova = anova(subset_data_50_50.train.glm,test="Chisq")
subset_data_50_50.train.glm.anova

#Predict
subset_data_50_50.test.pred = predict(subset_data_50_50.train.glm,newdata = subset_data_50_50.test)
subset_data_50_50.test.pred
summary(subset_data_50_50.test.pred)

#Confidence Intervals:
confint(subset_data_50_50.train.glm)

#Compare Actual Vs Prediction
subset_data_50_50.test.pred.k8 = kmeans(subset_data_50_50.test.pred,centers = 8)
subset_data_50_50.test.pred.k8
subset_data_50_50.test.ct.k8 = CrossTable(subset_data_50_50.test.pred.k8$cluster,subset_data_50_50.test.kmeans.k8$cluster,prop.chisq = TRUE)

# Calculate confusion matrix for 50-50 split
conf_matrix_50_50 <- table(subset_data_50_50.test.pred.k8$cluster, subset_data_50_50.test.kmeans.k8$cluster)

# Calculate True Positives (TP)
TP_50_50 <- sum(diag(conf_matrix_50_50))

# Calculate False Positives (FP)
FP_50_50 <- sum(conf_matrix_50_50) - TP_50_50

# Calculate False Negatives (FN)
FN_50_50 <- sum(conf_matrix_50_50) - TP_50_50

# Calculate True Negatives (TN)
TN_50_50 <- sum(conf_matrix_50_50) - FP_50_50

# Calculate Precision
precision_50_50 <- TP_50_50 / (TP_50_50 + FP_50_50)

# Calculate Recall (Sensitivity)
recall_50_50 <- TP_50_50 / (TP_50_50 + FN_50_50)

# Calculate Specificity
specificity_50_50 <- TN_50_50 / (TN_50_50 + FP_50_50)

# Calculate F1 Score
f1_score_50_50 <- 2 * precision_50_50 * recall_50_50 / (precision_50_50 + recall_50_50)

# Calculate Error
error_50_50 <- 1 - (TP_50_50 + TN_50_50) / sum(conf_matrix_50_50)

# Calculate Accuracy
accuracy_50_50 <- (TP_50_50 + TN_50_50) / sum(conf_matrix_50_50)

# View the results
precision_50_50
recall_50_50
specificity_50_50
f1_score_50_50
error_50_50
accuracy_50_50
FN_50_50
TN_50_50
FP_50_50
TP_50_50








