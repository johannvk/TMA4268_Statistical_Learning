# Require libraries:
library(ggplot2)
library(dendextend)

# Load data:
id <- "1VfVCQvWt121UN39NXZ4aR9Dmsbj-p9OU"  # google file ID
GeneData <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                             id), header = F)
colnames(GeneData)[1:20] = paste(rep("H", 20), c(1:20), sep = "")
colnames(GeneData)[21:40] = paste(rep("D", 20), c(1:20), sep = "")
row.names(GeneData) = paste(rep("G", 1000), c(1:1000), sep = "")

# Get the transpose of the data.frame in order to have observations as rows:
GeneData = t(GeneData)

# summary(GeneData)
# View(GeneData)
# head(GeneData)

### a) Plot the Euclidiean and Correlation Dendrograms. ###

label_colors = c("red", "blue")  # Disease- and Healthy-color.
colorLeaf = function(n){
  if (is.leaf(n)){
    a = attributes(n)
    type = substring(a$label, 1, 1)
    if (type == "D"){  # Found a disesase node
      color = label_colors[[1]]
    }
    else {  # Found a healthy node
      color = label_colors[[2]]
    }
    # print(a)
    attr(n, "nodePar") = c(a$nodePar, lab.col=color)
  }
  return(n)
}

# Plotting Dendrograms with Euclidian Dissimillarity:
Euclid_dist = dist(GeneData, method="euclidian")

hc_Euclid_complete = hclust(Euclid_dist, method="complete")
dend_Euclid_complete = as.dendrogram(hc_Euclid_complete, hang=0.1)
dend_Euclid_complete = dendrapply(dend_Euclid_complete, colorLeaf)

hc_Euclid_average = hclust(Euclid_dist, method="average")
dend_Euclid_average = as.dendrogram(hc_Euclid_average, hang=0.1)
dend_Euclid_average = dendrapply(dend_Euclid_average, colorLeaf)

hc_Euclid_single = hclust(Euclid_dist, method="single")
dend_Euclid_single = as.dendrogram(hc_Euclid_single, hang=0.1)
dend_Euclid_single = dendrapply(dend_Euclid_single, colorLeaf)

# Plot with colored leaf nodes:
par(mfrow=c(1, 3))
plot(dend_Euclid_complete, main="Complete Grouping", ylim=c(38, 52), ylab="Dissimillar Euclidian Height")
plot(dend_Euclid_average, main="Average Grouping", ylim=c(38, 50))
plot(dend_Euclid_single, main="Single Grouping", ylim=c(38, 47))
### TODO: Nevne i teksten i .Rmd-dokumentet at Blue: Frisk, Red: Syk.

# Plotting Dendrograms with Correlation Dissimillarity:
Corr_dist = as.dist(1.0 - cor(t(GeneData), t(GeneData)))

hc_Corr_complete = hclust(Corr_dist, method="complete")
dend_Corr_complete = as.dendrogram(hc_Corr_complete, hang=0.1)
dend_Corr_complete = dendrapply(dend_Corr_complete, colorLeaf)

hc_Corr_average = hclust(Corr_dist, method="average")
dend_Corr_average = as.dendrogram(hc_Corr_average, hang=0.1)
dend_Corr_average = dendrapply(dend_Corr_average, colorLeaf)

hc_Corr_single = hclust(Corr_dist, method="single")
dend_Corr_single = as.dendrogram(hc_Corr_single, hang=0.1)
dend_Corr_single = dendrapply(dend_Corr_single, colorLeaf)

par(mfrow=c(1, 3))
plot(dend_Corr_complete, main="Complete Grouping", ylim=c(0.4, 1.2), ylab="Dissimillar Correlation Height")
plot(dend_Corr_average, main="Average Grouping", ylim=c(0.5, 1.05))
plot(dend_Corr_single, main="Single Grouping", ylim=c(0.5, 1.0))


### b) Use the dendrograms to cluster the tissues into two groups:

# Lage funksjon for ? g? gjennom "cut"-objektene og finne hvor mange som var riktig klassifisert, 
# og hvor mange som ble "false positive"/"falske negative".

tree_confusion_matrix = function(cut_list){
  tissues = names(cut_list)
  groups = matrix(rep(0L, 4), nrow=2, ncol=2)
  # Count groups of Healthy and Disease tissues:
  # Group:  Healthy  Disease
  #     1         X        X
  #     2         X        X
  for (i in 1:length(cut_list)) {
    # Place the classified group in correct slot in matrix:
    if(substring(tissues[[i]], 1, 1) == "H") {  # Found a Healthy Leaf.
      col = 1
      row = cut_list[[i]]
      groups[[row, col]] = groups[[row, col]] + 1
    }
    else{  # Found a Disease Leaf. 
      col = 2
      row = cut_list[[i]]
      groups[[row, col]] = groups[[row, col]] + 1
    }
  }
  # Find which of the two groups have the maximum healthy- and 
  # disease- labels, and name that the corresponding prediction.
  health_group = 0L; disease_group = 0L
  max_healthy = which.max(groups[, 1])
  if (groups[[max_healthy, 2]] <= groups[[max_healthy, 1]]){
    health_group = max_healthy
    disease_group = (max_healthy %% 2) + 1L
  }
  else{
    disease_group = max_healthy
    health_group = (max_healthy %% 2) + 1L
  }
  confusion_matrix = matrix(rep(0L, 4), nrow=2, ncol=2)
  # Fill inn the Health column:
  confusion_matrix[[1, 1]] = groups[[health_group, 1]]
  confusion_matrix[[2, 1]] = groups[[disease_group, 1]]
  # Fill in the Disease column:
  confusion_matrix[[1, 2]] = groups[[health_group, 2]]
  confusion_matrix[[2, 2]] = groups[[disease_group, 2]]
  colnames(confusion_matrix) = c("Healthy, Actual", "Disease, Actual")
  rownames(confusion_matrix) = c("Healthy, Pred.", "Disease, Pred.")
  return(confusion_matrix)
}

# Find Euclidian cuts:
Euclid_complete_cut = cutree(hc_Euclid_complete, k=2)
Euclid_average_cut = cutree(hc_Euclid_average, k=2)
Euclid_single_cut = cutree(hc_Euclid_single, k=2)

# Generate Euclidian confusion matrices:
Euclid_complete_cm = tree_confusion_matrix(Euclid_complete_cut)
Euclid_average_cm = tree_confusion_matrix(Euclid_average_cut)
Euclid_single_cm = tree_confusion_matrix(Euclid_single_cut)

cat("Euclidian Confusion matrices:\n")
print(Euclid_complete_cm)
print(Euclid_average_cm)
print(Euclid_single_cm)


# Find Correlation cuts:
Corr_complete_cut = cutree(hc_Corr_complete, k=2)
Corr_average_cut = cutree(hc_Corr_average, k=2)
Corr_single_cut = cutree(hc_Corr_single, k=2)

# Generate correlation confusion matrices:
Corr_complete_cm = tree_confusion_matrix(Corr_complete_cut)
Corr_average_cm = tree_confusion_matrix(Corr_average_cut)
Corr_single_cm = tree_confusion_matrix(Corr_single_cut)

cat("Correlation Confusion matrices:\n")
print(Corr_complete_cm)
print(Corr_average_cm)
print(Corr_single_cm)

### d) PCA of GeneData:
# Looks good to center the gene-expressions and normalize to std.dev. = 1.
# Then we get separation as [-10, 10] in PC1. Very nice!
# Can explain that this was done as there was some difference accross gene 
# expressions in mean and variance:
cat("Range of means for gene expressions:\n", range(apply(GeneData, 2, mean)), "\n")
cat("Range of variance for gene expressions:\n", range(apply(GeneData, 2, var)), "\n")

# View(GeneData)
# dim(GeneData)
pca_GeneData = prcomp(GeneData, center=T, scale=T)

# names(pca_GeneData)
# dim(pca_GeneData$x)
# typeof(pca_GeneData$x)

pcs_df = as.data.frame(pca_GeneData$x)
pcs_df$tissue = c(rep("H", 20), rep("D", 20))
View(pcs_df)

pca_plot = ggplot(data=pcs_df, aes(x=PC1, y=PC2, color=tissue)) + geom_point(size=3.0) +
           labs(title="GeneData, PC2 versus PC1, Healthy and Disease Tissue")
print(pca_plot)
# Nice plot!

# Find porportion of variance explained:
pca_GD_var = pca_GeneData$sdev^2
variance_portion = pca_GD_var/sum(pca_GD_var)
cat("Variance portions from PCA:\n")
print(variance_portion)

print("Proportion of Variance explained by first five principal components:")
print(sum(variance_portion[1:5]))


### e) Use PCA to find which genes vary the most across the two groups:
# No idea. Use which genes have high weightings in the first principal component, which
# has a fifth of the variance? That seems reasonable, as it divides the two types of genes?

#  Construct imperical Correlation matrix for GeneData:
# First scale to zero mean and variance one:
scaled_GeneData = scale(GeneData)
scaled_Corr = (1.0/(nrow(GeneData)-1))*(t(scaled_GeneData) %*% scaled_GeneData)

# Retrieve the first column of the rotation matrix, witht the first eigenvector of Corr:
phi_1 = pca_GeneData$rotation[, 1]
# View(phi_1)

n = 150
reduced_phi_1 = rep(0.0, ncol(scaled_GeneData))
maxn_var_indices = order(abs(phi_1), decreasing = T)[1:n]
reduced_phi_1[maxn_var_indices] = phi_1[maxn_var_indices]

reduced_var_proportion = (t(reduced_phi_1) %*% scaled_Corr %*% reduced_phi_1) / sum(pca_GD_var)

# By using only 150 out of 1000 genes in the first PCA, we account for 4.8% of the variation in the GeneData.
# This is more than half of the variance explained by PC1 when using all 1000 genes.

cat("Proportion of variance explained by the", n, "highest magnintude components of PC1:", reduced_var_proportion)
cat("These genes are:\n")
print(names(phi_1[maxn_var_indices]))  # Mostly in the region [500, 600], with a few around [10, 20].


### f) K-means to seperate the data into two groups. Find the error rate.

kmeans_GeneData = kmeans(GeneData, centers=2, nstart=20)
pcs_df$cluster = as.factor(kmeans_GeneData$cluster)

#  Zero error-rate by K-means clustering:
k_means_plot = ggplot(pcs_df, aes(x=PC1, y=PC2, shape=cluster, color=tissue)) + geom_point(size=3.0) +
       labs(title="GeneData, PC1 vs. PC2. Cluster 1 and 2 from K-means")
print(k_means_plot)
