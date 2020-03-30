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

# Lage funksjo for å gå gjennom "cut"-objektene og finne hvor mange som var riktig klassifisert, 
# og hvor mange som ble "false positive"/"falske negative".

Euclid_complete_cut = cutree(hc_Euclid_complete, k=2)
Euclid_average_cut = cutree(hc_Euclid_average, k=2)
Euclid_single_cut = cutree(hc_Euclid_single, k=2)

names(Euclid_average_cut)


Corr_complete_cut = cutree(hc_Corr_complete, k=2)
Corr_average_cut = cutree(hc_Corr_average, k=2)
Corr_single_cut = cutree(hc_Corr_single, k=2)

Corr_single_cut

