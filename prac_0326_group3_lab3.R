# Lab, Mar 26
# Yueyang Li

# Read in data
wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
head(wine_data)
nrow(wine_data)

# Adding the variable names 
colnames(wine_data) <- c("Cvs", "Alcohol",
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash",
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols", "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", "Proline")
head(wine_data) # Now you can see the header names.

# Using heatmap to check correlations
heatmap(cor(wine_data),Rowv = NA, Colv = NA)

# declaring the cultivar_classes using the factor() function each cultivar Cv1,Cv2 and Cv3.
cultivar_classes <- factor(wine_data$Cvs) 
cultivar_classes

# PCA
# with scale()
wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)
