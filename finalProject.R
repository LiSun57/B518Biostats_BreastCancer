# Set the working directory to where the data files are located
setwd("/Users/lisun/Library/Mobile Documents/com~apple~CloudDocs/coursesIUI/1st/[QE]INFO-B518 Applied Biostats/project/data/breast+cancer+wisconsin+diagnostic")

# Read the data file
data <- read.table("wdbc.data", sep = ",", header = FALSE)
head(data)
summary(data)
str(data)





# Read the content of the file "wdbc.names"
names_info <- readLines("wdbc.names")
# Print the content to the console
print(names_info)








# Define the feature names based on the "wdbc.names"
feature_names <- c("ID", "Diagnosis",
                   "Radius_Mean", "Texture_Mean", "Perimeter_Mean", "Area_Mean", "Smoothness_Mean", "Compactness_Mean", "Concavity_Mean", "ConcavePoints_Mean", "Symmetry_Mean", "FractalDimension_Mean",
                   "Radius_SE", "Texture_SE", "Perimeter_SE", "Area_SE", "Smoothness_SE", "Compactness_SE", "Concavity_SE", "ConcavePoints_SE", "Symmetry_SE", "FractalDimension_SE",
                   "Radius_Worst", "Texture_Worst", "Perimeter_Worst", "Area_Worst", "Smoothness_Worst", "Compactness_Worst", "Concavity_Worst", "ConcavePoints_Worst", "Symmetry_Worst", "FractalDimension_Worst")
# Assign the column names to the dataframe
colnames(data) <- feature_names
# Check the first few rows to see the data with proper feature names
head(data)


# Save the dataframe as an RDS file
saveRDS(data, file = "wdbc_with_feature_names.rds")
# Load the dataframe
data <- readRDS("wdbc_with_feature_names.rds")
head(data)
str(data)








# Double-check for missing values
sum(is.na(data))  # Total number of missing values in the dataset

# Summary statistics for each column
summary(data)



# Generate histograms for all numeric features, except 'ID', which is the first column
numeric_features <- sapply(data[-1], is.numeric)
hist_data <- data[-1][, numeric_features]  # select only numeric columns

# Sort the columns by name
hist_data <- hist_data[, order(names(hist_data))]
par(mfrow = c(5, 6), mar = c(2, 5, 2, 1), oma = c(0, 0, 3, 0), cex.main = 0.8, cex.lab = 0.7, cex.axis = 0.7, las = 1)  # Create a matrix of 5 rows & 6 columns plots; mar set the bottom-left-top-right margin sizes

# Plot histograms
for (i in 1:ncol(hist_data)) {
  hist(hist_data[[i]], main = colnames(hist_data)[i], xlab = colnames(hist_data)[i], ylab = "")
}
# Add an overall title for the histogram matrix
mtext("Distributions of Numeric Features", side = 3, outer = TRUE, line = 1, cex = 1.2)


# # Reset par to default
# par(mfrow = c(1, 1))

# Plot boxplots 
# par(mfrow = c(5, 6))  
for (i in 1:ncol(hist_data)) {
  boxplot(hist_data[[i]], main = colnames(hist_data)[i], ylab = "")
}
# Add an overall title for the box plot matrix
mtext("Boxplots of Numeric Features", side = 3, outer = TRUE, line = 1, cex = 1.2)













# Data Exploration
# Univariate Analysis, e.g., use density plots to visualize the distribution of individual features
library(ggplot2)
# Filter columns that end with '_Mean'
mean_features <- data[,grepl("_Mean$", names(data))]
# Plotting each feature using histogram and overlay with normal curve
plot_list <- list()
for (feature in names(mean_features)) {
  mean_val <- mean(mean_features[[feature]], na.rm = TRUE)
  sd_val <- sd(mean_features[[feature]], na.rm = TRUE)
  
  # Create a dataframe for the normal curve
  curve_df <- data.frame(x = seq(min(mean_features[[feature]], na.rm = TRUE), 
                                 max(mean_features[[feature]], na.rm = TRUE), 
                                 length.out = 100))
  curve_df$y <- dnorm(curve_df$x, mean = mean_val, sd = sd_val)
  
  p <- ggplot(mean_features, aes_string(x = feature)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.5) +
    geom_line(data = curve_df, aes(x = x, y = y), color = "red", size = 1) +
    xlab(paste("Mean of", feature)) +
    ylab("Density") +
    ggtitle(paste("Histogram of", feature, "with Normal Curve"))
  
  plot_list[[feature]] <- p
}
# Display plots using gridExtra 
library(gridExtra)
do.call(grid.arrange, c(plot_list, ncol = 2))
















# library(reshape2)
# library(ggplot2)
# To plot density plots for all three dimensions (_Mean, _SE, _Worst) of the feature "Radius" together
# # the data frame is named 'data' and it has columns 'Radius_Mean', 'Radius_SE', 'Radius_Worst'
# # First, melt the data to long format
# radius_data <- melt(data, id.vars = "ID", measure.vars = c("Radius_Mean", "Radius_SE", "Radius_Worst"))
# 
# # Now, using ggplot2 to plot the densities
# ggplot(radius_data, aes(x = value, fill = variable)) + 
#   geom_density(alpha = 0.5) +
#   scale_fill_manual(values = c("blue", "green", "red")) +
#   labs(x = "Radius", y = "Density", title = "Density Plot of Radius for Mean, SE, and Worst") +
#   theme_minimal() +
#   guides(fill=guide_legend(title="Radius Measurements"))



# Bivariate Analysis, e.g., plot scatter plots to understand the relationship between two continuous variables
# Scatter plot between two features
ggplot(data, aes(x = Radius_Mean, y = Texture_Mean)) + 
  geom_point(aes(color = Diagnosis)) +
  xlab("Mean Radius") + 
  ylab("Mean Texture") +
  ggtitle("Scatter Plot of Mean Radius and Mean Texture")




# Multivariate Analysis, e.g., use a heatmap to visualize the correlation between features
library(corrplot)
# Calculate correlation matrix
cor_matrix <- cor(data[,3:32])  #  the first two columns are ID and Diagnosis
# Plot the heatmap with black labels and enable clustering
corrplot(cor_matrix, method = "color", order = "hclust", col = colorRampPalette(c("blue", "white", "red"))(200), tl.col="black", tl.srt=45)




