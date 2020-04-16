################################
# Data import
################################

# This analysis is using the Mushrooms classification dataset as per
# Mushroom records drawn from The Audubon Society Field Guide to North American Mushrooms (1981).
# G. H. Lincoff (Pres.), New York: Alfred A. Knopf 
#
# http://archive.ics.uci.edu/ml/datasets/Mushroom
# The csv is formatted as per the Kraggle distribution here:
# https://www.kaggle.com/uciml/mushroom-classification/data

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.r-project.org")

# Unable to retrieve the raw zip file due to a corrupted output (unzip error -1)
# the uncompressed csv file is not exceeding 365Kb, so it can be requested without
# any performance or network traffic concern
file_url <- "https://raw.githubusercontent.com/mberlanda/ph125-9x-data-science-capstone/master/mushroom-classification/mushrooms.csv"
csv_filepath <- "./mushroom-classification/mushrooms.csv"

if (!file.exists(csv_filepath)) {
  download.file(file_url, csv_filepath)
}

# Use read.csv to parse the file converting strings to factors
mushrooms <- read.csv(csv_filepath, header=TRUE, sep=",", stringsAsFactors=TRUE)
# Explore the columns and types of the dataset
str(mushrooms)

rm(file_url, csv_filepath)
################################
# Data wrangling
################################

formatted_mushrooms <- list(
  x = mushrooms %>% select(-class) %>% as.matrix,
  y = mushrooms$class
)

# All the features of this dataset are factors
# This means that the analysis cannot benefit of the most common libraries and methods of analysis
# e.g.
#
# hclust(train_set$x)
# Error in if (is.na(n) || n > 65536L) stop("size cannot be NA nor exceed 65536") : 
# missing value where TRUE/FALSE needed
#
# pca <- prcomp(train_set$x)
# Error in colMeans(x, na.rm = TRUE) : 'x' must be numeric

rm(formatted_mushrooms)

# A solution to this problem can be encoding the categorical features
# https://www.r-bloggers.com/a-guide-to-encoding-categorical-features-using-r/

x <- mushrooms %>% select(-class)

x_factors <- tibble(
  colname = colnames(x),
  n_factors = sapply(1:ncol(x), function(i) nrow(unique(x[i])))
) %>% arrange(desc(n_factors))

x_factors

x_factors %>% 
  ggplot(aes(colname, n_factors)) + geom_col() +
  geom_hline(yintercept = 1, col="red", linetype = "dashed") +
  geom_hline(yintercept = 2, col="blue", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
  coord_flip()

# Total number of max factors
sum(x_factors$n_factors)

###
# One-Hot
###

# Approach using vtreat
# if(!require(vtreat)) install.packages("vtreat", repos = "http://cran.r-project.org")
# tz <- vtreat::designTreatmentsZ(x, colnames(x))
# new_x <- vtreat::prepare(tz, x)
# ncol(new_x)
# colnames(new_x)
# detach("package:vtreat", unload = TRUE)

# model.matrix and contrasts

# remove costant values from the analysis
constantCols <- x_factors %>% filter(n_factors == 1) %>% pull(colname)
binaryCols <- x_factors %>% filter(n_factors == 2) %>% pull(colname)
multiCols <- x_factors %>% filter(n_factors > 2) %>% pull(colname)

# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/model.matrix
new_x <- model.matrix(
  ~.-1,
  data = x %>% select(-constantCols)  
)

class(new_x)
formatted_mushrooms <- list(
  x = new_x,
  y = ifelse(mushrooms$class == 'e', 1, 0)
)

rm(binaryCols, constantCols, multiCols)
rm(x, new_x, x_factors, mushrooms)

################################
# Train and test sets
################################

# Creating only a data partition without any further manipulation
set.seed(22, sample.kind="Rounding")
test_index <- createDataPartition(y=formatted_mushrooms$y, times=1, p=.30, list=FALSE)

train_set <- list(
  x = formatted_mushrooms$x[-test_index,],
  y = formatted_mushrooms$y[-test_index]
)
  
test_set <- list(
  x = formatted_mushrooms$x[test_index,],
  y = formatted_mushrooms$y[test_index]
)

rm(formatted_mushrooms, test_index)

################################
# Data Exploration
################################

dim(train_set$x)
mean(train_set$y)

pca <- prcomp(train_set$x)

data.frame(pca$x[,1:2], edible=train_set$y) %>% 
  ggplot(aes(PC1,PC2, fill=edible))+
  geom_point(cex=3, pch=21, alpha=.6) +
  coord_fixed(ratio=1)

df <- data.frame(PC=pca$x[,1], avg=rowMeans(train_set$x), edible=train_set$y) 
df %>% ggplot(aes(PC, avg, fill=edible))+
  geom_point(cex=3, pch=21, alpha=.6)

cor(df$avg, df$PC)
rm(df)

# remove the center
x <- with(train_set, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)

data.frame(pc$x[,1:2], edible=train_set$y) %>% 
  ggplot(aes(PC1,PC2, fill=edible))+
  geom_point(cex=3, pch=21, alpha=.6) +
  coord_fixed(ratio=1)

for(i in 1:10){
  boxplot(pc$x[,i] ~ train_set$y, main = paste("PC", i))
}

# plot the importance of the PC
foo <- summary(pc)
plot(foo$importance)
mm <- foo$importance %>% as.matrix() %>% t()
dim(mm)
sum(mm[,3] >= 0.5)

plot(summary(pc)$importance[3,]) + abline(h = 0.5)
rm(foo, mm)

# dendogram
d <- dist(train_set$x - rowMeans(train_set$x))
h <- hclust(d)
plot(h)

k <- kmeans(d, centers = 2)
plot(k$cluster)

table(k$cluster, train_set$y)

library(matrixStats)
x_centered <- sweep(train_set$x, 2, colMeans(train_set$x))
x_scaled <- sweep(x_centered, 2, colSds(train_set$x), "/")
heatmap(x_scaled)

rm(d, h, k)