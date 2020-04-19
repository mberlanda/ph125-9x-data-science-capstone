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

if(!require(e1071)) install.packages("e1071", repos = "http://cran.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.r-project.org")


# Automatically disable plots when computing the solution as a shell script
SHOW_PLOTS <- TRUE

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

if (SHOW_PLOTS) {
  # Number of leves for each factor
  x_factors %>% 
    ggplot(aes(colname, n_factors)) +
    geom_col(alpha=.6) +
    geom_hline(yintercept = 1, col="red", linetype = "dashed") +
    geom_hline(yintercept = 2, col="blue", linetype = "dashed") +
    theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
    coord_flip() +
    ggtitle("Number of levels by feature")

    # Odor effect
    mushrooms %>%
      group_by(odor) %>%
      summarize(n=n(), edible=mean(class=="e")) %>%
      mutate(is_edible=ifelse(
        edible==1, "yes", ifelse(edible==0, "no", "maybe"))
      ) %>%
      ggplot(aes(x=odor), group=1) +
      geom_bar(aes(y=n, fill=is_edible), stat="identity") +
      geom_text(aes(y=n, label=paste(100*round(edible, digits=3), "%", sep="")), position=position_dodge(width=0.9), vjust=-0.25) +
      scale_fill_manual(values=c("#f7ef99", "#f78e69","#afc97e")) +
      scale_x_discrete(labels=c(
        "a"="almond","l"="anise","c"="creosote","y"="fishy","f"="foul",
        "m"="musty","n"="none","p"="pungent","s"="spicy"
      )) +
      ggtitle("Mushrooms odor effect") +
      theme(axis.text.x = element_text(angle=45))
  
    # Cap color effect when no odor
    mushrooms %>%
      filter(odor =="n") %>%
      group_by(cap.color) %>%
      summarize(n=n(), edible=mean(class=="e")) %>%
      mutate(is_edible=ifelse(
        edible==1, "yes", ifelse(edible==0, "no", "maybe"))
      ) %>%
      ggplot(aes(x=cap.color), group=1) +
      geom_bar(aes(y=n, fill=is_edible), stat="identity") +
      geom_text(aes(y=n, label=paste(100*round(edible, digits=3), "%", sep="")), position=position_dodge(width=0.9), vjust=-0.25) +
      scale_fill_manual(values=c("#f7ef99", "#f78e69","#afc97e")) +
      scale_x_discrete(labels=c(
        "n"="brown","b"="buff","c"="cinnamon","g"="gray","r"="green",
        "p"="pink","u"="purple","e"="red","w"="white","y"="yellow"
      )) +
      ggtitle("Mushrooms cap color effect with none odor") +
      theme(axis.text.x = element_text(angle=45))

    # Gill color effect
    mushrooms %>%
      filter(odor =="n") %>%
      filter(cap.color %in% c("n", "b", "p", "w")) %>%
      group_by(gill.color) %>%
      summarize(n=n(), edible=mean(class=="e")) %>%
      mutate(is_edible=ifelse(
        edible==1, "yes", ifelse(edible==0, "no", "maybe"))
      ) %>%
      ggplot(aes(x=gill.color), group=1) +
      geom_bar(aes(y=n, fill=is_edible), stat="identity") +
      geom_text(aes(y=n, label=paste(100*round(edible, digits=3), "%", sep="")), position=position_dodge(width=0.9), vjust=-0.25) +
      scale_fill_manual(values=c("#f7ef99", "#f78e69","#afc97e")) +
      scale_x_discrete(labels=c(
        "k"="black","n"="brown","b"="buff","h"="chocolate","g"="gray",
        "r"="green","o"="orange","p"="pink","u"="purple","e"="red",
        "w"="white","y"="yellow"
      )) +
      ggtitle("Gill color effect on a subset of mushrooms") +
      theme(axis.text.x = element_text(angle=45))
}

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
# constantCols <- x_factors %>% filter(n_factors == 1) %>% pull(colname)
# binaryCols <- x_factors %>% filter(n_factors == 2) %>% pull(colname)
# multiCols <- x_factors %>% filter(n_factors > 2) %>% pull(colname)
# 
# new_x <- model.matrix(
#   ~.-1,
#   data = x %>% select(-constantCols)  
# )
# 
# class(new_x)
# formatted_mushrooms <- list(
#   x = new_x,
#   y = ifelse(mushrooms$class == 'e', 1, 0)
# )
# rm(binaryCols, constantCols, multiCols)
# rm(new_x, x_factors)
rm(x, x_factors)

# A more complex approach taking in consideration the ordinal variables
# and converting to binary all the categories with two values

###
# formatMushrooms function to reproduce the data manipulations
###

formatMushrooms <- function(df) {
  # private utility function
  mapValues <- function(v, m) {
    sapply(v, function(x) do.call("switch", prepend(m, x)))
  }
  
  # Select nominal categorical features with more than 2 categories
  new_df <- df %>%
    select(
      cap.color, cap.shape, cap.surface, gill.attachment, gill.color, habitat, odor,
      population, ring.type, spore.print.color, stalk.color.above.ring,
      stalk.color.below.ring, stalk.root, stalk.surface.above.ring,
      stalk.surface.below.ring, veil.color
    )
    
  # Nominal variables
  colorsMapping <- list(
    "b"=".buff","c"=".cinnamon","e"=".red","g"=".gray","h"=".chocolate","k"=".black",
    "n"=".brown","o"=".orange","p"=".pink","r"=".green","u"=".purple","w"=".white",
    "y"=".yellow"
  )
  surfacesMapping <- list("f"=".fibrous","g"=".grooves","y"=".scaly","s"=".smooth")
  
  capShapeMapping <- list("b"=".bell","c"=".conical","x"=".convex","f"=".flat", "k"=".knobbed","s"=".sunken")
  gillAttachmentMapping <- list("a"=".attached","d"=".descending","f"=".free","n"=".notched")
  habitatMapping <- list("g"=".grasses","l"=".leaves","m"=".meadows","p"=".paths", "u"=".urban","w"=".waste","d"=".woods")
  odorMapping <- list("a"=".almond","l"=".anise","c"=".creosote","y"=".fishy","f"=".foul", "m"=".musty","n"=".none","p"=".pungent","s"=".spicy")
  populationMapping <- list("a"=".abundant","c"=".clustered","n"=".numerous", "s"=".scattered","v"=".several","y"=".solitary")
  ringTypeMapping <- list("c"=".cobwebby","e"=".evanescent","f"=".flaring","l"=".large", "n"=".none","p"=".pendant","s"=".sheathing","z"=".zone")
  stalkRootMapping <- list("b"=".bulbous","c"=".club","u"=".cup","e"=".equal", "z"=".rhizomorphs","r"=".rooted","missing"=".missing")

  new_df$cap.color <- mapValues(df$cap.color, colorsMapping) %>% factor
  new_df$gill.color <- mapValues(df$gill.color, colorsMapping) %>% factor
  new_df$spore.print.color <- mapValues(df$spore.print.color, colorsMapping) %>% factor
  new_df$stalk.color.above.ring <- mapValues(df$stalk.color.above.ring, colorsMapping) %>% factor
  new_df$stalk.color.below.ring <- mapValues(df$stalk.color.below.ring, colorsMapping) %>% factor
  new_df$veil.color <- mapValues(df$veil.color, colorsMapping) %>% factor

  new_df$cap.surface <- mapValues(df$cap.surface, surfacesMapping) %>% factor
  new_df$stalk.surface.above.ring <- mapValues(df$stalk.surface.above.ring, surfacesMapping) %>% factor
  new_df$stalk.surface.below.ring <- mapValues(df$stalk.surface.below.ring, surfacesMapping) %>% factor

  new_df$cap.shape <- mapValues(df$cap.shape, capShapeMapping) %>% factor
  new_df$gill.attachment <- mapValues(df$gill.attachment, gillAttachmentMapping) %>% factor
  new_df$habitat <- mapValues(df$habitat, habitatMapping) %>% factor
  new_df$odor <- mapValues(df$odor, odorMapping) %>% factor
  new_df$population <- mapValues(df$population, populationMapping) %>% factor
  new_df$ring.type <- mapValues(df$ring.type, ringTypeMapping) %>% factor
  new_df$stalk.root <- mapValues(df$stalk.root, stalkRootMapping) %>% factor
  
  # Cleanup
  rm(
    colorsMapping, surfacesMapping, capShapeMapping, gillAttachmentMapping,
    habitatMapping, odorMapping, populationMapping, ringTypeMapping, stalkRootMapping
  )
  # One Hot encoding: convert nominal variables with more than 2 categories
  # https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/model.matrix
  new_df <- model.matrix(
    ~.-1,
    data = new_df
  )
  
  # ordinal variables
  # gill.spacing: "c"="close","w"="crowded","d"="distant"
  gillSpacingMapping <- list("c"=0,"w"=1,"d"=2)
  # ring.number: "n"="none","o"="one","t"="two"
  ringNumberMapping <-list("n"=0,"o"=1,"t"=2) 

  new_df <- new_df %>%
    as_tibble() %>%
    add_column(
      grill.spacing = mapValues(df$gill.spacing, gillSpacingMapping),
      ring.number = mapValues(df$ring.number, ringNumberMapping),
      bruises = ifelse(df$bruises=="t",1,0),
      gill.size.narrow = ifelse(df$gill.size=="n",1,0),
      stalk.shape.enrlarging = ifelse(df$stalk.shape=="e",1,0)
      # veil.type is ignored in this analysis since it is constant in the data set
      # veil.type.universal = ifelse(df$veil.type=="u",1,0)
    ) %>% as.matrix
  
  rm(gillSpacingMapping, ringNumberMapping)
  
  list(
    x = new_df,
    y = ifelse(df$class=="e",1,0) # edible
  )
}

formatted_mushrooms <- formatMushrooms(mushrooms)


rm(mushrooms, formatMushrooms)

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

if (SHOW_PLOTS) {
  data.frame(pca$x[,1:2], edible=train_set$y) %>% 
    ggplot(aes(PC1,PC2, fill=edible))+
    geom_point(cex=3, pch=21, alpha=.6) +
    coord_fixed(ratio=1)  
}

df <- data.frame(PC=pca$x[,1], avg=rowMeans(train_set$x), edible=train_set$y) 

if (SHOW_PLOTS) {
  df %>% ggplot(aes(PC, avg, fill=edible))+
    geom_point(cex=3, pch=21, alpha=.6)
}

cor(df$avg, df$PC)
rm(df, pca)

# remove the center
x <- with(train_set, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)

if (SHOW_PLOTS) {
  data.frame(pc$x[,1:2], edible=train_set$y) %>% 
    ggplot(aes(PC1,PC2, fill=edible))+
    geom_point(cex=3, pch=21, alpha=.6) +
    coord_fixed(ratio=1)

  for(i in 1:10){
    boxplot(pc$x[,i] ~ train_set$y, main = paste("PC", i))
  }
  
  rm(i)
}
rm(x)

# plot the importance of the PC
foo <- summary(pc)
mm <- foo$importance %>% as.matrix() %>% t()
dim(mm)
sum(mm[,3] >= 0.5)

if (SHOW_PLOTS) plot(summary(pc)$importance[3,]) + abline(h = 0.5)
rm(foo, mm, pc)

# dendogram
d <- dist(train_set$x - rowMeans(train_set$x))
h <- hclust(d)
if (SHOW_PLOTS) plot(h)

k <- kmeans(d, centers = 2)
if (SHOW_PLOTS) plot(k$cluster)

table(k$cluster, train_set$y)

library(matrixStats)
x_centered <- sweep(train_set$x, 2, colMeans(train_set$x))
x_scaled <- sweep(x_centered, 2, colSds(train_set$x), "/")
heatmap(x_scaled)

rm(d, h, k)

################################
# Methods
################################

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# 1 Logistic regression model
train_glm <- train(train_set$x, train_set$y, method = "glm")
glm_preds <- predict(train_glm, test_set$x)
mean(round(glm_preds) == test_set$y)

glm_rmse <- RMSE(test_set$y, glm_preds)
round(glm_rmse, 3)

# Train control: bootstrap vs cross validation
train_control <- trainControl(method="cv", number=10)

# 2 LDA and QDA model
train_lda <- train(train_set$x, factor(train_set$y), method = "lda", preProcess = c("center"))
train_lda

confusionMatrix(train_lda)

lda_preds <- predict(train_lda, test_set$x)
mean(lda_preds == test_set$y)

# train_qda <- train(train_set$x, factor(train_set$y), method = "qda")
# qda_preds <- predict(train_qda, test_set$x)
# mean(qda_preds == test_y)

# model fit failed for Resample01: parameter=none Error in qda.default(x, grouping, ...) : rank deficiency in group 0
# In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  ... :
#                           There were missing values in resampled performance measures.

# 3 LOESS model
set.seed(3, sample.kind = "Rounding")
train_loess <- train(train_set$x, factor(train_set$y), method = "gamLoess")
loess_preds <- predict(train_loess, test_set$x)
mean(loess_preds == test_set$y)

set.seed(2015, sample.kind = "Rounding")
cp <- seq(0, 0.1, 0.01)
train_rpart <- train(train_set$x, factor(train_set$y), method = "rpart", tuneGrid = data.frame(cp = cp))
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

ggplot(train_rpart, highlight = TRUE)

train_rpart$bestTune
# mushrooms %>%
#   mutate(
#     pred = ifelse(
#       odor == "n", 
#       "e",
#       ifelse(stalk.root == "c", "e", "p")
#     ),
#     ok = class == pred
#   ) %>% pull(ok) %>% mean()
# 0.945

rm(SHOW_PLOTS)
