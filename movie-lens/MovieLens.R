################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

################################
# Data Wrangling/Cleaning
################################

if(!require(lubridate)) install.packages("lubridate")
if(!require(stringr)) install.packages("stringr")

# Store edx and validation in data files to make them
# available for the Rmd report
t_specifications <- tibble(
  edx = summary.default(edx),
  validation = summary.default(validation)
)
edx_summary <- summary(edx)
edx_head <- head(edx)

wrangle_data <-function(ds) {
  ds %>%
    mutate(
      userId = factor(userId), # int to factor
      movieId = factor(movieId), # num to factor
      datetime = as_datetime(timestamp), # parse int into date
      movieYear = str_sub(title,-5,-2), # extract movieYear from title (xxxx),
      reviewYear = factor(year(datetime)),
      reviewWeek = factor(week(datetime))
    )
}

edx <- wrangle_data(edx)
validation <- wrangle_data(validation)

save(t_specifications, edx_summary, edx_head, edx, validation, file = "movie-lens/movie-data.rda")

# Separate the known observations dataframe into train_set and test_set
# Test set will be 10% of MovieLens data
set.seed(17, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Ensure again that userId and movieId in the test_set are included in the train_set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm(test_index, temp, removed)


################################
# Explore the dataset
################################

# edx colnames, number of rows and columns
colnames(edx)
dim(edx)
# check that all records are included
nrow(train_set) + nrow(test_set) == nrow(edx)


if(!require(gridExtra)) install.packages("gridExtra")
# Overall rating distribution in train_set  dataframe
p_rating_distribution <- train_set %>% group_by(rating) %>% count() %>% 
  as_tibble() %>% mutate(half = factor((rating * 2) %% 2), rating = factor(rating)) %>%
  ggplot(aes(x=rating, y=n, color=half, fill = half)) +
  geom_bar(stat ="identity", alpha = 0.3) + 
  ggtitle("Train set ratings distribution (half star vs whole star ratings fill)") 

p_rating_distribution_by_year <- train_set %>% group_by(reviewYear, rating) %>% count() %>% 
  ggplot(aes(x=reviewYear)) +
  geom_bar(aes(y=n, fill = factor(rating)), stat ="identity", alpha = 0.5) +
  geom_vline(xintercept = "2003", color = "red", linetype = "dashed") +
  ggtitle("Train set ratings distribution by year (half star ratings since 2003)") 
  
  as_tibble() %>% mutate(half = factor((rating * 2) %% 2), rating = factor(rating)) %>%
  ggplot(aes(x=rating, y=n, color=half, fill = half)) +
  geom_bar(stat ="identity", alpha = 0.3) + 
  ggtitle("Train set ratings distribution (half star vs whole star ratings fill)") 
p_rating_distribution
t_rating_summary <- summary(train_set$rating)

# color palette https://coolors.co/eb5e55-540d6e-88a2aa-e3d7ff-b2aa8e
# Explore the movieId effect on ratings
n_distinct(train_set$movieId)
p_bi <- train_set %>% group_by(movieId) %>% 
  summarize(b_i = mean(rating)) %>% 
  ggplot(aes(b_i)) +
  geom_histogram(bins = 10, color = "#540D6E", fill = "#540D6E", alpha = 0.3) +
  ggtitle("Train set average rating by movieId")

# Explore the userId effect on ratings
n_distinct(train_set$userId)
p_bu <- train_set %>% group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) +
  geom_histogram(bins = 10, color = "#EB5E55", fill = "#EB5E55", alpha = 0.3) +
  ggtitle("Train set average rating by userId")

grid.arrange(
  grobs = list(
    p_bi+coord_flip() + scale_color_hue(),
    p_bu+coord_flip()
  ), ncol = 2, as.table = FALSE
)

t_most10_rated_movies <- train_set %>% group_by(movieId, title) %>%
  summarize(count = n(), avg_rating = mean(rating)) %>%
  arrange(desc(count))  %>%
  head(10)
t_most10_rated_movies

# ordering by average rating is not really meaninful since the highest
# rated have usually less reviews
train_set %>% group_by(movieId, title) %>%
  summarize(count = n(), avg_rating = mean(rating)) %>%
  arrange(desc(avg_rating))  %>%
  head(10)

# Split multiple genres and group ratings by genre 

# This code take ages to run
# t_ratings_by_genre <- train_set %>% separate_rows(genres, sep = "\\|") %>%
#   group_by(genres) %>%
#   summarize(count = n(), avg_rating = mean(rating)) %>%
#   arrange(desc(count))
# t_ratings_by_genre

# Proof of concept of the following code
#
# tibble(g = c("a", "a", "a|b", "b"), r = c(5, 3, 1, 2)) %>%
#   group_by(g) %>% summarise(n = n(), tmp = sum(r)) %>%
#   separate_rows(g, sep = "\\|") %>%
#   group_by(g) %>% 
#   summarise(count = sum(n), avg = sum(tmp)/sum(n))
#

t_ratings_by_genre <- train_set %>%
  group_by(genres) %>%
  summarize(n = n(), tmp = sum(rating)) %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = sum(n), avg = sum(tmp)/sum(n)) %>%
  arrange(desc(count))
t_ratings_by_genre

p_ratings_by_genre <- t_ratings_by_genre %>%
  ggplot(aes(x = genres, group=1)) +
  # geom_line(aes(y = avg), linetype = "dashed") +
  geom_histogram(aes(y = count), stat = "identity", color = "#b2aa8e", fill = "#b2aa8e", alpha = 0.3) +
  geom_line(aes(y = (max(t_ratings_by_genre$count)/5)*avg), linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~./(max(t_ratings_by_genre$count)/5), name = "Average rating")) +
  ggtitle("Ratings by Genres") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_ratings_by_genre

# Visualize the distribution by timestamp
parsed_datetimes <- train_set %>% 
  select(rating, timestamp) %>%
  mutate(datetime = as_datetime(timestamp)) %>%
  select(rating, datetime)
#  eb5e55-540d6e-88a2aa-e3d7ff-b2aa8e
t_ratings_by_year <- parsed_datetimes %>%
  group_by(year = year(datetime)) %>% summarise(count = n(), avg = mean(rating))
t_ratings_by_year

p_ratings_by_year <- t_ratings_by_year %>%
  ggplot(aes(x = year)) +
  geom_histogram(aes(y = count), stat = "identity", color = "#eb5e55", fill = "#eb5e55", alpha = 0.3) +
  geom_line(aes(y = (max(t_ratings_by_year$count)/5)*avg), linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~./(max(t_ratings_by_year$count)/5), name = "Average rating")) +
  ggtitle("Ratings by year")
p_ratings_by_year

t_ratings_by_month <- parsed_datetimes %>%
  group_by(month = month(datetime, label=TRUE)) %>% summarise(count = n(), avg = mean(rating))
t_ratings_by_month

p_ratings_by_month <- t_ratings_by_month %>%
  ggplot(aes(x = month, group=1)) +
  geom_histogram(aes(y = count), stat = "identity", color = "#540d6e", fill = "#540d6e", alpha = 0.3) +
  geom_line(aes(y = (max(t_ratings_by_month$count)/5)*avg), linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~./(max(t_ratings_by_month$count)/5), name = "Average rating")) +
  ggtitle("Ratings by month")
p_ratings_by_month

t_ratings_by_week <- parsed_datetimes %>%
  group_by(week = week(datetime)) %>% summarise(count = n(), avg = mean(rating))
t_ratings_by_week

p_ratings_by_week <- t_ratings_by_week %>%
  ggplot(aes(x = week)) +
  geom_histogram(aes(y = count), stat = "identity", color = "#88a2aa", fill = "#88a2aa", alpha = 0.3) +
  geom_line(aes(y = (max(t_ratings_by_week$count)/5)*avg), linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~./(max(t_ratings_by_week$count)/5), name = "Average rating")) +
  ggtitle("Ratings by week")
p_ratings_by_week

# day of week
t_ratings_by_wday <- parsed_datetimes %>%
  group_by(wday = wday(datetime, label=TRUE)) %>% summarise(count = n(), avg = mean(rating))
t_ratings_by_wday
p_ratings_by_wday <- t_ratings_by_wday %>%
  ggplot(aes(x = wday, group=1)) +
  geom_histogram(aes(y = count), stat = "identity", color = "#e3d7ff", fill = "#e3d7ff", alpha = 0.5) +
  geom_line(aes(y = (max(t_ratings_by_wday$count)/5)*avg), linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~./(max(t_ratings_by_wday$count)/5), name = "Average rating")) +
  ggtitle("Ratings by week day")
p_ratings_by_wday

# Dump data to be reused in the Rmd without involving too many calculations
save(
  p_rating_distribution,
  t_rating_summary,
  p_bi, p_bu,
  t_most10_rated_movies,
  p_ratings_by_genre,
  t_ratings_by_genre,
  p_ratings_by_year,
  p_ratings_by_month,
  p_ratings_by_week,
  p_ratings_by_wday,
  t_ratings_by_genre,
  t_ratings_by_year,
  t_ratings_by_month,
  t_ratings_by_week,
  t_ratings_by_wday,
  
  file = "movie-lens/data/explore-dataset.rda"
)

rm(
  p_rating_distribution,
  t_rating_summary,
  p_bi, p_bu,
  t_most10_rated_movies,
  t_ratings_by_genre,
  p_ratings_by_year,
  t_ratings_by_year,
  p_ratings_by_month,
  t_ratings_by_month,
  p_ratings_by_week,
  t_ratings_by_week,
  p_ratings_by_wday,
  t_ratings_by_wday,
  parsed_datetimes
)

################################
# Modeling
################################

# Normalize outcome
normalizeOutcome <- function(x) { ceiling(x*2)/2 }

# Regression tree model
library(rpart)
y <- test_set$rating
rt_model <- rpart(rating ~ userId+movieId+genres, data = train_set)
rt_y_hat <- predict(rt_model, test_set)

rt_accuracy <- mean(normalizeOutcome(rt_y_hat) == test_set$rating)
save(
  rt_model,
  rt_accuracy,
  file = "movie-lens/data/model-regression-tree.rda"
)

# Random Forrest
# if(!require(randomForest)) install.packages("randomForest")
# library(randomForest)
# rf_model <- randomForest(rating~userId+movieId+genres, data = train_set, max_nodes = 10)
# 
# # Error: vector memory exhausted (limit reached?)

# 

train_x <- train_set %>% select(userId, movieId, genres) # %>%
#  mutate(userId = factor(userId), movieId = factor(movieId))
train_y <- factor(train_set$rating)


# 12 LDA and QDA model
train_lda <- train(train_x, train_y, method = "lda")
lda_preds <- predict(train_lda, test_x)
mean(lda_preds == test_y)

save(
  train_lda,
  file = "movie-lens/data/model-lda.rda"
)


train_qda <- train(train_x, train_y, method = "qda")
qda_preds <- predict(train_qda, test_x)
mean(qda_preds == test_y)

save(
  train_qda,
  file = "movie-lens/data/model-qda.rda"
)

# 11 Logistic regression model
train_glm <- train(train_x, train_y, method = "glm")
glm_preds <- predict(train_glm, test_x)
mean(glm_preds == test_y)

save(
  train_glm,
  file = "movie-lens/data/model-glm.rda"
)
 
# 13 Loess model
library(gam)

set.seed(3, sample.kind = "Rounding")
train_loess <- train(train_x, train_y, method = "gamLoess")
loess_preds <- predict(train_loess, test_x)
mean(loess_preds == test_y)

save(
  train_loess,
  file = "movie-lens/data/model-loess.rda"
)

# 14 K-nearest neighbors model
set.seed(7, sample.kind = "Rounding")
train_knn <- train(
  train_x, train_y,
  method = "knn",
  tuneGrid = data.frame(k = seq(3,21,2))
)

train_knn$bestTune

knn_preds <- predict(train_knn, test_x) 
# knn_preds <- predict(train_knn$finalModel, test_x)
# knn_preds <- ifelse(knn_preds[,1] >= .5, "B", "M") %>% factor(levels(test_y))
mean(knn_preds == test_y)

save(
  train_knn,
  file = "movie-lens/data/train_knn.rda"
)

# 15 Random Forest
set.seed(9, sample.kind = "Rounding")
train_rf <- train(
  train_x, train_y,
  method = "rf",
  tuneGrid = data.frame(mtry = seq(3,9,2)),
  importance = TRUE
)

save(
  train_rf,
  file = "movie-lens/data/train_rf.rda"
)
