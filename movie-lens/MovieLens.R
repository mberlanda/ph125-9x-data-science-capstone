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

# Store edx and validation in data files to make them
# available for the Rmd report
save(edx, validation, file = "movie-lens/movie-data.rda")

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

t_ratings_by_genre <- train_set %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

t_ratings_by_genre <- t_ratings_by_genre %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = sum(count)) %>%
  arrange(desc(count))

# Dump data to be reused in the Rmd without involving too many calculations
save(
  p_rating_distribution,
  t_rating_summary,
  p_bi, p_bu,
  t_most10_rated_movies,
  t_ratings_by_genre,
  file = "movie-lens/data/explore-dataset.rda"
)
