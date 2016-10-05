
getwd()
setwd("/Users/ingridbrizotti/Desktop/Ryerson/3.Data_Analytics_Capstone/MovieTweetings/latest")

load("~/Desktop/Ryerson/3.Data_Analytics_Capstone/MovieTweetings/latest/movies4.Rda")
load("~/Desktop/Ryerson/3.Data_Analytics_Capstone/MovieTweetings/latest/ratings.Rda")


##################################    Movies     ##########################################

# Freq main genre
freq <- cbind(Freq = table(movies4$genre1), 
              Cumul = cumsum(table(movies4$genre1)), 
              relative = round((prop.table(table(movies4$genre1))*100),2))
library(xlsx)
write.xlsx(freq, "freq_genre1.xlsx")

# Freq second genre
freq2 <- cbind(Freq = table(movies4$genre2), 
               Cumul = cumsum(table(movies4$genre2)), 
               relative = round((prop.table(table(movies4$genre2))*100),2))
library(xlsx)
write.xlsx(freq2, "freq_genre2.xlsx")


# Freq third genre
freq3 <- cbind(Freq = table(movies4$genre3), 
               Cumul = cumsum(table(movies4$genre3)), 
               relative = round((prop.table(table(movies4$genre3))*100),2))
library(xlsx)
write.xlsx(freq3, "freq_genre3.xlsx")


# year's release distribution
head(movies4)
hist(movies4$year, main="Year's movies distribution", xlab="year")
boxplot(movies4$year, main="Boxplot of year")

summary(movies4$year)

detach(movies4)

##################################    Ratings     ##########################################

attach(ratings)

# counts the amount of movies per user
t <- aggregate(cbind(count = movie_id) ~ user_id, 
               data = ratings, 
               FUN = function(x){NROW(x)})

summary(t$count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    1.00    2.00   11.93    7.00 1583.00 

t$new <- ifelse(t$count==1, "1",
                ifelse(t$count==2, "2",
                       ifelse(t$count==3, "3",
                              ifelse(t$count==4, "4",
                                     "5"  ))))
freq <- cbind(Freq = table(t$new), 
              Cumul = cumsum(table(t$new)), 
              relative = round((prop.table(table(t$new))*100),2))

library(xlsx)
write.xlsx(freq, "freq_movies_per_user.xlsx")


# counts the amount of ratings per movie
t2 <- aggregate(cbind(count = rating) ~ movie_id, 
               data = ratings, 
               FUN = function(x){NROW(x)})

summary(t2$count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    1.00    2.00   20.64    7.00 2966.00 

attach(t2)
t2$new <- ifelse(count==1, "1",
                ifelse(count==2, "2",
                       ifelse(count==3, "3",
                              ifelse(count==4, "4",
                                     ">4"  ))))


# freq rating
freq <- cbind(Freq = table(ratings$rating), 
              Cumul = cumsum(table(ratings$rating)), 
              relative = round((prop.table(table(ratings$rating))*100),2))

# graph rating
library(ggplot2)
library(scales)
vector_ratings <- as.vector(ratings$rating)
unique(vector_ratings)
vector_ratings <- factor(vector_ratings)
qplot(vector_ratings, ylim = c(0,150000), xlab="Ratings", ylab="Amount") + ggtitle("Distribution of ratings")


################################## Movies and Ratings #########################################

# merge rating and movies to do bivariate analysis
ratings2 <- data.frame(ratings, movie_id_n = as.numeric(ratings$movie_id))
f <- merge(x=ratings2, y=movies4,by="movie_id_n", all.x=TRUE)


# Ratings x Main Genre

# select the 9 main genre (reprsent 89% of all genre)
top9 <- c('Comedy','Drama','Action','Documentary','Horror','Crime','Adventure','Animation')

f2 <- f[f$genre1 %in% top9,]
boxplot(rating ~ genre1, data=f2, xlab="Main genre",
        ylab="Ratings",
        col=c("wheat","lightblue","thistle","light green","tomato","gray","pink","blue"))
     
# Rating x Year

# select movies released starts in 1980
f3<- f[f$year >= 1980,]
boxplot(year ~ rating, data=f3)


# Calculate correlation
cor(year,rating,method=c("pearson"))

# One-way ANOVA test
aov1 = aov(rating ~ genre1, data=f2)
summary(aov1)


