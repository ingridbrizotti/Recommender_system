############################### IMPORT THE DATA #################################################
getwd()
setwd("/Users/ingridbrizotti/Desktop/Ryerson/3.Data_Analytics_Capstone/MovieTweetings/latest")

# MOVIES #
mov <- readLines("movies.dat")
head(mov)
mov <- gsub("::", "*", mov)
movies <- read.table(text=mov, sep="*", header=FALSE, stringsAsFactor=TRUE, na.strings = "EMPTY", 
                     fileEncoding="UTF-8",fill = TRUE, quote = "")
colnames(movies) <- c("movie_id","movie_title_year","genre")
head(movies)

# RATINGS #
rat <- readLines("ratings.dat")
head(rat)
rat <- gsub("::", "*", rat)
ratings <- read.table(text=rat, sep="*", header=FALSE, stringsAsFactor=TRUE, na.strings = "EMPTY", 
                      fileEncoding="UTF-8",fill = TRUE, quote = "")
head(ratings)
colnames(ratings) <- c("user_id","movie_id","rating","rating_timestamp")
head(ratings)

# USERS  #
use <- readLines("users.dat")
head(use)
use <- gsub("::", "*", use)
users <- read.table(text=use, sep="*", header=FALSE, stringsAsFactor=TRUE, na.strings = "EMPTY", 
                    fileEncoding="UTF-8",fill = TRUE, quote = "")
head(users)
colnames(users) <- c("user_id","twitter_id")
head(users)

############################## Save the data base in R data sets ###############################

save(movies,file="movies.Rda")
save(ratings,file="ratings.Rda")
save(users,file="users.Rda")

############################# CHECK DUPLICITY AND MISSING ######################################

# Movies #

# Transform in matrix (it was a factor)
movies<- as.data.frame.matrix(movies)
str(movies)

# Tranform movie_id in numeric
movies2 <- data.frame(movies, movie_id_n = as.numeric(movies$movie_id))
summary(movies2$movie_id_n)

# Check duplicity of movie_id
dupli_m <- movies2[duplicated(movies2$movie_id_n),] 
# remove the duplicates
movies2 <- movies2[!duplicated(movies2$movie_id_n),]


# Check for NAs
missing <- movies2[is.na(movies2$movie_id_n),]
# Exclude NAs (7 observations)
movies2 <- movies2[!is.na(movies2$movie_id_n),]
summary(movies2$movie_id_n)


# Ratings #

missing <- ratings[is.na(ratings$user_id),]
missing <- ratings[is.na(ratings$movie_id),]

# Users #

# Check duplicity of user_id
dupli_u <- users[duplicated(users$user_id),] 

# Check for NAs
missing <- users[is.na(users$user_id),]
missing_t <- users[is.na(users$twitter_id),]


############################# EXTRACT NEW VARIABLES ######################################

# Movies #

# import the original movies datset to bring genre in separate columns

g <- readLines("movies.dat")
head(g)
g2 <- gsub("::", "|", g)
g2 <- read.table(text=g2, sep="|", header=FALSE, fill=TRUE, quote = "")
g2 <- g2[,-(2),drop=FALSE] 
colnames(g2) <- c("movie_id","genre1","genre2","genre3")
g2<- as.data.frame.matrix(g2)
g2 <- data.frame(g2, movie_id_n = as.numeric(g2$movie_id))
summary(g2$movie_id_n)

# merge files
movies3 <- merge(movies2,g2,by="movie_id_n")
movies3 <- subset(movies3, , -c(movie_id.x, movie_id.y))


# Split the variable movie_title_year to bring the year in a separate column
dt_year <- data.frame(do.call('rbind', strsplit(as.character(movies3$movie_title_year),'(',fixed=TRUE)))
dt_year2 <- data.frame(do.call('rbind', strsplit(as.character(dt_year$X2),')',fixed=TRUE)))
plus <- NA
dt_year3 <- rbind(dt_year2,plus)

movies4 <- cbind(movies3,dt_year3)
colnames(movies4) <- c("movie_id_n","movie_title_year","genre","genre1","genre2","genre3","year")
head(movies4)

# Transform year to numeric
movies4$year <- as.numeric(levels(movies4$year))[movies4$year]

# Save the dataset
save(movies4,file="movies4.Rda")
write.table(movies4,file = "movies4.csv",sep = " ", quote = FALSE, append = FALSE, na = "NA")


