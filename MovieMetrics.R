#make connection with SQL server "imdb"
library(RMySQL)
mydb <- dbConnect(MySQL(), user='root', password='romale10', dbname='imdb', host='localhost' )

#list tables in imdb database
dbListTables(mydb)

#run test query
kind_type <- dbGetQuery(mydb, "select * from kind_type")

####read in files####
movie_info <- dbGetQuery(mydb, "select * from movie_info")
info_type <- dbGetQuery(mydb, "select * from info_type")
title = dbGetQuery(mydb, "select * FROM title where kind_id = 1 and production_year >= 2000") 


####merge build flat files into relational database####
#movie info and movie info type
movie.info <- merge(movie_info,info_type,by.x=("info_type_id"),by.y=c("id"))

#drop unpopulated note and id columns
movie.info[1:2] <- list(NULL)
movie.info[3] <- NULL

#transpose movie info to get one row per movie
movie_info_wide <- reshape(movie.info, timevar="info.y", idvar=c("movie_id"), direction="wide")

#merge movie info onto  title list
parent_movie_table <- merge(title,movie_info_wide,by.x=("id"),by.y=c("movie_id"))

#merge kind to main movie list
parent_movie_table <- merge(parent_movie_table,kind_type,by.x=("kind_id"),by.y=c("id"))

#limit to only movies produced from 2000 onwards
limited_parent <- subset(parent_movie_table, kind_id==1 & production_year >= 2000)

#keep only budget and sales data reported in USD
limited_parent2 <- limited_parent[grepl("$", limited_parent$info.x.budget, fixed = TRUE), ]
limited_parent3 <- limited_parent2[grepl("$", limited_parent2$info.x.gross, fixed = TRUE), ]

#remove non-numeric characters from budget and gross and convert to numeric
limited_parent3$budget2 <-as.numeric(gsub('[$,]','',as.character(limited_parent3$info.x.budget)))
limited_parent3$gross2<-as.numeric(gsub("[^\\d]+", "", sub(" .*", "", limited_parent3$info.x.gross), perl=TRUE))

str(limited_parent3)
View(limited_parent3)
install.packages("ggplot2")
library(ggplot2)
ggplot(limited_parent3)

budgetsquared <- limited_parent3$budget2^2

model1 <- lm(limited_parent3$gross2~limited_parent3$budget2, data = limited_parent3)
model2 <- lm(limited_parent3$gross2~limited_parent3$budget2 + budgetsquared, data = limited_parent3)

summary(model2)
plot(model2)
#close mysql connection
on.exit(dbDisconnect(con))
