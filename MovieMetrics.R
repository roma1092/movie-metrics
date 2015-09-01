###
#Authors: Flavio Cangemi and Max Mucenic
#
#The purpose of this program is to clean and parse movie data, merge it into a parent table, and perform a statistical #analysis of the gross box office income. The ultimate aim of the project is to develop a predictive model of box office #income. 
#
#The data was downloaded from IMDb servers via a modified version of the IMDbPY script. For more information, please visit #http://imdb.com and http://imdbpy.sourceforge.net/

#make connection with SQL server "imdb"
library(RMySQL)
mydb <- dbConnect(MySQL(), dbname='imdb', host='localhost')

#list tables in imdb database
dbListTables(mydb)

#run test query
kind_type <- dbGetQuery(mydb, "select * from kind_type")

####read in file####
movie_info <- dbGetQuery(mydb, "select * from movie_info")
info_type <- dbGetQuery(mydb, "select * from info_type")
names = dbGetQuery(mydb, "select * from Name")
cast_info = dbGetQuery(mydb, "select * from cast_info")
title = dbGetQuery(mydb, "select * FROM title where kind_id = 1 and production_year >= 2000") 
role_type = dbGetQuery(mydb, "select * from role_type")
comp_cast = dbGetQuery(mydb, "select * from complete_cast")
movie_info_idx = dbGetQuery(mydb, "select * from movie_info_idx")

#merge flat tables into relational database#
#movie info and movie info type
movie_info2 <- merge(movie_info,info_type,by.x=("info_type_id"),by.y=c("id"))

#drop unpopulated note and id columns
movie_info2[1:2] <- list(NULL)
movie_info2[3] <- NULL

#transpose movie info to get one row per movie
movie_info_wide <- reshape(movie_info2, timevar="info.y", idvar=c("movie_id"), direction="wide")

#merge movie info onto title list
parent_movie_table <- merge(title,movie_info_wide,by.x=("id"),by.y=c("movie_id"))

#merge kind to main movie list
parent_movie_table <- merge(parent_movie_table,kind_type,by.x=("kind_id"),by.y=c("id"))

#limit to only movies produced from 2000 onwards
limited_parent <- subset(parent_movie_table, kind_id==1 & production_year >= 2000)

#limit to where budget and gross ticket sales are populated
limited_parent2<-limited_parent[!rowSums(is.na(limited_parent[c("info.x.budget","info.x.gross")])), ]

#keep only budget and sales data reported in USD
limited_parent3 <- limited_parent2[grepl("$", limited_parent2$info.x.budget,fixed=TRUE), ]
limited_parent4 <- limited_parent3[grepl("$", limited_parent3$info.x.gross,fixed=TRUE), ]

#remove non-numeric characters from budget and gross and convert to numeric
limited_parent4$budget2 <- as.numeric(gsub('[$,]','',as.character(limited_parent4$info.x.budget)))
limited_parent4$gross2 <- as.numeric(gsub("[^\\d]+", "", sub(" .*", "", limited_parent4$info.x.gross), perl=TRUE))


####ANALYSIS####
#initial plots#
library(scales)

#scatter
qplot(data=popular_genres,x=budget2,y=gross2,color=info.x.genres) +
  scale_y_continuous(labels = comma) +
  labs(x = "Budget ($)", y = "Box Office Gross ($)")
ggsave('Scatter Gross by Budget.png')

#box and whisker
ggplot(data=popular_genres, aes(x=info.x.genres, y=gross2)) + 
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(x = "Genre", y = "Box Office Gross ($)")
ggsave('Box and Whisker by Genre.png')

#interaction plot
ggplot(data=popular_genres, aes(x=budget2, 
                                y=gross2, 
                                colour=info.x.genres)) + 
  geom_jitter() +
  geom_smooth(aes(budget2,gross2)) +
  facet_wrap(~ info.x.genres,) +
  theme(legend.position="none") +
  labs(x = "Budget ($)", y = "Box Office Gross ($)")
ggsave('Genre and Budget Plot.png')

###Regressions###
#bivariate#
model1 <- lm(gross2~budget2, data = limited_parent4)

#subset to only genres with more than 20 observations
popular_genres <- subset(limited_parent4, table(info.x.genres)[info.x.genres] >= 20)

#convert genre to factor#
popular_genres$info.x.genres <- as.factor(popular_genres$info.x.genres)

#multivariate interaction#
model2 <- lm(gross2~info.x.genres*budget2, data = popular_genres)

#output table of regression results#
library(stargazer) 
stargazer(model1, type="html",
          dep.var.labels="Gross Ticket Sales ($)",
          out="model1.htm")
stargazer(model2, type="html",
          dep.var.labels="Gross Ticket Sales ($)",
          out="model2.htm")

#output predicted regression lines
qplot(gross2, budget2, data = popular_genres,
  geom = c("point","smooth"), method = "lm",  main="Effect of Budget on Box Office Gross" ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(x = "Budget ($)", y = "Box Office Gross ($)")
ggsave('Model 1 Plot.png')

qplot(gross2, budget2, data = popular_genres, colour = info.x.genres,
  geom = c("point","smooth"), method = "lm",size=I(1.5),main="Effect of Budget on Box Office Gross by Genre" ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(x = "Budget ($)", y = "Box Office Gross ($)")
ggsave('Model 2 Plot.png')

qplot(gross2, budget2, data = popular_genres, colour = info.x.genres,
      geom = "smooth", method = "lm",main="Effect of Budget on Box Office Gross by Genre" ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(x = "Budget ($)", y = "Box Office Gross ($)")
ggsave('Model 2 Plot wo Points.png')

#close mysql connection
on.exit(dbDisconnect(con))
