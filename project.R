#Importing libraries
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(psych)

#clear all variables
rm(list=ls())

#Creating the path to read the tables
path <- 'C:/Users/mathe/OneDrive/Área de Trabalho/basketball.sqlite'

#Creating the connection with the SQLite
con <- dbConnect(SQLite(), 
                 dbname = path)

#Checking all the tables in the database
alldat <- lapply(setNames(nm = dbListTables(con)), dbReadTable, conn = con)


#1) Does a team's payroll positively influence in the number of wins?

#getting the table to analyze
player_salary <- dbReadTable(con, name = 'Player_salary')

#manipulating data to group it by team and getting the salaries in millions
by_team <- player_salary %>% group_by(nameTeam) %>% summarise(total_salary_millions = sum(value)/1000000) %>% arrange(total_salary_millions)

#creating the visualization                                                     
ggplot(data=by_team, aes(x=reorder(nameTeam, -total_salary_millions), y=total_salary_millions)) +
  geom_bar(stat="identity", color="blue", fill="white") +
  labs(title = "Total Salary in millions ($) per team", x = "Team", y = "Salary in millions ($)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Based on the data, we can see that Bucks, Utah Jazz and Golden State Warriors were the teams that spent more with salary. 
#Now I will check the % of wins for each team based on how many games they played. 


#query to get the win percentage per team
query3 = "
    SELECT 
        TEAM_ID_HOME AS team_id,
        TEAM_NAME_HOME AS team_name,
        SUM(CASE WL_HOME 
                WHEN'W' THEN 1
                ELSE 0
            END) AS win_count,
        COUNT(TEAM_ID_HOME) AS team_game_count
    FROM Game
    GROUP BY SEASON_ID, TEAM_ID_HOME 
    
    UNION
    
    SELECT 
        TEAM_ID_AWAY AS team_id,
        TEAM_NAME_AWAY AS team_name,
        SUM(CASE WL_AWAY 
                WHEN'W' THEN 1
                ELSE 0
            END) AS win_count,
        COUNT(TEAM_ID_AWAY) AS team_game_count
    FROM Game
    GROUP BY SEASON_ID, TEAM_ID_AWAY
"

#Transforming the query in a data frame
wins <- dbGetQuery(con,query3)

#Creating a new variable to check the % of wins
wins$win_percentage <- round(100*wins$win_count/wins$team_game_count,2)
df_wins_grouped <- wins %>% group_by(team_name) %>% summarise(avg_wins = mean(win_percentage)) %>% arrange(-avg_wins) %>% slice(1:30)

#Creating the visualization
ggplot(data=df_wins_grouped, aes(x=reorder(team_name, -avg_wins), y=avg_wins)) +
  geom_bar(stat="identity", color="blue", fill="white") +
  labs(title = "Average % wins per team between 1949-2021", x = "Team", y = "% of wins") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#After checking the % of wins for each team I can tell that there is no correlation between the amount spent in salaries
#and the number of wins. Top 5 teams for wins (home and away) were Chicago, Los Angeles Clippers, Utah Jazz, Syracuse Nationals
#and Boston Celtics. These teams are not the ones who spent more in salaries. Besides of Utah Jazz and Boston Celtics and Los Angeles Clippers
#the rest of the teams does not show it. 



#2) The taller the player, the greater the chance of being chosen among the first in the draft? 

#Getting the height of the players and the number of pick in the draft
query1 <- 
  "SELECT 
                        Player_Attributes.ID AS Player_ID,
                        Player_Attributes.DISPLAY_FIRST_LAST AS Full_Name,
                        Draft.numberPickOverall AS Pick_Number_Overall,
                        Player_Attributes.Height AS Height
                        FROM Player_Attributes
                        JOIN Draft ON Player_Attributes.ID = Draft.idPlayer
                        ;"


#using the data from the query to have a dataframe
height <- dbGetQuery(con,query1)

#getting only the two variables to check the correlation
height_num <- height %>% select(Height, Pick_Number_Overall)

#correlation plot
corPlot(height_num)

#Based on the correlation plot we can see that, there is no correlation between the height and the number of pick in the draft. 


#3) Taller players make more points and rebounds?

#getting the table 
player_attributes <- dbReadTable(con, name = 'Player_Attributes')

#selecting the variables to check their correlation
stats_players <- player_attributes %>% select(REB, HEIGHT, AST, PTS, WEIGHT, SEASON_EXP)


#correlation between height, rebounds, points, assists, weight, experience
cor.plot(stats_players)

#So, the correlation between Rebounds and Height is 0.37, which is a positive, but weak correlation. Height and Points does not show any correlation
#but when talking about Rebounds and Points, we can state that players who make more rebounds has more chance to score more points.
#The correlation between rebounds and Points is 0.67, which is a positive moderate correlation. 

#4)Shorter and lighter players give more assists and score more points?

#Again, analyzing the correlation plot we can see that Height and Assists have a negative, but weak correlation, which is -0.35.
#This correlation says that the taller the player is, less assists he will do. Same when analyzing Weight and Assists. These variables
#have a negative, but weak correlation, which is -0.30. It means that the lighter is the player, more assists he will do. 
#Height and Weight do not show any correlation with Points scored. 


#5)Does a high average age negatively influence wins?

#Based on the correlation plot, there is a strong correlation between years of experience playing and number of points scored.
#It represents 0.62, which is a positive moderate correlation. It means that the more years playing, more points the player will score
#Thinking about it, the age will not influence negatively in the number of wins, because the more points you score, more chance you have to win. 
#But, lets check it using the age variable. 

#query to get the data necessary to the analysis
query <- "
    SELECT 
        namePlayer AS player_name,
        nameTeam AS team_name,
        2022 - strftime('%Y', Player_Attributes.BIRTHDATE) AS age,
        Player_Attributes.DRAFT_YEAR AS draft_year,
        2022 - Player_Attributes.DRAFT_YEAR AS years_in_NBA,
        Player_Attributes.POSITION AS game_position,
        ROUND(value/1000000) AS salary_in_millions
    FROM Player_Salary
    JOIN Player ON
        Player_Salary.namePlayer = Player.full_name
    JOIN Player_Attributes ON
        Player.ID = Player_Attributes.ID
    ORDER BY salary_in_millions DESC
    ;
"

#transforming the query in a data frame
df_age <- dbGetQuery(con,query)


#grouping it by age and getting the average mean
df_grouped_age <- df_age %>% group_by(team_name) %>% summarise(age_mean = mean(age)) %>% arrange(-age_mean)


#creating the visualization
ggplot(data=df_grouped_age, aes(x=reorder(team_name, -age_mean), y=age_mean)) +
  geom_bar(stat="identity", color="blue", fill="white") +
  labs(title = "Average age", x = "Team", y = "Average Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Using the last data we manipulated to check the number of wins, top 5 teams in number of wins were:
#Boston Celtics, New York Knicks, Los Angeles Lakers, Detroit Pistons and Chicago Bulls. 
#Based on the graph, we can see that Boston Celtics has one of the lowest average age and he was number one in number of wins.
#Los Angeles Lakers has the highest average age and was number three in number of wins. 
#If you check the other teams, we can see that there is no correlation between these variables. 
#This is a case to investigate more deep.

#6) Does a player's number of points, assists or rebounds positively influence the salary?

#query to get the necessary data
query2 <- "
    SELECT 
        namePlayer AS player_name,
        nameTeam AS team_name,
        2022 - strftime('%Y', Player_Attributes.BIRTHDATE) AS age,
        Player_Attributes.DRAFT_YEAR AS draft_year,
        2022 - Player_Attributes.DRAFT_YEAR AS years_in_NBA,
        Player_Attributes.POSITION AS game_position,
        Player_Attributes.AST AS assists,
        Player_Attributes.PTS AS points,
        Player_Attributes.REB AS rebounds,
        Player_Attributes.HEIGHT AS height,
        Player_Attributes.WEIGHT AS weight,
        ROUND(value/1000000) AS salary_in_millions
    FROM Player_Salary
    JOIN Player ON
        Player_Salary.namePlayer = Player.full_name
    JOIN Player_Attributes ON
        Player.ID = Player_Attributes.ID
    ORDER BY salary_in_millions DESC
    ;
"

#transforming the query in a data frame
df_players<- dbGetQuery(con,query2)

#Creating a new variable where it will be the sum of assists, rebounds, points divided by 3
df_players$new_var <- (df_players$assists + df_players$rebounds + df_players$points)/3

#I created that variable because as higher as it is, better is gonna be the player. 

#selecting the variables and Checking the correlation between this new variable and salary
df_players_new_var <- df_players %>% select(new_var, salary_in_millions)
corPlot(df_players_new_var)

#We can confirm that there is a strong positive correlation between the number of rebounds, assists and points and the salary of the player.
#The correlation is 0.78 and it makes sense because players with good statistics are the ones who make difference. 


#selecting the variables to analyze it per position
stats_players_position <- player_attributes %>% select(FIRST_NAME, LAST_NAME, POSITION, REB, HEIGHT, AST, PTS, WEIGHT, SEASON_EXP)

#Before analyzing all the positions, I will set a scale for the new variable I created to decide if the player is good to hire or not
#For that, I will consider the most important skills for each position based on their correlations.
#For the model, it will be very important to predict if this is a good choice or not.
#Also, in the model, other variables will be used, such as height, weight and number of years playing.


#Getting the top 30 players to show it
df_10_new_var <- df_players %>% distinct(player_name, new_var) %>% arrange(-new_var) %>% slice(1:30)
df_10_new_var

#Based on the findings and in the question I want to answer, I will analyze separate each position to see the correlation with the most important stats. So the question is: 
#According to the player's position, which variables have more value and should become a reference to maximize the probability of success in hiring new players?


#Guards - usually is the best dribbler and passer of the team. He also is responsible for making many assists and score many points. Thats why
#in the top 10 high paying salaries, there is 7 guards.

#filtering only for guards
guards <- stats_players_position %>% filter(POSITION == "Guard" | POSITION == "Guard-Forward")
guards$new_var <- (guards$REB + guards$AST + guards$PTS)/3
  
#selecting variables to check the correlation
guard_corr <- guards %>% select(REB, HEIGHT, AST, PTS, WEIGHT, SEASON_EXP)

#correlation plot
corPlot(guard_corr)

#summary of each variable
summary(guards)

#For the Guards I will take Assists and Points as the most important variables and see the correlation with others.
#So, to be a good Guard, based on the correlations, the player has to have good numbers in rebounds, as the correlation with
#assists and points are strong and positive, 0.76 and 0.61. Also more years of experience playing is important as the correlation is also positive
#and relevant, 0.61 and 0.63. So, to hire a good guard, first of all he has to have good numbers in points and assists, after that good numbers
#in rebounds and lastly years of experience playing. Weight and Height does not show any difference. Budget is also important,
#because as showed, the best guards have the highest salaries.

#Building the model
#I will create a model to analyze Assists and another one to analyze Points



#Plotting histograms to decide the parameters to hire
hist(guards$PTS, main = "Points", xlab = "Number of Points", labels = TRUE, col = "lightblue")
abline(v = 6.681,                     # Add line for mean
       col = "red",
       lwd = 3)

#Points - 0 to 6 (Bad player) ; 6 to 10 (Common player) ; 10 to 15 (Good player) ; 15+ (Very Good player) 

hist(guards$AST, main = "Assists", xlab = "Number of Assists", labels = TRUE, col = "lightblue")
abline(v = 2.084,                     # Add line for mean
       col = "red",
       lwd = 3)


#Assists - 0 to 2 (Bad player) ; 2 to 4 (Common player) ; 4 to 6 (Good player) ; 6+ (Very Good player)

#Assists
guards_model_ast <- lm(AST ~ REB + PTS + SEASON_EXP + HEIGHT + WEIGHT, guards)
summary(guards_model_ast)

#as we can see, Weight is not statistically significant. So we will not use this variable. 

guards_model_ast_1 <- lm(AST ~ REB + PTS + SEASON_EXP + HEIGHT, guards)
summary(guards_model_ast_1)

#all the variables are significant statistically for the model, and the R squared is 65%, which is moderate. 


#Points
guards_model_pts <- lm(PTS ~ REB + AST + SEASON_EXP + HEIGHT + WEIGHT, guards)
summary(guards_model_pts)

#again, Weight is not significant statistically and we will not use this variable.

guards_model_pts1 <- lm(PTS ~ REB + AST + HEIGHT + SEASON_EXP, guards)
summary(guards_model_pts1)

#all the variables are significant and the R squared is 68%, which means that the independent variables explain the behavior 
#of the dependent variable 68% of the times. 


#Centers - usually the tallest player in the team and he is responsible for scoring points and rebounds as they play near the basket.

#filtering only for centers
centers <- stats_players_position %>% filter(POSITION == "Center")
centers$new_var <- (centers$REB + centers$AST + centers$PTS)/3

#selecting the variables to check the correlation
centers_corr <- centers %>% select(REB, HEIGHT, AST, PTS, WEIGHT, SEASON_EXP)

#correlation plot
corPlot(centers_corr)

#summary of each variable
summary(centers)

#For the Centers, I will take Rebounds and Points as the most important variables to analyze. The correlation shows that Rebounds and
#Points are highly correlated, 0.88, and it states that these two variables are the most common in Centers. After these two, we can see that
#years of experience has a moderate influence in rebounds and points scored, 0.63 and 0.58. Also, a good center is good assisting other players.
#The correlation between these two variables is 0.77. Lastly, the average height is 8"2. So, after analyzing rebounds and points, a good center
#needs years of experience playing and lastly assists. 

#Building the model
#I will create a model to analyze Rebounds and another one to analyze Points

#Plotting histograms to decide the parameters to hire
hist(centers$PTS, main = "Points", xlab = "Number of Points", labels = TRUE, col = "lightblue")
abline(v = 5.85,                     # Add line for mean
       col = "red",
       lwd = 3)

#Points - 0 to 6 (Bad player) ; 6 to 11 (Common player) ; 11 to 15 (Good player) ; 15+ (Very Good player) 

hist(centers$REB, main = "Rebounds", xlab = "Number of Rebounds", labels = TRUE, col = "lightblue")
abline(v = 4.42,                     # Add line for mean
       col = "red",
       lwd = 3)



#Rebounds
centers_model_reb <- lm(REB ~ AST + PTS + SEASON_EXP + HEIGHT + WEIGHT, centers)
summary(centers_model_reb)

#Height is not significant to predict rebounds, so we will not use it.

centers_model_reb1 <- lm(REB ~ AST + PTS + SEASON_EXP + WEIGHT, centers)
summary(centers_model_reb1)

#all the variables are significant and the R Squared is strong, 80%. The model is significant.


#Points
centers_model_pts <- lm(PTS ~ AST + REB + SEASON_EXP, centers)
summary(centers_model_pts)

#Weight and Height not significant. 
#For this model, all the variables are significant. and the R Squared is 81%.



#Forwards - they play offensively, but when defending the most important function for these players are rebounding. 


#Filtering only for forward
forwards <- stats_players_position %>% filter(POSITION == "Center-Forward" | POSITION == "Forward" | POSITION == "Guard-Forward")
forwards$new_var <- (forwards$REB + forwards$AST + forwards$PTS)/3

#selecting the variables to check the correlation
forwards_corr <- forwards %>% select(REB, HEIGHT, AST, PTS, WEIGHT, SEASON_EXP)

#correlation plot
corPlot(forwards_corr)

#summary of each variable
summary(forwards)

#For the Forwards, I will take Rebounds and Points to analyze as the most important variables. Again, years of experience playing have
#a important correlation with rebounds, 0.60, and points, 0.65. Another important correlation is assists with rebounds, 0.63, and with points 0.77.
#Height is also important for these players because they are responsible for rebounds. The average height is 7"5. After analyze, I can say
#that to hire a good forward, besides of good numbers in rebounds and points, he has to have good number in assists and years of experience playing.


#Building the model
#I will create a model to analyze Rebounds and another one to analyze Points

#Plotting histograms to decide the parameters to hire
hist(forwards$PTS, main = "Points", xlab = "Number of Points", labels = TRUE, col = "lightblue")
abline(v = 6.24,                     # Add line for mean
       col = "red",
       lwd = 3)

#Points - 0 to 6 (Bad player) ; 6 to 10 (Common player) ; 10 to 15 (Good player) ; 15+ (Very Good player) 

hist(forwards$REB, main = "Rebounds", xlab = "Number of Rebounds", labels = TRUE, col = "lightblue")
abline(v = 3.35,                     # Add line for mean
       col = "red",
       lwd = 3)


#Rebounds
forwards_model_reb <- lm(REB ~ AST + PTS + SEASON_EXP + HEIGHT + WEIGHT, forwards)
summary(forwards_model_reb)

#all the variables are significant, p value is very small and the R squared is 62%. 


#Points
forwards_model_pts <- lm(PTS ~ AST + REB + SEASON_EXP + HEIGHT + WEIGHT, forwards)
summary(forwards_model_pts)

#Height is not significant statistically and we will not use this variable.

forwards_model_pts1 <- lm(PTS ~ AST + REB + SEASON_EXP + WEIGHT, forwards)
summary(forwards_model_pts1)


#For this model, all the variables are significant the R squared is 77%.

#After all this analysis, I can tell that years of experience playing is very important to hire good players in basketball,
#no matter the position. This analysis can be improved if there is available data as number of blocks, number of minutes played per season, for example. 
#For each position we can see that some variables have more weigh than others. It is very important. 


#Disconnecting the database
dbDisconnect(con)

