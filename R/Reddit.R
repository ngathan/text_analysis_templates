# This template is used for cleaning text data from Reddit 
# I use Pushshift.io to get reddit data. More information here: http://pushshift.io/
# The output of this process is a data that is ready for topic modeling analysis 


install.packages("rjson")
install.packages("jsonlite")
install.packages("runner")
install.packages("cld2")
install.packages("tm")
install.packages("NLP")
library(NLP)
library(tm)
library(tidyverse)
library(cld2)
library("rjson")
library(jsonlite)
library (tidyverse)
library(lubridate)
library(ggplot2)
library(tidyquant)
library(runner)
library(stringi)

# Read json data into R 
# The data from pushshift.io comes in a json format. 
# create an R dataframe for readability, and scrutinization. 

json_file <- "data.json"
json_data <- fromJSON(json_file, flatten = TRUE)
data <- as.data.frame(do.call("cbind", json_data))


# If you want to work with date for time series analysis for example, converting utc date to normal day-moth-year format is helpful 

data$date <-  as_datetime(data$data.created_utc)

# get the date only  

data$date2 <-  as_date(data$date)
dput(max(data$date2) - min(data$date2)) # Number of days in the dataset 

# Examine the most active users

userpost <- table(data$data.author)
userpost <- as.data.frame(userpost)

# in this data set 1139 is the cutoff point for the top 10 users 
# this code could be improved/more elegant 

top_10 <- subset(userpost, userpost$Freq>=1139)

save(data  , file = "data.RData")
save(top_10  , file = "top_10.RData")

# Creating a subset of variables of interest 
# After getting reddit data from Pushshitf.io, I merged three fields submission,comments and self_text into one merged_text field. 
# The data that I want to examine lives in merged_text 

myvars <- c("data.author","data.author_fullname",
            "data.id", "data.domain", 
            "data.updated_utc", "data.created_utc",
            "date","date2","data.full_link",
            "data.is_original_content", "data.is_self",
            "data.num_comments", "data.over_18",
            "data.merged_text", 
            "data.body", "data.selftext","data.title",
            "data.subreddit", "data.subreddit_id",
            "data.domain","data.subreddit","data.subreddit_id", 
            "data.num_comments","data.score", "data.stickied",
            "data.no_follow",
            "data.subreddit_subscribers",
            "data.parent_id","data.link_id",
            "data.is_crosspostable", "data.is_video",
            "data.gilded")

mydata <- data[myvars]

# create a new language column to check what kind of language is each post in
# Using Google cld2 package to detect language of each document 

mydata$lang <- detect_language(mydata$data.merged_text)
save(mydata  , file = "mydata.RData")

# Keep only documents in English or not deteremined by algorithm 

mydata_clean <- subset(mydata, is.na(lang)| ( mydata$lang == "en") )
save(mydata_clean  , file = "mydata_clean.RData")

# Remove a few redundant words [removed], [deleted]
# There are many [removed] and [deleted] documents in reddit datasets.
# They are possibly posts that have been removed by moderators

mydata_clean$data.merged_text <- gsub("\\[removed]","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\[deleted]","", mydata_clean$data.merged_text)

#subsetting a file that has no empty rows (resulted from removing [removed] and [deleted] above)

mydata_clean <- subset(mydata_clean, mydata_clean$data.merged_text != "")

# Using tidyverse to remove emojis  

mydata_clean$data.merged_text <- iconv(mydata_clean$data.merged_text, "latin1", "ASCII", sub="")

# Again subsetting a file that has no empty rows (resulted from removing emjois above): this step could be combined with the subsetting function above.
# Yet I do them separately to ensure that I do each step right. 

mydata_clean <- subset(mydata_clean, mydata_clean$data.merged_text != "")

# Manually remove some emojis that did not get removed from automation 

mydata_clean$data.merged_text <- gsub("\\&lt;3","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\:)","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\&lt;3","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\:/","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\:D","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\( X )","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\:'D","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\*)_$","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\ಠ_ಠ","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\ʕ •ᴥ•ʔ","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\;)","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\ಥ_ಥ","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\つ ◕_◕ )つ","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\ZUCC.jpg","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\[ &#x200B","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\&#x200B","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\#x200B;","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\&amp","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\&gt;","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\#x200B","", mydata_clean$data.merged_text)
mydata_clean$data.merged_text <- gsub("\\&gt","", mydata_clean$data.merged_text)


# drop the empty rows 

mydata_clean <- subset(mydata_clean, mydata_clean$data.merged_text != "")

# remember to save data! 

save(mydata_clean  , file = "mydata_clean.RData")


# Remove URL using stringi

removeURL <- function(x){gsub("http[[:alnum:][:punct:]]*", "", x)}
mydata_clean$data.merged_text <- removeURL(mydata_clean$data.merged_text)

# drop empty rows 
mydata_clean <- subset(mydata_clean, mydata_clean$data.merged_text != "")
save(mydata_clean  , file = "mydata_clean.RData")

# Manually remove bots. I really do not have a list of bots on reddit to automate this process 
# Bots are very active, and contribute a lot to the content production on reddit. 
# I want to focus my topic model analysis on community members' contribution only 

mydata_clean <- subset(mydata_clean, data.author !="__a_lot_bot__")
mydata_clean <- subset(mydata_clean, data.author !="alphabotical")
mydata_clean<- subset(mydata_clean, data.author !="YTubeInfoBot")
mydata_clean<- subset(mydata_clean, data.author !="youtubefactsbot")
mydata_clean<- subset(mydata_clean, data.author !="yourewelcome_bot")
mydata_clean<- subset(mydata_clean, data.author !="YoMommaJokeBot")
mydata_clean<- subset(mydata_clean, data.author !="WikiTextBot")
mydata_clean<- subset(mydata_clean, data.author !="video_descriptionbot")
mydata_clean<- subset(mydata_clean, data.author !="trpbot")
mydata_clean<- subset(mydata_clean, data.author !="ToxicityReportBot")
mydata_clean<- subset(mydata_clean, data.author !="totes_meta_bot")
mydata_clean<- subset(mydata_clean, data.author !="ToMetricBot")
mydata_clean<- subset(mydata_clean, data.author !="these_days_bot")
mydata_clean<- subset(mydata_clean, data.author !="tiny_smile_bot")
mydata_clean<- subset(mydata_clean, data.author !="TitleLinkHelperBot")
mydata_clean<- subset(mydata_clean, data.author !="ThePickupLineBot")
mydata_clean<- subset(mydata_clean, data.author !="SovietRussiaBot")
mydata_clean<- subset(mydata_clean, data.author !="SmileBot-2020")
mydata_clean <- subset(mydata_clean, data.author !="SuicideAwarenessBot")
mydata_clean<- subset(mydata_clean, data.author !="CommonMisspellingBot")
mydata_clean<- subset(mydata_clean, data.author !="thank_mr_skeltal_bot")
mydata_clean<- subset(mydata_clean, data.author !="BooCMB")
mydata_clean <- subset(mydata_clean, data.author !="BooBCMB")
mydata_clean<- subset(mydata_clean, data.author !="haikubot-1911")
mydata_clean <- subset(mydata_clean, data.author !="sneakpeekbot")
mydata_clean <- subset(mydata_clean, data.author !="B0tRank")
mydata_clean<- subset(mydata_clean, data.author !="couldshouldwouldbot")
mydata_clean <- subset(mydata_clean, data.author !="tupac_cares_bot")
mydata_clean<- subset(mydata_clean, data.author !="Darnit_Bot")
mydata_clean <- subset(mydata_clean, data.author !="LimbRetrieval-Bot")
mydata_clean <- subset(mydata_clean, data.author !="FatFingerHelperBot")
mydata_clean <- subset(mydata_clean, data.author !="RedditSilverRobot")
mydata_clean <- subset(mydata_clean, data.author !="these_days_bot")
mydata_clean <- subset(mydata_clean, data.author !="agree-with-you")
mydata_clean<- subset(mydata_clean, data.author !="_youtubot_")
mydata_clean<- subset(mydata_clean, data.author !="_YOU_DROPPED_THIS_")
mydata_clean <- subset(mydata_clean, data.author !="aardBot")
mydata_clean <- subset(mydata_clean, data.author !="AnimalFactsBot")
mydata_clean <- subset(mydata_clean, data.author !="autowikibot")
mydata_clean <- subset(mydata_clean, data.author !="autoarguebot")
mydata_clean <- subset(mydata_clean, data.author !="BadDadBot")
mydata_clean <- subset(mydata_clean, data.author !="BigLebowskiBot")
mydata_clean <- subset(mydata_clean, data.author !="Bot_Metric" & 
                      data.author !="BotPaperScissors" &
                      data.author !="BOTS_RISE_UP"&
                      data.author !="by-accident-bot")
mydata_clean<- subset(mydata_clean, data.author !="CakeDay--Bot" & 
                     data.author !="CantHearYouBot" &
                     data.author !="Chuck_Norris_Jokebot"&
                     data.author !="classhole_bot"&
                     data.author !="ClickableLinkBot")
mydata_clean <- subset(mydata_clean, data.author !="colorcodebot" & 
                      data.author !="ComeOnMisspellingBot" &
                      data.author !="comment_preview_bot"&
                      data.author !="ComplimentingBot"&
                      data.author !="converter-bot")
mydata_clean <- subset(mydata_clean, data.author !="Defiantly_Not_A_Bot" & 
                      data.author !="doggobotlovesyou" &
                      data.author !="DollarSignBot"&
                      data.author !="gifv-bot"&
                      data.author !="good-Human_Bot")
mydata_clean <- subset(mydata_clean, data.author !="gram_bot" & 
                      data.author !="grootbot" &
                      data.author !="HappyNumberBot"&
                      data.author !="have_bot"&
                      data.author !="HelperBot_")
mydata_clean<- subset(mydata_clean, data.author !="icarebot" & 
                     data.author !="image_linker_bot" &
                     data.author !="Image-to-Braille-Bot"&
                     data.author !="JobsHelperBot"&
                     data.author !="lerobinbot")
mydata_clean <- subset(mydata_clean, data.author !="hotlinehelpbot" & 
                      data.author !="Mentioned_Videos" &
                      data.author !="LinkReplyBot"&
                      data.author !="MCTerminologyBot"&
                      data.author !="PayRespects-Bot")
mydata_clean <- subset(mydata_clean, data.author !="Polite_Users_Bot" & 
                      data.author !="RemindMeBot" &
                      data.author !="serendipitybot"&
                      data.author !="stfubot"&
                      data.author !="Subjunctive__Bot")

mydata_clean<- subset(mydata_clean, data.author !="note-to-self-bot" & 
                      data.author !="VerseBot" &
                      data.author !="TCT_Bot"&
                      data.author !="autourbanbot"&
                      data.author !="smilesbot")
mydata_clean <- subset(mydata_clean, data.author !="SadFaceBot")
mydata_clean <- subset(mydata_clean, data.author !="AutoModerator" &
                      data.author !="ObamaRobot")
mydata_clean <- subset(mydata_clean, data.author !="ComeOnComeOnMB" & 
                      data.author !="friendly-bot" &
                      data.author !="imgurtranscriber"&
                      data.author !="qkme_transcriber")
mydata_clean <- subset(mydata_clean, data.author !="hearingaid_bot" & 
                      data.author !="I-Am-Dad-Bot" &
                      data.author !="dadjokes_bot"&
                      data.author !="Extra_Cheer_Bot")

mydata_clean <- subset(mydata_clean, data.author !="allinonebot" & 
                      data.author !="bowie747" &
                      data.author !="fatFingersbot"&
                      data.author !="SmallSubBot")
mydata_clean <- subset(mydata_clean, data.author !="the_timezone_bot" & 
                      data.author !="The-Worst-Bot" &
                      data.author !="timezone_bot"&
                      data.author !="totes_meta_bot")
mydata_clean<- subset(mydata_clean, data.author !="BeTheChangeBot" & 
                      data.author !="CMBDeletebot" &
                      data.author !="HonestlyBot"&
                      data.author !="imguralbumbot" & data.author !="Fiverr_Bot")

#remove the usertags to ensure anonymity per IRB request 
# User tag on reddit has the form of \\u\username 

RemoveName <- function(x){gsub("@\\w+", " ", x)}
mydata_clean$data.merged_text<- RemoveName(mydata_clean$data.merged_text)

# drop empty rows 

mydata_clean <- subset(mydata_clean, mydata_clean$data.merged_text != "")
save(mydata_clean  , file = "mydata_clean.RData")

# Create a lenghth sentence variable 

library(stringi)
mydata_clean$length <- sapply(strsplit(mydata_clean$data.merged_text, " "), length)

# save the final data for other type of analyses 

save(mydata_clean, file = "mydata_clean.RData")


