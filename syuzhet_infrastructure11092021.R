# Sample program for using rtweet, syuzhet for sentiment analysis
# Be sure you get Twitter developer account
# Use vignette("auth", package = "rtweet") for authentication
# Documentation: vignette("intro", package = "rtweet")
vignette("auth", package = "rtweet")
# install.packages("tidytext")
library(rtweet)
library(dplyr)
library(ggplot2)
library(tidytext)
library(data.table)
install.packages("syuzhet")
library(syuzhet) # sentiment analysis package 
library(lubridate) # for time/date data management

# Practice using some functions in rtweet
# Get trend
Dallastrend=get_trends("dallas")
ustrend=get_trends("United States")

# Download tweets by keyword, note to use retryonratelimit (may take hours!)
infra <- search_tweets("infrastructure deal", n=10000, retryonratelimit = TRUE)
infra_sm <- search_tweets("infrastructure deal", n=1000, retryonratelimit = TRUE)

# It is important to save the file in R and csv format.
save(infra, file="infrastructure_11092021.RData")

# Saving to Excel directly is not recommended!
# fwrite is fast and neat! :)
data.table::fwrite(infra, file = "infrastructure_11092021.csv")

# Map it
geo_inf <- lat_lng(infra)
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .55)

with(geo_inf, points(lng, lat, pch = 20, cex = 1, col = "firebrick"))
# with(geo_inf, points(lng, lat, pch = 16, cex = 1, col = rgb(0, .3, .7, .75)))

# Plot by time

ts_plot(infra,"mins",cex=.25,alpha=1) +
  theme_bw() +
  theme(text = element_text(family="Palatino"),
        plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),plot.caption = element_text(hjust = 0.5)) +
  labs(title = "Frequency of keyword 'infrastructure or Build Back Better' used in last 100,000 Twitter tweets",
       subtitle = "Twitter tweet counts aggregated per minute interval ",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet",hjust = 0.5)

# Sentiment analysis

library(syuzhet)
infratweets <- iconv(infra$text)
infra_sent <- get_nrc_sentiment(infratweets)
barplot(colSums(infra_sent),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Tweets')