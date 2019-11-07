# Sample program for using rtweet, sentiment analysis
# Use vignette("auth", package = "rtweet") for authentication
# Documentation: vignette("intro", package = "rtweet")


library(rtweet)
library(dplyr)
library(ggplot2)
library(tidytext)
library(data.table)

Dallastrend=get_trends("dallas")
ustrend=get_trends("United States")

# Download tweets by keyword, note to use retryonratelimit (may take hours!)
impch <- search_tweets("impeach", n=100000, retryonratelimit = TRUE)

# It is important to save the file in R and csv format.
save(impch, file="impch_11072019.RData")

# Saving to Excel directly is not recommended!
# fwrite is fast and neat! :)
fwrite(impch, file = "impch_11072019.csv")

# Map it
rt <- lat_lng(impch)
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
with(rt, points(lng, lat, pch = 16, cex = .5, col = rgb(0, .3, .7, .75)))

# Plot by time

ts_plot(impcrt,"mins",cex=.25,alpha=1) +
  theme_bw() +
  theme(text = element_text(family="Apple Garamond"),
        plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),plot.caption = element_text(hjust = 0.5)) +
  labs(title = "Frequency of keyword 'impeach' used in last 100,000 Twitter tweets",
       subtitle = "Twitter tweet counts aggregated per minute interval ",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet",hjust = 0.5)


# Get timeline

tmls <- get_timelines(c("realDonaldTrump","JoeBiden","AOC"), n = 5000,retryonratelimit = TRUE)

## plot the frequency of tweets for each user over time

tmls %>%
  dplyr::filter(created_at > "2016-10-29") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by Donald Trump, JB, AOC",
    subtitle = "Twitter status (tweet) counts aggregated by day",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# Get users by hashtag
usrsfhk <- search_users("#FreeHK", n = 1000)

## More text analytics
## Sentiment analysis

textDF <- tibble(impch$text)

tidytwt= textDF %>% 
  unnest_tokens(word, impch$text)
# Call in the stop word dictionary
library(tm)
library(data.table)
data(stop_words)

## Adding "trump" as stop word
nws_x=array(c("trump","Trump","https"))
nws_y=rep("custom", each=dim(nws_x))
newsw <- data.frame( "word" = nws_x, "lexicon" = nws_y)
stop_words1=do.call("rbind", list(newsw, stop_words))

tidytwt <- tidytwt %>%  anti_join(stop_words1)


tidytwt %>%
  count(word, sort = TRUE) %>%
  filter(n > 5000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab("Keyword") + ylab("Count") +
  coord_flip() + theme_bw()

tidytwt <- tidytwt %>%
  mutate(linenumber = row_number())

# Joining bing lexicon using on average tweet of 12 words.

sentiment_imp <- tidytwt %>%          
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 12, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
ggplot(sentiment_imp, aes(index, sentiment)) +
  geom_col(show.legend = FALSE)+theme_bw()

# Positive and negative balance
sentiment_imp$posneg=ifelse(sentiment_imp$sentiment>0,1,ifelse(sentiment_CTT$sentiment<0,-1,0))

# Use Plotly library to plot density chart
library(plotly)
ggplot(sentiment_imp, aes(sentiment, fill = posneg)) + 
geom_density(alpha = 0.5, position = "stack") + 
  ggtitle("stacked sentiment density chart")+theme_bw()


# ggplotly(p)
ggsave("impeach_sentden.svg", device=svg, dpi=600)
bing_word_counts <- tidytwt %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Impeachment sentiments, November 7, 2019",
       x = NULL) +
  coord_flip() + theme_bw()+ theme(strip.text.x = element_text(family="Apple Garamond"), 
                                   axis.title.x=element_text(face="bold", size=15,family="Apple Garamond"),
                                   axis.title.y=element_text(family="Apple Garamond"), 
                                   axis.text.x = element_text(family="Apple Garamond"), 
                                   axis.text.y = element_text(family="Apple Garamond"))
ggsave("impeach_sentiments.svg", device=svg, dpi=600)

