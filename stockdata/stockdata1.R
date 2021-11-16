# Collecting Stock data and plotting stock data as time series
# install.packages(c("quantmod", "ggplot2", "magrittr","broom"))
# lapply(c("quantmod", "ggplot2", "magrittr","broom"), require, character.only = TRUE)
library(quantmod)
library(ggplot2)
library(magrittr)
library(broom)

# Setting time period
start = as.Date("2010-07-01") 
end = as.Date("2021-11-16")

# Download Taiwan Weighted Index
getSymbols("^TWII", src="yahoo") # TWSE:IND

# Download Dow Jones Industrial Average
getSymbols("DJIA", src="yahoo") # Dow Jones Industrial Average

# Simple plot
plot(TWII, col="darkblue")
     
# Plot candle stick and other charts using quantmod
chartSeries(DJIA)
chartSeries(DJIA, type = c("auto", "candlesticks", "matchsticks", "bars","line"), subset='last 4 months',theme = "white")
barChart(DJIA,multi.col=TRUE,theme = 'white')
lineChart(DJIA,line.type = 'l', theme = 'white') # line, choices include l, h, c, b
lineChart(DJIA,line.type = 'h',theme = chartTheme('white', up.col='steelblue')) # histogram
candleChart(DJIA,subset = "2020-11/2021-01", multi.col=TRUE,theme = chartTheme('white'))
## grey => Open[t] < Close[t] and Op[t] < Cl[t-1]
## white => Op[t] < Cl[t] and Op[t] > Cl[t-1]
## red => Op[t] > Cl[t] and Op[t] < Cl[t-1]
## black => Op[t] > Cl[t] and Op[t] > Cl[t-1]

## Plotting multiple series using ggplot2
# Collect stock names from Yahoo Finance
getSymbols(c("AAPL", "MSFT", "AMZN", "TSLA", "GOOGL"), src = "yahoo", from = start, to = end)

# Prepare data as xts (time series object)
stocks = as.xts(data.frame(AAPL = AAPL[, "AAPL.Adjusted"], 
                           MSFT = MSFT[, "MSFT.Adjusted"], 
                           AMZN = AMZN[, "AMZN.Adjusted"],
                           GOOGL = GOOGL[, "GOOGL.Adjusted"],
                           TSLA = TSLA[, "TSLA.Adjusted"]))

# Index by date
names(stocks) = c("Apple", "Microsoft", "Amazon", "Google", "Tesla")
index(stocks) = as.Date(index(stocks))

# Plot
stocks_series = tidy(stocks) %>% 
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line(cex=1) +
  theme_bw()
stocks_series

# Plot TWII
TWII_series = tidy(TWII$TWII.Adjusted) %>% 
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line(cex=1) +
  theme_bw()
TWII_series


stocks_series = tidy(stocks) %>% 
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line(cex=1) +
  theme_bw() +
  labs(title = "Daily Stock Prices, 7/1/2010 - 11/16/2021",
     subtitle = "End of Day Adjusted Prices",
     caption = "Source: Yahoo Finance") +
  xlab("Date") + ylab("Price") +
  scale_color_manual(values = c("steelblue", "firebrick","purple", "forestgreen","darkgray")) +
  theme(text = element_text(family = "Palatino"), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.position="top")
stocks_series
