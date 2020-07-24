# load packages
library(readr)
library(tidyr)
library(zoo)
library(arules)
library(arulesViz)
library(lubridate)
library(ggplot2)
library(knitr)
library(dplyr)
library(plyr)
library(plotly)
library(RColorBrewer)


# load data googlestore
googlestore <- read.csv("C:/Users/detap/Desktop/Online.csv")

#Data Preprocessing and Understanding
# buang missing value
google <- googlestore[complete.cases(googlestore),] # sisa 54139 observasi
summary(google)

# ubah tipe variabel
google <- google %>% mutate(`Product` = as.factor(Product))
google$TransID <- as.numeric(as.character(google$Transaction.ID))
google$Date <- as.Date(as.character(google$Date), "%Y%m%d")
glimpse(google)

# RATA2 CUSTOMER BELI BERAPA BARANG
google %>% 
  group_by(TransID) %>% 
  summarize(n_items = mean(Quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 1000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,20))

# 10 BARANG TERLARIS
tmp <- google %>% 
  group_by(Product.SKU, Product) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp %>% 
  ggplot(aes(x=reorder(Product,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()


# jadiin csv
google_sorted <- google[order(google$TransID),]
itemlist <- ddply(google,c("TransID","Date"), 
                  function(df1)paste(df1$Product, 
                                     collapse = ","))
itemlist$TransID <- NULL
itemlist$Date <- NULL
head(itemlist)
write.csv(itemlist,"market_basket1.csv", quote = FALSE, row.names = TRUE)


# jadiin data transaksi
tr <- read.transactions('C:/Users/detap/Documents/market_basket1.csv', 
                        format = 'basket', sep=',')
summary(tr)


# BUAT ASSOCIATION RULES DGN ALGORITMA APRIORI
rules <- apriori(tr, parameter = list(supp=0.0002,conf=0.6))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

inspect(rules[1:10]) #liat 10 rules teratas

toprules<-rules[1:30] #ambil 30 rules utk diplot & interpretasi
plot(toprules)
plot(toprules,method='graph') #lingkaran besar lbh gede probnya
plot(toprules,method='grouped')

