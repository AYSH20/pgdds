setwd("F:/PGDDS")

textData<-readLines("sachin book feedback.txt")
#if data is in csv, then read.csv
library(tm)

corp <- Corpus(VectorSource(textData))
#if csv format dataFrameSource() for corpus
#=====
corp <- tm_map(corp,tolower)
head(corp)
corp<-tm_map(corp,removePunctuation)
corp<-tm_map(corp,removeNumbers)
corp<-tm_map(corp,PlainTextDocument)
corp<-tm_map(corp,removeWords,stopwords("en"))

#display a particular line
writeLines(as.character(corp[[1]]))

inspect(corp[1:3])
dtm<-TermDocumentMatrix(corp)
m<-as.matrix(dtm)
findFreqTerms(dtm,10)
findAssocs(dtm,"fixing",0.3)


#WORD CLOUD
install.packages("wordcloud")
library(wordcloud)
#calculate the total freq of words
v<-sort(rowSums(m),decreasing = T)
myNames<-names(v)
d<-data.frame(word=myNames,freq=v)

#create palette
pal2<-brewer.pal(8,"Dark2")
wordcloud(d$word,d$freq,random.order = F,min.freq = 1,colors = pal2)


#ggplot
term.freq<-rowSums(m)
term.freq<-subset(term.freq,term.freq>=10)
term.freq

#make a data frame using the object term.freq
df<-data.frame(term=names(term.freq),freq=term.freq)
library(ggplot2)
ggplot(df,aes(x=term,y=freq))+
  geom_bar(stat = "identity")+
  xlab("Terms")+
  ylab("Count")+
  coord_flip()