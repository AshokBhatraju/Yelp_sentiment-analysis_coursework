#Assignment 4 - Ashok Bhatraju, Shourya Narayan, Vivek Kumar


library('tidyverse')

# the data file uses ';' as delimiter, and for this we use the read_csv2 function
resReviewsData <- read_csv2('C:/Users/shour/Downloads/yelpResReviewSample/yelpResReviewSample.csv')

#Q1

#number of reviews by start-rating
resReviewsData %>% group_by(stars) %>% count()
counts<- table(resReviewsData$stars)
barplot(counts, main="Stars Count",
        xlab="Stars",col='darkblue')
#hist(resReviewsData$stars)
ggplot(resReviewsData, aes(x= funny, y=stars)) +geom_point()
ggplot(resReviewsData, aes(x= cool, y=stars)) +geom_point()
ggplot(resReviewsData, aes(x= useful, y=stars)) +geom_point()


#The reviews are from various locations -- check
resReviewsData %>%   group_by(state) %>% tally() %>% view()
#Can also check the postal-codes`

#If you want to keep only the those reviews from 5-digit postal-codes  
rrData <- resReviewsData %>% filter(str_detect(postal_code, "^[0-9]{1,5}"))



#Tokenizing

library(tidytext)
library(SnowballC)
library(textstem)

#tokenize the text of the reviews in the column named 'text'
rrTokens <- rrData %>% unnest_tokens(word, text)
# this will retain all other attributes
#Or we can select just the review_id and the text column
rrTokens1 <- rrData %>% select(review_id, stars, text ) %>% unnest_tokens(word, text)

#How many tokens?
rrTokens1 %>% distinct(word) %>% dim()

#remove stopwords
rrTokens1 <- rrTokens1 %>% anti_join(stop_words)
#compare with earlier - what fraction of tokens were stopwords?
rrTokens1 %>% distinct(word) %>% dim()


#count the total occurrences of differet words, & sort by most frequent
rrTokens1 %>% count(word, sort=TRUE) %>% top_n(30)

#Are there some words that occur in a large majority of reviews, or which 
#are there in very few reviews?   Let's remove the words which are not 
#present in at least 10 reviews
rareWords <-rrTokens1 %>% count(word, sort=TRUE) %>% filter(n<10)
xx<-anti_join(rrTokens1, rareWords)

#check the words in xx .... 
xx %>% count(word, sort=TRUE) %>% view()
#you will see that among the least frequently occurring words are those 
#starting with or including numbers (as in 6oz, 1.15,...).  To remove these
xx2<- xx %>% filter(str_detect(word,"[0-9]")==FALSE)
#the variable xx, xx2 are for checking ....if this is what we want, set the rrTokens to the reduced set of words.  And you can remove xx, xx2 from the environment.
rrTokens1<- xx2



#Q2


#Check words by star rating of reviews
rrTokens1 %>% group_by(stars) %>% count(word, sort=TRUE)
#or...
rrTokens1 %>% group_by(stars) %>% count(word, sort=TRUE) %>% arrange(desc(stars)) %>% view()

#proportion of word occurrence by star ratings
ws <- rrTokens1 %>% group_by(stars) %>% count(word, sort=TRUE)
ws<-  ws %>% group_by(stars) %>% mutate(prop=n/sum(n))

#check the proportion of 'love' among reviews with 1,2,..5 stars 
#ws %>% filter(word=='nice')
#ws %>% filter(word=='delicious')
#ws %>% filter(word=='love')
#ws %>% filter(word=='friendly')
#ws %>% filter(word=='pretty')
#ws %>% filter(word=='fresh')
#ws %>% filter(word=='amazing')
#ws %>% filter(word=='die')
#ws %>% filter(word=='bad')
#ws %>% filter(word=='tasty')
#ws %>% filter(word=='excellent')
#ws %>% filter(word=='awesome')

#what are the most commonly used words by star rating
ws %>% group_by(stars) %>% arrange(stars, desc(prop)) %>% view()

#to see the top 20 words by star ratings
ws %>% group_by(stars) %>% arrange(stars, desc(prop)) %>% filter(row_number()<=20L) %>% view()

#To plot this
ws %>% group_by(stars) %>% arrange(stars, desc(prop)) %>% filter(row_number()<=20L) %>% ggplot(aes(word, prop))+geom_col()+coord_flip()+facet_wrap((~stars))

#Or, separate plots by stars
ws %>% filter(stars==1)  %>%  ggplot(aes(word, n)) + geom_col()+coord_flip()

ws %>% filter(stars==2)  %>%  ggplot(aes(word, n)) + geom_col()+coord_flip()

ws %>% filter(stars==3)  %>%  ggplot(aes(word, n)) + geom_col()+coord_flip()

ws %>% filter(stars==4)  %>%  ggplot(aes(word, n)) + geom_col()+coord_flip()

ws %>% filter(stars==5)  %>%  ggplot(aes(word, n)) + geom_col()+coord_flip()

#Can we get a sense of which words are related to higher/lower star raings 
#in general? 
#One approach is to calculate the average star rating associated with each 
#word - can sum the star ratings associated with reviews where each word 
#occurs in.  Can consider the proportion of each word among reviews with 
#a star rating.
xx<- ws %>% group_by(word) %>% summarise(totWS=sum(stars*prop))

#What are the 20 words with highest and lowerst star rating
xx %>% top_n(20)
xx %>% top_n(-20)


#Q3:

#Stemming and Lemmatization
#```{r , cache=TRUE}
rrTokens_stem<-rrTokens1 %>%  mutate(word_stem = SnowballC::wordStem(word))
rrTokens_lemm<-rrTokens1 %>%  mutate(word_lemma = textstem::lemmatize_words(word))
#Check the original words, and their stemmed-words and word-lemmas

#Term-frequency, tf-idf
#```{r  message=FALSE , cache=TRUE}

#tokenize, remove stopwords, and lemmatize (or you can use stemmed words instead of lemmatization)
rrTokens1<-rrTokens1 %>%  mutate(word = textstem::lemmatize_words(word))

#Or, to you can tokenize, remove stopwords, lemmatize  as
#rrTokens <- resReviewsData %>% select(review_id, stars, text, ) %>% unnest_tokens(word, text) %>%  anti_join(stop_words) %>% mutate(word = textstem::lemmatize_words(word))


#We may want to filter out words with less than 3 characters and those with more than 15 characters
rrTokens1<-rrTokens1 %>% filter(str_length(word)<=3 | str_length(word)<=15)


rrTokens1<- rrTokens1 %>% group_by(review_id, stars) %>% count(word)

#count total number of words by review, and add this in a column
totWords<-rrTokens1  %>% group_by(review_id) %>%  count(word, sort=TRUE) %>% summarise(total=sum(n))
xx<-left_join(rrTokens1, totWords)
# now n/total gives the tf values
xx<-xx %>% mutate(tf=n/total)
head(xx)

#We can use the bind_tfidf function to calculate the tf, idf and tfidf values
# (https://www.rdocumentation.org/packages/tidytext/versions/0.2.2/topics/bind_tf_idf)
rrTokens1<-rrTokens1 %>% bind_tf_idf(word, review_id, n)
head(rrTokens)


#Sentiment analysis using the 3 sentiment dictionaries available with tidytext (use library(textdata))
#AFINN http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010
#bing  https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html 
#nrc http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm

#```{r message=FALSE , cache=TRUE}

library(textdata)

#take a look at the wordsin the sentimennt dictionaries
get_sentiments("bing") %>% view()
get_sentiments("nrc") %>% view()
get_sentiments("afinn") %>% view()

#sentiment of words in rrTokens
rrSenti_bing<- rrTokens1 %>% left_join(get_sentiments("bing"), by="word")

#if we want to retain only the words which match the bing sentiment dictionary, do an inner-join
rrSenti_bing<- rrTokens1 %>% inner_join(get_sentiments("bing"), by="word")

#Analyze Which words contribute to positive/negative sentiment - we can 
#count the ocurrences of positive/negative sentiment words in the reviews
#xx2<-rrSenti_bing %>% group_by(word, sentiment) 
#xx2<-xx2 %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))

xx<-rrSenti_bing %>% group_by(word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))

#negate the counts for the negative sentiment words
xx<- xx %>% mutate (totOcc=ifelse(sentiment=="positive", totOcc, -totOcc))

#the most positive and most negative words
xx<-ungroup(xx)
xx %>% top_n(25)
xx %>% top_n(-25)

#You can plot these
rbind(top_n(xx, 25), top_n(xx, -25)) %>% ggplot(aes(word, totOcc, fill=sentiment)) +geom_col()+coord_flip()

#or, with a better reordering of words
rbind(top_n(xx, 25), top_n(xx, -25)) %>% mutate(word=reorder(word,totOcc)) %>% ggplot(aes(word, totOcc, fill=sentiment)) +geom_col()+coord_flip()

#summarise positive/negative sentiment words per review
revSenti_bing <- rrSenti_bing %>% group_by(review_id, stars) %>% summarise(nwords=n(),posSum=sum(sentiment=='positive'), negSum=sum(sentiment=='negative'))

revSenti_bing<- revSenti_bing %>% mutate(posProp=posSum/nwords, negProp=negSum/nwords)
revSenti_bing<- revSenti_bing %>% mutate(sentiScore=posProp-negProp)

#Do review start ratings correspond to the the positive/negative sentiment words
revSenti_bing %>% group_by(stars) %>% summarise(avgPos=mean(posProp), avgNeg=mean(negProp), avgSentiSc=mean(sentiScore))

#we can consider reviews with 1 to 2 stars as positive, and this with 4 to 5 stars as negative
revSenti_bing <- revSenti_bing %>% mutate(hiLo=ifelse(stars<=2,-1, ifelse(stars>=4, 1, 0 )))
revSenti_bing <- revSenti_bing %>% mutate(pred_hiLo=ifelse(sentiScore >0, 1, -1)) 
#filter out the reviews with 3 stars, and get the confusion matrix for hiLo vs pred_hiLo
xx<-revSenti_bing %>% filter(hiLo!=0)
table(actual=xx$hiLo, predicted=xx$pred_hiLo )

#Accuracy
Metric <- c("Accuracy")
Result <- c(round(mean(xx$hiLo==xx$pred_hiLo),2))
p <- as.data.frame(cbind(Metric, Result))
knitr::kable(p, align = c('c', 'c'))


#Q - does this 'make sense'?  Do the different dictionaries give similar results; do you notice much difference?


#with "nrc" dictionary
#rrSenti_nrc<-rrTokens1 %>% inner_join(get_sentiments("nrc"), by="word") %>% group_by (word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))
rrSenti_nrc<-rrTokens1 %>% inner_join(get_sentiments("nrc"), by="word")

#Adding column to add positie and negetive class for sentiment
rrSenti_nrc <-rrSenti_nrc %>% mutate(goodBadsent=ifelse(sentiment %in% c('anger', 
'disgust', 'fear', 'sadness', 'negative'), "negetive", 
ifelse(sentiment %in% c('positive', 'joy', 'anticipation', 'trust'), "positive", "neutral")))

#Grouping of words
rrSenti_nrc_new <- rrSenti_nrc %>% group_by (word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))
#How many words for the different sentiment categories
rrSenti_nrc_new %>% group_by(sentiment) %>% summarise(count=n(), sumn=sum(totOcc))
 
#In 'nrc', the dictionary contains words defining different sentiments, 
#like anger, disgust, positive, negative, joy, trust,.....   you should 
#check the words deonting these different sentiments
#rrSenti_nrc %>% filter(sentiment=='anticipation') %>% view()
#rrSenti_nrc %>% filter(sentiment=='fear') %>% view()
#...

#Suppose you want   to consider  {anger, disgust, fear sadness, negative} 
#to denote 'bad' reviews,  and {positive, joy, anticipation, trust} to 
#denote 'good' reviews
xx_nrc <- rrSenti_nrc_new %>% mutate(goodBad=ifelse(sentiment %in% c('anger', 
'disgust', 'fear', 'sadness', 'negative'), -totOcc, 
ifelse(sentiment %in% c('positive', 'joy', 'anticipation', 'trust'), totOcc, 0)))

xx_nrc<-ungroup(xx_nrc)
top_n(xx_nrc, 10)
top_n(xx_nrc, -10)

rbind(top_n(xx_nrc, 25), top_n(xx_nrc, -25)) %>% mutate(word=reorder(word,goodBad)) %>% ggplot(aes(word, goodBad, fill=goodBad)) +geom_col()+coord_flip()

#summarise positive/negative sentiment words per review
revSenti_nrc <- rrSenti_nrc %>% group_by(review_id, stars) %>% summarise(nwords=n(),posSum=sum(sentiment=='positive'), negSum=sum(sentiment=='negative'))

revSenti_nrc<- revSenti_nrc %>% mutate(posProp=posSum/nwords, negProp=negSum/nwords)
revSenti_nrc<- revSenti_nrc %>% mutate(sentiScore=posProp-negProp)

#Do review start ratings correspond to the the positive/negative sentiment words
revSenti_nrc %>% group_by(stars) %>% summarise(avgPos=mean(posProp), avgNeg=mean(negProp), avgSentiSc=mean(sentiScore))

#we can consider reviews with 1 to 2 stars as positive, and this with 4 to 5 stars as negative
revSenti_nrc <- revSenti_nrc %>% mutate(hiLo=ifelse(stars<=2,-1, ifelse(stars>=4, 1, 0 )))
revSenti_nrc <- revSenti_nrc %>% mutate(pred_hiLo=ifelse(sentiScore >0, 1, -1)) 
#filter out the reviews with 3 stars, and get the confusion matrix for hiLo vs pred_hiLo
xx<-revSenti_nrc %>% filter(hiLo!=0)
table(actual=xx$hiLo, predicted=xx$pred_hiLo )

#Accuracy
Metric <- c("Accuracy")
Result <- c(round(mean(xx$hiLo==xx$pred_hiLo),2))
p <- as.data.frame(cbind(Metric, Result))
knitr::kable(p, align = c('c', 'c'))


#AFINN carries a numeric value for positive/negative sentiment -- how would 
#you use these



#Analysis by review sentiment
#So far, we have analyzed overall sentiment across reviews, now let's look 
#into sentiment by review and see how that relates to review's star ratings
#```{r message=FALSE , cache=TRUE}
rrSenti_afinn<- rrTokens1 %>% inner_join(get_sentiments("afinn"), by="word")

rrSenti_afinn_new <- rrSenti_afinn %>% group_by (word, value) %>% summarise(totOcc=sum(n)) %>% arrange(value, desc(totOcc))
#How many words for the different sentiment categories
rrSenti_afinn_new %>% group_by(value) %>% summarise(count=n(), sumn=sum(totOcc))

xx_afinn<- rrSenti_afinn_new %>% mutate (totOcc=ifelse(value > 0, totOcc, -totOcc))

xx_afinn<-ungroup(xx_afinn)
top_n(xx_afinn, 10)
top_n(xx_afinn, -10)

rbind(top_n(xx_afinn, 25), top_n(xx_afinn, -25)) %>% mutate(
word=reorder(word,value)) %>% ggplot(aes(word, totOcc, fill=value)) +
geom_col()+coord_flip()

#with AFINN dictionary words....following similar steps as above, but noting that AFINN assigns negative to positive sentiment value for words matching the dictionary
rrSenti_afinn<- rrTokens1 %>% inner_join(get_sentiments("afinn"), by="word")

revSenti_afinn <- rrSenti_afinn %>% group_by(review_id, stars) %>% summarise(nwords=n(), sentiSum =sum(value))

revSenti_afinn %>% group_by(stars) %>% summarise(avgLen=mean(nwords), avgSenti=mean(sentiSum))

#```


#Can we classify reviews on high/low stats based on aggregated sentiment of words in the reviews
#```{r message=FALSE , cache=TRUE}

#we can consider reviews with 1 to 2 stars as positive, and this with 4 to 5 stars as negative
revSenti_afinn <- revSenti_afinn %>% mutate(hiLo=ifelse(stars<=2,-1, ifelse(stars>=4, 1, 0 )))
revSenti_afinn <- revSenti_afinn %>% mutate(pred_hiLo=ifelse(sentiSum >0, 1, -1)) 
#filter out the reviews with 3 stars, and get the confusion matrix for hiLo vs pred_hiLo
xx<-revSenti_afinn %>% filter(hiLo!=0)
table(actual=xx$hiLo, predicted=xx$pred_hiLo )

#Accuracy
Metric <- c("Accuracy")
Result <- c(round(mean(xx$hiLo==xx$pred_hiLo),2))
p <- as.data.frame(cbind(Metric, Result))
knitr::kable(p, align = c('c', 'c'))

#Question 4:

#considering only those words which match a sentiment dictionary (for 
#eg.  bing)

#use pivot_wider to convert to a dtm form where each row is for a review 
#and columns correspond to words  
#revDTM_sentiBing <- rrSenti_bing %>%  pivot_wider(id_cols = review_id, names_from = word, values_from = tf_idf)

#SVM model

#Or, since we want to keep the stars column
revDTM_sentiBing <- rrSenti_bing %>%  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = tf_idf)  %>% ungroup()
#Note the ungroup() at the end -- this is IMPORTANT;  we have grouped based on (review_id, stars), and this grouping is retained by default, and can cause problems in the later steps

#filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_sentiBing <- revDTM_sentiBing %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% select(-stars)


library(ranger)

#replace all the NAs with 0

revDTM_sentiBing<-revDTM_sentiBing %>% replace(., is.na(.), 0)
revDTM_sentiBing$hiLo<- as.factor(revDTM_sentiBing$hiLo)

library(rsample)

revDTM_sentiBing_split<- initial_split(revDTM_sentiBing, 0.5)
revDTM_sentiBing_trn<- training(revDTM_sentiBing_split)
revDTM_sentiBing_tst<- testing(revDTM_sentiBing_split)

#SVM model
library(e1071)

svmM1 <- svm(as.factor(hiLo) ~., data = revDTM_sentiBing_trn %>%select(-review_id),
             kernel="radial", cost=1, scale=FALSE) 

#SVM confusion matrix
revDTM_predTrn_svm1<-predict(svmM1, revDTM_sentiBing_trn)
revDTM_predTst_svm1<-predict(svmM1, revDTM_sentiBing_tst)
table(actual= revDTM_sentiBing_trn$hiLo, predicted= revDTM_predTrn_svm1)

#Accuracy SVM
Metric <- c("Accuracy")
Result <- c(round(mean(revDTM_sentiBing_trn$hiLo==revDTM_predTrn_svm1),2))
p <- as.data.frame(cbind(Metric, Result))
knitr::kable(p, align = c('c', 'c'))

#SVM model 2
system.time( svmM2 <- svm(as.factor(hiLo) ~., data = revDTM_sentiBing_trn
                          %>% select(-review_id), kernel="radial", cost=5, gamma=5, scale=FALSE) )
revDTM_predTrn_svm2<-predict(svmM2, revDTM_sentiBing_trn)
table(actual= revDTM_sentiBing_trn$hiLo, predicted= revDTM_predTrn_svm2)

#Accuracy SVM model 2
Metric <- c("Accuracy")
Result <- c(round(mean(revDTM_sentiBing_trn$hiLo==revDTM_predTrn_svm2),2))
p <- as.data.frame(cbind(Metric, Result))
knitr::kable(p, align = c('c', 'c'))


#Random Forest Model using Bing Liu Dictionary

#Or, since we want to keep the stars column
revDTM_sentiBing <- rrSenti_bing %>%  pivot_wider(id_cols = c(review_id,
stars), names_from = word, values_from = tf_idf)  %>% ungroup()
#Note the ungroup() at the end -- this is IMPORTANT;  we have grouped based on (review_id, stars), and this grouping is retained by default, and can cause problems in the later steps

#filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_sentiBing <- revDTM_sentiBing %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% select(-stars)

#how many review with 1, -1  'class'
revDTM_sentiBing %>% group_by(hiLo) %>% tally()

#develop a random forest model to predict hiLo from the words in the reviews

library(ranger)

#replace all the NAs with 0
revDTM_sentiBing<-revDTM_sentiBing %>% replace(., is.na(.), 0)

revDTM_sentiBing$hiLo<- as.factor(revDTM_sentiBing$hiLo)


library(rsample)
revDTM_sentiBing_split<- initial_split(revDTM_sentiBing, 0.5)
revDTM_sentiBing_trn<- training(revDTM_sentiBing_split)
revDTM_sentiBing_tst<- testing(revDTM_sentiBing_split)

rfModel1<-ranger(dependent.variable.name = "hiLo", 
data=revDTM_sentiBing_trn %>% select(-review_id), num.trees = 500, 
importance='permutation', probability = TRUE)

rfModel1

#which variables are important
importance(rfModel1) %>% view()


#Obtain predictions, and calculate performance
revSentiBing_predTrn<- predict(rfModel1, revDTM_sentiBing_trn %>% select(-review_id))$predictions

revSentiBing_predTst<- predict(rfModel1, revDTM_sentiBing_tst %>% select(-review_id))$predictions

table(actual=revDTM_sentiBing_trn$hiLo, preds=revSentiBing_predTrn[,2]>0.5)
table(actual=revDTM_sentiBing_tst$hiLo, preds=revSentiBing_predTst[,2]>0.5)
#Q - is 0.5 the best threshold to use here?  Can find the optimal threshold from the     ROC analyses

#Accuracy
Metric <- c("Accuracy")
Result <- c(round(precision(revSentiBing_predTrn,revDTM_sentiBing_trn$hiLo),2)) 
Result <- c(round(mean(revDTM_sentiBing_trn$hiLo==revSentiBing_predTrn),2))
p <- as.data.frame(cbind(Metric, Result))
knitr::kable(p, align = c('c', 'c'))


library(pROC)
rocTrn <- roc(revDTM_sentiBing_trn$hiLo, revSentiBing_predTrn[,2], levels=c(-1, 1))
rocTst <- roc(revDTM_sentiBing_tst$hiLo, revSentiBing_predTst[,2], levels=c(-1, 1))

plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
       col=c("blue", "red"), lwd=2, cex=0.8, bty='n')


#Best threshold from ROC analyses
bThr<-coords(rocTrn, "best", ret="threshold", transpose = FALSE)
table(actual=revDTM_sentiBing_trn$hiLo, preds=revSentiBing_predTrn[,2]>bThr)

```


Develop a model on broader set of terms (not just those matching a sentiment dictionary)
```{r message=FALSE, cache=TRUE}

#if we want to remove the words which are there in too many or too few of the reviews
#First find out how many reviews each word occurs in
rWords<-rrTokens %>% group_by(word) %>% summarise(nr=n()) %>% arrange(desc(nr))

#How many words are there
length(rWords$word)

top_n(rWords, 20)
top_n(rWords, -20)

#Suppose we want to remove words which occur in > 90% of reviews, and those which are in, for example, less than 30 reviews
reduced_rWords<-rWords %>% filter(nr< 6000 & nr > 30)
length(reduced_rWords$word)

#reduce the rrTokens data to keep only the reduced set of words
reduced_rrTokens <- left_join(reduced_rWords, rrTokens)

#Now convert it to a DTM, where each row is for a review (document), and columns are the terms (words)
revDTM  <- reduced_rrTokens %>%  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = tf_idf)  %>% ungroup()

#Check
dim(revDTM)
#do the numberof columsnmatch the words -- we should also have the stars column and the review_id

#create the dependent variable hiLo of good/bad reviews absed on stars, and remove the review with stars=3
revDTM <- revDTM %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% select(-stars)

#replace NAs with 0s
revDTM<-revDTM %>% replace(., is.na(.), 0)

revDTM$hiLo<-as.factor(revDTM$hiLo)

revDTM_split<- initial_split(revDTM, 0.5)
revDTM_trn<- training(revDTM_split)
revDTM_tst<- testing(revDTM_split)

#this can take some time...the importance ='permutation' takes time (we know why)
rfModel2<-ranger(dependent.variable.name = "hiLo", data=revDTM_trn %>% select(-review_id), num.trees = 500, importance='permutation', probability = TRUE)


#Naive Bayes: work in progress
nbModel1<-naiveBayes(hiLo ~ ., data=revDTM_sentiBing_trn %>% select(-review_id))

revSentiBing_NBpredTrn<-predict(nbModel1, revDTM_sentiBing_trn, type = "raw")

auc(as.numeric(revDTM_sentiBing_trn$hiLo), revSentiBing_NBpredTrn[,2])


revSentiBing_NBpredTrn<-revSentiBing_NBpredTrn %>% replace(., is.na(.), 0)

table(actual= revDTM_sentiBing_trn$hiLo, predicted= revSentiBing_NBpredTrn)

