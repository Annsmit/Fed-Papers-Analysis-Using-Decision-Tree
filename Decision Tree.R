
## LIBRARIES
library(stringr)
# install.packages("ggplot2")
library(ggplot2)
#install.packages("tidyverse")
library(tidyverse)
library(magrittr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Cairo)
library(forcats)
library(dplyr)
library(stringr)
library(class)
library(lattice)
#install.packages("e1071")
library(e1071)
#install.packages("mlr")
library(mlr)
#install.packages("naivebayes")
library(naivebayes)

library(tm)
#install.packages("tm")
library(stringr)
library(wordcloud)
install.packages("SnowballC")
library("SnowballC")
#install.packages("slam")
library(slam)
library(quanteda)
#install.packages("quanteda")
library(arules)
#install.packages('proxy')
library(proxy)
library(cluster)
library(stringi)
library(proxy)
library(Matrix)
library(tidytext) # convert DTM to DF
library(plyr) ## for adply
library(ggplot2)
#install.packages("factoextra")
library(factoextra) # for fviz
library(mclust) # for Mclust EM clustering

#Make sure working directory is set correctly
getwd()

## Next, load in the documents (the corpus)
FedCorpus <- Corpus(DirSource("Fed_Corpus"))
#Line of code to confirm how many Federalist Papers were loaded into R
(ndocs<-length(FedCorpus))
##The following will show you that you read in all the documents
(summary(FedCorpus))
#Apply English stop words 
(STOPS <-stopwords('english'))
#My stopwords
MyStopWords <- c("the", "also", "and", "I", "their")
getTransformations()
# ignore extremely rare words i.e. terms that appear in less then 2% of the documents
(minTermFreq <- ndocs * 0.0002)
# ignore overly common words i.e. terms that appear in more than 50% of the documents
(maxTermFreq <- ndocs * .5)

#Clean and prepare the text documents

Fed_dtm <- DocumentTermMatrix(FedCorpus,
                              control = list(
                                wordLengths=c(3, 10),
                                removePunctuation = T,
                                removeNumbers = T,
                                tolower=T,
                                stemming = F,
                                remove_separators = T,
                                stopwords = T,
                                MyStopWords = F,
                                stripWhitespace = T,
                                bounds = list(global = c(minTermFreq, maxTermFreq))
                              ))
return(Fed_dtm)

#Convert dtm to a quantitative matrix of numerical count of words
Fed_mat <- as.matrix(Fed_dtm)

#Have a look at first 5 rows and first 25 columns
(Fed_mat[1:20, 1:8])

##Data Exploration
## Look at word freuqncies
(WordFreq <- colSums(as.matrix(Fed_mat)))
(head(WordFreq))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord)])
(WordFreq[tail(ord)])

library(kableExtra)
WordFreq[tail(ord)] %>%
  kable() %>%
  kable_styling()

## Row Sums (Can tell if all documents are same length an if any normalization is necessary)
(Row_Sum_Per_doc <- rowSums((as.matrix(Fed_mat))))

#Put CleanCorpus into a dataframe
(Corpus_dataframe <- data.frame(Fed_mat))

#Retrieve Row names from the dataframe and set as the first column 
library(data.table)
setDT(Corpus_dataframe, keep.rownames = TRUE)
colnames(Corpus_dataframe)

#View table
table(Corpus_dataframe$rn)

#Write it to view it
write.csv(Corpus_dataframe, "Corpusoutput.csv")

#library(dplyr)
#library(forcats)
#Find wildcard to get this to work
#Corpus_dataframe$rn <-Corpus_dataframe$rn %>% fct_collapse(Hamilton = c("Hamilton","HM"))


## From this table, I can see multiple versions of the author column 
## I need to combine these.
Corpus_dataframe$rn[Corpus_dataframe$rn == "dispt_fed_49.txt"] <- "dispt"
Corpus_dataframe$rn[Corpus_dataframe$rn == "dispt_fed_50.txt"] <- "dispt"
Corpus_dataframe$rn[Corpus_dataframe$rn == "dispt_fed_51.txt"] <- "dispt"
Corpus_dataframe$rn[Corpus_dataframe$rn == "dispt_fed_52.txt"] <- "dispt"
Corpus_dataframe$rn[Corpus_dataframe$rn == "dispt_fed_53.txt"] <- "dispt"
Corpus_dataframe$rn[Corpus_dataframe$rn == "dispt_fed_54.txt"] <- "dispt"
Corpus_dataframe$rn[Corpus_dataframe$rn == "dispt_fed_55.txt"] <- "dispt"
Corpus_dataframe$rn[Corpus_dataframe$rn == "dispt_fed_56.txt"] <- "dispt"
Corpus_dataframe$rn[Corpus_dataframe$rn == "dispt_fed_57.txt"] <- "dispt"
Corpus_dataframe$rn[Corpus_dataframe$rn == "dispt_fed_62.txt"] <- "dispt"
Corpus_dataframe$rn[Corpus_dataframe$rn == "dispt_fed_63.txt"] <- "dispt"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_1.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_11.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_12.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_13.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_15.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_16.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_17.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_21.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_22.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_23.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_24.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_25.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_26.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_27.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_28.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_29.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_30.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_31.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_32.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_33.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_34.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_35.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_36.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_59.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_6.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_60.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_61.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_66.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_67.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_68.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_69.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_7.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_70.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_71.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_72.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_73.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_74.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_75.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_76.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_76.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_78.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_79.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_8.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_65.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_77.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_80.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_81.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_82.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_83.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_84.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_85.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Hamilton_fed_9.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "HM_fed_18.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "HM_fed_19.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "HM_fed_20.txt"] <- "Hamilton"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Jay_fed_2.txt"] <- "Jay"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Jay_fed_3.txt"] <- "Jay"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Jay_fed_4.txt"] <- "Jay"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Jay_fed_5.txt"] <- "Jay"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Jay_fed_64.txt"] <- "Jay"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_10.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_14.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_37.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_38.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_39.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_40.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_41.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_42.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_43.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_44.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_45.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_46.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_47.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_48.txt"] <- "Madison"
Corpus_dataframe$rn[Corpus_dataframe$rn == "Madison_fed_58.txt"] <- "Madison"

#View table again
table(Corpus_dataframe$rn)


#Make first column (the Fed papers a factor)
library(dplyr)
Corpus_dataframe$rn<- as.factor(Corpus_dataframe$rn)
str(Corpus_dataframe)
colnames(Corpus_dataframe)

#Visual of words
wordcloud(colnames(Fed_dtm), Fed_mat[13, ], max.words = 70)
(head(sort(as.matrix(Fed_dtm)[13,], decreasing = TRUE), n=20))


#(table(CleanDF))
#library(pander)
#pandoc.table(CleanDF)

#####  CREATE TEST SET (just dispt) and TRAIN SET (all others) ####
test <- filter(Corpus_dataframe, rn=="dispt")
train <- filter(Corpus_dataframe, rn != "dispt")

###################DECISION TREE#############
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
tree <- rpart(rn ~., train, method="class",
              control = rpart.control(minsplit = 1, minbucket = 1, cp = .1 ))
printcp(tree)
predicted= predict(tree,test, type="class")

table_dt <- table(test$rn, predicted)
table_dt
#What's my accuracy
(accuracy_test <- sum(diag(table_dt))/sum(table_dt))

#(Results <- data.frame(predicted,test))

fancyRpartPlot(tree, uniform=TRUE,
               main="Federalist Paper's Classification Tree", 
               cex=.6, tweak = .7)
text(DT, use.n=TRUE, all=TRUE, cex=1)


#Prune the tree
library(caret)
tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
plotcp(tree)
ptree <- prune(tree, cp = 0.1)
fancyRpartPlot(ptree, uniform=TRUE, main = "Pruned Classification Tree")
(test_accuracy <- predict(ptree,test,type="class"))


