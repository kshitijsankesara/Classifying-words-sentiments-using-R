#Importing the necessary libraries to work on JSON files and plots

install.packages("tm")
library(tm)
install.packages("wordcloud")
library(wordcloud)
install.packages("RCurl")
library(RCurl)
install.packages("jsonlite")
library(jsonlite)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggmap")
library(ggmap)
install.packages("RJSONIO")                         
library(RJSONIO)


#Part A: Load and condition the text file that contains the speech  

#2.	The key column to focus on is the ???freeText??? column.

dataset <- "hotelSurveyBarriot.json"                              #Loading JSON Dataset
hotelsurvey <- fromJSON(dataset, simplify = TRUE, nullValue = NA)  
HotelSurvey <-  data.frame(hotelsurvey)
View(HotelSurvey)
str(HotelSurvey)
summary(HotelSurvey)



#Part B: Create a list of word counts from the speech


#3.	Starting with the code at the bottom of page 180 in the text book, 
#use a similar approach to transform the free text into a term document matrix, 
#and then determine positive and negative word matches.

wordsvec = VectorSource(HotelSurvey$freeText)        #To create a vector from the given text
wordscorpus = Corpus(wordsvec)                       #To create a collection of all text
wordscorpus


#Operations done on the text doc

#1 Converting the text to lower case
wordscorpus = tm_map (wordscorpus, content_transformer(tolower))

#2 Removing the punctuation
wordscorpus = tm_map(wordscorpus, removePunctuation)

#3 Removing numbers from text
wordscorpus = tm_map(wordscorpus, removeNumbers) 

#4 Getting rid of stop words
wordscorpus = tm_map(wordscorpus, removeWords, stopwords('english'))


#The mathematical matrix defines the frequency of occurence of texts
tdm = TermDocumentMatrix(wordscorpus)
tdm
inspect(tdm) #Used to consider all the relevant options, here 
#gives the occurence of words


Matrix = as.matrix(tdm) #Generating a matrix  
Matrix


#To get the sum of occurences of each word
wordcounts = rowSums(Matrix)

#sorting in descending order
wordcounts = sort(wordcounts, decreasing=TRUE)
wordcounts

#Getting the names
Name = names(wordcounts)
Name

#Getting the total no. of unique words
UniqueName = length(wordcounts)
UniqueName

#Getting the total number of words
TotalName = sum(wordcounts)
TotalName


#Scanning the positive words text file
scanpos = scan("positive-words.txt", sep = "\n", character(0)) 
dim(scanpos)
str(scanpos)
summary(scanpos)

#getting rid of irrelevant rows
scanpos = scanpos[-1:-34]
View(scanpos)


#Scanning the negative words text document
scanneg = scan("negative-words.txt", sep = "\n", character(0)) 
dim(scanneg)
str(scanneg)
summary(scanneg)

#getting rid of the irrelevant rows
scanneg = scanneg[-1:-34]
View(scanneg)


#finding the matching positive words rom the text document
matchedpos = match(Name, scanpos, nomatch = 0)
head(matchedpos,10)
matchedpos

#Finding the matching negative words from the text document
matchedneg = match(Name, scanneg, nomatch = 0)
head(matchedneg,10)
matchedneg

#Getting the matched words
WPos = wordcounts[which(matchedpos!= 0)]
WPos
length(WPos)

#calculating the total positively matched words
NumPos = sum(WPos)
NumPos


#Getting the matched words
WNeg = wordcounts[which(matchedneg != 0)]
WNeg
length(WNeg)

#Calculatin the negatively matched words
NumNeg = sum(WNeg)
NumNeg



#4.	Calculate the percent positive words and negative words.

#getting the ratio and percentage of positive words
ratioPos = NumPos/TotalName
ratioPos
PerPos = ratioPos*100
PerPos

#getting the ratio and percentage of negative words
ratioNeg = NumNeg/TotalName
ratioNeg
PerNeg = ratioNeg*100
PerNeg


#5.	Write a block comment that summarizes what you learned from ratioPos and ratioNeg.
#The number of positive words are more in compared to negative words. 
#So we know that the customers liked the hotel experience.



#Part C: Visualize the results


#6.	Create a word cloud


#Creating colums and assigning column names
cloud=data.frame(word=names(wordcounts),freq=wordcounts)
View(cloud)

#Creating the wordcloud
wordcloud(cloud$word,cloud$freq)


#7.	Create a barplot of the positive and negative words that matched (at least twice)

#Creating barplots according to the required condition
matchedPositive = WPos[which(WPos >=2)]
View(matchedPositive)
barplot(matchedPositive, main = "Positive Words", las=2)

matchedNegative = WNeg[which(WNeg >= 2)]
View(matchedNegative)
barplot(matchedNegative, main= 'Negative Words', las = 2)


#8.	Write a block comment on what you observe from these two barplots and the wordcloud. 
#9.	Does these results make sense to you in terms of the kinds of emotions you see?
#Which do you think is more informative ??? barplot or the wordcloud?

#From both the barplots, we can determine the frequency of any word which is used to provide the review of the hotel.
#By using that we can get the overall customer satisfaction in a hotel.

#From the barplots and wordcloud above, it is easier to determing the most frequent word (one with the biggest size in the wordcloud).
#The occurence of positive words are more, thus positive customer reviews are clearly evident.

#According to me, barplots would be more helpful as the clearly help in determining the positive factors leading to satisfaction.
#It shows the words which lead to dissatisfaction too.
#Moreover, barplots are a better form of visualization in this case.




#Part D: Evaluate Happy and not Happy customer responses


#10.	Create two subset of the text vectors: one for happy customers and one for not happy customers (based on overall customer satisfaction).

#Generating two separate datasets

happycust = HotelSurvey[which(HotelSurvey$overallCustSat >= 8), names(HotelSurvey) %in% c("hotelSize","checkInSat",
"hotelState", "hotelClean", "hotelFriendly", "gender","guestAge","lengthOfStay","whenBookedTrip", "freeText")]
View(happycust)

unhappycust = HotelSurvey[which(HotelSurvey$overallCustSat < 8), names(HotelSurvey) %in% c("hotelSize","checkInSat",                 
"hotelState", "hotelClean", "hotelFriendly", "gender","guestAge","lengthOfStay","whenBookedTrip", "freeText")]
View(unhappycust)


#11.	Redo Steps B, C & D, for these two subsets of the text strings.
#The comments in these sections would be the same as used in previous parts and it will be applied to both the datasets.

#For happy Customers
wordsvechappy = VectorSource(happycust$freeText)
wordscorpushappy = Corpus(wordsvechappy)
wordscorpushappy

wordscorpushappy = tm_map(wordscorpushappy, content_transformer(tolower))
wordscorpushappy = tm_map(wordscorpushappy, removePunctuation)
wordscorpushappy = tm_map(wordscorpushappy, removeNumbers) 
wordscorpushappy= tm_map(wordscorpushappy, removeWords, stopwords('english'))

tdmhappy = TermDocumentMatrix(wordscorpushappy)
tdmhappy
inspect(tdmhappy)

Matrixhappy = as.matrix(tdmhappy)
Matrixhappy

wordcountshappy = rowSums(Matrixhappy)
wordcountshappy = sort(wordcountshappy, decreasing=TRUE)
wordcountshappy

wordshappy = names(wordcountshappy)
wordshappy

Uniquehappy = length(wordcountshappy)
Uniquehappy
Totalhappy = sum(wordcountshappy)
Totalhappy

scanposhappy = scan("positive-words.txt", sep = "\n", character(0)) 
scanposhappy = scanposhappy[-1:-34]

matchedhappy <- match(wordshappy, scanposhappy, nomatch = 0)
head(matchedhappy,10)
matchedhappy

WPoshappy = wordcountshappy[which(matchedhappy!= 0)]
WPoshappy
length(WPoshappy)

NumPoshappy = sum(WPoshappy)
NumPoshappy

ratioPoshappy = NumPoshappy/Totalhappy
ratioPoshappy

perhappy = ratioPoshappy*100
perhappy
#1. 27.54237


cloudFramehappy = data.frame(wordshappy=names(wordcountshappy),freq_happy=wordcountshappy)
View(cloudFramehappy)
wordcloud(cloudFramehappy$word,cloudFramehappy$freq)

matchedPositivehappy = WPoshappy[which(WPoshappy >=2)]
View(matchedPositivehappy)
barplot(matchedPositivehappy, main="Positive Words", las=2)


#For unhappy Customers

wordsvecunhappy = VectorSource(unhappycust$freeText)
wordscorpusunhappy = Corpus(wordsvecunhappy)
wordscorpusunhappy

wordscorpusunhappy = tm_map(wordscorpusunhappy, content_transformer(tolower))
wordscorpusunhappy =tm_map(wordscorpusunhappy, removePunctuation)
wordscorpusunhappy =tm_map(wordscorpusunhappy, removeNumbers) 
wordscorpusunhappy= tm_map(wordscorpusunhappy, removeWords, stopwords('english'))

tdmunhappy = TermDocumentMatrix(wordscorpusunhappy)
tdmunhappy
inspect(tdmunhappy)

Matrixunhappy = as.matrix(tdmunhappy)
Matrixunhappy

wordcountsunhappy = rowSums(Matrixunhappy)
wordcountsunhappy = sort(wordcountsunhappy, decreasing=TRUE)
wordcountsunhappy

wordsunhappy = names(wordcountsunhappy)
wordsunhappy

Uniqueunhappy = length(wordcountsunhappy)
Uniqueunhappy
Totalunhappy = sum(wordcountsunhappy)
Totalunhappy

scannegunhappy = scan("negative-words.txt", sep = "\n", character(0)) 
scannegunhappy = scanneg[-1:-34]

matchedunhappy = match(wordsunhappy, scannegunhappy, nomatch = 0)
head(matchedunhappy,10)
matchedunhappy

WNegunhappy = wordcountsunhappy[which(matchedunhappy!= 0)]
WNegunhappy
length(WNegunhappy)

NumNegunhappy = sum(WNegunhappy)
NumNegunhappy

ratioNegunhappy = NumNegunhappy/Totalunhappy
ratioNegunhappy

perunhappy = ratioNegunhappy*100
perunhappy

#2. 8.158508

cloudFrameunhappy = data.frame(wordsunhappy=names(wordcountsunhappy),freq_unhappy=wordcountsunhappy)
View(cloudFrameunhappy)
wordcloud(cloudFrameunhappy$word,cloudFrameunhappy$freq)


matchedNegativeunhappy = WNegunhappy[which(WNegunhappy >=2)]
View(matchedNegativeunhappy)
barplot(matchedNegativeunhappy, main="Negative Words", las=2)


#12.	Compare the positive and negative ratios for these two different group of customers 
#The percentage of positive reviews are (approx thrice) more than the percentage of negative reviews. 
#For positive reviews the predominant words and "friendly and great" and 
#for the negative words the word used commonly are "bad and expensive".
