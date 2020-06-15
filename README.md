# Sentiment Analysis of Hotel Surveys

Perform analysis on survey data of hotel customers and sentiment analysis on the same. I started by using str and summary functions in R to understand the data. I created a Wordcloud to understand different sentiments of customers. 

**Data Cleaning:**
1. Converted text to lower case
2. Removed the Punctuations
3. Removing numbers from text
4. Getting rid of the stop words

I calculated the *frequency* of each word in the text document to analyze the ratings of customers. I analyzed the words by calculating the unique words and frequency of each. I separated the **positive words** with **negative words** to better understand the sentiment. I calculated the percentage of positive sentiment and negative sentiments. I visualized each sentiment with bar plots. 

I created two subsets of positive and negative customers based on their reviews and later analyzed them by making word clouds and different visualizations.

**Language: R**

**Libraries: Wordcloud, TM, RCurl, Jsonlite, ggplot2, ggmap, RJSONIO**

**Visualization: WordClouds, Barplots, Distribution Plots**
