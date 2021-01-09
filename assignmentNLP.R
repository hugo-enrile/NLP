################################################################################
# Author: Hugo Enrile Lacalle                                                  #
# Contact: hugoenrilelacalle@gmail.com // hugo.enrile@alumnos.upm.es           #
# Course: Intelligent Systems, MSc in Data Science UPM 2020-2021               #
################################################################################

#### Installing packages and loading libraries ####
# Package for web scrapping, NLP models and visualization
install.packages("rvest")
install.packages("wordcloud2")
install.packages("openNLPmodels.en")
# Loading needed libraries
library(rvest)
library(rJava)
.jinit(parameters="-Xmx4g")
library(NLP) 
library(openNLP) 
library(tm)
library(stringr)
library(dplyr)
library(tm)
library(wordcloud)
library(ggplot2)
library(reshape2)
library(wordcloud2)

#### Web Scrapping ####
# Save the url where we are going to look for the headlines
url.marca <- "https://www.marca.com/"
url.as <- "https://as.com/"
url.elpais <- "https://elpais.com/deportes/"
url.sport <- "https://www.sport.es/es/"
url.elmundo <- "https://www.elmundo.es/deportes.html"
# Create an html object of the previous urls
web.marca <- read_html(url.marca)
web.as <- read_html(url.as)
web.elpais <- read_html(url.elpais)
web.sport <- read_html(url.sport)
web.elmundo <- read_html(url.elmundo)
# Here we can display the resulting objects
web.marca
web.as
web.elpais
web.sport
web.elmundo
# Extract headlines. By observing the source code, I have identifyied which tag
# should I look for to extract the headlines that I wanted
titulares.marca <- html_nodes(web.marca, "h2")
titulares.as <- html_nodes(web.as, "h2")
titulares.elpais <- html_nodes(web.elpais, "h2")
titulares.sport <- html_nodes(web.sport, "h2")
titulares.elmundo <- html_nodes(web.elmundo, "h2")
# Save them as text
titulares.marca.text <- html_text(titulares.marca)
titulares.as.text <- html_text(titulares.as)
titulares.elpais.text <- html_text(titulares.elpais)
titulares.sport.text <- html_text(titulares.sport)
titulares.elmundo.text <- html_text(titulares.elmundo)
# Here we can display what we extract before
titulares.marca.text
titulares.as.text
titulares.elpais.text
titulares.sport.text
titulares.elmundo.text
# Due to the fact headlines could have a not desired format, we clean the results
# to prepare the data which we are going to work with
titulares.marca.text <- gsub("\n", "", titulares.marca.text)
titulares.as.text <- gsub("\n", "", titulares.as.text)
titulares.elpais.text <- gsub("\n", "", titulares.elpais.text)
titulares.sport.text <- gsub("\n", "", titulares.sport.text)
titulares.elmundo.text <- gsub("\n", "", titulares.elmundo.text)

titulares.marca.text <- gsub("\"", "", titulares.marca.text)
titulares.as.text <- gsub("\"", "", titulares.as.text)
titulares.elpais.text <- gsub("\"", "", titulares.elpais.text)
titulares.sport.text <- gsub("\"", "", titulares.sport.text)
titulares.elmundo.text <- gsub("\"", "", titulares.elmundo.text)

titulares.marca.text <- trimws(titulares.marca.text)
titulares.as.text <- trimws(titulares.as.text)
titulares.elpais.text <- trimws(titulares.elpais.text)
titulares.sport.text <- trimws(titulares.sport.text)
titulares.elmundo.text <- trimws(titulares.elmundo.text)
# Here we can display the results
titulares.marca.text
titulares.as.text
titulares.elpais.text
titulares.sport.text
titulares.elmundo.text

#### Functions by Raúl García Castro (rgarcia@fi.upm.es) ####
getAnnotationsFromDocument = function(doc){
  x=as.String(doc)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator, y1)
  return(y2)  
} 

getAnnotatedMergedDocument = function(doc,annotations){
  x=as.String(doc)
  y2w <- subset(annotations, type == "word")
  tags <- sapply(y2w$features, '[[', "POS")
  r1 <- sprintf("%s/%s", x[y2w], tags)
  r2 <- paste(r1, collapse = " ")
  return(r2)  
} 

getAnnotatedPlainTextDocument = function(doc,annotations){
  x=as.String(doc)
  a = AnnotatedPlainTextDocument(x,annotations)
  return(a)  
}

detectPatternOnDocument <- function(doc, pattern) {
  x=as.String(doc)
  res=str_match_all(x,pattern)
  
  dimrow=dim(res[[1]])[1]
  dimcol=dim(res[[1]])[2]
  
  # If there are no rows, no matches have been found
  if (dimrow == 0) {
    return(NA)
  }else{
    if (dimcol > 2){
      # If there are three or more columns, we have to paste all the groups together
      for (i in 1:dimrow) {
        res[[1]][i,2] = paste(res[[1]][i,2:dimcol], collapse = ' ')
      }
    }
    
    # We return all the results found separated by ','
    if (dimcol != 1) {
      result = paste(res[[1]][,2], collapse = ', ')
    }else{
      result = paste(res[[1]][,1], collapse = ', ')
    }
    return(result)
  }
}

detectPatternOnDocumentWithContext <- function(doc, pattern) {
  txt=as.String(doc)
  number=50
  coord=str_locate(txt,pattern)
  res3=substr(txt,coord[1]-number,coord[2]+number)
  return (res3)
}

detectPatternsInCorpus = function(corpus, patterns){
  vallEntities <- data.frame(matrix(NA, ncol = length(patterns)+1, 
                                    nrow = length(corpus)))
  names(vallEntities) <- c("File",patterns)
  for (i in 1:length(patterns)) {
    vallEntities[,i+1]=unlist(lapply(corpus, detectPatternOnDocument, 
                                     pattern=patterns[i]))
  }
  for (i in 1:length(corpus)) {
    vallEntities$File[i]=meta(corpus[[i]])$id
  }
  return (vallEntities)  
}

detectPatternsInTaggedCorpus = function(corpus, taggedCorpus, patterns){
  vallEntities <- data.frame(matrix(NA, ncol = length(patterns)+1, 
                                    nrow = length(corpus)))
  names(vallEntities) <- c("File",patterns)
  for (i in 1:length(patterns)) {
    vallEntities[,i+1]=unlist(lapply(taggedCorpus, detectPatternOnDocument, 
                                     pattern=patterns[i]))
  }
  for (i in 1:length(corpus)) {
    vallEntities$File[i]=meta(corpus[[i]])$id
  }
  return (vallEntities)  
}

countMatchesPerColumn = function (df) {
  entityCountPerPattern <- data.frame(matrix(NA, ncol = 2, 
                                             nrow = length(names(df))-1))
  names(entityCountPerPattern) <- c("Entity","Count")
  
  for (i in 2:length(names(df))) {
    entityCountPerPattern$Entity[i-1] = names(df)[i]
    entityCountPerPattern$Count[i-1] = nrow(subset(df, !is.na(df[i])))
  }
  return (entityCountPerPattern)
}

countMatchesPerRow = function (df) {
  entityCountPerFile <- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  names(entityCountPerFile) <- c("File","Count")
  
  for (i in 1:nrow(df)) {
    entityCountPerFile$File[i] = df$File[i]
    entityCountPerFile$Count[i] = length(Filter(Negate(is.na),df[i,2:length(df[i,])]))
  }
  return (entityCountPerFile[entityCountPerFile[2]!=0,])
}

printMatchesPerPattern = function (patterns, matches) {
  for (i in 1:length(patterns)){
    print(paste("PATTERN: ",patterns[i]))
    strings = matches[,i+1][!is.na(unlist(matches[,i+1]))]
    print(strings)
    print(" ") 
  }
}

mergeAllMatchesInLists = function (df) {
  matchesPerFile = rep(list(list()), nrow(df))
  for (i in 1:nrow(df)) {    
    matches=list()
    for (j in 2:ncol(df)){
      if (grepl(',',df[i,j])){
        b=strsplit(as.character(df[i,j]),split=',')
        for (j in 1:length(b[[1]])){
          matches= c(matches,str_trim(b[[1]][j]))
        }
      }else{
        if (!(is.na(df[i,j]))){
          matches = c(matches,str_trim(df[i,j]))
        }
      }
    }
    matches = unique(matches)
    matchesPerFile[[i]]=append(matchesPerFile[[i]],matches)
  }
  
  files = df[,1]
  matches = matchesPerFile
  
  allMatches<- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  names(allMatches) <- c("Files","Matches")
  
  allMatches$Files=files
  allMatches$Matches=matches
  
  return (allMatches)
}

mergeGoldStandardInLists = function (df) {
  matchesPerFile = rep(list(list()), nrow(df))
  
  for (i in 1:nrow(df)) {    
    matches=as.list(unlist(Filter(Negate(is.na),df[i,2:length(df)])))
    matchesPerFile[[i]]=append(matchesPerFile[[i]],matches)
  }
  
  files = df[,1]
  matches = matchesPerFile
  
  allMatches<- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  names(allMatches) <- c("Files","Matches")
  
  allMatches$Files=files
  allMatches$Matches=matches
  
  return (allMatches)
}

calculateMetrics = function (matches, matches.gs) {
  
  metrics<- data.frame(matrix(NA, ncol = 3, nrow = 1))
  names(metrics) <- c("Precision","Recall","Fmeasure")
  
  numCorrect = 0
  allAnswers = 0
  possibleAnswers = 0
  
  for (i in 1:nrow(matches)) {    
    if (length(matches.gs$Matches[[i]])!=0) {
      l = str_trim(unlist(matches[i,2]))
      l.gs = unname(unlist(matches.gs[i,2]))
      intersection = intersect(l, l.gs)
      numCorrect = numCorrect + length(intersect(l, l.gs))
      allAnswers = allAnswers + length (l)
      possibleAnswers = possibleAnswers + length(l.gs)    
    }
  }
  
  metrics$Precision = numCorrect / allAnswers
  metrics$Recall = numCorrect / possibleAnswers
  
  beta = 1
  if ((metrics$Precision == 0) & (metrics$Recall == 0)) {
    metrics$Fmeasure = 0
  } else {
    metrics$Fmeasure = ((sqrt(beta)+1) * metrics$Precision * metrics$Recall) / 
      ((sqrt(beta)*metrics$Precision) + metrics$Recall)
  }
  
  return(metrics)
}

#### Create the dataframe ####
df = as.data.frame(c(titulares.marca.text, titulares.as.text, titulares.elpais.text, titulares.sport.text, titulares.elmundo.text))
df <- df[df$`c(titulares.marca.text, titulares.as.text, titulares.elpais.text, titulares.sport.text, titulares.elmundo.text)` != "",]
df <- as.data.frame(df)
df <- rename(df, 'Headlines' = 'df')
corpus <- Corpus(VectorSource(df$Headlines))
inspect(corpus)
# This task can take a time
annotations = lapply(corpus, getAnnotationsFromDocument)

corpus.tagged = Map(getAnnotatedPlainTextDocument, corpus, annotations)
inspect(corpus.tagged[[1]])

corpus.taggedText = Map(getAnnotatedMergedDocument, corpus, annotations)
corpus.taggedText[[1]]

#### Word recognition ####
# First attempt to see how word recognition works and how can I face the problem
pattern0 = c("de")
pattern0 = c(pattern0, "a")
pattern0 = c(pattern0, "con")

matches0 = detectPatternsInCorpus(corpus, pattern0)
matches0[!is.na(matches0[3]),c(1,3)]
countMatchesPerRow(matches0) 
countMatchesPerColumn(matches0) 

for (i in 1:length(pattern0)){
  print(paste("PATTERN: ",pattern0[i]))
  strings = lapply(corpus, detectPatternOnDocumentWithContext, pattern=pattern0[i])
  print(unlist(strings[!is.na(unlist(strings))]))
  print(" ")
}

#### Easy pattern recognition ####
# Second attempt, this time using regular expressions to make more general searching
pattern1 = c("([A-Z][a-z]*)[\\s-]([A-Z][a-z]*):")
pattern1 = c(pattern1, " a ([A-Z][a-z]*)[\\s-]([A-Z][a-z]*)")
pattern1 = c(pattern1, " de ([A-Z][a-z]*)[\\s-]([A-Z][a-z]*)")
pattern1 = c(pattern1, " de ([A-Za-z]*)[\\s-]([A-Z][a-z]*)+$")

pattern1 = c("([^a-z ,.:¿¡][a-ú]*)( [^a-z ,.:¿¡][a-ú]*){0,1}")

matches1 = detectPatternsInCorpus(corpus, pattern1)
matches1[!is.na(matches1[4]),c(1,4)]
countMatchesPerRow(matches1) 
countMatchesPerColumn(matches1) 
printMatchesPerPattern(pattern1, matches1)
countMatchesPerRow(matches1) 

#### Pattern recognition using part-of speech tags ####
# Improvement of the previous attempt and the final one. By this way I can find
# in the headlines almost everything I am interested in
pattern2 = c("([^a-z ,.:¿¡][A-ú]*)/NNP( [^a-z ,.:¿¡][a-ú]*){0,1}/NNP se/NN")
pattern2 = c(pattern2, "([^a-z ,.:¿¡][A-ú]*)/NNP se/NN")
pattern2 = c(pattern2, "([^a-z ,.:¿¡][a-ú]*)/NNP( [^a-z ,.:¿¡][a-ú]*){0,1}/NNP y/NN")
pattern2 = c(pattern2, " ([^a-z ,.:¿¡][a-ú]*)/NNP y/NN")
pattern2 = c(pattern2, " y/NN ([^a-z ,.:¿¡][a-ú]*)/NNP( [^a-z ,.:¿¡][a-ú]*){0,1}/NNP ")
pattern2 = c(pattern2, " y/NN ([^a-z ,.:¿¡][a-ú]*)/NNP")
pattern2 = c(pattern2, " de/IN ([^a-z ,.:¿¡][a-ú]*)/NNP( [^a-z ,.:¿¡][a-ú]*){0,1}/NNP ")
pattern2 = c(pattern2, " de/IN ([^a-z ,.:¿¡][a-ú]*)/NNP ")
pattern2 = c(pattern2, "([^a-z ,.:¿¡][A-ú]*)/NNP( [^a-z ,.:¿¡][A-ú]*){0,1}/NNP :/:")
pattern2 = c(pattern2, "([^a-z ,.:¿¡][A-ú]*)/NNP :/:")
pattern2 = c(pattern2, "a/DT ([^a-z ,.:¿¡][a-ú]*){0,1}/NNP")
pattern2 = c(pattern2, "a/DT ([^a-z ,.:¿¡][a-ú]*)/NNP( [^a-z ,.:¿¡][a-ú]*){0,1}/NNP")
pattern2 = c(pattern2, "([^a-z ,.:¿¡][A-ú]*)/NNP")
pattern2 = c(pattern2, " de/IN ([^a-z ,.:¿¡\\B][a-ú]*)/NNP ") 
pattern2 = c(pattern2, " ([^a-z ,.:¿¡][a-ú]*)/FW")
pattern2 = c(pattern2, " ([^a-z ,.:¿¡][a-ú]*)/NNP( [^a-z ,.:¿¡][a-ú]*)/NNP")
pattern2 = c(pattern2, " ([^a-z ,.:¿¡][a-ú]*)/NNP( [^a-z ,.:¿¡][a-ú]*)/NNP ,/,")
pattern2 = c(pattern2, " ([^a-z ,.:¿¡][a-ú]*)/NNP ,/,")
pattern2 = c(pattern2, "([A-z]*)/NNP ([A-z]*)/NNP ")
pattern2 = c(pattern2, '([^a-z ,.:"¿¡][A-ú]*)/NNP( [^a-z ,.:¿¡][A-ú]*)/NNP ')

# Look for the patterns in the created corpus
allEntities = detectPatternsInTaggedCorpus(corpus, corpus.taggedText, pattern2)
printMatchesPerPattern(pattern2, allEntities)
colnames(allEntities[1])
allEntities[,2]
# Clean the dataframe
for (i in 1:length(allEntities)){
  for (j in 1:length(allEntities[,i])){
    allEntities[j,i] <- gsub("/NNP", "", allEntities[j,i])
  }
}
temp2 <- list()
for (i in 2:dim(allEntities[1])){
  temp = 0
  for (j in 1:length(allEntities)){
    if (is.na(allEntities[i,j])){
      temp = temp + 1
    }
  }
  if (temp == length(allEntities)-1){
    temp2 <- append(temp2,i)
  }
}
item = 1
for (item in rev(temp2)){
  allEntities <- allEntities[allEntities$File != (item), ]
}
allEntities <- allEntities[-1]
a <- list()
for (k in 1:length(allEntities)){
  a <- append(a,Filter(Negate(is.na),allEntities[[k]]))
}
a_df <- as.data.frame(a, header = FALSE)
a_df <- data.frame(t(a_df[-1]))
a_df <- rename(a_df, 'Names' = 't.a_df..1..')
for (i in 1:length(a_df)){
  a_df$Names[i] <- as.String(a_df$Names[i])
}

# Now, we can order the dataframe based on the number of times each name appears
# in the headlines
df.order = a_df %>% 
  group_by(Names) %>% 
  count(Names) %>%
  arrange(desc(n))

# Cleaning and preparing the final dataframe
df.order <- df.order[nchar(df.order$Names) > 3,]
# Deleting white spaces, duplicated ones...
for (i in 1:length(df.order$Names)){
  df.order$Names[i] <- trimws(df.order$Names[i])
  df.order$Names[i] <- gsub("  ", " ", df.order$Names[i])
  df.order$Names[i] <- gsub("El","", df.order$Names[i])
  df.order$Names[i] <- gsub("El,","", df.order$Names[i])
  df.order$Names[i] <- gsub("La","", df.order$Names[i])
  df.order$Names[i] <- gsub("La,","", df.order$Names[i])
  df.order$Names[i] <- gsub("-","", df.order$Names[i])
  df.order$Names[i] <- gsub("'","", df.order$Names[i])
  df.order$Names[i] <- gsub(",","", df.order$Names[i])
  
  df.order$Names[i] <- trimws(df.order$Names[i])
} 

# This is not done but was an attempt to merge rows that refer to the same person
# or object but in a different way (only by the surname, name + surname...). I 
# finally decided not to use it because the program works well without it
#for (i in 1:length(df.order$Names)){
#  if(df.order$n[i] != 0){
#   for (j in 2:length(df.order$Names)){
#      if(nchar(df.order$Names[i] >= nchar(df.order$Names[j]))){
#       if(grepl(df.order$Names[j], df.order$Names[i], fixed = TRUE) == TRUE){
#         df.order$n[i] <- df.order$n[i]+df.order$n[j]
#       }
#     }
#   }
# }
#}
# Deleting extremely large rows because of not good retrievals 
for (i in 1:length(df.order$Names)){
  els <- strsplit(df.order$Names[i], " ")
  els <- unlist(els)
  if (length(els) > 3){
    df.order$n[i] <- 0
  }
}
df.order <- df.order[(df.order$n) != 0,]

#### Visualization ####
# Based on the rpubs that we have seen in class from Raúl García Castro

# First wordcloud using the standard wordcloud library
pal = brewer.pal(8, "Blues")
set.seed(1234)
wordcloud(words = df.order$Names, freq = df.order$n, min.freq = 4, max.words = 30,
          random.order = FALSE, rot.per = 0.35, colors = pal, scale = c(2.5, 0.2))

# Second wordcloud using the wordcloud2 library. Allow us to personalize more the
# wordcloud
df.order <- rename(df.order, 'word' = 'Names')
df.order <- rename(df.order, 'freq' = 'n')
wordcloud2(data = df.order, size = 1.6)