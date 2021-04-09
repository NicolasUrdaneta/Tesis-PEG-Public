################################################################################
##                                                                            ##
##                                2. Lasso                                    ##
##                                                                            ##
################################################################################
##                                                                            ##
##  This program takes the comments dataset and runs a lasso regression       ##
##  with all the most frequent words in the comments. It must clean the       ##
##  dataset, convert it into a document term matrix and then run the lasso.   ##
##                                                                            ##
##  A) Open Datasets                                                          ##
##  B) Comment cleaning                                                       ##
##  C) Create Word Matrix data                                                ##
##  D) Create Word Matrix data for stemmed words                              ##
##  E) Clean datasets for lasso                                               ##
##  F) Lasso stemming words                                                   ##
##                                                                            ##
################################################################################


library(tidyverse)
library(tidytext)
library(stringr)
library(caret)
library(tm)



##____________________________________________________________________________##
##                                                                            ##
##                            A) Open Datasets                                ##
##____________________________________________________________________________##


db.comments<-LoadRds("Comentarios Encuestas Root", modified)[1:10000,] %>% 
  filter(is.na(sexo_prof)==F)

##__________________________________________________________________________##
##                                                                          ##
##                    D) Create most frequent word lists                    ##
##__________________________________________________________________________##


word.list.fix<-read_excel(paste0(wordlist,"/Words list RAE - Adjustment.xlsx"), 
                          sheet="Words list RAE - Adjustment")
nombre.apellido<-read_excel(paste0(wordlist,"/Words list professor manual.xlsx")
                            , sheet="Nombre-Apellido")

## Replace manual fixes to root_word
word.list.fix$root_word<-ifelse(is.na(word.list.fix$root_word_fix)==T,
                                word.list.fix$root_word,word.list.fix$root_word_fix)

word.list.fix<-word.list.fix %>% 
  dplyr::select(word,root_word,n)

nombre.apellido<-nombre.apellido %>% 
  dplyr::select(word,Nombre,Apellido) %>% 
  filter(Nombre==1 | Apellido==1)

words<-full_join(word.list.fix,nombre.apellido, by="word")
words$Nombre<-ifelse(is.na(words$Nombre)==T,0,words$Nombre)
words$Apellido<-ifelse(is.na(words$Apellido)==T,0,words$Apellido)
words$root_word<-ifelse(words$Nombre==1,"NOMBRE",words$root_word)
words$root_word<-ifelse(words$Apellido==1,"APELLIDO",words$root_word)
words$word<-ifelse(words$Nombre==1,"NOMBRE",words$word)
words$word<-ifelse(words$Apellido==1,"APELLIDO",words$word)
words_original<-words %>% 
  group_by(word) %>% 
  summarise(count=sum(n)) %>% 
  ungroup() %>% 
  filter(count>120)
words_root<-words %>% 
  group_by(root_word) %>% 
  summarise(count=sum(n)) %>% 
  ungroup() %>% 
  filter(count>41)

top5000.words<-as.vector(words$word)
top5000.root.words<-as.vector(words_root$root_word)

##__________________________________________________________________________##
##                                                                          ##
##                          B) Comment cleaning                             ##
##__________________________________________________________________________##


db.comments$line.number=as.numeric(rownames(db.comments))

db.comments.tokens.original<-db.comments %>%
  unnest_tokens(word, comment_original) %>% 
  count(line.number,word, sort=F) %>% 
  filter(word %in% top5000.words)

db.comments.tokens.root<-db.comments %>%
  unnest_tokens(word, comment_root) %>% 
  count(line.number,word, sort=F) %>% 
  filter(word %in% top5000.root.words)


##__________________________________________________________________________##
##                                                                          ##
##                       C) Create Word Matrix data                         ##
##__________________________________________________________________________##


dfm_original<-db.comments.tokens.original %>% 
  cast_dtm(document = line.number, term = word, value = n)
dfm_root<-db.comments.tokens.root %>% 
  cast_dtm(document = line.number, term = word, value = n)

rm(db.comments.tokens.original, db.comments.tokens.root)

y_original<-as.data.frame(db.comments[as.numeric(dfm_original$dimnames$Docs),"sexo_prof"])
y_root<-as.data.frame(db.comments[as.numeric(dfm_root$dimnames$Docs),"sexo_prof"])

dim(dfm)
dim(db.y)

lambda <- 10^seq(-2, -0.5, length = 10)
set.seed(1234)
lasso<-train(x = as.data.frame(as.matrix(dfm[,1:2500])),
             y = make.names(factor(y$sexo_prof)), method = "glmnet",
             trControl = trainControl("cv", number = 3,
                                      summaryFunction = twoClassSummary,
                                      classProbs=TRUE), 
             tuneGrid = expand.grid(alpha = 1,lambda=lambda),
             preProcess = c("center", "scale"))

lasso

plot(lasso$finalModel)
plot(lasso)

coefs<-as.data.frame(coef(lasso$finalModel,lasso$bestTune$lambda)[,1])
summary(coefs$`coef(lasso$finalModel, lasso$bestTune$lambda)[, 1]`)

