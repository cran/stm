### R code from vignette source 'stmVignette.Rnw'

###################################################
### code chunk number 1: stmVignette.Rnw:166-168
###################################################
library(stm)
set.seed(02138)


###################################################
### code chunk number 2: stmVignette.Rnw:177-182 (eval = FALSE)
###################################################
## data <- read.csv("poliblogs2008.csv")
## data <- data[,-1] #removing row numbers
## processed <- textProcessor(data$documents, metadata=data)
## out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
## meta<-out$meta


###################################################
### code chunk number 3: stmVignette.Rnw:197-202
###################################################
library(lda)
data(poliblog.documents) # these are the word counts
data(poliblog.vocab)     # =words in the vocabulary
data(poliblog.ratings)   # rating of blog as
                         # conservative or liberal


###################################################
### code chunk number 4: stmVignette.Rnw:208-215
###################################################
poliblog.ratings <- as.factor(ifelse(poliblog.ratings==-100,
                                     "Liberal",
                                     "Conservative"))
out <-prepDocuments(poliblog.documents, poliblog.vocab, poliblog.ratings)
poliblog.documents <- out$documents
poliblog.ratings <- out$meta
poliblog.vocab <- out$vocab


###################################################
### code chunk number 5: stmVignette.Rnw:223-234 (eval = FALSE)
###################################################
## library(textir)
## data(congress109)
## temp <- readCorpus(congress109Counts, type="Matrix")
## documents.gs <- temp$documents
## vocab.gs <- temp$vocab
## metadata.gs <- congress109Ideology #this is our metadata.
## out <-prepDocuments(documents.gs, vocab.gs, metadata.gs)
## documents.gs <- temp$documents
## vocab.gs <- temp$vocab
## metadata.gs <- out$meta
## rm(temp)


###################################################
### code chunk number 6: stmVignette.Rnw:252-255 (eval = FALSE)
###################################################
## poliblogPrevFit <- stm(out$documents,out$vocab,K=20,
##             prevalence =~ rating, max.em.its=75, data=meta)
## 


###################################################
### code chunk number 7: stmVignette.Rnw:260-261
###################################################
 load(url("http://dl.dropboxusercontent.com/u/12848660/ModelObjectsSTMFinal.RData"))


###################################################
### code chunk number 8: stmVignette.Rnw:270-273 (eval = FALSE)
###################################################
## poliblogSelect <- selectModel(out$documents,out$vocab,K=20,
##         prevalence =~ rating, max.em.its=j, data=meta,runs=10)
## 


###################################################
### code chunk number 9: stmVignette.Rnw:281-282
###################################################
plotModels(poliblogSelect)


###################################################
### code chunk number 10: stmVignette.Rnw:305-306
###################################################
labelTopics(poliblogPrevFit,topics=c(1,3))


###################################################
### code chunk number 11: stmVignette.Rnw:311-312
###################################################
k<-findThoughts(poliblogPrevFit, texts=shortdoc,  n=2, topics=5)$docs[[1]]


###################################################
### code chunk number 12: stmVignette.Rnw:317-318
###################################################
plotQuote(k, width=60, main="Topic 5")


###################################################
### code chunk number 13: stmVignette.Rnw:333-335
###################################################
meta$rating<-as.factor(meta$rating)
prep <- estimateEffect(1:2 ~ rating,poliblogPrevFit, meta=meta)


###################################################
### code chunk number 14: stmVignette.Rnw:343-348
###################################################
plot.estimateEffect(prep, "rating", model="poliblogPrevFit", method="difference",
            cov.value1="Liberal",cov.value2="Conservative",
           xlab="Liberal-Conservative",
           main="Effect of Liberal vs. Conservative"
           )


###################################################
### code chunk number 15: stmVignette.Rnw:360-362 (eval = FALSE)
###################################################
## poliblogContent <- stm(out$documents,out$vocab,K=20, prevalence =~ rating,
##         content=~rating, max.em.its=75, data=meta)


###################################################
### code chunk number 16: stmVignette.Rnw:370-371
###################################################
plot.STM(poliblogContent,type="perspectives", topics=5)


###################################################
### code chunk number 17: stmVignette.Rnw:381-382
###################################################
plot.STM(poliblogPrevFit,type="perspectives", topics=c(5,6))


###################################################
### code chunk number 18: stmVignette.Rnw:394-397 (eval = FALSE)
###################################################
## poliblogSmoothing <- stm(out$documents,out$vocab,K=20,
##         prevalence =~ rating + s(day), max.em.its=75, data=meta)
## 


###################################################
### code chunk number 19: stmVignette.Rnw:400-403
###################################################
prep <- estimateEffect(c(17,18) ~ rating + s(day), 
                       poliblogSmoothing, metadata=meta, 
                       uncertainty="None")


###################################################
### code chunk number 20: stmVignette.Rnw:410-412
###################################################
plot.estimateEffect(prep, "day", method="continuous", topics=17,
        model=poliblogSmoothing,printlegend=FALSE)


###################################################
### code chunk number 21: stmVignette.Rnw:422-425 (eval = FALSE)
###################################################
## poliblogInteraction <- stm(out$documents,out$vocab,K=20,
##     prevalence =~ rating*day, max.em.its=j, data=meta)
## 


###################################################
### code chunk number 22: stmVignette.Rnw:431-435
###################################################
prep <- estimateEffect(c(7) ~ rating*day,
        poliblogInteraction, metadata=meta, uncertainty="None")
plot.estimateEffect(prep, covariate="day", model=poliblogInteraction,
                 method="continuous")


###################################################
### code chunk number 23: stmVignette.Rnw:452-453
###################################################
plot.STM(poliblogPrevFit,type="summary")


###################################################
### code chunk number 24: stmVignette.Rnw:462-463
###################################################
mod.out.corr<-topicCorr(poliblogPrevFit)


###################################################
### code chunk number 25: stmVignette.Rnw:468-469
###################################################
plot.topicCorr(mod.out.corr)


###################################################
### code chunk number 26: stmVignette.Rnw:492-494
###################################################
plot(poliblogPrevFit$convergence$bound,type="l", ylab="Approximate Objective",
    main="Convergence")


###################################################
### code chunk number 27: stmVignette.Rnw:571-572 (eval = FALSE)
###################################################
## mod.out <- stm(documents,vocab,K=5, max.em.its=5)


###################################################
### code chunk number 28: stmVignette.Rnw:576-577 (eval = FALSE)
###################################################
## str(mod.out,1)


###################################################
### code chunk number 29: stmVignette.Rnw:589-593 (eval = FALSE)
###################################################
## mod.out <- selectModel(documents,vocab, K=10,
##                prevalence= ~party, data=metadata,
##                content=~party, runs=3)
## plotModels(models)


###################################################
### code chunk number 30: stmVignette.Rnw:602-607 (eval = FALSE)
###################################################
## ?ploteffect()
## ?prep.plot()
## ?plot.stm()
## ?plottopics()
## ?plotquote()


###################################################
### code chunk number 31: stmVignette.Rnw:618-620
###################################################
mod.out <- stm(poliblog.documents,poliblog.vocab,K=5,
               max.em.its=3,verbose=FALSE)


###################################################
### code chunk number 32: stmVignette.Rnw:627-628
###################################################
labelTopics(mod.out,n=3)


###################################################
### code chunk number 33: stmVignette.Rnw:636-638 (eval = FALSE)
###################################################
## mod.out <- stm(documents,vocab, K=10,
##                prevalence= ~party, data=metadata,max.em.its=5)


###################################################
### code chunk number 34: stmVignette.Rnw:644-646
###################################################
length(poliblog.documents) #there are 773 documents
length(poliblog.vocab)     #and 1290 items in the vocab


