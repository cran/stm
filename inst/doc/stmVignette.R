### R code from vignette source 'stmVignette.Rnw'

###################################################
### code chunk number 1: stmVignette.Rnw:132-134
###################################################
library("stm")
set.seed(02138)


###################################################
### code chunk number 2: stmVignette.Rnw:153-165 (eval = FALSE)
###################################################
## #read in your data that is in a spreadsheet form .csv file here)
## #You can find the cleaned version of the poliblog data
## # we used for this paper here: \url{http://goo.gl/tsprNO}.
## data <- read.csv("poliblogs2008.csv")
## #stemming/stopword removal, etc.
## processed <- textProcessor(data$documents, metadata = data)
## #structure and index for usage in the stm model. Verify no-missingness.
## out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
## #output will have object meta, documents, and vocab
## docs <- out$documents
## vocab <- out$vocab
## meta  <-out$meta


###################################################
### code chunk number 3: stmVignette.Rnw:187-188 (eval = FALSE)
###################################################
## plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))


###################################################
### code chunk number 4: stmVignette.Rnw:222-225 (eval = FALSE)
###################################################
## poliblogPrevFit <- stm(out$documents, out$vocab, K = 20,
##             prevalence =~ rating + s(day), max.em.its = 75,
##             data = out$meta, seed = 5926696)


###################################################
### code chunk number 5: stmVignette.Rnw:230-231
###################################################
 load(url("http://goo.gl/91KbfS"))


###################################################
### code chunk number 6: stmVignette.Rnw:245-248 (eval = FALSE)
###################################################
## poliblogSelect <- selectModel(out$documents, out$vocab, K = 20,
##         prevalence =~ rating + s(day), max.em.its = 75,
##         data = meta, runs = 20, seed = 8458159)


###################################################
### code chunk number 7: stmVignette.Rnw:256-257
###################################################
plotModels(poliblogSelect)


###################################################
### code chunk number 8: stmVignette.Rnw:266-267 (eval = FALSE)
###################################################
## poliblogPrevFit <- poliblogSelect$runout[[3]] #choose the third model


###################################################
### code chunk number 9: stmVignette.Rnw:277-287 (eval = FALSE)
###################################################
## storage <- searchK(out$documents, out$vocab, K = c(7, 10),
##         prevalence =~ rating + s(day), data = meta)
## #storage<-manyTopics(out$documents,out$vocab,K=c(7,10),
## #        prevalence =~ rating+s(day),data=meta, runs=10)
## #This chooses the output, a single run of STM that was selected,
## #from the runs of the 3 topic model
## t <- storage$out[[1]]
## #This chooses the output, a single run of STM that was selected,
## #from the runs of the 4 topic model
## t <- storage$out[[2]]


###################################################
### code chunk number 10: stmVignette.Rnw:311-312
###################################################
labelTopics(poliblogPrevFit, c(1, 7, 10))


###################################################
### code chunk number 11: stmVignette.Rnw:321-327
###################################################
thoughts1 <- findThoughts(poliblogPrevFit, texts = shortdoc,
                        n = 2, topics = 1)$docs[[1]]
thoughts7 <- findThoughts(poliblogPrevFit, texts = shortdoc,
                          n = 2, topics = 7)$docs[[1]]
thoughts10 <- findThoughts(poliblogPrevFit, texts = shortdoc,
                           n = 2, topics = 10)$docs[[1]]


###################################################
### code chunk number 12: stmVignette.Rnw:331-333
###################################################
thoughts7 <- c("The Sarah Palin Digest: What We Know About McCain's Running Mate   Very little was known nationally about Alaska Gov. Sarah Palin (R) until Sen. John McCain (R-AZ) selected her as his running mate on Aug. 29. Tonight, Palin will be speaking in", "Here it is: The bio video of Sarah Palin that was supposed to air at the Republican convention last night before her speech.  You should watch it. The video, which was leaked (surprisingly) to Fox News, gives us a glimpse into how the Republicans were")
thoughts10 <- c("Karl Rove orchestrating the Bush Legacy project. President Bush's interview with ABC's Charlie Gibson this week was the first of several planned exit interviews. According to White House press secretary Dana Perino, Bush", "Flashback: Seven years ago today, Bush received Bin Laden Determined to Strike in U.S. memo.  Today marks seven years since the day President Bush received a President's Daily Brief entitled Bin Laden Determined to Strike in U.S.")


###################################################
### code chunk number 13: stmVignette.Rnw:337-341
###################################################
par(mfrow = c(2, 2),mar = c(.5, .5, 1, .5))
plotQuote(thoughts1, width = 40, main = "Topic 1")
plotQuote(thoughts7, width = 40, main = "Topic 7")
plotQuote(thoughts10, width = 40, main = "Topic 10")


###################################################
### code chunk number 14: stmVignette.Rnw:359-362
###################################################
meta$rating <- as.factor(meta$rating)
prep <- estimateEffect(1:20 ~ rating + s(day), poliblogPrevFit,
         meta = meta, uncertainty = "Global")


###################################################
### code chunk number 15: stmVignette.Rnw:379-380
###################################################
plot.STM(poliblogPrevFit, type = "summary", xlim = c(0, .3))


###################################################
### code chunk number 16: stmVignette.Rnw:403-411
###################################################
plot.estimateEffect(prep, covariate = "rating", topics = c(1, 7, 10),
        model = poliblogPrevFit, method = "difference",
        cov.value1 = "Liberal", cov.value2 = "Conservative",
        xlab = "More Conservative ... More Liberal",
        main = "Effect of Liberal vs. Conservative",
        xlim = c(-.1, .1), labeltype = "custom",
        custom.labels = c('Jeremiah Wright', 'Sarah Palin',
                          'Bush Presidency'))


###################################################
### code chunk number 17: stmVignette.Rnw:426-434
###################################################
plot.estimateEffect(prep, "day", method = "continuous", topics = 7,
model = z, printlegend = FALSE, xaxt = "n", xlab = "Time (2008)")
monthseq <- seq(from = as.Date("2008-01-01"),
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,
     at = as.numeric(monthseq) - min(as.numeric(monthseq)),
     labels = monthnames)


###################################################
### code chunk number 18: stmVignette.Rnw:446-449 (eval = FALSE)
###################################################
## poliblogContent <- stm(out$documents, out$vocab, K = 20,
##         prevalence =~ rating + s(day), content =~ rating,
##         max.em.its = 75, data = out$meta, seed = 5593453)


###################################################
### code chunk number 19: stmVignette.Rnw:457-458
###################################################
plot.STM(poliblogContent, type = "perspectives", topics = 10)


###################################################
### code chunk number 20: stmVignette.Rnw:469-470
###################################################
plot.STM(poliblogPrevFit, type = "perspectives", topics = c(9, 10))


###################################################
### code chunk number 21: stmVignette.Rnw:483-486 (eval = FALSE)
###################################################
## poliblogInteraction <- stm(out$documents, out$vocab, K = 20,
##         prevalence =~ rating * day, max.em.its = 75,
##         data = out$meta, seed = 5926696)


###################################################
### code chunk number 22: stmVignette.Rnw:491-505
###################################################
prep <- estimateEffect(c(1) ~ rating * day, poliblogInteraction,
        metadata = meta, uncertainty = "None")

plot.estimateEffect(prep, covariate = "day", model = poliblogInteraction,
        method = "continuous", xlab = "Days", moderator = "rating",
        moderator.value = "Liberal", linecol = "blue", ylim = c(0, .08),
        printlegend = F)

plot.estimateEffect(prep, covariate = "day", model = poliblogInteraction,
        method = "continuous", xlab = "Days", moderator = "rating",
        moderator.value = "Conservative", linecol = "red", add = T,
        printlegend = F)
legend(0, .08, c("Liberal", "Conservative"),
       lwd = 2, col = c("blue", "red"))


###################################################
### code chunk number 23: stmVignette.Rnw:522-523
###################################################
cloud(poliblogPrevFit, topic = 7, scale = c(2,.25))


###################################################
### code chunk number 24: stmVignette.Rnw:534-535
###################################################
mod.out.corr <- topicCorr(poliblogPrevFit)


###################################################
### code chunk number 25: stmVignette.Rnw:541-542
###################################################
plot.topicCorr(mod.out.corr)


###################################################
### code chunk number 26: stmVignette.Rnw:583-586
###################################################
plot(poliblogPrevFit$convergence$bound, type = "l",
     ylab = "Approximate Objective",
     main = "Convergence")


###################################################
### code chunk number 27: stmVignette.Rnw:637-639 (eval = FALSE)
###################################################
## set.seed(02138)
## heldout <- make.heldout(poliblog5k.docs, poliblog5k.voc)


###################################################
### code chunk number 28: stmVignette.Rnw:644-647 (eval = FALSE)
###################################################
## slam <- stm:::doc.to.ijv(heldout$documents)
## slam <- simple_triplet_matrix(slam$i, slam$j, slam$v)
## colnames(slam) <- heldout$vocab


###################################################
### code chunk number 29: stmVignette.Rnw:651-659 (eval = FALSE)
###################################################
## mod1 <- CTM(slam, k = 100)
## control_CTM_VEM <- list(estimate.beta = TRUE, verbose = 1,
##                        seed = as.integer(2138), nstart = 1L, best = TRUE,
##                        var = list(iter.max = 20, tol = 1e-6),
##                        em = list(iter.max = 1000, tol = 1e-3),
##                        initialize = "random",
##                        cg = list(iter.max = -1, tol = 1e-6))
## mod2 <- CTM(slam, k = 100, control = control_CTM_VEM)


###################################################
### code chunk number 30: stmVignette.Rnw:663-667 (eval = FALSE)
###################################################
## stm.mod1 <- stm(heldout$documents, heldout$vocab, K = 100,
##                 init.type = "Spectral")
## stm.mod2 <- stm(heldout$documents, heldout$vocab, K = 100,
##                 init.type = "Spectral", emtol = 1e-3)


