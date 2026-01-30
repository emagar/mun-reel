########################################
## Code for ayuntamiento vote analysis ##
## Date started: 26nov2023             ##
## Revised/updated: 12dic2025          ##
## By emagar at itam dot mx            ##
#########################################

##########################################################
## Run this code for data prep in case data has changed ##
##########################################################
#source("/home/eric/Desktop/MXelsCalendGovt/elecReturns/code/ay.r")                    ## slow!! preps coalagg and coalsplit datasets
#source("/home/eric/Desktop/MXelsCalendGovt/reelec/mun-reel/code/ay-vote-data-prep.r") ## saves a new ay-mu-vote-analysis.RData

######################
## read saved image ##
######################
library(DataCombine) # easy lags
library(plyr)
rm(list = ls())
##
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data"
setwd(wd)
load(file = "ay-mu-vote-analysis.RData")

## Describe turnout
ids$trienio <- as.numeric(as.character(ids$trienio)) ## make not factor
tapply(X=vot$turn.ln, INDEX=ids$trienio, summary) ## by trienio
with(vot, aggregate(turn.ln, by = list(ids$trienio), FUN=summary))
library(vioplot)
for (i in 1:32){
    pdf(file=paste0("../plots/turn-descrip/",i,".pdf"))
    vioplot(vot$turn.ln[ids$edon==i] ~ as.numeric(as.character(ids$trienio[ids$edon==i])), xlab = "", ylab = "", ylim = c(0,1))
    abline(h=mean(vot$turn.ln, na.rm=TRUE), lty=1)
    title(main=edon2estado(i), cex.main = 2)
    ## plot(c(0,10), c(0,1), xlim=c(0.5,10.5), type = "n", xlab = "", ylab = "", axes = FALSE)
    ## axis(1, at=1:10, labels=seq(1997,2024,3)); axis(2)
    ## vioplot(vot$turn.ln[ids$edon==i] ~ as.numeric(as.character(ids$trienio[ids$edon==i])), add = TRUE)
    ## abline(h=mean(vot$turn.ln, na.rm=TRUE), lty=1)
    ## title(main=edon2estado(i), cex.main = 2)
    dev.off()
}

sel.r <- grep("jal-19", vot$emm)
vot$turn.ln[sel.r]
vot$emm[sel.r[20]]

## Data for error correction model: L stands for lags, D stands for deltas
table(lnrlag $emm == lnrdelta $emm) ## check order
tmp <- lnrlag; colnames(tmp)[-1:-2] <- paste0("L", colnames(tmp)[-1:-2])
lnrecm <- tmp
tmp <- lnrdelta; colnames(tmp)[-1:-2] <- paste0("D", colnames(tmp)[-1:-2])
lnrecm <- cbind(lnrecm, tmp[,-1:-2])
colnames(lnrecm)
lnrecm <- cbind(lnrecm[,-1:-2], ids) ## add ids

##
## wrap lm commands in function
mylm <- function(dv, predictors, data, subset = NULL){
    ## data <- lnr
    ## dv <- "Dpan"
    ## predictors <- c("dincballot * dinc", "dgov", "dpres", "vhat.", "popshincab")
    ## subset <- "ord>2005"
    ## predictors <- c("di", "diballno", "diball", "dgov", "dpres", "vhat.", "popshincab")
    ##
    dv2 <- sub("^[Dl]o?g?[(]?([a-z.]+)[)]?", "\\1", dv) ## drop log from name, if any
    if (dv2 %notin% c("pan","pri","morena","left","oth","turn.ln")) stop("Wrong party")
    data <- data[order(data$ord),] # sort
    ids  <- ids [order(ids $ord),] # sort
    data <- cbind(ids, data[,-1])  # merge
    ## subset
    if (!is.null(subset)){
        subset <- sub ("^([a-z])", "data$\\1",  subset)
        subset <- gsub(" ([a-z])", " data$\\1", subset)
        subset <- str2expression(subset)
        data <- data[eval(subset),]
    }
    ## ## drop NAs in dv
    ## sel <- paste0("!is.na(data$", dv, ")")
    ## sel <- str2expression(sel)
    ## sel <- eval(sel)
    ## data <- data[sel,]
    ##
    ## add dv to party-specific predictors
    sel <- grep("^D?di|^D?dgov|^D?dpres|^D?vhat.|^L$", predictors)
    #sel <- which(predictors %in% c("dinc", "dincballot", "dgov", "dpres", "vhat.", "di", "diballno", "diballpan"))
    if (length(sel)>0 & dv2!="turn.ln") predictors[sel] <- paste0(predictors[sel], dv2)
    ## collapse predictors
    predictors <- paste(predictors, collapse = " + ")
    formula <- paste(dv, predictors, sep = " ~ ")
    model <- lm(formula = formula, data = data)
    return(model)
}

## models of voting
tmpp <- c("dincballot * dinc", "dgov", "dpres", "vhat.", "popshincab", "wsdalt", "lats", "p5lish", "lumwpop20", "as.factor(trienio)"); tmp <- mylm(dv="pan", data=lnr, subset="yr>1999", predictors = tmpp); summary(tmp)
##
tmpp <- c("di", "diballno", "diball", "dgov", "dpres", "vhat.", "popshincab", "wsdalt", "lats", "p5lish", "lumwpop20", "as.factor(trienio)"); tmp <- mylm(dv="pan", data=lnr, subset="yr>1999", predictors = tmpp); summary(tmp)
##
tmpp <- c("di", "diballno", "diball", "dgov", "dpres", "vhat.", "ncand", "popshincab", "wsdalt", "lats", "p5lish", "lumwpop20", "as.factor(trienio)"); tmp <- mylm(dv="pan", data=vo4, subset="yr>1999", predictors = tmpp); summary(tmp)
##
tmpp <- c("di", "diballno", "diball", "dgov", "dpres", "ncand", "popshincab", "wsdalt", "lats", "p5lish", "lumwpop20", "as.factor(trienio)"); tmp <- mylm(dv="pan", data=res, subset="yr>1999", predictors = tmpp); summary(tmp)
##
tmpp <- c("di", "diballno", "diball", "ncand", "dgov", "dpres"); tmp <- mylm(dv="pan", data=lnrdelta, subset="yr>1999", predictors = tmpp); summary(tmp)
##
tmpp <- c("L", "Ddi", "Ddiballno", "Ddiball", "Ddgov", "Ddpres", "wsdalt", "lats", "lumwpop20", "as.factor(trienio)"); tmp <- mylm(dv="Dpan", data = lnrecm, subset = "yr>2017", predictors = tmpp); summary(tmp)#
#
tmpp <- c("dincballot * dinc", "ncand", "dgov", "dpres"); tmp <- mylm(dv="pan", data=lnrdelta, subset="yr>1999", predictors = tmpp); summary(tmp)
##
#############
## turnout ##
#############
tmpp <- c("dconcgo", "dconcdf", "dincballot", "mg", "popshincab", "wsdalt", "lats", "p5lish", "lumwpop20", "as.factor(trienio)"); tmp <- mylm(dv="turn.ln", data=lnr, subset="yr>2017", predictors = tmpp); summary(tmp)
##
tmpp <- c("dconcgo", "dconcpr", "dincballot", "mg", "as.factor(trienio)"); tmp <- mylm(dv="turn.ln", data=lnrdelta, subset="yr>2017", predictors = tmpp); summary(tmp)
##
tmpp <- c("dconcgo", "dconcpr", "dconcdf", "dincballot", "mg", "wsdalt", "as.factor(trienio)"); tmp <- mylm(dv="turn.ln", data=lnrdelta, subset="yr>2017", predictors = tmpp); summary(tmp)
##
## ############################
## ## error correction model ##
## ############################
## colnames(lnrecm)
## tmpp <- c("dconcgo", "dconcdf", "dincballot", "mg", "as.factor(trienio)"); tmp <- mylm(dv="Dturn.ln", data=lnrecm, subset="yr>1999", predictors = tmpp); summary(tmp)

## #############################################
## ## OJO: 29ene2026 esto lo quité ahora, cuando use aymu1970-on.coalition-candidacies será más fina la atribución de candidaturas
## ## assign coalition vote to one party only ##
## #############################################
## luro[1,]
## luro$win.simple       <- luro$win
## luro$part2nd.simple   <- luro$part2nd
## luro$win.prior.simple <- luro$win.prior
## ## pan-prd 2018 and post to pan
## funfun <- function(variab=luro$win.simple , yr=2018){
##     sel.1 <- grep("pan"    , variab)
##     sel.2 <- grep("prd"    , variab)
##     sel.3 <- which(luro$yr>=yr)
##     sel.4 <- intersect(sel.1, sel.2)
##     sel.5 <- intersect(sel.4, sel.3)
##     print(paste("N manip:", length(sel.5)))
##     variab[sel.5] <- gsub("prd-|-prd", "", luro$win.simple[sel.5])
##     return(variab)
## }
## luro$win.simple       <- funfun(luro$win.simple       , 2018)
## luro$part2nd.simple   <- funfun(luro$part2nd.simple   , 2018)
## luro$win.prior.simple <- funfun(luro$win.prior.simple , 2021)
## table(luro$win.prior.simple[luro$yr>=2018])
## ## pan-pri dgo 2022 and post to pri
## funfun <- function(variab=luro$win.simple , yr=2022){
##     sel.1 <- grep("pan"    , variab)
##     sel.2 <- grep("pri"    , variab)
##     sel.3 <- which(luro$yr>=yr)
##     sel.4 <- intersect(sel.1, sel.2)
##     sel.5 <- intersect(sel.4, sel.3)
##     print(paste("N manip:", length(grep("^dgo", luro$emm[sel.5]))))
##     tmp <- variab[sel.5] ## subset for manip state by state
##     tmp[grep("^dgo", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^dgo", luro$emm[sel.5])])  ## to pri
##     tmp -> variab[sel.5]  ## return after manip
##     return(variab)
## }
## luro$win.simple       <- funfun(luro$win.simple       , 2022)
## luro$part2nd.simple   <- funfun(luro$part2nd.simple   , 2022)
## luro$win.prior.simple <- funfun(luro$win.prior.simple , 2025)

## ## pan-pri post 2018 (none in 2018)
## funfun <- function(variab=luro$win.simple , yr=2018){
##     sel.1 <- grep("pan"    , variab)
##     sel.2 <- grep("pri"    , variab)
##     sel.3 <- which(luro$yr>yr)
##     sel.4 <- intersect(sel.1, sel.2)
##     sel.5 <- intersect(sel.4, sel.3)
##     print(paste("N manip:", length(sel.5)))
##     tmp <- variab[sel.5] ## subset for manip state by state
##     tmp[grep("^ags", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^ags", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^bc" , luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^bc" , luro$emm[sel.5])])  ## to pan
##     tmp[grep("^bcs", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^bcs", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^cam", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^cam", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^coa", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^coa", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^col", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^col", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^cps", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^cps", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^cua", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^cua", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^df" , luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^df" , luro$emm[sel.5])])  ## to pan
##     tmp[grep("^dgo", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^dgo", luro$emm[sel.5])])  ## to pan (has exception above))
##     tmp[grep("^gua", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^gua", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^gue", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^gue", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^hgo", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^hgo", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^jal", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^jal", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^mex", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^mex", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^mic", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^mic", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^mor", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^mor", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^nay", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^nay", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^nl" , luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^nl" , luro$emm[sel.5])])  ## to pri
##     tmp[grep("^oax", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^oax", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^pue", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^pue", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^que", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^que", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^qui", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^qui", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^san", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^san", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^sin", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^sin", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^son", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^son", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^tab", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^tab", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^tam", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^tam", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^tla", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^tla", luro$emm[sel.5])])  ## to pri
##     tmp[grep("^ver", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^ver", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^yuc", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^yuc", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^zac", luro$emm[sel.5])] <- gsub("pan-|-pan", "", tmp[grep("^zac", luro$emm[sel.5])])  ## to pri
##     tmp -> variab[sel.5]  ## return after manip
##     return(variab)
## }
## luro$win.simple       <- funfun(luro$win.simple       , 2018)
## luro$part2nd.simple   <- funfun(luro$part2nd.simple   , 2018)
## luro$win.prior.simple <- funfun(luro$win.prior.simple , 2021)
## ## pri-pvem in 2018
## funfun <- function(variab=luro$win.simple , yr=2018){
##     sel.1 <- grep("pri"    , variab)
##     sel.2 <- grep("pvem"   , variab)
##     sel.3 <- which(luro$yr==yr)
##     sel.4 <- intersect(sel.1, sel.2)
##     sel.5 <- intersect(sel.4, sel.3)
##     tmp <- variab[sel.5] ## subset for manip state by state
##     tmp[grep("^cps", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^cps", luro$emm[sel.5])])  ## to pvem
##     tmp -> variab[sel.5]  ## return after manip
##     return(variab)
## }
## luro$win.simple       <- funfun(luro$win.simple       , 2018)
## luro$part2nd.simple   <- funfun(luro$part2nd.simple   , 2018)
## luro$win.prior.simple <- funfun(luro$win.prior.simple , 2021)
## ## ## check
## ## sel.1 <- grep("pri"    , luro$win.simple)
## ## sel.2 <- grep("pvem"   , luro$win.simple)
## ## sel.3 <- which(luro$yr==2018)
## ## sel.4 <- intersect(sel.1, sel.2)
## ## sel.5 <- intersect(sel.4, sel.3)
## ## table(luro$win.simple[sel.5])
## ## table(luro$emm[sel.5])
## ## table(luro$yr[sel.5])
## ##
## ## Before 2018
## ## pan-prd pre 2018
## funfun <- function(variab=luro$win.simple , yr=2018){
##     sel.1 <- grep("pan"    , variab)
##     sel.2 <- grep("prd"    , variab)
##     sel.3 <- which(luro$yr<yr)
##     sel.4 <- intersect(sel.1, sel.2)
##     sel.5 <- intersect(sel.4, sel.3)
##     tmp <- variab[sel.5] ## subset for manip state by state
##     tmp[grep("^bc" , luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^bc" , luro$emm[sel.5])])  ## to pan
##     tmp[grep("^coa", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^coa", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^col", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^col", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^cps", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^cps", luro$emm[sel.5])])  ## to     prd
##     tmp[grep("^cua", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^cua", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^dgo", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^dgo", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^gue", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^gue", luro$emm[sel.5])])  ## to     prd
##     tmp[grep("^hgo", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^hgo", luro$emm[sel.5])])  ## to pan --- Xóchitl to pan
##     tmp[grep("^jal", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^jal", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^mex", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^mex", luro$emm[sel.5])])  ## to pan --- few cases all 2015
##     tmp[grep("^mic", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^mic", luro$emm[sel.5])])  ## to     prd
##     tmp[grep("^jal", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^jal", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^oax", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^oax", luro$emm[sel.5])])  ## to     prd
##     tmp[grep("^pue", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^pue", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^qui", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^qui", luro$emm[sel.5])])  ## to     prd
##     tmp[grep("^san", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^san", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^sin", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^sin", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^son", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^son", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^tam", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^tam", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^ver", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^ver", luro$emm[sel.5])])  ## to pan --- some 2000, rest 2017
##     tmp[grep("^yuc", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^yuc", luro$emm[sel.5])])  ## to pan
##     tmp[grep("^zac", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^zac", luro$emm[sel.5])])  ## to     prd
##     tmp -> variab[sel.5]  ## return after manip
##     return(variab)
## }
## luro$win.simple       <- funfun(luro$win.simple       , 2018)
## luro$part2nd.simple   <- funfun(luro$part2nd.simple   , 2018)
## luro$win.prior.simple <- funfun(luro$win.prior.simple , 2021)
## ##
## ## pan-pri pre 2018 all in mic
## funfun <- function(variab=luro$win.simple , yr=2018){
##     sel.1 <- grep("pan"    , variab)
##     sel.2 <- grep("pri"    , variab)
##     sel.3 <- which(luro$yr<yr)
##     sel.4 <- intersect(sel.1, sel.2)
##     sel.5 <- intersect(sel.4, sel.3)
##     tmp <- variab[sel.5] ## subset for manip state by state
##     tmp[grep("^mic", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^mic", luro$emm[sel.5])])  ## to     pri
##     tmp -> variab[sel.5]  ## return after manip
##     return(variab)
## }
## luro$win.simple       <- funfun(luro$win.simple       , 2018)
## luro$part2nd.simple   <- funfun(luro$part2nd.simple   , 2018)
## luro$win.prior.simple <- funfun(luro$win.prior.simple , 2018)
## ## clean
## rm(funfun)

## ## check and manip major-minor
## sel.1 <- grep("pan"    , luro$win.simple)
## table(luro$win.simple[sel.1])
## luro$win.simple[sel.1] <- "pan" ## all to pan
## ##
## ## pri-pvem pre 2018 al pri OJO: en cps 2012-2017 solo hubo coal en tapachula 2015 y el cand era priista Neftalí del Toro
## sel.1 <- grep("pri"    , luro$win.simple)
## sel.2 <- grep("pvem"   , luro$win.simple)
## sel.3 <- which(luro$yr<2018)
## sel.4 <- intersect(sel.1, sel.2)
## sel.5 <- intersect(sel.4, sel.3)
## table(luro$win.simple[sel.5])
## luro$win.simple[sel.5] <- "pri"



## check
table(luro$win.simple)
##
#########################
## DO SAME FOR PART2ND ##
#########################
##
###############################################################################
## OJO: cuando incluya a pvem y mc en el análisis tendré que tejer más fino. ##
## Habrá que sacar mc de su eterna coalición con prd-pt (difícil) y sacar    ##
## pvem de sus coaliciones en cps y en san. Afecta solo a la izquierda.      ##
###############################################################################
##
## ## Respalda win, análisis continúa con win.simple
## luro$win.premanip <- luro$win
## luro$win          <- luro$win.simple
## luro$win.simple   <- NULL

## AQUI ME QUEDE, FALTA MANIP part2nd IGUAL
## inspect prd and morena towards left recoding
sel.1 <- grep("prd"    , luro$win)
sel.2 <- grep("morena" , luro$part2nd)
sel.3 <- intersect(sel.1, sel.2)
luro[sel.3[1],]
x

## replicate lucardi rosas
luro[1,]
x
##
## rename prd/morena as left. left is prd pre-2015, morena since 2018, or either in between
luro$win       <- sub("morena", "left", luro$win)
luro$part2nd   <- sub("morena", "left", luro$part2nd)
luro$win.prior <- sub("morena", "left", luro$win.prior)
luro$run.prior <- sub("morena", "left", luro$run.prior)
ltmp <- luro[luro$yr < 2015,]
ltmp$win       <- sub("prd", "left", ltmp$win)
ltmp$part2nd   <- sub("prd", "left", ltmp$part2nd)
ltmp$win.prior <- sub("prd", "left", ltmp$win.prior)
ltmp$run.prior <- sub("prd", "left", ltmp$run.prior)
ltmp -> luro[luro$yr < 2015,]
rm(ltmp)
## DVs
## OJO 1: tengo que romper las major-pty coalitions para dársela al partido importante. Do otro modo codifica triunfos múltiples... table(luro$win)
## OJO 2: tengo también que analizar pvem y mc
luro$dpanwin  <- 0; luro$dpanwin [grep("pan" , luro$win)] <- 1
luro$dpriwin  <- 0; luro$dpriwin [grep("pri" , luro$win)] <- 1
luro$dleftwin <- 0; luro$dleftwin[grep("left", luro$win)] <- 1
## 
## pre-selectors (still need to filter margin)
luro$dselpan <- 0
tmp <- grep("pan", luro$win.prior)
luro$dselpan[tmp] <- 1
tmp <- grep("pan", luro$run.prior)
luro$dselpan[tmp] <- 1
##
luro$dselpri <- 0
tmp <- grep("pri", luro$win.prior)
luro$dselpri[tmp] <- 1
tmp <- grep("pri", luro$run.prior)
luro$dselpri[tmp] <- 1
##
luro$dselleft <- 0
tmp <- grep("left", luro$win.prior)
luro$dselleft[tmp] <- 1
tmp <- grep("left", luro$run.prior)
luro$dselleft[tmp] <- 1
##
## Re-compute margin according to party's 1st/runner-up status
luro$mgpan <- NA
tmp <- grep("pan",  luro$win.prior)
luro$mgpan[tmp] <-  luro$mg.prior[tmp]
tmp <- grep("pan",  luro$run.prior)
luro$mgpan[tmp] <- -luro$mg.prior[tmp]
luro$mgpan[luro$dselpan==0] <- NA
##
luro$mgpri <- NA
tmp <- grep("pri", luro$win.prior)
luro$mgpri[tmp] <- luro$mg.prior[tmp]
tmp <- grep("pri", luro$run.prior)
luro$mgpri[tmp] <- -luro$mg.prior[tmp]
luro$mgpri[luro$dselpri==0] <- NA
##
luro$mgleft <- NA
tmp <- grep("left", luro$win.prior)
luro$mgleft[tmp] <- luro$mg.prior[tmp]
tmp <- grep("left", luro$run.prior)
luro$mgleft[tmp] <- -luro$mg.prior[tmp]
luro$mgleft[luro$dselleft==0] <- NA
##
## left=prd|morena in 2015:17 generates no 1st/2nd left overlap
intersect(grep("left", luro$win), grep("left", luro$part2nd)) # check it is empty

## ##
## ## all prior incumbents version pending --- ojo: coalitions inflate party reelection rates
## ## Pending. No sé si lo entiendo cabalmente: selecciono solamente partidos que participaron en t-2, para de ellos tomar los que quedaron en 1o o 2do lugares en t-1, y estimar prob elección en t ---> needs t-2 lags
## luro <- within(luro, {
##     dallwin <- 0;
##     dselall <- 0;
##     mgall <- NA;
## }
## ## pan
## sel.r <- grep("pan", luro$win.prior)
## luro$dselall[sel.r] <- 1
## luro$win
## ##
## grep("^pvem$", luro$win.prior)
## unique(luro$win.prior)
## x
## ## their sample: 1997--2010
## sel.r <- which(luro$yr > 1996 & luro$yr < 2011)
## luro <- luro[sel.r,]
##
##
#################################################
## ########################################### ##
## ## restrict to cases with abs(mg) <= .10 ## ##
## ########################################### ##
#################################################
summary(luro$mg.prior)
sel.r <- which(luro$mg.prior <= 0.1)
luro <- luro[sel.r,]
luro[1,]
dim(luro)
table(luro$dincballot) / nrow(luro)
##
## aqui puedo codificar la recomendación de garfias: regresión con dincballot==0 solamente, pre y post reforma. También podría añadir una dummy=1 si dincballot(t+1)==1

## add edon
tmp <- sub("^([a-z]+)-[0-9].+$", "\\1", x=luro$emm) ## extract edo vector from emm ids
luro$edon <- edo2edon(tmp)
rm(tmp)

yrKickIn <- c(2019, 2019, 2018, 2018, 2018, 2018, 2018, 2018, 2021, 2019, 2018, 2018, 0, 2018, 2018, 2018, 2018, 2024, 2018, 2018, 2021, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2024, 0, 2018, 2018)
## as data frame with edo
yrKickIn <- data.frame(
    edo=edon2edo(1:32),
    yr=yrKickIn)
## 
## add pre-ref dummy
luro$dprekick <- 0
for (i in 1:32){
    ##i <- 1 # debug
    tmp <- luro[luro$edon==i,] ## subset state i's years
    tmp2 <- as.numeric(tmp$yr < yrKickIn$yr[i])
    tmp$dprekick[tmp$edon==i] <- tmp2
    tmp -> luro[luro$edon==i,] ## return after manipulation
}
head(luro)

##############################
## SUBSET SELECTION FILTERS ##
##############################
##
## FILTER 1: WHICH PARTY
tmp <- luro[luro$dselleft==1,] # subset
tmp <- luro[luro$dselpri==1,] # subset
tmp <- luro[luro$dselpan==1,] # subset
##
## FILTER 2: TIME PERIOD FOR ALL SUBSETS IS SINCE 1997 
sel.r <- which(tmp$yr > 1996)
tmp <- tmp[sel.r,]
## SUBSET FOR BLOG ESTIMATION, EXTENDING LUC-ROSAS = FILTERS 1+2
tmpALL <- tmp ## checar: esto debería intersectar por completo ab1 ab2 ab3 y d3
##
## FILTER 3.1: TIME PERIOD BEFORE AMLO
sel.r <- which(tmp$yr < 2018)
## SUBSET TO REPLICATE LUCARDI-ROSAS = FILTERS 1+2+3A
setab1 <- tmp[sel.r,]
tmpREPLICA <- setab1
##
## FILTERS 3.2, 3.3, and 3.4: TIME PERIOD SINCE AMLO WITH EXTRA CONDITIONS
## POST-AMLO
sel.r <- which(tmp$yr > 2017)
tmpALLAMLO <- tmp[sel.r,]
## IN PRE-REFORM STATES ONLY 
sel.r <- which(tmp$yr > 2017 & tmp$dprekick==1)
setab2 <- tmp[sel.r,]
tmpPREKICK <- setab2
## IN POST-REFORM STATES ONLY WHERE INCUMBENT RAN
sel.r <- which(tmp$yr > 2017 & tmp$dprekick==0 & tmp$dincballot==1)
setd3 <- tmp[sel.r,]
## IN POST-REFORM STATES ONLY WHERE INCUMBENT DID NOT RUN
sel.r <- which(tmp$yr > 2017 & tmp$dprekick==0 & tmp$dincballot==0)
setab3 <- tmp[sel.r,]
##
## SUBSET TO ESTIMATE DIFF-IN-DISC MODEL
tmpPOSTKICK <- rbind(setab3, setd3)
##
## SUBSET TO EXTEND LUCARDI-ROSAS IN HGO VER ONLY
sel.r <- which(tmp$edon==13 | tmp$edon==30)
tmpNONREF <- tmp[sel.r,] ## ab1 and ab2 in these states
##
## function generalizing party-specific variables and generating interactions for analysis
genx <- function(x){ ## x is the dataset to manipulate
    tmp <- x ## rename data
    tmp$dwin <- tmp$dpanwin
    tmp$mg   <- tmp$mgpan ## magpan is lagged +/- mg conditional on incumbency status
    ##tmp$mg   <- tmp$mg.panor ## get lagged margin
    ##tmp$dincballot <- tmp$dincballotpan
    tmp <- within(tmp, {
        dneg            <- as.numeric( mgpan<0 )
        dnegxmg         <- dneg * mg
        dnegxpost       <- dneg      * dpostref
        dnegxmgxpost    <- dneg * mg * dpostref 
        dnegxincball    <- dneg      * dincballot
        dnegxmgxincball <- dneg * mg * dincballot 
        dpos            <- 1 - dneg
        dposxmg         <- dpos * mg
        dposxpost       <- dpos      * dpostref
        dposxmgxpost    <- dpos * mg * dpostref
        dposxincball    <- dpos      * dincballot
        dposxmgxincball <- dpos * mg * dincballot 
    })
    return(tmp)
}
tmpREPLICA   <- genx(tmpREPLICA  )
tmpPREKICK   <- genx(tmpPREKICK  )
tmpPOSTKICK  <- genx(tmpPOSTKICK )
tmpALL       <- genx(tmpALL      )
tmpALLAMLO   <- genx(tmpALLAMLO  )
tmpNONREF    <- genx(tmpNONREF   )
rm(genx)

## check N
nrow(tmpREPLICA  )
nrow(tmpPREKICK  )
nrow(tmpPOSTKICK )
nrow(tmpALL      )
nrow(tmpALLAMLO  )
nrow(tmpNONREF   )

## Put all subsets in a list
tmpDATA <- list(
    tmpREPLICA
  , tmpPREKICK
  , tmpPOSTKICK
  , tmpALL
  , tmpALLAMLO
  , tmpNONREF
)
##
## headers for plots
encab <- c(
    "Réplica LuRo 1997-2017"
  , "LuRo post-AMLO 2018-2025"
  , "Post-reforma 2018-2025"
  , "Todos 1997-2025"
  , "Todos post-AMLO 2018-2025"
  , "No reforma 1997-2025"
)
##
## file names
file.names <- c(
    "REPLICA"
  , "PREKICK"
  , "POSTKICK"
  , "ALL"
  , "ALLAMLO"
  , "NONREF"
)

#################################
## Select a model for analysis ##
#################################
funfun <- function(model = 1
                 , pty  = "pan" ## will be used for filenames
                 , plot.to.pdf = FALSE ## default is to view the plot not save it
                 , data = tmpDATA){
    the.data <- data[[model]] ## extract data subset from list
    ##
    ## dwin ~ dneg * ( 1 + mg + dincball + mg*dincball ) + dpos * ( 1 + mg + dincball + mg*dincball )
    ## dwin ~ dneg*1 + dneg*mg + dneg*dincball + dneg*mg*dincball + dpos*1 + dpos*mg + dpos*dincball + dpos*mg*dincball
    ## dwin ~ dneg   + dnegxmg + dnegxpost     + dnegxmgxpost     + dpos   + dposxmg   + dposxpost   + dposxmgxpost
    ##
    ## Estima luc+rosas model
    if (model %in% c(1,2,6)){
        rd <- lm(dwin ~ dneg                + dnegxmg                   + dpos                + dposxmg                   - 1, data = the.data)
    }
    ## Estima model que controla incumbent
    if (model %in% c(3,4,5)){
        rd <- lm(dwin ~ dneg + dnegxincball + dnegxmg + dnegxmgxincball + dpos + dposxincball + dposxmg + dposxmgxincball - 1, data = the.data)
    }
    print(summary.lm(rd))
    ##
    ## plot
    if (plot.to.pdf==TRUE){
        ##png("../plots/pan-luro97-23-lpm.png")
        pdf(file=paste0("../mun-reel/plots/", pty, file.names[model], "lpm.pdf"))
    }
    plot(x = c(-.1,.1), y = c(0,1), type = "n", main = paste0(pty, "\n", encab[model]),
         xlab = expression("Margin"[t]), ylab = expression("Pr(win)"[t+1]))
    abline(v=0)
    ##
    ## incumbent not running
    segments(x0 = -.1, y0 = (  rd$coefficients ["dneg"] +
                               rd$coefficients ["dnegxmg"] * -.1  ),
             x1=  0,   y1 = (  rd$coefficients ["dneg"]   ))
    segments(x0 =  .1, y0 = (  rd$coefficients ["dpos"] +
                               rd$coefficients ["dposxmg"] *  .1  ),
             x1=  0,   y1 = (  rd$coefficients ["dpos"]   ))
    ## incumbent running
    if (model %in% c(3,4,5)){
        segments(x0 = -.1, y0 = ( (rd$coefficients ["dneg"]    + rd$coefficients ["dnegxincball"]) +
                                  (rd$coefficients ["dnegxmg"] + rd$coefficients ["dnegxmgxincball"]) * -.1),
                 x1=  0,   y1 = (  rd$coefficients ["dneg"]    + rd$coefficients ["dnegxincball"]), lty = 2)
        segments(x0 =  .1, y0 = ( (rd$coefficients ["dpos"]    + rd$coefficients ["dposxincball"]) +
                                  (rd$coefficients ["dposxmg"] + rd$coefficients ["dposxmgxincball"]) *  .1),
                 x1=  0,   y1 = (  rd$coefficients ["dpos"]    + rd$coefficients ["dposxincball"]), lty = 2)
        ## legend
        legend("topright", legend = c("incumbent running","open seat"), lty = c(2,1))
    }
    if (plot.to.pdf==TRUE){
        dev.off()
    }
    return(rd)
}

res <- funfun(model=1
            , pty="pan"
            , plot.to.pdf=TRUE)
x

## addGubYr <- function(X){
##     ## input a data.frame
##     ## output same with a dummy indicating if election concurs w gub el
##     ## note: has info between 1997 and 2027, inclusive
##     tmp <- X
##     tmp$dconcgo <- NA ## add column for new dummy
##     gyrs <- list(
##         c(1998, 2004, 2010, 2016, 2022),
##         c(2001, 2007, 2013, 2019, 2021, 2027),
##         c(1999, 2005, 2011, 2015, 2021, 2027),
##         c(1997, 2003, 2009, 2015, 2021, 2027),
##         c(1999, 2005, 2011, 2017, 2023),
##         c(1997, 2003, 2005, 2009, 2015, 2016, 2021, 2027),
##         c(2000, 2006, 2012, 2018, 2024),
##         c(1998, 2004, 2010, 2016, 2021, 2027),
##         c(1997, 2000, 2006, 2012, 2018, 2024),
##         c(1998, 2004, 2010, 2016, 2022),
##         c(2000, 2006, 2012, 2018, 2024),
##         c(1999, 2005, 2011, 2015, 2021, 2027),
##         c(1999, 2005, 2010, 2016, 2022),
##         c(2000, 2006, 2012, 2018, 2024),
##         c(1999, 2005, 2011, 2017, 2023),
##         c(2001, 2007, 2011, 2015, 2021, 2027),
##         c(2000, 2006, 2012, 2018, 2024),
##         c(1999, 2005, 2011, 2017, 2021, 2027),
##         c(1997, 2003, 2009, 2015, 2021, 2027),
##         c(1998, 2004, 2010, 2016, 2022, 2027),
##         c(1998, 2004, 2010, 2016, 2018, 2019, 2024),
##         c(1997, 2003, 2009, 2015, 2021, 2027),
##         c(1999, 2005, 2010, 2016, 2022),
##         c(1997, 2003, 2009, 2015, 2021, 2027),
##         c(1998, 2004, 2010, 2016, 2021, 2027),
##         c(1997, 2003, 2009, 2015, 2021, 2027),
##         c(2000, 2001, 2006, 2012, 2018, 2024),
##         c(1998, 2004, 2010, 2016, 2022),
##         c(1998, 2004, 2010, 2016, 2021, 2027),
##         c(1998, 2004, 2010, 2016, 2018, 2024),
##         c(2001, 2007, 2012, 2018, 2024),
##         c(1998, 2004, 2010, 2016, 2021, 2027)
##     )
##     for (i in 1:32){
##         ##i <- 1 # debug
##         tmp2 <- tmp$yr[tmp$edon==i] ## subset state i's years
##         tmp3 <- as.numeric(tmp2 %in% gyrs[[i]])
##         tmp$dconcgo[tmp$edon==i] <- tmp3
##     }
##     return(tmp)
## }
## luro <- addGubYr(luro)
## rm(addGubYr)

#######################
#######################
##  JAGS ESTIMATION  ##
#######################
#######################
library(R2jags)
antilogit <- function(X){ exp(X) / (exp(X)+1) }
#########################################
## SWR MODEL W POSTREFORM INTERACTIONS ##
#########################################
logitModel <- function() {
    ### linear probability model with logit link
    for (n in 1:N){                ## loop over observations
        depvar[n] ~ dbern(p[n]);   
        logit(p[n]) <- inprod(beta[],X[n,]);  ## FLEXIBLE SPECIFICATION FOR VARYING N OF REGRESSORS, PREPARE depvar AND X IN R
    }
    ############################
    ## NON-INFORMATIVE LEFTORS ##
    ############################
    for (k in 1:K){                ## loop over regressors
        beta[k] ~ dnorm(0, .0001);
    }
}
##
######################################
### EXTRA DATA PREP FOR JAGS MODEL ###
######################################
depvar <- tmp$dwin
N <- length(depvar)
X <- data.frame(
    dneg=tmp$dneg, dnegxincball=tmp$dnegxincball, dnegxmg=tmp$dnegxmg, dnegxmgxincball=tmp$dnegxmgxincball
  , dpos=tmp$dpos, dposxincball=tmp$dposxincball, dposxmg=tmp$dposxmg, dposxmgxincball=tmp$dposxmgxincball
    )
##
## labels to interpret parameters
var.labels <- colnames(X)
K <- length(var.labels)
X <- as.matrix(X)
### Data, initial values, and parameter vector for jags
dl.data <- list("N", "K", "depvar", "X")
dl.inits <- function (){
    list (
    beta=rnorm(K)
    ##beta=summary(fit2)$coefficients[,1] # use lm's estimates
    )
    }
dl.parameters <- c("beta")
#dm.parameters <- c("beta", "sigma", "depvar.hat")
## test ride
fit1jags <- jags (data=dl.data, inits=dl.inits, dl.parameters,
             model.file=logitModel, n.chains=3,
             n.iter=100, n.thin=10
             )
## estimate
fit1jags <- jags (data=dl.data, inits=dl.inits, dl.parameters,
                  model.file=logitModel, n.chains=3,
                  n.iter=100000, n.thin=100,
                  )
fit1jags
tmp.bak -> fit1jags
fit1jags <- update(fit1jags, 10000) # continue updating to produce 10000 new draws per chain
traceplot(fit1jags) # visually check posterior parameter convergence

##
fit1jags$var.labels <- var.labels # add object to interpret coefficients
summary(fit1jags$BUGSoutput$summary)
##


## load saved posterior samples
load(file =  "pan-1997-2023-jags.RData")   ## pan1jags
load(file =  "pri-1997-2023-jags.RData")   ## pri1jags
load(file = "left-1997-2023-jags.RData")  ## left1jags
load(file =  "pan-2018-2023-jags.RData")   ## pan2jags
load(file =  "pri-2018-2023-jags.RData")   ## pri2jags
load(file = "left-2018-2023-jags.RData")  ## left2jags
##
antilogit <- function(X){ exp(X) / (exp(X)+1) }
## use one for plots/analysis with posterior sample
pri2jags -> fit1jags


## sims bayesian
## pr(win)
coefs <- fit1jags$BUGSoutput$sims.matrix; coefs <- coefs[,-grep("deviance", colnames(fit1jags$BUGSoutput$sims.matrix))]
scenario <- c(
    dneg = 1              ## dneg <- c(0,1)
  , dnegxincball = 1      ## dnegxincball
  , dnegxmg = -.1         ## dnegxmg
  , dnegxmgxincball = -.1 ## dnegxmgxincball
  , dpos = 1              ## dpos <- c(0,1)
  , dposxincball = 1      ## dposxincball
  , dposxmg =  .1         ## dposxmg
  , dposxmgxincball =  .1 ## dposxmgxincball
)
##
n <- nrow(coefs)
sc <- matrix(rep(scenario, n), nrow = n, byrow = TRUE)
sc <- as.data.frame(sc)
colnames(sc) <- var.labels
## change dpos/dneg by alternating 0,1
sc$dneg <- rep ( 1:0, n/2)
sc$dpos <- 1 - sc$dneg
sc$dincball <- rep( c(0,0,1,1), n/4)
tmp.mg <- seq(from= .1, to=0, length.out = n/4)
tmp.mg <- rep(tmp.mg, 4)
tmp.mg <- tmp.mg[order(-tmp.mg)]
sc$mg <- tmp.mg; rm(tmp.mg)
sc$dnegxincball <- sc$dneg * sc$dincball
sc$dposxincball <- sc$dpos * sc$dincball
sc$dnegxmg <- sc$dneg * -sc$mg
sc$dposxmg <- sc$dpos *  sc$mg
sc$dnegxmgxincball <- sc$dneg * -sc$mg * sc$dincball
sc$dposxmgxincball <- sc$dpos *  sc$mg * sc$dincball
sc$dincball <- sc$mg <- NULL
head(sc)
sc <- as.matrix(sc)
#
tmp.mean    <- fit1jags$BUGSoutput$summary[grep("beta", rownames(fit1jags$BUGSoutput$summary)),1] # coef point pred (mean posterior)
pointPred <- sc %*% diag(tmp.mean) # right side achieves multiplication of matrix columns by vector
pointPred <- antilogit(rowSums(pointPred)) # will plug this in sc later
tmp.10   <- apply(X=fit1jags$BUGSoutput$sims.matrix[,grep("beta", colnames(fit1jags$BUGSoutput$sims.matrix))]
                , 2, FUN=function(X) quantile(X,.1)) # coef 10%
LL        <- sc %*% diag(tmp.10) # right side achieves multiplication of matrix columns by vector
LL        <- antilogit(rowSums(LL)) # will plug this in sc later
tmp.90   <- apply(X=fit1jags$BUGSoutput$sims.matrix[,grep("beta", colnames(fit1jags$BUGSoutput$sims.matrix))]
                , 2, FUN=function(X) quantile(X,.9)) # coef 90%
UL        <- sc %*% diag(tmp.90) # right side achieves multiplication of matrix columns by vector
UL        <- antilogit(rowSums(UL)) # will plug this in sc later
rm(tmp.mean, tmp.10, tmp.90)
##
pred <- sc * coefs
pred <- antilogit(rowSums(pred)) # will plug this in sc later
#
sc <- as.data.frame(sc); colnames(sc) <- var.labels
sc$pred <- pred; rm(pred)
sc$pointPred <- pointPred; rm(pointPred)
sc$LL <- LL; rm(LL)
sc$UL <- UL; rm(UL)
head(sc)
##
## Add sims to output object
fit1jags$post.estim.sims <- sc
##
## rename/save party estimation
##left1jags <- fit1jags


##########
## plot ##
##########
##png("../plots/pan-97-17-mcmc.png")
##pdf("../plots/pan-97-17-mcmc.pdf")
##plot(x = c(-.1,.1), y = c(0,1), type = "n", main = "PAN 1997-2023", xlab = expression("Margin"[t]), ylab = expression("Probability of winning"[t+1]))
plot(x = c(-.1,.1), y = c(0,1), type = "n", main = "PAN 1997-2017", xlab = expression("Margen en t"), ylab = expression("Probabilidad de ganar en t+1"))
points(sc$dnegxmg[sc$dneg==1 & sc$dnegxincball==1], sc$pred[sc$dneg==1 & sc$dnegxincball==1], pch = 19, col = rgb(.4,.6,.2, alpha=.25), cex = .65)
points(sc$dnegxmg[sc$dneg==1 & sc$dnegxincball==0], sc$pred[sc$dneg==1 & sc$dnegxincball==0], pch = 19, col = rgb(.6,.4,.2, alpha=.25), cex = .65)
##
## polygon(x = c(sc$dnegxmg[sc$dneg==1 & sc$dnegxincball==1], rev(sc$dnegxmg[sc$dneg==1 & sc$dnegxincball==1])),
##         y = c(sc$LL     [sc$dneg==1 & sc$dnegxincball==1], rev(sc$UL     [sc$dneg==1 & sc$dnegxincball==1])),
##         col = rgb(.5,.5,.5, alpha=.25))
## polygon(x = c(sc$dnegxmg[sc$dneg==1 & sc$dnegxincball==0], rev(sc$dnegxmg[sc$dneg==1 & sc$dnegxincball==0])),
##         y = c(sc$LL     [sc$dneg==1 & sc$dnegxincball==0], rev(sc$UL     [sc$dneg==1 & sc$dnegxincball==0])),
##         col = rgb(.5,.5,.5, alpha=.25))
##
#points(sc$dposxmg[sc$dpos==1 & sc$dposxincball==1], sc$pred[sc$dpos==1 & sc$dposxincball==1], pch = 19, col = rgb(.4,.6,.2, alpha=.25), cex = .65)
points(sc$dposxmg[sc$dpos==1 & sc$dposxincball==0], sc$pred[sc$dpos==1 & sc$dposxincball==0], pch = 19, col = rgb(.6,.4,.2, alpha=.25), cex = .65)
##
## polygon(x = c(sc$dposxmg[sc$dpos==1 & sc$dposxincball==1], rev(sc$dposxmg[sc$dpos==1 & sc$dposxincball==1])),
##         y = c(sc$LL     [sc$dpos==1 & sc$dposxincball==1], rev(sc$UL     [sc$dpos==1 & sc$dposxincball==1])),
##         col = rgb(.5,.5,.5, alpha=.25))
## polygon(x = c(sc$dposxmg[sc$dpos==1 & sc$dposxincball==0], rev(sc$dposxmg[sc$dpos==1 & sc$dposxincball==0])),
##         y = c(sc$LL     [sc$dpos==1 & sc$dposxincball==0], rev(sc$UL     [sc$dpos==1 & sc$dposxincball==0])),
##         col = rgb(.5,.5,.5, alpha=.25))
##
abline(v=0)
## incumbent on the ballot
segments(x0 = -.1, y0 = (  sc$pointPred[sc$dnegxmg==-.1 & sc$dnegxincball==1]  ),
         x1=  0,   y1 = (  sc$pointPred[sc$dnegxmg==0   & sc$dnegxincball==1]  ), lwd = 2, col = rgb(.4,.6,.2))
segments(x0 =  .1, y0 = (  sc$pointPred[sc$dposxmg== .1 & sc$dposxincball==1]  ),
         x1=  0,   y1 = (  sc$pointPred[sc$dposxmg==0   & sc$dposxincball==1]  ), lwd = 2, col = rgb(.4,.6,.2))
## open seat
segments(x0 = -.1, y0 = (  sc$pointPred[sc$dnegxmg==-.1 & sc$dnegxincball==0 & sc$dneg==1]  ),
         x1=  0,   y1 = (  sc$pointPred[sc$dnegxmg==0   & sc$dnegxincball==0 & sc$dneg==1]  ), lwd = 2, col = rgb(.6,.4,.2))
segments(x0 =  .1, y0 = (  sc$pointPred[sc$dposxmg== .1 & sc$dposxincball==0 & sc$dpos==1]  ),
         x1=  0,   y1 = (  sc$pointPred[sc$dposxmg==0   & sc$dposxincball==0 & sc$dpos==1]  ), lwd = 2, col = rgb(.6,.4,.2))
## legend
##legend("topright", legend = c("incumbent running","open seat"), lty = c(1,1), col = c(rgb(.4,.6,.2),rgb(.6,.4,.2)), lwd = c(2,2))
legend("topright", legend = c("alcalde en la boleta","silla vacía"), lty = c(1,1), col = c(rgb(.4,.6,.2),rgb(.6,.4,.2)), lwd = c(2,2))
dev.off()

########################
# simulations end here #
########################
fit1jags$var.labels
## N
dim(tmp)
## pan 4758
## pri 7293
## left 2889
## left 18-23 394

## change in intercept
##(dneg - dpos): 
table( (fit1jags$BUGSoutput$sims.list$beta[,5]) - fit1jags$BUGSoutput$sims.list$beta[,1] < 0) / 1500
## pan 1
## pri 1
## left 1
## left 18-23 1
## (dneg + dnegxdinc) - (dpos + dposxdinc)
table(( ( fit1jags$BUGSoutput$sims.list$beta[,5] + fit1jags$BUGSoutput$sims.list$beta[,6] )
      - ( fit1jags$BUGSoutput$sims.list$beta[,1] + fit1jags$BUGSoutput$sims.list$beta[,2] ) < 0)) / 1500
## pan  .197
## pri  .146
## left .199
## left 18-23 .203

var.labels
pan1jags$BUGSoutput$summary
pri1jags$BUGSoutput$summary
left1jags$BUGSoutput$summary
left2jags$BUGSoutput$summary

pan1jags <- fit1jags

## save bugs objects
save(pan1jags,  file =  "pan-1997-2023-jags.RData")
save(pri1jags,  file =  "pri-1997-2023-jags.RData")
save(left1jags, file = "left-1997-2023-jags.RData")
save(pan2jags,  file =  "pan-2018-2023-jags.RData")
save(pri2jags,  file =  "pri-2018-2023-jags.RData")
save(left2jags, file = "left-2018-2023-jags.RData")
save(fit1jags,  file =  "pan-1997-2017-jags.RData")
save(fit1jags,  file =  "pri-1997-2017-jags.RData")
save(fit1jags,  file = "left-1997-2017-jags.RData")

load(file =  "pan-1997-2023-jags.RData")
load(file =  "pri-1997-2023-jags.RData")
load(file = "left-1997-2023-jags.RData")
load(file =  "pan-2018-2023-jags.RData")
load(file =  "pri-2018-2023-jags.RData")
load(file = "left-2018-2023-jags.RData")
load(file =  "pan-1997-2017-jags.RData")
load(file =  "pri-1997-2017-jags.RData")
load(file = "left-1997-2017-jags.RData")

summary(lm(pan ~ dcoalpan + dcoalpan, data = vot)) ## Para ilustrar endogeneidad
ls()
dim(vot)

 




