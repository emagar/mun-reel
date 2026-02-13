#########################################
## Code for ayuntamiento vote analysis ##
## Date started: 26nov2023             ##
## Revised/updated: 13feb2026          ##
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

## #############################################################################################################
## ## in-progress 2feb2026: prep procedure to use coalition candidate allocation info (still few cases coded) ##
## #############################################################################################################
## coal.alloc <- vot[, c("emm","ncoal","coal1","candcoal1","coal2","candcoal2","coal3","candcoal3","coal4","candcoal4")]
## coal.alloc <- coal.alloc[coal.alloc$ncoal>0,] ## drop cases wo coalitions
## ## find panistas
## grep("pan", coal.alloc$candcoal1)
## table(coal.alloc$ncoal)
## vot[1,]
## x
## ##
## ###############################
## ## In luro, simplify winners ##
## ###############################
## table(ids.luro$emm == luro$emm) ## check ids and luro in same order
## luro[1,]
## ids.luro[1,]
## ## add cols for manipulated vars
## luro <- within( luro, {
##     win.simple <- win
##     part2nd.simple   <- part2nd
##     win.prior.simple <- win.prior
## } )
## tmp.done <- data.frame(win=rep(0, nrow(luro)), part2nd=rep(0, nrow(luro)), win.prior=rep(0, nrow(luro))) ## indicates manipulated cases 
## tmp.done[ids.luro$ncoal==0, 1:2] <- 1 ## cases wo coals done
## ########################################################################################################
## ## until I finish systematizing how coalitions allocated candidacies, will rely on simplifications    ##
## ## nota 6feb2026: preparé funcion funfun para hacer esto en caso cps2018 pri-pvem y dgo2022on pan-pri ##
## ## falta hacerla para los casos restantes                                                             ##
## ########################################################################################################
## ##
## ############################################
## ## no hyphen => no coal                   ##
## ## code single party cases as manipulated ##
## ############################################
## sel.1 <- grep("-"    , luro$win.simple      , invert = TRUE)
## tmp.done$win      [sel.1] <- 1
## sel.1 <- grep("-"    , luro$part2nd.simple  , invert = TRUE)
## tmp.done$part2nd  [sel.1] <- 1
## sel.1 <- grep("-"    , luro$win.prior.simple, invert = TRUE)
## tmp.done$win.prior[sel.1] <- 1
## ##
## ######################################
## ## pri-pvem in 2018 in cps to pvem  ##
## ######################################
## funfun <- function(manip=1 , yr=2018){  ## manip 1=win.simple 2=part2nd.simple 3=win.prior.simple 
##     variab <- luro[, c("win.simple", "part2nd.simple", "win.prior.simple")] [,manip] ## duplicate column to manipulate
##     sel.0 <- which(tmp.done[,manip]==0  & luro$yr>=2018 & ids.luro$edon==7) ## unmanipulated cases only
##     sel.1 <- grep("pri"    , variab)
##     sel.2 <- grep("pvem"   , variab)
##     sel.3 <- Reduce(intersect, list(sel.0,sel.1,sel.2))
##     ##table(variab[sel.3])
##     ##table(ids.luro$edon[sel.3])
##     if (length(sel.3)>0){
##         variab[sel.3] <- "pvem"
##         tmp.done[sel.3, manip] <- 1  ## indicate manip
##         print(paste("N manip:", length(sel.3)))
##     }
##     return(variab)
## }
## luro$win.simple       <- funfun(manip=1 , 2018)
## luro$part2nd.simple   <- funfun(manip=2 , 2018)
## luro$win.prior.simple <- funfun(manip=3 , 2021)
## ##
## #########################################
## ## any single-major coalition to major ##
## #########################################
## sel.0 <- which(luro$done.manip==0)
## sel.1 <- grep("pan"    , luro$win)
## sel.2 <- grep("pri"    , luro$win, invert = TRUE)
## sel.3 <- grep("prd"    , luro$win, invert = TRUE)
## sel.4 <- Reduce(intersect, list(sel.0,sel.1, sel.2, sel.3))
## table(luro$win.simple[sel.4])
## luro$win.simple[sel.4] <- "pan"
## luro$done.manip[sel.4] <- 1
## ##
## sel.0 <- which(luro$done.manip==0)
## sel.1 <- grep("pri"    , luro$win)
## sel.2 <- grep("pan"    , luro$win, invert = TRUE)
## sel.3 <- grep("prd"    , luro$win, invert = TRUE)
## sel.4 <- Reduce(intersect, list(sel.0,sel.1, sel.2, sel.3))
## table(luro$win.simple[sel.4])
## luro$win.simple[sel.4] <- "pri"
## luro$done.manip[sel.4] <- 1
## ##
## sel.0 <- which(luro$done.manip==0)
## sel.1 <- grep("prd"    , luro$win)
## sel.2 <- grep("pan"    , luro$win, invert = TRUE)
## sel.3 <- grep("pri"    , luro$win, invert = TRUE)
## sel.4 <- Reduce(intersect, list(sel.0,sel.1, sel.2, sel.3))
## table(luro$win.simple[sel.4])
## luro$win.simple[sel.4] <- "prd"
## luro$done.manip[sel.4] <- 1
## ##
## sel.0 <- which(luro$done.manip==0)
## sel.1 <- grep("morena"    , luro$win)
## sel.2 <- Reduce(intersect, list(sel.0,sel.1))
## table(luro$win.simple[sel.2])
## luro$win.simple[sel.2] <- "morena"
## luro$done.manip[sel.2] <- 1
## ##
## ## all pvem wo majors to pvem
## sel.0 <- which(luro$done.manip==0)
## sel.1 <- grep("pvem"   , luro$win.simple)
## sel.2 <- grep("pan"    , luro$win.simple, invert = TRUE)
## sel.3 <- grep("pri"    , luro$win.simple, invert = TRUE)
## sel.4 <- grep("prd"    , luro$win.simple, invert = TRUE)
## sel.5 <- Reduce(intersect, list(sel.0,sel.1,sel.2, sel.3, sel.4))
## table(luro$win.simple[sel.5])
##       luro$win.simple[sel.5] <- "pvem"
##       luro$done.manip[sel.5] <- 1
## ##
## ## all mc wo majors to mc
## sel.0 <- which(luro$done.manip==0)
## sel.1 <- grep("mc|conve", luro$win.simple)
## sel.2 <- grep("pan"     , luro$win.simple, invert = TRUE)
## sel.3 <- grep("pri"     , luro$win.simple, invert = TRUE)
## sel.4 <- grep("prd"     , luro$win.simple, invert = TRUE)
## sel.5 <- Reduce(intersect, list(sel.0,sel.1,sel.2, sel.3, sel.4))
## table(luro$win.simple[sel.5])
##       luro$win.simple[sel.5] <- "mc"
##       luro$done.manip[sel.5] <- 1
## ##
## ## all pt wo majors to mc
## sel.0 <- which(luro$done.manip==0)
## sel.1 <- grep("pt"   , luro$win.simple)
## sel.2 <- grep("pan"    , luro$win.simple, invert = TRUE)
## sel.3 <- grep("pri"    , luro$win.simple, invert = TRUE)
## sel.4 <- grep("prd"    , luro$win.simple, invert = TRUE)
## sel.5 <- Reduce(intersect, list(sel.0,sel.1,sel.2, sel.3, sel.4))
## table(luro$win.simple[sel.5])
##       luro$win.simple[sel.5] <- "pt"
##       luro$done.manip[sel.5] <- 1
## ##
## ## others solo
## sel.1 <- grep("-"    , luro$win.simple, invert = TRUE)
## sel.2 <- grep("pan|pri|prd|morena|mc|conve|pvem|pt"     , luro$win.simple, invert = TRUE)
## sel.3 <- Reduce(intersect, list(sel.1,sel.2))
## table(luro$win.simple[sel.3])
##       luro$win.simple[sel.3] <- "oth"
## ##
## ## all rest wo majors to oth
## sel.0 <- which(luro$done.manip==0)
## sel.1 <- grep("pan"     , luro$win.simple, invert = TRUE)
## sel.2 <- grep("pri"     , luro$win.simple, invert = TRUE)
## sel.3 <- grep("prd"     , luro$win.simple, invert = TRUE)
## sel.4 <- Reduce(intersect, list(sel.0,sel.1,sel.2, sel.3))
## table(luro$win.simple[sel.4])
##       luro$win.simple[sel.4] <- "oth"
##       luro$done.manip[sel.4] <- 1
## ##
## ## all post-2018 pan-prd wo pri to pan
## sel.0 <- which(luro$done.manip==0 & luro$yr > 2018)
## sel.1 <- grep("prd"    , luro$win.simple)
## sel.2 <- grep("pri"    , luro$win.simple, invert = TRUE)
## sel.3 <- Reduce(intersect, list(sel.0,sel.1, sel.2))
## table(luro$win.simple[sel.3])
## table(ids.luro$edon[sel.3])
## luro$win.simple[sel.3] <- "pan"
## luro$done.manip[sel.3] <- 1
## ##
## ## all post-2018 pri-prd wo pan to pri
## sel.0 <- which(luro$done.manip==0 & luro$yr > 2018)
## sel.1 <- grep("prd"    , luro$win.simple)
## sel.2 <- grep("pri"    , luro$win.simple)
## sel.3 <- grep("pan"    , luro$win.simple, invert = TRUE)
## sel.4 <- Reduce(intersect, list(sel.0,sel.1, sel.2, sel.3))
## table(luro$win.simple[sel.4])
## table(ids.luro$edon[sel.4])
## luro$win.simple[sel.4] <- "pri"
## luro$done.manip[sel.4] <- 1
## ##
##
## ## Before 2018
## ## pan-prd pre 2018
## sel.0 <- which(luro$done.manip==0 & luro$yr <= 2018)
## sel.1 <- grep("prd"    , luro$win.simple)
## sel.2 <- grep("pri"    , luro$win.simple, invert = TRUE)
## sel.3 <- Reduce(intersect, list(sel.0,sel.1, sel.2))
## table(luro$win.simple[sel.3])
## ##
## tmp <- luro$win.simple[sel.3] ## subset to manipulate
## tmp[grep("^bc" , luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^bc" , luro$emm[sel.3])])  ## to pan
## tmp[grep("^coa", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^coa", luro$emm[sel.3])])  ## to pan
## tmp[grep("^col", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^col", luro$emm[sel.3])])  ## to pan
## tmp[grep("^cps", luro$emm[sel.3])] <- sub("pan-|-pan" , "" , tmp[grep("^cps", luro$emm[sel.3])])  ## to     prd
## tmp[grep("^cua", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^cua", luro$emm[sel.3])])  ## to pan
## tmp[grep("^dgo", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^dgo", luro$emm[sel.3])])  ## to pan
## tmp[grep("^gue", luro$emm[sel.3])] <- sub("pan-|-pan" , "" , tmp[grep("^gue", luro$emm[sel.3])])  ## to     prd
## tmp[grep("^hgo", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^hgo", luro$emm[sel.3])])  ## to pan --- Xóchitl to pan
## tmp[grep("^jal", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^jal", luro$emm[sel.3])])  ## to pan
## tmp[grep("^mex", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^mex", luro$emm[sel.3])])  ## to pan --- few cases all 2015
## tmp[grep("^mic", luro$emm[sel.3])] <- sub("pan-|-pan" , "" , tmp[grep("^mic", luro$emm[sel.3])])  ## to     prd
## tmp[grep("^jal", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^jal", luro$emm[sel.3])])  ## to pan
## tmp[grep("^oax", luro$emm[sel.3])] <- sub("pan-|-pan" , "" , tmp[grep("^oax", luro$emm[sel.3])])  ## to     prd
## tmp[grep("^pue", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^pue", luro$emm[sel.3])])  ## to pan
## tmp[grep("^qui", luro$emm[sel.3])] <- sub("pan-|-pan" , "" , tmp[grep("^qui", luro$emm[sel.3])])  ## to     prd
## tmp[grep("^san", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^san", luro$emm[sel.3])])  ## to pan
## tmp[grep("^sin", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^sin", luro$emm[sel.3])])  ## to pan
## tmp[grep("^son", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^son", luro$emm[sel.3])])  ## to pan
## tmp[grep("^tam", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^tam", luro$emm[sel.3])])  ## to pan
## tmp[grep("^ver", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^ver", luro$emm[sel.3])])  ## to pan --- some 2000, rest 2017
## tmp[grep("^yuc", luro$emm[sel.3])] <- sub("prd-|-prd" , "" , tmp[grep("^yuc", luro$emm[sel.3])])  ## to pan
## tmp[grep("^zac", luro$emm[sel.3])] <- sub("pan-|-pan" , "" , tmp[grep("^zac", luro$emm[sel.3])])  ## to     prd
## tmp -> luro$win.simple[sel.3]  ## return after manip
## luro$done.manip[sel.3] <- 1
## ##
## ## pan-pri pre 2018 all in mic
## sel.0 <- which(luro$done.manip==0 & luro$yr <= 2018)
## sel.1 <- grep("pan"    , luro$win.simple)
## sel.2 <- grep("pri"    , luro$win.simple)
## sel.3 <- Reduce(intersect, list(sel.0,sel.1, sel.2))
## table(luro$win.simple[sel.3])
## table(ids.luro$edon[sel.3])
## tmp <- luro$win.simple[sel.3] ## subset for manip state by state
## tmp[grep("^mic", luro$emm[sel.3])] <- gsub("pan-|-pan" , "" , tmp[grep("^mic", luro$emm[sel.3])])  ## to     pri
## tmp -> luro$win.simple[sel.3]  ## return after manip
## luro$done.manip[sel.3] <- 1
##
## ## pan-pri dgo 2022 and post to pri
## luro[1,]
## funfun <- function(variab=luro$win.simple , yr=2022){
##     variab <- luro[, c("win.simple", "part2nd.simple", "win.prior.simple")] [,manip] ## duplicate column to manipulate
##     sel.0 <- which(tmp.done[,manip]==0) ## unmanipulated cases only
##     sel.1 <- grep("pan"    , variab)
##     sel.2 <- grep("pri"    , variab)
##     sel.3 <- which(luro$yr>=yr)
##     sel.4 <- Reduce(intersect, list(sel.0,sel.1,sel.2,sel.3))
##     tmp <- variab[sel.4]        ## subset for manip state by state
##     tmp[grep("^dgo", luro$emm[sel.4])] <- "pri"
##     print(paste("N manip:", length(tmp[grep("^dgo", luro$emm[sel.4])])))
##     tmp -> variab[sel.4]        ## return after manip
##     tmp.done[sel.4[grep("^dgo", luro$emm[sel.4])] , manip] <- 1 ## indicate manip
##     return(variab)
## }
## luro$win.simple       <- funfun(manip=1 , 2022)
## luro$part2nd.simple   <- funfun(manip=2 , 2022)
## luro$win.prior.simple <- funfun(manip=3 , 2025)
##
## table(luro$done.manip)
## table(luro$win.simple[luro$done.manip==0])
##
## sel.1 <- which(luro$done.manip==0)
## luro$win.prior[sel.1] <-
##     gsub("-pvem|-mc|-pt|-pna|-ph", "", luro$win.prior[sel.1])
## x

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
##
## ## check
## table(luro$win.simple)

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


###############################
## recode prd/morena as left ##
###############################
## before 2015 left is prd
sel.r <- which(ids.luro$yr<2015)
grep("morena", luro$win    [sel.r])  ## should be empty
grep("morena", luro$part2nd[sel.r])  ## should be empty
luro$win      [sel.r] <- sub("prd", "left", luro$win       [sel.r])
luro$part2nd  [sel.r] <- sub("prd", "left", luro$part2nd   [sel.r])
## ## win.prior and run.prior manipulated solo below
## sel.r <- which(ids.luro$yr<2018)
## grep("morena", luro$win.prior    [sel.r])  ## should be empty
## luro$win.prior[sel.r] <- sub("prd", "left", luro$win.prior [sel.r])
##
sel.r <- which(ids.luro$yr>=2018)
luro$win      [sel.r] <- sub("morena", "left", luro$win       [sel.r])
luro$part2nd  [sel.r] <- sub("morena", "left", luro$part2nd   [sel.r])
sel.1 <- grep("prd", luro$win       [sel.r])
table(ids.luro$yr[sel.r][sel.1], ids.luro$edon[sel.r][sel.1])
## prd since 2018 will be coded as pan
luro$win      [sel.r] <- sub("prd", "pan", luro$win       [sel.r])
luro$part2nd  [sel.r] <- sub("prd", "pan", luro$part2nd   [sel.r])

## cases where morena/prd won or runner-up 2015-2018
sel.0 <- which(ids.luro$yr>=2015 & ids.luro$yr<2018)
sel.1 <- grep("morena|prd", luro$win)
sel.2 <- grep("morena|prd", luro$part2nd)
sel.3 <- Reduce(intersect, list(sel.0,sel.1,sel.2))
data.frame(ids.luro$emm[sel.3], ids.luro$yr[sel.3], luro$win[sel.3], luro$part2nd[sel.3], luro$mg[sel.3])
rm(sel.1, sel.2, sel.3)
## all cases where prd came 1st or 2nd will be coded as pan
sel.1 <- grep("prd", luro$win)
sel.2 <- Reduce(intersect, list(sel.0,sel.1))
luro$win    [sel.2] <- "pan"
rm(sel.1, sel.2)
sel.1 <- grep("prd", luro$part2nd)
sel.2 <- Reduce(intersect, list(sel.0,sel.1))
luro$part2nd[sel.2] <- "pan"
rm(sel.1, sel.2)
##
sel.1 <- grep("morena", luro$win)
sel.2 <- Reduce(intersect, list(sel.0,sel.1))
luro$win    [sel.2] <- "left"
rm(sel.1, sel.2)
sel.1 <- grep("morena", luro$part2nd)
sel.2 <- Reduce(intersect, list(sel.0,sel.1))
luro$part2nd[sel.2] <- "left"
rm(sel.0, sel.1, sel.2)

#################################################################
## Re-lag win so win.prior and run.prior inherit left recoding ##
#################################################################
## verify time series cross section's structure (for grouped lags)
table(order(ids.luro$inegi, ids.luro$cycle)==order(luro$ord))
table(ids.luro$ord==luro$ord)
## add inegi/cycle to luro
luro$inegi <- ids.luro$inegi; luro$cycle <- ids.luro$cycle
luro <- luro[order(    luro$ord) ,] # verify sorted before lags
ids.luro <- luro[order(ids.luro$ord) ,] # verify sorted before lags
## lag to create race-prior variables ##
luro <- slide(luro, Var = "win"    , NewVar = "new.win.prior", TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
luro <- slide(luro, Var = "part2nd", NewVar = "new.run.prior", TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
head(luro)
table(luro$new.win.prior==luro$win.prior)
sel.0 <- which(luro$new.win.prior!=luro$win.prior)
## looks ok!!!
with(luro[sel.0,], data.frame(emm, yr, win.prior, new.win.prior))
## replace manip
luro$win.prior <- luro$new.win.prior
luro$run.prior <- luro$new.run.prior
luro$new.win.prior <- luro$new.run.prior <- NULL

## replicate lucardi rosas
luro[1,]
x
##
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
sel.0 <- intersect(grep("left", luro$win), grep("left", luro$part2nd)) # check it is empty
luro$emm[sel.0]
##data.frame(luro$emm[sel.0], luro$yr[sel.0], luro$win[sel.0], luro$part2nd[sel.0], luro$win.prior[sel.0])

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

###################################################################################
## 10feb2026 attempt top fix three cases where both prd and morena coded as left ##
###################################################################################
## gue-17.021 recode prd as pan to avoid left confusion
## gue-17.042 recode pri-prd as pri to avoid left confusion
## pue-17.166 recode pan-pri-prd as pan-pri to avoid left confusion
luro$dincballotprd[which(luro$emm %in%  c("gue-17.021", "gue-17.042", "pue-17.166"))] <- 0
luro$dselleft     [which(luro$emm %in%  c("gue-17.021", "gue-17.042", "pue-17.166"))] <- 0 ## drop the cases from analysis



################################
## Function estimating models ##
################################
funfun <- function(model = 1           ## 1 2 3 4 5 or 6
                 , pty  = "pan"        ## subsets data and used for filenames
                 , plot.to.pdf = FALSE ## default is to view the plot not save it
                   ){
    ##############################
    ## SUBSET SELECTION FILTERS ##
    ##############################
    ##
    ## FILTER 1: WHICH PARTY
    if (pty=="left") tmp <- luro[luro$dselleft==1,] # subset
    if (pty=="pri")  tmp <- luro[luro$dselpri==1,] # subset
    if (pty=="pan")  tmp <- luro[luro$dselpan==1,] # subset
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
    sel.r <- which((tmp$edon==13 | tmp$edon==30) & tmp$yr>2017)
    tmpNONREF <- tmp[sel.r,] ## ab1 and ab2 in these states
    ##
    ## function generalizing party-specific variables and generating interactions for analysis
    genx <- function(x){ ## x is the dataset to manipulate
        tmp <- x ## rename data
        if (pty=="pan")  tmp$dwin <- tmp$dpanwin
        if (pty=="pri")  tmp$dwin <- tmp$dpriwin
        if (pty=="left") tmp$dwin <- tmp$dleftwin
        if (pty=="pan")  tmp$mg   <- tmp$mgpan ## magpan is lagged +/- mg conditional on incumbency status
        if (pty=="pri")  tmp$mg   <- tmp$mgpri
        if (pty=="left") tmp$mg   <- tmp$mgleft
        ##tmp$mg   <- tmp$mg.panor ## get lagged margin
        ##tmp$dincballot <- tmp$dincballotpan
        tmp <- within(tmp, {
            if (pty=="pan")  dneg            <- as.numeric( mgpan<0 )
            if (pty=="pri")  dneg            <- as.numeric( mgpri<0 )
            if (pty=="left") dneg            <- as.numeric( mgleft<0 )
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
    ## rm(genx)
    ##
    ## check N
    print( paste(
        "1 N=", nrow(tmpREPLICA  ),
        "2 N=", nrow(tmpPREKICK  ),
        "3 N=", nrow(tmpPOSTKICK ),
        "4 N=", nrow(tmpALL      ),
        "5 N=", nrow(tmpALLAMLO  ),
        "6 N=", nrow(tmpNONREF   )
    ))
    ##
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
    ## ## español
    ## encab <- c(
    ##     paste0("Todos 1997-2017 (réplica LuRo "        , "N=", nrow(tmpREPLICA  ), ")")
    ##   , paste0("Pre-patada 2018-2025 (extensión LuRo " , "N=", nrow(tmpPREKICK  ), ")")
    ##   , paste0("Post-patada 2018-2025 ("               , "N=", nrow(tmpPOSTKICK ), ")")
    ##   , paste0("Todos 1997-2025 ("                     , "N=", nrow(tmpALL      ), ")")
    ##   , paste0("Todos 2018-2025 ("                     , "N=", nrow(tmpALLAMLO  ), ")")
    ##   , paste0("No reformistas 2018-2025 (Hgo y Ver "  , "N=", nrow(tmpNONREF   ), ")")
    ## )
    ## inglés
    encab <- c(
        paste0("1997-2017 all (Lucardi-Rosas replica "  , "N=", nrow(tmpREPLICA  ), ")")
      , paste0("2018-2025 pre-kickoff states ("         , "N=", nrow(tmpPREKICK  ), ")")
      , paste0("2018-2025 post-kickoff states ("        , "N=", nrow(tmpPOSTKICK ), ")")
      , paste0("1997-2025 all states ("                 , "N=", nrow(tmpALL      ), ")")
      , paste0("2018-2025 all states ("                 , "N=", nrow(tmpALLAMLO  ), ")")
      , paste0("2018-2025 non-reform states (Hgo & Ver ", "N=", nrow(tmpNONREF   ), ")")
      ##   paste0("All 1997-2017 (LuRo replica "           , "N=", nrow(tmpREPLICA  ), ")")
      ## , paste0("Pre-kickoff 2018-2025 (LuRo extension " , "N=", nrow(tmpPREKICK  ), ")")
      ## , paste0("Post-kickoff 2018-2025 ("               , "N=", nrow(tmpPOSTKICK ), ")")
      ## , paste0("All 1997-2025 ("                        , "N=", nrow(tmpALL      ), ")")
      ## , paste0("All 2018-2025 ("                        , "N=", nrow(tmpALLAMLO  ), ")")
      ## , paste0("Non reformers 2018-2025 (Hgo & Ver "    , "N=", nrow(tmpNONREF   ), ")")
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
    ##
    the.data <- tmpDATA[[model]] ## extract data subset from list
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
    plot(x = c(-.1,.1), y = c(0,1), type = "n", main = paste0(toupper(pty), "\n", encab[model]),
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

###########################################
## Select a party and model for analysis ##
###########################################
res <- funfun(model=2
            , pty="left"
            , plot.to.pdf=FALSE)

## summarize
tmp <- luro[luro$dselpan==1 & luro$yr>2017,]
nrow(tmp)
table(tmp$dincballotpan) / nrow(tmp)
table(tmp$win, tmp$dleftwin)
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

 




