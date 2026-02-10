
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
tmp <- luro[luro$dselpan==1,] # subset
tmp <- luro[luro$dselpri==1,] # subset
tmp <- luro[luro$dselleft==1,] # subset
##
## FILTER 2: TIME PERIOD FOR ALL SUBSETS IS SINCE 1997 
sel.r <- which(tmp$yr > 1996)
tmp <- tmp[sel.r,]
## SUBSET FOR BLOG ESTIMATION, EXTENDING LUC-ROSAS = FILTERS 1+2
tmpBLOG <- tmp ## checar: esto debería intersectar por completo b1 + b2 + c2 + d2
##
## FILTER 3.1: TIME PERIOD BEFORE AMLO
sel.r <- which(tmp$yr < 2018)
## SUBSET TO REPLICATE LUCARDI-ROSAS = FILTERS 1+2+3A
setb1 <- tmp[sel.r,]
tmpORIG <- tmp[sel.r,]
##
## FILTERS 3.2, 3.3, and 3.4: TIME PERIOD SINCE AMLO WITH EXTRA CONDITIONS
## IN PRE-REFORM STATES ONLY 
sel.r <- which(tmp$yr > 2017 & tmp$dprekick==1)
setb2 <- tmp[sel.r,]
## IN POST-REFORM STATES ONLY WHERE INCUMBENT RAN
sel.r <- which(tmp$yr > 2017 & tmp$dprekick==0 & tmp$dincballot==1)
setc2 <- tmp[sel.r,]
## IN POST-REFORM STATES ONLY WHERE INCUMBENT DID NOT RUN
sel.r <- which(tmp$yr > 2017 & tmp$dprekick==0 & tmp$dincballot==0)
setd2 <- tmp[sel.r,]
##
## SUBSET TO PROJECT LUCARDI-ROSAS SINCE AMLO ONLY
tmpAMLO <- setb2
## SUBSET TO ESTIMATE SELF-SELECTION ONLY
tmpSELF <- rbind(setc2, setd2)
## SUBSET TO ESTIMATE DIFF-IN-DISC MODEL
tmpDND <- rbind(setc2, setb2)
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
tmpORIG <- genx(tmpORIG)
tmpAMLO <- genx(tmpAMLO)
tmpBLOG <- genx(tmpBLOG)
tmpSELF <- genx(tmpSELF)
tmpDND  <- genx(tmpDND)
rm(genx)

## check N
nrow(tmpORIG)
nrow(tmpAMLO)
nrow(tmpBLOG)
nrow(tmpSELF)
nrow(tmpDND)

## headers for plots
headORIG <- "Pre-AMLO"
headAMLO <- "Pre-ref & post-AMLO"
headBLOG <- "All 1997-2025"
headSELF <- "Post-ref & post-AMLO"
headDND  <- "(Post-ref & incRan | Pre-ref) & post-AMLO"

#######################################
## Select a data subset for analysis ##
#######################################
tmp <- list(data = tmpAMLO,
            head = headAMLO,
            pty  = "PAN")
## dwin ~ dneg * ( 1 + mg + dincball + mg*dincball ) + dpos * ( 1 + mg + dincball + mg*dincball )
## dwin ~ dneg*1 + dneg*mg + dneg*dincball + dneg*mg*dincball + dpos*1 + dpos*mg + dpos*dincball + dpos*mg*dincball
## dwin ~ dneg   + dnegxmg + dnegxpost     + dnegxmgxpost     + dpos   + dposxmg   + dposxpost   + dposxmgxpost
##
## luc+rosas
rd.lr <- lm(dwin ~ dneg                + dnegxmg                   + dpos                + dposxmg                   - 1, data = tmp$data)
summary.lm(rd.lr)
## ##
## controlando reforma
rd    <- lm(dwin ~ dneg + dnegxincball + dnegxmg + dnegxmgxincball + dpos + dposxincball + dposxmg + dposxmgxincball - 1, data = tmp$data)
summary.lm(rd)
##
## plot
##png("../plots/pan-luro97-23-lpm.png")
##pdf("../plots/pan-luro97-23-lpm.pdf")
plot(x = c(-.1,.1), y = c(0,1), type = "n", main = paste0(tmp$pty, "\n", tmp$head), xlab = expression("Margin"[t]), ylab = expression("Pr(win)"[t+1]))
abline(v=0)
##
segments(x0 = -.1, y0 = (  rd.lr$coefficients ["dneg"] +
                           rd.lr$coefficients ["dnegxmg"] * -.1  ),
         x1=  0,   y1 = (  rd.lr$coefficients ["dneg"]   ))
segments(x0 =  .1, y0 = (  rd.lr$coefficients ["dpos"] +
                           rd.lr$coefficients ["dposxmg"] *  .1  ),
         x1=  0,   y1 = (  rd.lr$coefficients ["dpos"]   ))
##dev.off()
##
## plot
##png("../plots/pan-luro97-23-lpm.png")
##pdf("../plots/pan-luro97-23-lpm.pdf")
plot(x = c(-.1,.1), y = c(0,1), type = "n", main = paste0(tmp$pty, "\n", tmp$head), xlab = expression("Margin"[t]), ylab = expression("Pr(win)"[t+1]))
abline(v=0)
## incumbent not running
segments(x0 = -.1, y0 = (  rd$coefficients ["dneg"] +
                           rd$coefficients ["dnegxmg"] * -.1  ),
         x1=  0,   y1 = (  rd$coefficients ["dneg"]   ))
segments(x0 =  .1, y0 = (  rd$coefficients ["dpos"] +
                           rd$coefficients ["dposxmg"] *  .1  ),
         x1=  0,   y1 = (  rd$coefficients ["dpos"]   ))
## incumbent running
segments(x0 = -.1, y0 = ( (rd$coefficients ["dneg"]    + rd$coefficients ["dnegxincball"]) +
                          (rd$coefficients ["dnegxmg"] + rd$coefficients ["dnegxmgxincball"]) * -.1),
         x1=  0,   y1 = (  rd$coefficients ["dneg"]    + rd$coefficients ["dnegxincball"]), lty = 2)
segments(x0 =  .1, y0 = ( (rd$coefficients ["dpos"]    + rd$coefficients ["dposxincball"]) +
                          (rd$coefficients ["dposxmg"] + rd$coefficients ["dposxmgxincball"]) *  .1),
         x1=  0,   y1 = (  rd$coefficients ["dpos"]    + rd$coefficients ["dposxincball"]), lty = 2)
## legend
legend("topright", legend = c("incumbent running","open seat"), lty = c(2,1))
##dev.off()


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







## lag to create race-prior variables ##
inc <- slide(inc, Var = "race.after", NewVar = "race.prior", TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
inc <- slide(inc, Var = "part",       NewVar = "win.prior",  TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
inc <- slide(inc, Var = "part2nd",    NewVar = "run.prior",  TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
inc <- slide(inc, Var = "mg",         NewVar = "mg.prior",   TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
str(inc)




funfun <- function(manip=1 , yr=2022){  ## manip 1=win.simple 2=part2nd.simple 3=win.prior.simple 
    variab <- luro[, c("win.simple", "part2nd.simple", "win.prior.simple")] [,manip] ## duplicate column to manipulate
    sel.0 <- which(tmp.done[,manip]==0) ## unmanipulated cases only
    sel.1 <- grep("pan"    , variab)
    sel.2 <- grep("pri"    , variab)
    sel.3 <- which(luro$yr>=yr)
    sel.4 <- Reduce(intersect, list(sel.0,sel.1,sel.2,sel.3))
    tmp <- variab[sel.4]        ## subset for manip state by state
    tmp[grep("^dgo", luro$emm[sel.4])] <- "pri"
    print(paste("N manip:", length(tmp[grep("^dgo", luro$emm[sel.4])])))
    tmp -> variab[sel.4]        ## return after manip
    tmp.done[sel.4[grep("^dgo", luro$emm[sel.4])] , manip] <- 1 ## indicate manip
    return(variab)
}
luro$win.simple       <- funfun(luro$win.simple       , 2022)
luro$part2nd.simple   <- funfun(luro$part2nd.simple   , 2022)
luro$win.prior.simple <- funfun(luro$win.prior.simple , 2025)



[1] "zac-18.025" "zac-18.057"
> data.frame(luro$emm[sel.0], luro$yr[sel.0], luro$win[sel.0], luro$part2nd[sel.0], luro$win.prior[sel.0])
  luro.emm.sel.0. luro.yr.sel.0.  luro.win.sel.0. luro.part2nd.sel.0. luro.win.prior.sel.0.
1      zac-18.025           2021 pt-pvem-left-pna                left            pan-pan-mc
2      zac-18.057           2021 pt-pvem-left-pna                left                   pri





funfun <- function(variab=luro$win.simple , yr=2018){
    sel.1 <- grep("pri"    , variab)
    sel.2 <- grep("pvem"   , variab)
    sel.3 <- which(luro$yr==yr)
    sel.4 <- intersect(sel.1, sel.2)
    sel.5 <- intersect(sel.4, sel.3)
    tmp <- variab[sel.5] ## subset for manip state by state
    tmp[grep("^cps", luro$emm[sel.5])] <- gsub("pri-|-pri", "", tmp[grep("^cps", luro$emm[sel.5])])  ## to pvem
    tmp -> variab[sel.5]  ## return after manip
    return(variab)
}



funfun <- function(variab=luro$win.simple , yr=2018){
    sel.1 <- grep("pan"    , variab)
    sel.2 <- grep("pri"    , variab)
    sel.3 <- which(luro$yr<yr)
    sel.4 <- intersect(sel.1, sel.2)
    sel.5 <- intersect(sel.4, sel.3)
    tmp <- variab[sel.5] ## subset for manip state by state
    tmp[grep("^mic", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^mic", luro$emm[sel.5])])  ## to     pri
    tmp -> variab[sel.5]  ## return after manip
    return(variab)



table(ids.luro$edon[sel.3])

funfun <- function(variab=luro$win.simple , yr=2018){
    sel.0 <- 
    sel.1 <- grep("pan"    , variab)
    sel.2 <- grep("prd"    , variab)
    sel.3 <- which(luro$yr<yr)
    sel.4 <- intersect(sel.1, sel.2)
    sel.5 <- intersect(sel.4, sel.3)
    tmp <- variab[sel.5] ## subset for manip state by state
    tmp[grep("^bc" , luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^bc" , luro$emm[sel.5])])  ## to pan
    tmp[grep("^coa", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^coa", luro$emm[sel.5])])  ## to pan
    tmp[grep("^col", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^col", luro$emm[sel.5])])  ## to pan
    tmp[grep("^cps", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^cps", luro$emm[sel.5])])  ## to     prd
    tmp[grep("^cua", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^cua", luro$emm[sel.5])])  ## to pan
    tmp[grep("^dgo", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^dgo", luro$emm[sel.5])])  ## to pan
    tmp[grep("^gue", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^gue", luro$emm[sel.5])])  ## to     prd
    tmp[grep("^hgo", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^hgo", luro$emm[sel.5])])  ## to pan --- Xóchitl to pan
    tmp[grep("^jal", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^jal", luro$emm[sel.5])])  ## to pan
    tmp[grep("^mex", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^mex", luro$emm[sel.5])])  ## to pan --- few cases all 2015
    tmp[grep("^mic", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^mic", luro$emm[sel.5])])  ## to     prd
    tmp[grep("^jal", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^jal", luro$emm[sel.5])])  ## to pan
    tmp[grep("^oax", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^oax", luro$emm[sel.5])])  ## to     prd
    tmp[grep("^pue", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^pue", luro$emm[sel.5])])  ## to pan
    tmp[grep("^qui", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^qui", luro$emm[sel.5])])  ## to     prd
    tmp[grep("^san", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^san", luro$emm[sel.5])])  ## to pan
    tmp[grep("^sin", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^sin", luro$emm[sel.5])])  ## to pan
    tmp[grep("^son", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^son", luro$emm[sel.5])])  ## to pan
    tmp[grep("^tam", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^tam", luro$emm[sel.5])])  ## to pan
    tmp[grep("^ver", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^ver", luro$emm[sel.5])])  ## to pan --- some 2000, rest 2017
    tmp[grep("^yuc", luro$emm[sel.5])] <- gsub("prd-|-prd" , "" , tmp[grep("^yuc", luro$emm[sel.5])])  ## to pan
    tmp[grep("^zac", luro$emm[sel.5])] <- gsub("pan-|-pan" , "" , tmp[grep("^zac", luro$emm[sel.5])])  ## to     prd
    tmp -> variab[sel.5]  ## return after manip
    return(variab)
}
luro$win.simple       <- funfun(luro$win.simple       , 2018)
luro$part2nd.simple   <- funfun(luro$part2nd.simple   , 2018)
luro$win.prior.simple <- funfun(luro$win.prior.simple , 2021)
