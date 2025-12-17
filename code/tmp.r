


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
##
## FILTER 2: TIME PERIOD
sel.r <- which(tmp$yr > 2017)
##sel.r <- which(tmp$yr > 1997 & tmp$yr < 2018)
tmp <- tmp[sel.r,]
##
## FILTER 3: STAGGERED CHOICE
sel.r <- tmp$dpostref==1 & tmp$dincballot==1
sel.r <- tmp$dprekick==1



addGubYr <- function(X){
    ## input a data.frame
    ## output same with a dummy indicating if election concurs w gub el
    ## note: has info between 1997 and 2027, inclusive
    tmp <- X
    tmp$dconcgo <- NA ## add column for new dummy
    gyrs <- list(
        c(1998, 2004, 2010, 2016, 2022),
        c(2001, 2007, 2013, 2019, 2021, 2027),
        c(1999, 2005, 2011, 2015, 2021, 2027),
        c(1997, 2003, 2009, 2015, 2021, 2027),
        c(1999, 2005, 2011, 2017, 2023),
        c(1997, 2003, 2005, 2009, 2015, 2016, 2021, 2027),
        c(2000, 2006, 2012, 2018, 2024),
        c(1998, 2004, 2010, 2016, 2021, 2027),
        c(1997, 2000, 2006, 2012, 2018, 2024),
        c(1998, 2004, 2010, 2016, 2022),
        c(2000, 2006, 2012, 2018, 2024),
        c(1999, 2005, 2011, 2015, 2021, 2027),
        c(1999, 2005, 2010, 2016, 2022),
        c(2000, 2006, 2012, 2018, 2024),
        c(1999, 2005, 2011, 2017, 2023),
        c(2001, 2007, 2011, 2015, 2021, 2027),
        c(2000, 2006, 2012, 2018, 2024),
        c(1999, 2005, 2011, 2017, 2021, 2027),
        c(1997, 2003, 2009, 2015, 2021, 2027),
        c(1998, 2004, 2010, 2016, 2022, 2027),
        c(1998, 2004, 2010, 2016, 2018, 2019, 2024),
        c(1997, 2003, 2009, 2015, 2021, 2027),
        c(1999, 2005, 2010, 2016, 2022),
        c(1997, 2003, 2009, 2015, 2021, 2027),
        c(1998, 2004, 2010, 2016, 2021, 2027),
        c(1997, 2003, 2009, 2015, 2021, 2027),
        c(2000, 2001, 2006, 2012, 2018, 2024),
        c(1998, 2004, 2010, 2016, 2022),
        c(1998, 2004, 2010, 2016, 2021, 2027),
        c(1998, 2004, 2010, 2016, 2018, 2024),
        c(2001, 2007, 2012, 2018, 2024),
        c(1998, 2004, 2010, 2016, 2021, 2027)
    )
    for (i in 1:32){
        ##i <- 1 # debug
        tmp2 <- tmp$yr[tmp$edon==i] ## subset state i's years
        tmp3 <- as.numeric(tmp2 %in% gyrs[[i]])
        tmp$dconcgo[tmp$edon==i] <- tmp3
    }
    return(tmp)
}
luro <- addGubYr(luro)
rm(addGubYr)

###############
## ######### ##
## ## PAN ## ##
## ######### ##
###############
##
tmp <- luro[luro$dselpan==1,] # subset
##########################
## restrict time period ##
##########################
sel.r <- which(tmp$yr > 2017)
##sel.r <- which(tmp$yr > 1997 & tmp$yr < 2018)
tmp <- tmp[sel.r,]
dim(tmp)
## generalize party-specific variables
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
## dwin ~ dneg * ( 1 + mg + dincball + mg*dincball ) + dpos * ( 1 + mg + dincball + mg*dincball )
## dwin ~ dneg*1 + dneg*mg + dneg*dincball + dneg*mg*dincball + dpos*1 + dpos*mg + dpos*dincball + dpos*mg*dincball
## dwin ~ dneg   + dnegxmg + dnegxpost     + dnegxmgxpost     + dpos   + dposxmg   + dposxpost   + dposxmgxpost
##
rdpan.lr <- lm(dwin ~ dneg + dpos + dnegxmg + dposxmg - 1, data = tmp) ## luc+rosas
summary.lm(rdpan.lr)
## ##
## ##
rdpan    <- lm(dwin ~ dneg + dnegxincball + dnegxmg + dnegxmgxincball + dpos + dposxincball + dposxmg + dposxmgxincball - 1,
           data = tmp) ## controlando reforma
summary.lm(rdpan)
##
## plot
##png("../plots/pan-luro97-23-lpm.png")
##pdf("../plots/pan-luro97-23-lpm.pdf")
plot(x = c(-.1,.1), y = c(0,1), type = "n", main = "PAN \n LPM 1997-2017", xlab = expression("Margin"[t]), ylab = expression("Pr(win)"[t+1]))
abline(v=0)
##
segments(x0 = -.1, y0 = (  rdpan.lr$coefficients ["dneg"] +
                           rdpan.lr$coefficients ["dnegxmg"] * -.1  ),
         x1=  0,   y1 = (  rdpan.lr$coefficients ["dneg"]   ))
segments(x0 =  .1, y0 = (  rdpan.lr$coefficients ["dpos"] +
                           rdpan.lr$coefficients ["dposxmg"] *  .1  ),
         x1=  0,   y1 = (  rdpan.lr$coefficients ["dpos"]   ))
##dev.off()
##
## plot
##png("../plots/pan-luro97-23-lpm.png")
##pdf("../plots/pan-luro97-23-lpm.pdf")
plot(x = c(-.1,.1), y = c(0,1), type = "n", main = "PAN \n LPM 1997-2023", xlab = expression("Margin"[t]), ylab = expression("Pr(win)"[t+1]))
abline(v=0)
## incumbent not running
segments(x0 = -.1, y0 = (  rdpan$coefficients ["dneg"] +
                           rdpan$coefficients ["dnegxmg"] * -.1  ),
         x1=  0,   y1 = (  rdpan$coefficients ["dneg"]   ))
segments(x0 =  .1, y0 = (  rdpan$coefficients ["dpos"] +
                           rdpan$coefficients ["dposxmg"] *  .1  ),
         x1=  0,   y1 = (  rdpan$coefficients ["dpos"]   ))
## incumbent running
segments(x0 = -.1, y0 = ( (rdpan$coefficients ["dneg"]    + rdpan$coefficients ["dnegxincball"]) +
                          (rdpan$coefficients ["dnegxmg"] + rdpan$coefficients ["dnegxmgxincball"]) * -.1),
         x1=  0,   y1 = (  rdpan$coefficients ["dneg"]    + rdpan$coefficients ["dnegxincball"]), lty = 2)
segments(x0 =  .1, y0 = ( (rdpan$coefficients ["dpos"]    + rdpan$coefficients ["dposxincball"]) +
                          (rdpan$coefficients ["dposxmg"] + rdpan$coefficients ["dposxmgxincball"]) *  .1),
         x1=  0,   y1 = (  rdpan$coefficients ["dpos"]    + rdpan$coefficients ["dposxincball"]), lty = 2)
## legend
legend("topright", legend = c("incumbent running","open seat"), lty = c(2,1))
##dev.off()
