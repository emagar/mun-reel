#########################################
## Code for ayuntamiento vote analysis ##
## Date started: 26nov2023             ##
## Revised/updated: 26feb2025          ##
## By emagar at itam dot mx            ##
#########################################

rm(list = ls())
##
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data"
setwd(wd)

## useful functions
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
## Reads function
source( paste(pth, "moveme.r", sep = "/") )
source( paste(pth, "notin.r", sep = "/") )
rm(pth)
## needed
library(DataCombine) # easy lags


#########################
## get municipal votes ##
#########################
vot  <- read.csv(paste0(dd, "aymu1970-on.coalSplit.csv"), stringsAsFactors = FALSE)
vot[1,]

## get coal agg version to take some info
vcoa <- read.csv(paste0(dd, "aymu1970-on.coalAgg.csv"),   stringsAsFactors = FALSE)
table(vot$emm==vcoa$emm) # same order?
## clean
vot <- within(vot, notas <- NULL)
## add info from vcoa
vot$dextra <- vcoa$dextra
vot$win    <- vcoa$win
vot$mg <- round((vcoa$v01 - vcoa$v02) / vcoa$efec, digits = 4)
## compute coal dummies
sel <- grep("l[0-9]+", colnames(vcoa))
l <- vcoa[,sel]
vot$dcoalpan <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-pan|pan-", x)) > 0, 1, 0))
vot$dcoalpri <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-pri|pri-", x)) > 0, 1, 0))
vot$dcoalprd <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-prd|prd-", x)) > 0, 1, 0))
vot$dcoalmor <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-morena|morena-", x)) > 0, 1, 0))
vot$dcoalpve <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-pvem|pvem-", x)) > 0, 1, 0))
vot$dcoalpt  <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-pt|pt-", x)) > 0, 1, 0))
vot$dcoalmc  <- apply(X = l, MARGIN = 1, FUN = function(x) ifelse(length(grep("-mc|mc-", x)) > 0, 1, 0))

###################################
## 28jan2025: drop 1988 was here ##
###################################

#########################################################
## drop municipios that had any usos y costumbres vote ##
#########################################################
## Other than 2 cases that were uyc only in 1995, municipios that became uyc in 1995 are included in this vector of ife codes
sel.r <- c(20001, 20003, 20004, 20009, 20010, 20013, 20014, 20018, 20019, 20020, 20021, 20022, 20023, 20025, 20027, 20029, 20033, 20034, 20035, 20036, 20040, 20042, 20045, 20046, 20047, 20049, 20050, 20053, 20054, 20058, 20060, 20061, 20062, 20063, 20064, 20065, 20068, 20070, 20073, 20074, 20075, 20080, 20081, 20082, 20083, 20084, 20088, 20089, 20090, 20091, 20092, 20093, 20094, 20095, 20096, 20097, 20098, 20101, 20102, 20103, 20104, 20105, 20106, 20107, 20108, 20110, 20111, 20112, 20113, 20114, 20116, 20117, 20118, 20119, 20120, 20122, 20123, 20124, 20125, 20126, 20128, 20129, 20130, 20132, 20134, 20135, 20136, 20137, 20139, 20141, 20142, 20143, 20144, 20145, 20146, 20148, 20149, 20150, 20151, 20152, 20153, 20155, 20156, 20158, 20159, 20160, 20161, 20162, 20163, 20165, 20168, 20169, 20171, 20172, 20173, 20174, 20176, 20177, 20180, 20185, 20186, 20187, 20188, 20191, 20192, 20193, 20194, 20195, 20196, 20197, 20198, 20201, 20202, 20203, 20204, 20205, 20206, 20207, 20208, 20209, 20210, 20211, 20212, 20213, 20214, 20215, 20216, 20217, 20218, 20219, 20220, 20221, 20222, 20223, 20224, 20226, 20227, 20228, 20229, 20230, 20231, 20233, 20234, 20235, 20236, 20238, 20239, 20240, 20241, 20242, 20243, 20244, 20246, 20247, 20248, 20249, 20250, 20251, 20253, 20254, 20255, 20256, 20257, 20259, 20261, 20262, 20263, 20264, 20265, 20266, 20267, 20268, 20269, 20270, 20271, 20272, 20273, 20274, 20275, 20277, 20278, 20279, 20280, 20281, 20282, 20284, 20285, 20286, 20287, 20289, 20290, 20291, 20294, 20295, 20297, 20299, 20301, 20302, 20304, 20309, 20311, 20312, 20313, 20314, 20315, 20316, 20318, 20319, 20320, 20321, 20323, 20324, 20326, 20327, 20328, 20329, 20330, 20331, 20333, 20335, 20336, 20337, 20338, 20339, 20340, 20341, 20343, 20344, 20346, 20347, 20348, 20349, 20350, 20351, 20352, 20353, 20354, 20355, 20356, 20357, 20358, 20359, 20361, 20362, 20363, 20365, 20366, 20368, 20369, 20370, 20371, 20372, 20373, 20374, 20375, 20377, 20379, 20380, 20381, 20383, 20384, 20385, 20387, 20389, 20390, 20391, 20393, 20394, 20395, 20396, 20397, 20398, 20399, 20400, 20402, 20403, 20404, 20405, 20406, 20408, 20409, 20410, 20411, 20412, 20413, 20417, 20420, 20421, 20423, 20424, 20425, 20426, 20428, 20429, 20430, 20431, 20433, 20434, 20436, 20438, 20439, 20441, 20443, 20444, 20445, 20446, 20447, 20449, 20450, 20451, 20452, 20453, 20454, 20455, 20458, 20460, 20461, 20462, 20464, 20465, 20466, 20467, 20469, 20471, 20472, 20474, 20476, 20477, 20478, 20479, 20480, 20482, 20483, 20488, 20489, 20491, 20492, 20493, 20494, 20495, 20496, 20497, 20498, 20499, 20500, 20501, 20502, 20503, 20504, 20505, 20508, 20510, 20511, 20512, 20514, 20516, 20517, 20518, 20519, 20521, 20522, 20523, 20524, 20526, 20527, 20528, 20529, 20530, 20531, 20532, 20533, 20535, 20538, 20540, 20541, 20542, 20544, 20546, 20547, 20548, 20551, 20552, 20554, 20556, 20559, 20561, 20562, 20563, 20564, 20565, 20567, 20569)
## Oaxaca ifes that entered uyc in 1998
sel.r <- c(sel.r, c(20079, 20308, 20345, 20367, 20401))
## Oaxaca ifes that left uyc in 2013
sel.r <- c(sel.r, 20085)
## Oaxaca ifes in and out of uyc 
sel.r <- c(sel.r, 20473)
## Chiapas ife that entered uyc in 2018
sel.r <- c(sel.r, 7064)
## Guerrero ife that entered uyc in 2018 
sel.r <- c(sel.r, 12012)
## Guerrero ife that entered uyc upon creation in 2021
sel.r <- c(sel.r, 12082)
## Michoacán ife that entered uyc in 2011
sel.r <- c(sel.r, 16024)
## Morelos ifes that entered uyc upon creation in 2021
sel.r <- c(sel.r, c(17034, 17035, 17036))
##
sel.r <- which(vot$ife %in% sel.r)
vot <- vot[-sel.r,]
##
## clean
rm(l,vcoa,sel.r)
##
## drop Belisario Domínguez, litigio after 2nd election
drop.r <- grep("xxx", vot$emm)
vot <- vot[-drop.r,]
rm(drop.r)
table(vot$status)
## drop these obs from analysis
##drop.r <- grep("cancelled|litigio", vot$status)
drop.r <- grep("cancelled|voided|litigio|missing|pending|appoint", vot$status)
vot <- vot[-drop.r,]
table(vot$status)
rm(drop.r)
##
## drop runoffs held in san luis potosí (win/mg retain eventual winner/margin)
sel <- grep("san-[0-9]+b", vot$emm) # these are first round races that led to runoff
tmp <- vot$emm[sel] ## get their ids
tmp <- sub(pattern = "^([a-z]+[-][0-9]+)b([.0-9]+)$", replacement = "\\1\\2", tmp) ## drop bs
sel2 <- which(vot$emm %in% tmp) ## select runoffs
vot$win[sel] <- vot$win[sel2] ## replace 1st round winners with runoff winners
vot$mg[sel]  <- vot$mg[sel2]  ## replace 1st round margin with runoff margin
vot <- vot[-sel2,] ## and drop runoffs
rm(sel,sel2,tmp)

## # lo usé para detectar casos en aymu.incumbents con info que no correspondía con aymu.coalAgg
## sel <- c("emm", "edon", "mun", "munn", "ife", "inegi", "yr", "dy", "mo", "win")
## i <- merge(x = inc, y = vot[,sel],
##            by = c("emm", "edon", "mun", "munn", "ife", "inegi", "yr", "mo", "dy", "win"),
##            all = TRUE)
## write.csv(i, file = paste(dd, "tmp.csv", sep = ""), row.names = FALSE)

## check efec (ignore small mismatch)
v <- vot[, grep("v[0-9]{2}", colnames(vot))] ## extract votes only
table(rowSums(v) - vot$efec)

#################################################################
## prepare object with pan pri prd pvem pt mc morena oth votes ##
#################################################################
v7 <- vot # duplicate for manipulation
sel.c <- grep("emm|^yr$|^status$|^[vl][0-9]{2}", colnames(v7))
v7 <- v7[,sel.c]
v7$status <- NA ## may be used to record manip progress
v7[1,]
dim(v7)
## narrow v01..v25 into single column v l pair for manipulation
v7$n <- 1:nrow(v7) # obs no
v7$r <- 1          # round
v7$v <- v7$v01     # vote
v7$l <- v7$l01     # label
#
tmp.orig <- v7 # duplicate
tmp <- tmp.orig
tmp$r <- 2
tmp$v <- tmp$v02; tmp$l <- tmp$l02;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 3
tmp$v <- tmp$v03; tmp$l <- tmp$l03;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 4
tmp$v <- tmp$v04; tmp$l <- tmp$l04;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 5
tmp$v <- tmp$v05; tmp$l <- tmp$l05;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 6
tmp$v <- tmp$v06; tmp$l <- tmp$l06;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 7
tmp$v <- tmp$v07; tmp$l <- tmp$l07;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 8
tmp$v <- tmp$v08; tmp$l <- tmp$l08;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 9
tmp$v <- tmp$v09; tmp$l <- tmp$l09;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 10
tmp$v <- tmp$v10; tmp$l <- tmp$l10;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 11
tmp$v <- tmp$v11; tmp$l <- tmp$l11;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 12
tmp$v <- tmp$v12; tmp$l <- tmp$l12;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 13
tmp$v <- tmp$v13; tmp$l <- tmp$l13;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 14
tmp$v <- tmp$v14; tmp$l <- tmp$l14;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 15
tmp$v <- tmp$v15; tmp$l <- tmp$l15;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 16
tmp$v <- tmp$v16; tmp$l <- tmp$l16;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 17
tmp$v <- tmp$v17; tmp$l <- tmp$l17;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 18
tmp$v <- tmp$v18; tmp$l <- tmp$l18;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 19
tmp$v <- tmp$v19; tmp$l <- tmp$l19;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 20
tmp$v <- tmp$v20; tmp$l <- tmp$l20;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 21
tmp$v <- tmp$v21; tmp$l <- tmp$l21;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 22
tmp$v <- tmp$v22; tmp$l <- tmp$l22;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 23
tmp$v <- tmp$v23; tmp$l <- tmp$l23;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 24
tmp$v <- tmp$v24; tmp$l <- tmp$l24;
v7 <- rbind(v7,tmp)
tmp <- tmp.orig
tmp$r <- 25
tmp$v <- tmp$v25; tmp$l <- tmp$l25;
v7 <- rbind(v7,tmp)
v7[1,]
## make sure all v and l columns have been added before next pair of commands
v7$v01 <- v7$v02 <- v7$v03 <- v7$v04 <- v7$v05 <- v7$v06 <- v7$v07 <- v7$v08 <- v7$v09 <- v7$v10 <- v7$v11 <- v7$v12 <- v7$v13 <- v7$v14 <- v7$v15 <- v7$v16 <- v7$v17 <- v7$v18 <- v7$v19 <- v7$v20 <- v7$v21 <- v7$v22 <- v7$v23 <- v7$v24 <- v7$v25 <- NULL
v7$l01 <- v7$l02 <- v7$l03 <- v7$l04 <- v7$l05 <- v7$l06 <- v7$l07 <- v7$l08 <- v7$l09 <- v7$l10 <- v7$l11 <- v7$l12 <- v7$l13 <- v7$l14 <- v7$l15 <- v7$l16 <- v7$l17 <- v7$l18 <- v7$l19 <- v7$l20 <- v7$l21 <- v7$l22 <- v7$l23 <- v7$l24 <- v7$l25 <- NULL
##
## rebrand conve to mc etc
v7$l[grep("indep_", v7$l)] <- "indep"
v7$l[v7$l %in% c("conve","cdppn")] <- "mc"
v7$l[v7$l %in% c("pt1","ptc")] <- "pt" ## pt-conve to pt
v7$l[v7$l %in% c("pesm","pest")] <- "pes"
table(v7$l)
##
## fdn to prd
v7$l[grep("prd-fdn", v7$l)] <- "prd"
v7$l[grep("prd-pps-pfcrn", v7$l)] <- "prd"
## pcdp-pt to pcdp
v7$l[grep("pcdp-pt", v7$l)] <- "pcdp"
## mc-psdc to mc
v7$l[grep("mc-psdc", v7$l)] <- "mc"
## prepare object with pan pri prd pvem pt mc morena oth votes ##
v7$oth <- v7$morena <- v7$mc <- v7$pt <- v7$pvem <- v7$prd <- v7$pri <- v7$pan <- 0
## control major party coals
v7$dmajcoal <- 0
## check if unbroken coalitions left
paste("If true: No unbroken coalitions remaining", length(v7$emm[grep("-", v7$l)])==0)
## ##If false, which remain?
## v7$emm[grep("-", v7$l)]
## v7[grep("-", v7$l),]
##
rm(tmp, tmp.orig)
#

## #################################################################################################
## ## 26jan2025: breaking coalitions in coalSplit is no longer needed.                            ##
## ## Script ay.r in elecRtrns does it prior to saving aymucoalSplit.csv.                         ##
## ## Keeping this commented, redundant block in case same sort of manipulation needed elsewhere. ##
## #################################################################################################
## ## # change prd/morena to left
## ## sel <- which(v7$yr<=2015)
## ## v7$l[sel] <- sub("prd","left",v7$l[sel])
## ## sel <- which(v7$yr>=2015)
## ## v7$l[sel] <- sub("morena","left",v7$l[sel])
##
## # deal with major-party coalition below
## sel <- grep("(?=.*pan)(?=.*prd)", v7$l, perl = TRUE)
## v7$status[sel] <- "majors"
## v7$dmajcoal[sel] <- 1
## ## sel <- grep("(?=.*pan)(?=.*left)", v7$l, perl = TRUE)
## ## v7$status[sel] <- "majors"
## ## v7$dmajcoal[sel] <- 1
## sel <- grep("(?=.*pan)(?=.*pri)", v7$l, perl = TRUE)
## v7$status[sel] <- "majors"
## v7$dmajcoal[sel] <- 1
## sel <- grep("(?=.*pri)(?=.*prd)", v7$l, perl = TRUE)
## v7$status[sel] <- "majors"
## v7$dmajcoal[sel] <- 1
## ## sel <- grep("(?=.*pri)(?=.*left)", v7$l, perl = TRUE)
## ## v7$status[sel] <- "majors"
## ## v7$dmajcoal[sel] <- 1
## #
## sel1 <- which(is.na(v7$status))
## sel <- grep("pan-|-pan|^pan$", v7$l[sel1])
## v7$pan[sel1][sel] <- v7$v[sel1][sel]
## v7$v[sel1][sel] <- 0; v7$l[sel1][sel] <- "0"; v7$status[sel1][sel] <- "done"
## #
## sel1 <- which(is.na(v7$status))
## sel <- grep("pri-|-pri|^pri$", v7$l[sel1])
## v7$pri[sel1][sel] <- v7$v[sel1][sel]
## v7$v[sel1][sel] <- 0; v7$l[sel1][sel] <- "0"; v7$status[sel1][sel] <- "done"
## #
## ## sel1 <- which(is.na(v7$status))
## ## sel <- grep("left-|-left|^left$", v7$l[sel1])
## ## v7$morena[sel1][sel] <- v7$v[sel1][sel]
## ## v7$v[sel1][sel] <- 0; v7$l[sel1][sel] <- "0"; v7$status[sel1][sel] <- "done"
## #
## sel1 <- which(is.na(v7$status))
## sel <- grep("prd-|-prd|^prd$", v7$l[sel1])
## v7$prd[sel1][sel] <- v7$v[sel1][sel]
## v7$v[sel1][sel] <- 0; v7$l[sel1][sel] <- "0"; v7$status[sel1][sel] <- "done"
## #
## sel1 <- which(is.na(v7$status))
## sel <- grep("morena-|-morena|^morena$", v7$l[sel1])
## v7$morena[sel1][sel] <- v7$v[sel1][sel]
## v7$v[sel1][sel] <- 0; v7$l[sel1][sel] <- "0"; v7$status[sel1][sel] <- "done"
## #
## # rest are other
## sel1 <- which(is.na(v7$status)) 
## v7$oth[sel1] <- v7$v[sel1]
## v7$v[sel1] <- 0; v7$l[sel1] <- "0"; v7$status[sel1] <- "done"
## #
## # 3-majors coalition in mun split in thirds
## sel <- which(v7$status=="majors") 
## sel1 <- grep("(?=.*pan)(?=.*pri)(?=.*prd)", v7$l[sel], perl = TRUE) # 
## v7$pan[sel][sel1] <- v7$v[sel][sel1] / 3; v7$pri[sel][sel1] <- v7$v[sel][sel1] / 3; v7$prd[sel][sel1] <- v7$v[sel][sel1] / 3; v7$v[sel][sel1] <- 0; v7$l[sel][sel1] <- "0"; v7$status[sel][sel1] <- "done"
## v7$dmajcoal[sel][sel1] <- 1
## ##
## sel <- which(v7$status=="majors") 
## sel1 <- grep("(?=.*pan)(?=.*pri)(?=.*prd)", v7$l[sel], perl = TRUE) # 
## v7$pan[sel][sel1] <- v7$v[sel][sel1] / 3; v7$pri[sel][sel1] <- v7$v[sel][sel1] / 3; v7$prd[sel][sel1] <- v7$v[sel][sel1] / 3; v7$v[sel][sel1] <- 0; v7$l[sel][sel1] <- "0"; v7$status[sel][sel1] <- "done"
## v7$dmajcoal[sel][sel1] <- 1
## #
## # pan-pri to pri (19 cases in mic07 mic11 mic15)
## sel <- which(v7$status=="majors" & v7$edon==16) 
## sel1 <- grep("(?=.*pan)(?=.*pri)", v7$l[sel], perl = TRUE) # 
## v7$pri[sel][sel1] <- v7$v[sel][sel1]; v7$v[sel][sel1] <- 0; v7$l[sel][sel1] <- "0"; v7$status[sel][sel1] <- "done"
## v7$dmajcoal[sel][sel1] <- 1
## #
## # pri-prd to pri (chihuahua and guanajuato)
## sel <- which(v7$status=="majors") 
## sel1 <- grep("(?=.*pri)(?=.*prd)", v7$l[sel], perl = TRUE) # 
## v7$pri[sel][sel1] <- v7$v[sel][sel1]; v7$v[sel][sel1] <- 0; v7$l[sel][sel1] <- "0"; v7$status[sel][sel1] <- "done"
## v7$dmajcoal[sel][sel1] <- 1
## #
## # rest are pan-prd
## #
## # pan-prd to pan (bc coa2009 coa col00 col18 cua dgo jal que san sin son tam yuc)
## sel <- which(v7$status=="majors" & (v7$edon==2 | v7$edon==5 | v7$edon==6 | v7$edon==8 | v7$edon==10 | v7$edon==14 | v7$edon==22 | v7$edon==24 | v7$edon==25 | v7$edon==26 | v7$edon==28  | v7$edon==31)) 
## v7$pan[sel] <- v7$v[sel]; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd in 2018 to pan (bcs cps df gue mex mic oax pue qui tab zac)
## sel <- which(v7$status=="majors" & v7$yr==2018) 
## v7$pan[sel] <- v7$v[sel]; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd to prd (cps2004, cps2010)
## sel <- which(v7$status=="majors" & v7$edon==7 & v7$yr<=2010) 
## v7$prd[sel] <- v7$v[sel]; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd to pan (nay1999 nay2017 ver2000 ver2017)
## sel <- which(v7$status=="majors" & (v7$edon==18 | v7$edon==30) & (v7$yr==1999 | v7$yr==2000 | v7$yr==2017)) 
## v7$pan[sel] <- v7$v[sel]; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd split halfway (qui2016)
## sel <- which(v7$status=="majors" & v7$edon==23 & v7$yr==2016)
## v7$pan[sel] <- v7$v[sel] / 2; 
## v7$prd[sel] <- v7$v[sel] / 2; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd split halfway (pue2010 pue2013 qui2013 qui2010)
## sel <- which(v7$status=="majors" & (v7$edon==21 | v7$edon==23))
## v7$pan[sel] <- v7$v[sel]; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd split halfway (gue2015 hgo2011 mic2015 oax2010 oax2013 oax2016 zac2013, zac2016)
## sel <- which(v7$status=="majors" & (v7$edon==12 | v7$edon==13 | v7$edon==16 | v7$edon==20 | v7$edon==32) & v7$yr<2018) 
## v7$pan[sel] <- v7$v[sel] / 2; 
## v7$prd[sel] <- v7$v[sel] / 2; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## # pan-prd split halfway (mex2006)
## sel <- which(v7$status=="majors" & v7$edon==15 & v7$yr==2006) 
## v7$pan[sel] <- v7$v[sel] / 2; 
## v7$prd[sel] <- v7$v[sel] / 2; v7$v[sel] <- 0; v7$l[sel] <- "0"; v7$status[sel] <- "done"
## v7$dmajcoal[sel] <- 1
## #
## ## # used to check by hand
## ## sel <- which(v7$status=="majors") 
## ## sel1 <- grep("(?=.*pan)(?=.*prd)", v7$l[sel], perl = TRUE) # 
## ## table(v7$edon[sel])
## ## table(v7$yr[sel])
## ## table(v7$emm[sel][sel1], v7$l[sel][sel1])
## ## table(v7$emm[sel][sel1], v7$yr[sel][sel1])

## move vote to proper column
sel.r <- which(v7$l=="pan")
v7$pan[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
sel.r <- which(v7$l=="pri")
v7$pri[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
sel.r <- which(v7$l=="prd")
v7$prd[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
sel.r <- which(v7$l=="pvem")
v7$pvem[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
sel.r <- which(v7$l=="pt")
v7$pt[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
sel.r <- which(v7$l=="mc")
v7$mc[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
sel.r <- which(v7$l=="morena")
v7$morena[sel.r] <- v7$v[sel.r]
v7$v[sel.r] <- 0 # to zero, has been moved
##
## remainder non-zero vs are other
v7$oth <- v7$v
##
tail(v7[v7$r==1,])
tail(v7[v7$r==2,])
tail(v7[v7$r==3,])
tail(v7[v7$r==4,])
tail(v7[v7$r==5,])
tail(v7[v7$r==6,])
tail(v7[v7$r==7,])
tail(v7[v7$r==8,])
tail(v7[v7$r==9,])

## consolidate
tmp <- v7[v7$r==1,] # will receive consolidated data
tmp$v <- tmp$l <- tmp$r <- NULL
dim(tmp)
##
for (i in 1:max(v7$n)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, max(v7$n)))
    tmp2 <- v7[v7$n==i, c("pan","pri","prd","pvem","pt","mc","morena","oth")]
    tmp2 <- colSums(tmp2)
    ##sum(tmp2)
    tmp[tmp$n==i,       c("pan","pri","prd","pvem","pt","mc","morena","oth")] <- tmp2 # plug consolidated data
}
v7 <- tmp
## clean
table(v7$status)
v7$n <- v7$status <- NULL
##
rm(tmp,i,sel.r,sel.c,tmp2)

## compute vote shares
v7[1,]
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","oth")
v <- v7[,sel.c] # subset votes columns
efec <- rowSums(v) # re-compute effective vote to remove rounding
efec[which(efec==0)] <- NA # put NA vote to missing races
v <- v / efec
##v <- round(v, digits = 4)
table(rowSums(v))
##v$oth <- 1 - rowSums(v[,-8]) # oth will absorb any dif from 1 due to rounding
v7[,sel.c] <- v # return vote shares to data
v7$efec <- efec
v7[1,]
vot[1,]
rm(efec,sel.c,v)

## return to vot
vot <- cbind(vot, v7[,c("pan","pri","prd","pvem","pt","mc","morena","oth")])
summary(vot$efec - v7$efec)
##vot$efec <- v7$efec # commenting this line keeps rounded efec
# keep named vote cols, drop rest
vot <- within(vot, v01 <- v02 <- v03 <- v04 <- v05 <- v06 <- v07 <- v08 <- v09 <- v10 <- v11 <- v12 <- v13 <- v14 <- v15 <- v16 <- v17 <- v18 <- v19 <- v20 <- v21 <- v22 <- v23 <- v24 <- v25 <- NULL)
vot <- within(vot, l01 <- l02 <- l03 <- l04 <- l05 <- l06 <- l07 <- l08 <- l09 <- l10 <- l11 <- l12 <- l13 <- l14 <- l15 <- l16 <- l17 <- l18 <- l19 <- l20 <- l21 <- l22 <- l23 <- l24 <- l25 <- NULL)
## inspect
vot[1,]
## arrange cols
vot <- vot[moveme(names(vot), "efec last")]
vot <- vot[moveme(names(vot), "lisnom last")]
## verify
table(vot$status)
##
## clean
rm(v7)

## Change san luis ids followed by runoff
sel <- grep("san-[0-9]+b", vot$emm) # these are first round races that led to runoff
tmp <- vot$emm[sel]
tmp <- sub(pattern = "(san-[0-9]+)b([.0-9]+$)", replacement = "\\1\\2", tmp)
vot$emm[sel] <- tmp
rm(tmp,sel)

## Fill missing NAs
sel.r <- which(vot$efec==0)
vot[sel.r,c("pan","pri","prd","pvem","pt","mc","morena","oth","efec")] <- NA
## inegi to num
vot$inegi <- as.numeric(vot$inegi)
vot$ife   <- as.numeric(vot$ife)

###########################################################################
## v7 is now cbinded to vot, export the object for use in other analysis ##
###########################################################################
getwd()
write.csv(vot, file = "aymu1988-on-v7-coalSplit.csv", row.names = FALSE)

##############################################################
## 28jan2025: drop 1988 moved here, might affect code below ##
##############################################################
## keep copy with all votes to use in some hard lags below
tmp.votpre1988 <- vot                                     
## keep 1997-on                                           
sel <- which(vot$yr<1988)                                 
vot <- vot[-sel,]                                         

#####################################################################
## Get incumbency data                                             ##
## relies on prior.inc.part, added to aymu.incumbent.csv, for this ##
#####################################################################
inc <- read.csv(paste0(dd, "aymu1989-on.incumbents.csv"), stringsAsFactors = FALSE)
table(inc$yr)
## drop pre-1988 and unanalyzed cols   31JUL2024: MOVED FURTHER DOWN TO AVOID NA WHEN LAGGING 
sel.r <- which(inc$yr < 1988)
inc <- inc[-sel.r,]
sel.c <- which(colnames(inc) %in% c("ord","dextra","edon","source","dmujer","runnerup","dlegacy","who","drepe","drepg"))
inc <- inc[,-sel.c]

## change conve to mc
table(inc$part)
inc$part           <- sub("conve|cdppn", "mc", inc$part)
inc$prior.inc.part <- sub("conve|cdppn", "mc", inc$prior.inc.part)
inc$inc.part.after <- sub("conve|cdppn", "mc", inc$inc.part.after)
## verify time series cross section's structure (for grouped lags)
inc$cycle <- as.numeric(sub("^[a-z]+[-]([0-9]+)[.].+$", "\\1", inc$emm))
inc <- inc[order(inc$inegi, inc$cycle),] # verify sorted before lags
inc$ord <- 1:nrow(inc)
inc <- slide(inc, Var = "cycle", NewVar = "cycle.lag", TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
verif <- inc$cycle - inc$cycle.lag
table(verif) # verify: all should be 1 then ok to lag
rm(verif)
tail(inc) ## check lag or lead
inc$cycle.lag <- NULL
##
## lag to create race-prior variables ##
inc <- slide(inc, Var = "race.after", NewVar = "race.prior", TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
inc <- slide(inc, Var = "part",       NewVar = "win.prior",  TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
inc <- slide(inc, Var = "part2nd",    NewVar = "run.prior",  TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
inc <- slide(inc, Var = "mg",         NewVar = "mg.prior",   TimeVar = "cycle", GroupVar = "inegi", slideBy = -1)
##
## merge inc into dat
#sel <- which(inc$emm %notin% vot$emm)
#data.frame(emm=inc$emm[sel], yr=inc$yr[sel], mun=inc$mun[sel], incumbent=inc$incumbent[sel])
vot <- merge(x = vot, y = inc[,c("emm","race.prior","prior.inc.part","part2nd","win.prior","run.prior","mg.prior")], by = "emm", all.x = TRUE, all.y = FALSE)
vot <- vot[,moveme(colnames(vot), "part2nd after win; mg.prior after mg; run.prior after mg; win.prior after mg")]
## compute incumbent dummies
vot <- within(vot, {
    dincoth <- 0
    dincmorena <- 0
    dincmc <- 0
    dincpt <- 0
    dincpvem <- 0
    dincprd <- 0
    dincpri <- 0
    dincpan <- 0
})
sel.r <- grep("Reelected|Reran", vot$race.prior)
vot2 <- vot[sel.r,] # subset for manipulation
prior.inc.part <- vot2$prior.inc.part ## indicate progress here
## code dummies (emptying race.prior as it proceeds)
sel <- grep("pan",    prior.inc.part); vot2$dincpan   [sel] <- 1; prior.inc.part <- sub("pan",    "", prior.inc.part)
sel <- grep("pri",    prior.inc.part); vot2$dincpri   [sel] <- 1; prior.inc.part <- sub("pri",    "", prior.inc.part)
sel <- grep("prd",    prior.inc.part); vot2$dincprd   [sel] <- 1; prior.inc.part <- sub("prd",    "", prior.inc.part)
sel <- grep("pvem",   prior.inc.part); vot2$dincpvem  [sel] <- 1; prior.inc.part <- sub("pvem",   "", prior.inc.part)
sel <- grep("pt",     prior.inc.part); vot2$dincpt    [sel] <- 1; prior.inc.part <- sub("pt",     "", prior.inc.part)
sel <- grep("mc",     prior.inc.part); vot2$dincmc    [sel] <- 1; prior.inc.part <- sub("mc",     "", prior.inc.part)
sel <- grep("morena", prior.inc.part); vot2$dincmorena[sel] <- 1; prior.inc.part <- sub("morena", "", prior.inc.part)
##
prior.inc.part <- sub("^(-+)$", "", prior.inc.part) ## slashes with no text to empty (will become dincoth=0)
prior.inc.part <- sub("^concejo municipal$", "", prior.inc.part) ## will become dincoth=0
prior.inc.part <- sub("^incumbent out$", "", prior.inc.part) ## will become dincoth=0
##
table(prior.inc.part, useNA = "ifany") ## only minor parties or none should remain
##
## code dummy
vot2$dincoth <- ifelse(prior.inc.part=="", 0, 1) ## empty to 0, rest to 1
## return to data
vot[sel.r,] <- vot2
## clean
rm(prior.inc.part,vot2,sel,sel.c,sel.r)
vot$prior.inc.part <- NULL
## inspect
tail(vot)
table(vot$status)


## 15mar24: Lack of squareness dealt differently --- drops missing yrs so that lag uses last available obs
## For lags: vot xsts not square, use inc (which is) to add missing obs
## add cycle
tmp <- vot$emm
tmp <- sub("^[a-z]+-([0-9]{2})[ab]?[.][0-9]+$", "\\1", tmp) ## ab for anuladas and pre-runoffs
table(tmp)
tmp <- as.numeric(tmp)
vot$cycle <- tmp
rm(tmp)
##
table( vot $cycle)
table( inc $cycle)
##
## ## Comment to not square xsts (dropping missing cycles performs < -2 lags)
## tmp <- inc[, c("emm","cycle")] ## keep ids and temp ids only
## tmp <- merge(x=vot, y=tmp, by=c("emm","cycle"), all=TRUE)
## tmp <- tmp[order(tmp$emm),]
## dim(tmp)
## dim(inc)
## dim(vot)
## drop these after merge
## sel.r <- which(vot$emm %in% c("cps-15.xxx", "cps-16.xxx", "cps-17.xxx", "oax-09.088", "oax-10.088", "oax-11.088", "oax-12.088", "oax-13.088", "oax-14.088", "oax-15.088", "oax-16.088", "oax-17.088", "oax-18.088"))
## tmp <- tmp[-sel.r,]
## dim(tmp)
## dim(inc)
## dim(vot)
## ## fill missing ids in new obs
## sel.r <- which(is.na(tmp$inegi))
## if (length(sel.r) > 0){
##     ## function to complete missing ifes
##     pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
##                    "~/Dropbox/data/useful-functions",
##                    "https://raw.githubusercontent.com/emagar/useful-functions/master"
##                    )
##     source( paste(pth, "inegi2ife.r", sep = "/") )
##     source( paste(pth, "edo2edon.r", sep = "/") )
##     rm(pth)
##     edon <- sub("^([a-z]+)-.+$", "\\1", tmp$emm[sel.r])
##     edon <- edo2edon(edon)
##     tmp$edon[sel.r] <- edon
##     inegi <- as.numeric(sub("^[a-z]+-[0-9]+[.]([0-9]+)$", "\\1", tmp$emm[sel.r]))
##     inegi <- edon*1000 + inegi
##     tmp$inegi[sel.r] <- inegi
##     ##
##     tmp$ife[sel.r] <- inegi2ife(tmp$inegi[sel.r])
##     rm(inegi2ife,ife2inegi,inegi2mun,ife2mun,edo2edon,edon2edo)
##     ## fill missing yr
##     tmp2 <- sub("([a-z]+[-][0-9]{2})[.][0-9]{3}", "\\1", tmp$emm[sel.r]) ## get edo-cycle
##     tmp2 <- data.frame(edocy=tmp2, yr=NA, date=NA)
##     ## function returniong the mode, from https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
##     Mode <- function(x) {
##         ux <- unique(x)
##         ux[which.max(tabulate(match(x, ux)))]
##     }
##     for (i in 1:nrow(tmp2)){
##         sel <- grep(tmp2$edocy[i], tmp.votpre1996$emm)
##         tmp2$yr  [i] <- Mode(tmp.votpre1996$yr  [sel]) ## return the modal date (assumes off-yr extra elecs are minority of cycle's yrs)
##         tmp2$date[i] <- Mode(tmp.votpre1996$date[sel]) ## return the modal year 
##     }
##     ## return yrs and dates
##     tmp$yr  [sel.r] <- tmp2$yr
##     tmp$date[sel.r] <- tmp2$date
##     ## replace dat with manipulated object
##     vot <- tmp
##     rm(edon, inegi, sel.r, sel, i, Mode, tmp2)
## }
## ##
## table(is.na(tmp$yr))
## table(is.na(tmp$date))
## ##
## rm(inc) ## clean
##
## get electoral calendar
cal <- read.csv(file = paste0(dd, "../../calendariosReelec/data/fechasEleccionesMexicoDesde1994.csv"))
cal$info <- NULL ## drop citation col
table(cal$elec)
sel.r <- which(cal$elec=="gob" | cal$elec=="dip"  | cal$elec=="pres"); cal <- cal[sel.r,] # subset dip and gob
cal[1:4,]
## translate months to english
library(stringr)
to.eng <- function(x) str_replace_all(x, c("ene"="jan", "abr"="apr", "ago"="aug", "dic"="dec"))
cal <- as.data.frame(sapply(cal, to.eng))
str(cal)

## dates to yyyymmdd
sel.c <- grep("^y[0-9]{4}", colnames(cal))
year <- as.numeric(sub("y", "", colnames(cal)[sel.c]))
for (i in 1:nrow(cal)) cal[i,sel.c] <- paste0(cal[i,sel.c], year)
for (i in 1:nrow(cal)) cal[i,sel.c] <- sub("--[0-9]{4}", "", cal[i,sel.c])
for (i in 1:nrow(cal)) cal[i,sel.c] <- sub("([0-9]+)([a-z]+)([0-9]{4})", "\\3\\2\\1", cal[i,sel.c])
for (i in 1:nrow(cal)) cal[i,sel.c] <- sub("([a-z]{1})([1-9]{1})$", "\\10\\2", cal[i,sel.c]) # add heading zeroes for single-digit days
to.num <- function(x) str_replace_all(x, c("jan"="01", "feb"="02", "mar"="03", "apr"="04", "may"="05", "jun"="06", "jul"="07", "aug"="08", "sep"="09", "oct"="10", "nov"="11", "dec"="12"))
cal <- as.data.frame(sapply(cal, to.num))
##for (i in 1:length(sel.c)) cal[,sel.c[i]] <- ymd(cal[,sel.c[i]]) ## proceed columnwise to retain date format
## dummies
vot$dconcdf <- 0
vot$dconcpr <- 0
vot$dconcgo <- 0
for (i in 1:32){
    #i <- 26
    sel <- cal[cal$edon==i, sel.c]
    sel.r <- which(vot$date[vot$edon==i] %in% sel)
    if (length(sel.r)>0) vot$dconcgo[vot$edon==i][sel.r] <- 1
    sel <- cal[cal$elec=="dip", sel.c]
    sel.r <- which(vot$date[vot$edon==i] %in% sel)
    if (length(sel.r)>0) vot$dconcdf[vot$edon==i][sel.r] <- 1
    sel <- cal[cal$elec=="pres", sel.c]
    sel.r <- which(vot$date[vot$edon==i] %in% sel)
    if (length(sel.r)>0) vot$dconcpr[vot$edon==i][sel.r] <- 1
}
table(go=vot$dconcgo, df=vot$dconcdf)
table(pr=vot$dconcpr, df=vot$dconcdf)
rm(cal)
## code reform date
## get electoral calendar again
cal <- read.csv(file = paste0(dd, "../../calendariosReelec/data/fechasEleccionesMexicoDesde1994.csv"))
sel.r <- which(cal$elec=="ayun"); cal <- cal[sel.r, c("edon","elec","yr1st")] # subset ayun
cal$elec <- NULL
cal$yr1st[cal$edon==13] <- 3000
cal$yr1st <- as.numeric(cal$yr1st)
vot <- merge(x = vot, y = cal, by = "edon", all = TRUE)
rm(cal,sel.r)

##################################################
## Dates with missing day arbitrarily set to 15 ##
##################################################
sel.r <- which(vot$date<10000000)
vot$date[sel.r] ## inspect
vot$date[sel.r] <- vot$date[sel.r]*100 + 15

## Get governor parties, code incumbent gov at munic election
gov <- read.csv(file = paste0(dd, "../../mxBranches/statesBranches/32stategovts.csv"))
head(gov[gov$edon==7,])
sel.r <- which(is.na(gov$govin) | gov$govin < 1000000) ## drop redundant rows
gov <- gov[-sel.r, c("edon","yr","govin","govpty")]    ## keep selected columns
gov <- slide(gov, Var = "govin", NewVar = "govout", TimeVar = "govin", GroupVar = "edon", slideBy = 1) ## lead 1
##gov$govin <- ymd(gov$govin)
##gov$govout <- ymd(gov$govout) - days(1) ## end of term
sel.r <- which(is.na(gov$govout))
##gov$govout[sel.r] <- gov$govin[sel.r] + years(6) - days(1) ## add future ends of term
gov$govout[sel.r] <- gov$govin[sel.r] + 60000 ## add future ends of term (6 yrs)
## Import gov pty to vot
vot$govpty <- NA
for (i in 1:32){ ## loop over states
    #i <- 7
    gov2 <- gov[gov$edon==i,]
    vot2 <- vot[vot$edon==i,] ## head(vot2[,1:10])
    for (j in 1:nrow(gov2)){ ## loop over state cycles
        #j <- 1
        sel.r <- which(vot2$date >= gov2$govin[j] & vot2$date < gov2$govout[j])
        if (length(sel.r) > 0) vot2$govpty[sel.r] <- gov2$govpty[j]
    }
    vot[vot$edon==i,] <- vot2 ## return to vot
}
table(vot$govpty, useNA = "ifany")
table(edon=vot$edon[is.na(vot$govpty)], yr=vot$yr[is.na(vot$govpty)]) ## missing cases in 1988 ignored (due to faulty election/munin dates)
##
## Get governor parties again, code midterm elections
gov <- read.csv(file = paste0(dd, "../../mxBranches/statesBranches/32stategovts.csv"))
head(gov[gov$edon==7,])
gov <- gov[, c("edon","yr","govterm","dnewgov")]    ## keep selected columns
##tmp <- vot ## backup
vot$ord <- 1:nrow(vot) ## to retain sort order
vot <- merge(x = vot, y = gov, by = c("edon","yr"), all.x = TRUE, all.y = FALSE)
vot$dmidterm <- as.numeric(vot$govterm <=4 & vot$dnewgov==0)
vot <- vot[order(vot$ord),] # sort
vot$ord <- vot$govterm <- vot$dnewgov <- NULL ## clean
## clean
rm(vot2, gov, gov2, i, j, sel, sel.c, sel.r, year, to.eng, to.num)

## Code incumbent governor dummies (at municipal election) 
vot <- within(vot, {
    dgovmorena <- as.numeric(govpty=="morena")
    dgovmc     <- as.numeric(govpty=="mc")
    dgovpvem   <- as.numeric(govpty=="pvem")
    dgovprd    <- as.numeric(govpty=="prd")
    dgovpri    <- as.numeric(govpty=="pri")
    dgovpan    <- as.numeric(govpty=="pan")
})
## vot <- within(vot, {
##     dpresmorena <- as.numeric(vote >= ymd("20181201") & vote < ymd("20301001"))
##     dprespri    <- as.numeric( vote <  ymd("20001201") |
##                               (vote >= ymd("20121201") & vote < ymd("20181201")))
##     dprespan    <- as.numeric(vote >= ymd("20001201") & vote < ymd("20121201"))
## })
vot <- within(vot, {
    dpresmorena <- as.numeric(date >= 20181201 & date < 20301001)
    dprespri    <- as.numeric(date <  20001201 |
                             (date >= 20121201 & date < 20181201))
    dprespan    <- as.numeric(date >= 20001201 & date < 20121201)
})
##
## code midterm*govparty (these are presidentes municipales that the governor should push to reelect)
vot$dmidpan    <- vot$dmidterm * vot$dgovpan
vot$dmidpri    <- vot$dmidterm * vot$dgovpri
vot$dmidprd    <- vot$dmidterm * vot$dgovprd
vot$dmidmorena <- vot$dmidterm * vot$dgovmorena
vot$dmidpvem   <- vot$dmidterm * vot$dgovpvem
vot$dmidmc     <- vot$dmidterm * vot$dgovmc

## Incumbent ayuntamiento party
vot <- vot[order(vot$inegi, vot$cycle),]
vot <- slide(vot, Var = "win", NewVar = "winlast", TimeVar = "cycle", GroupVar = "inegi", slideBy = -1) ## lag 1
## compute dummies --- equal 1 if party won alone or in coalition last cycle
vot$daymorena <- vot$daymc     <- vot$daypt     <- vot$daypvem   <- vot$dayprd    <- vot$daypri    <- vot$daypan    <- 0
sel.r <- grep("pan",    vot$winlast); vot$daypan   [sel.r] <- 1
sel.r <- grep("pri",    vot$winlast); vot$daypri   [sel.r] <- 1
sel.r <- grep("prd",    vot$winlast); vot$dayprd   [sel.r] <- 1
sel.r <- grep("pvem",   vot$winlast); vot$daypvem  [sel.r] <- 1
sel.r <- grep("pt",     vot$winlast); vot$daypt    [sel.r] <- 1
sel.r <- grep("mc",     vot$winlast); vot$daymc    [sel.r] <- 1
sel.r <- grep("morena", vot$winlast); vot$daymorena[sel.r] <- 1
rm(sel.r)

## triennium cat var (cycle sequence breaks when state calendars change)
## ## cuts centered on fed election, ie [1996,1998] [1999,2001] etc.
## vot$trienio <- cut(vot$yr, 
##                    breaks=c(-Inf, seq(1992,2028,3), Inf),
##                    labels=seq(from=1991, to=2030, by=3))
## cuts starting with fed election, ie [1997,1999] [2000,2002] etc.
vot$trienio <- cut(vot$yr,
                   breaks=seq(1991,(max(vot$yr)+3),3),
                   right=FALSE,
                   labels=seq(1991, max(vot$yr)   ,3))
table(vot$yr,vot$trienio) ## check
vot <- vot[,moveme(names(vot), "trienio before status")]

## state capital municipalities
sel <- which(vot$ife  %in%  c( 1001,
                               2002,
                               3003,
                               4001,
                               5030,
                               6001,
                               7102,
                               8019,
                               9015, # makes Cuauhtémoc DF's capital
                              10005,
                              11015,
                              12001,
                              13047,
                              14041,
                              15107,
                              16054,
                              17007,
                              18017,
                              19040,
                              20066,
                              21115,
                              22014,
                              23007,
                              24028,
                              25006,
                              26049,
                              27004,
                              28041,
                              29033,
                              30089,
                              31050,
                              32056))
vot$dcapital <- 0; vot$dcapital[sel] <- 1
rm(sel)

##################################################
## Get altitude and population from localidades ##
##################################################
## ## With 2010 censo
## ## unzip, read, then delete unzipped file
## system("unzip /home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/2010censo/00_nacional_2010_iter_zip.ZIP")
## ## get municipio altitude variance (from censo 2010 @ localidad level)
## library(foreign)
## alt <- read.dbf("ITER_nalDBF10/ITER_NALDBF10.dbf", as.is = TRUE)
## table(is.na(alt$X))
## dim(alt)
## ## erase unzipped file from disk, too large
## system("rm -r ITER_nalDBF10/")
## # selected columns only
## colnames(alt)
## sel <- grep("entidad|mun|loc|longitud|latitud|altitud|pobtot", colnames(alt), ignore.case = TRUE, perl = TRUE)
## alt <- alt[,sel]
## # rename variables
## colnames(alt) <- c("edon","inegi","mun","locn","localidad","long","lat","alt","ptot","drop")
## alt$drop <- NULL
## alt[1:15,]
##
## From 2020 censo
alt <- data.frame() # prep
for (i in 1:9){
    tmp <- read.csv(paste0("../../censos/raw/2020censo/localidades/poblacion/ITER_0", i, "CSV20.csv"))
    ## Keep selected indicators only
    tmp <- tmp[, c("ENTIDAD","MUN","NOM_MUN","LOC","NOM_LOC","LONGITUD","LATITUD","ALTITUD","POBTOT","P_18YMAS","P_5YMAS","P5_HLI")]
    alt <- rbind(alt, tmp)
}
for (i in 10:32){
    tmp <- read.csv(paste0("../../censos/raw/2020censo/localidades/poblacion/ITER_", i, "CSV20.csv"))
    ## Keep selected indicators only
    tmp <- tmp[, c("ENTIDAD","MUN","NOM_MUN","LOC","NOM_LOC","LONGITUD","LATITUD","ALTITUD","POBTOT","P_18YMAS","P_5YMAS","P5_HLI")]
    alt <- rbind(alt, tmp)
}
colnames(alt) <- c("edon","inegi","mun","locn","localidad","long","lat","alt","ptot","p18","p5","p5li")
alt[5:10,]
## 2020 long lat need manip
alt$long <- gsub("[°'\" WN]", "", alt$long)
alt$lat <- gsub("[°'\" WN]", "", alt$lat)
## 2020 alt
sel.r <- grep("^00[-]", alt$alt) ## negative altitudes with leading zeroes
table(alt$alt[sel.r])
alt$alt <- gsub("^00[-]", "-", alt$alt)
## numeric vars
alt$edon <- as.numeric(as.character(alt$edon))
alt$inegi <- as.numeric(as.character(alt$inegi))
alt$mun <- as.character(alt$mun)
alt$locn <- as.numeric(as.character(alt$locn))
alt$localidad <- as.character(alt$localidad)
alt$long <- as.numeric(as.character(alt$long))
alt$lat <- as.numeric(as.character(alt$lat))
alt$alt <- as.numeric(as.character(alt$alt))
alt$ptot <- as.numeric(as.character(alt$ptot))
## alt$p18 <- as.numeric(as.character(alt$p18))
## alt$p5 <- as.numeric(as.character(alt$p5))
## alt$p5li <- as.numeric(as.character(alt$p5li))
## add edon to inegi
alt$inegi <- alt$edon*1000 + alt$inegi
## drop aggregate rows
sel <- which(alt$locn==0|alt$locn==9998|alt$locn==9999)
alt <- alt[-sel,]
## standardize long lat to 0=further E/S to 1=further W/N
alt <- within(alt, {
    longs <- (long - (min(long))) / (max(long) - min(long))
    lats  <- (lat  - (min(lat)))  / (max(lat)  - min(lat))
})
## mun pop share in localidad
alt$tmp      <- ave(alt$ptot, as.factor(alt$inegi), FUN=sum, na.rm=TRUE)
alt$popsh    <- alt$ptot / alt$tmp
alt$tmp <- NULL
summary(alt$popsh)
## Effective num of localidades (pop-wise)
alt$effloc <- ave(alt$popsh, as.factor(alt$inegi), FUN=function(x) 1/sum(x^2))
summary(alt$effloc)
##
## Population share in cabecera
## should capture center/periphery conflict
## ## 2010 localidad name has char encoding issues, this imports a previously exported version converted to utf-8
## localidades <- read.csv("../ancillary/localidades-2010-utf8.csv")
## table(localidades$locn==alt$locn) ## check all same order
## alt$localidad <- localidades$localidad
## rm(localidades)
## Cabecera is locn=1 in municipio, with very few exceptions. Most cabeceras share mun's name (not all: eg. Santa Rosalía for Mulegé)
alt$dcabecera <- as.numeric(alt$locn==1)
sel.r <- which(alt$inegi== 7033 & alt$locn== 42);  alt$dcabecera[sel.r] <- 1 ## inegi== 7033 Francisco León
sel.r <- which(alt$inegi==24056 & alt$locn== 2);   alt$dcabecera[sel.r] <- 1 ## inegi==24056 Villa de Arista
## ## cabeceras of municipios created after 2010 (for use when generating variables with censo 2010)
## sel.r <- which(alt$inegi== 2001 & alt$locn==857); alt$dcabecera[sel.r] <- 1 ## inegi== 2006 SAN QUINTIN
## sel.r <- which(alt$inegi== 7072 & alt$locn== 54;  alt$dcabecera[sel.r] <- 1 ## inegi== 7121 RINCON CHAMULA SAN PEDRO
## sel.r <- which(alt$inegi== 7080 & alt$locn== 13;  alt$dcabecera[sel.r] <- 1 ## inegi== 7125 HONDURAS DE LA SIERRA
## sel.r <- which(alt$inegi== 7092 & alt$locn== 31;  alt$dcabecera[sel.r] <- 1 ## inegi== 7124 MEZCALAPA
## sel.r <- which(alt$inegi== 7102 & alt$locn== 71;  alt$dcabecera[sel.r] <- 1 ## inegi== 7123 EMILIANO ZAPATA
## sel.r <- which(alt$inegi== 7107 & alt$locn==166;  alt$dcabecera[sel.r] <- 1 ## inegi== 7122 PARRAL--EL
## sel.r <- which(alt$inegi==14093 & alt$locn== 58;  alt$dcabecera[sel.r] <- 1 ## inegi==14126 CAPILLA DE GUADALUPE
## sel.r <- which(alt$inegi==23004 & alt$locn== 11;  alt$dcabecera[sel.r] <- 1 ## inegi==23010 BACALAR
## sel.r <- which(alt$inegi==23005 & alt$locn== 24;  alt$dcabecera[sel.r] <- 1 ## inegi==23011 PUERTO MORELOS
## ## cabeceras of municipios created after 2020 (for use when generating variables with censo 2020)
sel.r <- which(alt$inegi==4001 & alt$locn== 7);  alt$dcabecera[sel.r] <- 1 ## inegi==4013 DZITBALCHE
sel.r <- which(alt$inegi==4001 & alt$locn %in% c(3,7));  alt$inegi[sel.r] <- 4013 ## inegi==4013 localidades in DZITBALCHE
## sel.r <- which(alt$inegi==14093 & alt$locn== 58;  alt$dcabecera[sel.r] <- 1 ## inegi==14126 CAPILLA DE GUADALUPE
alt$popshincab <- alt$popsh * alt$dcabecera
alt$popshincab <- ave(alt$popshincab, as.factor(alt$inegi), FUN=sum) ## put value in all localidades
summary(alt$popshincab)
## weighted mean(alt) and sd(alt)
alt$altpopsh <- alt$popsh * alt$alt 
alt$wmeanalt <- ave(alt$altpopsh, as.factor(alt$inegi), FUN=sum, na.rm=TRUE)
alt$altpopsh <- alt$popsh * (alt$alt - alt$wmeanalt)^2
alt$wsdalt <-   ave(alt$altpopsh, as.factor(alt$inegi), FUN=sum, na.rm=TRUE)
alt$wsdalt <-   sqrt(alt$wsdalt)
alt$altpopsh <- NULL # clean
# mean(alt) and sd(alt)
alt$meanalt <- ave(alt$alt, as.factor(alt$inegi), FUN=mean, na.rm=TRUE)
alt$sdalt <-   ave(alt$alt, as.factor(alt$inegi), FUN=sd,   na.rm=TRUE)
# cases with single localidad sd=NA
sel <- which(is.na(alt$sdalt)==TRUE & is.na(alt$meanalt)==FALSE)
alt$inegi[sel]
alt$wsdalt[sel] <- 0
alt$sdalt[sel] <- 0
##
## drop redundant rows and cols
alt <- alt[-duplicated(as.factor(alt$inegi))==FALSE,]
alt[1,]
alt$locn <- alt$localidad <- alt$alt <- alt$mun <- alt$edon <- alt$popsh <- alt$dcabecera <- NULL
alt$wmeanalt <- round(alt$wmeanalt, 1)
alt$wsdalt <- round(alt$wsdalt, 1)
alt$meanalt <- round(alt$meanalt, 1)
alt$sdalt <- round(alt$sdalt, 1)
#summary(alt$sdalt)
#summary(alt$wsdalt)
##
## add geo vars to vot
dim(alt)
alt[1,]
vot[1,]
sel.c <- c("inegi","longs","lats","effloc","popshincab","wmeanalt","wsdalt","meanalt","sdalt") # columns to merge into vot
vot <- merge(x = vot, y = alt[,sel.c], by = "inegi", all.x = TRUE, all.y = FALSE)
rm(i,sel,sel.r,sel.c,tmp) # clean
summary(vot$sdalt)
##

###################################################################################
## Luminosity                                                                    ##
## Read seccion-level 2020 populations and generate pop-weighted municipal stats ##
###################################################################################
cese <- read.csv("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/data/censo2020-se.csv")
cese <- cese[, c("edon","seccion","inegi","ife","POBTOT")] # trim columns
colnames(cese)[5] <- "ptot"
cese$seccion <- cese$edon*10000 + cese$seccion
head(cese)
##
## Gets state conversion function
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
source( paste(pth, "edo2edon.r", sep = "/") )
rm(pth)
##
## reads seccion-level 2018 luminosity data, adds 2020 population
lumu <- data.frame()
for (i in 1:32){
    ##i <- 2
    pth <- paste0("/home/eric/Downloads/Desktop/data/mapas/luminosity/data/secciones/", edon2edo(i), "/lum2018.csv")
    luse <- read.csv(pth)  ## read state's se-level luminosity
    luse$seccion <- luse$edon*10000 + luse$seccion
    luse <- merge(x = luse, y = cese[, c("seccion", "ife", "ptot")], by = "seccion", all.x = TRUE, all.y = FALSE) ## add ptot inegi
    luse$ife <- luse$edon*1000 + luse$ife
    luse$PTOT <- ave(luse$ptot, as.factor(luse$ife), FUN=function(x) sum(x, na.rm=TRUE)) ## sum municipal pops
    luse$meanwpop <- luse$mean * luse$ptot / luse$PTOT  ## step to generate meanwpop
    luse$meanwpop <- round(ave(luse$meanwpop, as.factor(luse$ife), FUN=function(x) sum(x, na.rm=TRUE)), 3) ## generate meanwpop
    tmp <- luse[duplicated(luse$ife)==FALSE, c("ife", "meanwpop")]
    lumu <- rbind(lumu, tmp)
    ##luse[1,]
}
summary(lumu)
##
vot <- merge(x = vot, y = lumu, by = "ife", all.x = TRUE, all.y = FALSE)
vot$lumwpop20 <- vot$meanwpop; vot$meanwpop <- NULL
##
rm(cese,lumu,i,pth,tmp)

## re-define party-by-party incumbency variables
## rename vars: dincpan will now mean outgoing mayor is pan etc, dincballotpan that outgoing mayior is in ballot again
colnames(vot) <- gsub("^dinc", "dincballot", colnames(vot))
## generic dummy dincballot=1 if outgoing mayor (regardless of party) is in ballot again
vot <- within(vot, {
    dincballot <- as.numeric(dincballotpan==1 | dincballotpri==1 | dincballotprd==1 | dincballotpvem==1 | dincballotpt==1 | dincballotmc==1 | dincballotmorena==1 | dincballototh==1)
})
## rename accordingly daypan to dincpan etc
colnames(vot) <- gsub("^day", "dinc", colnames(vot))

## Separate id and non-time varying vars into own data.frame
vot[1000,]
vot <- vot[order(vot$inegi, vot$cycle),]
vot$ord <- 1:nrow(vot)
sel.c <- which(colnames(vot) %in% c("inegi","edon","cycle","yr","ife","mun","date","trienio","status","yr1st","dcapital","longs","lats","effloc","popshincab","wmeanalt","wsdalt","meanalt","sdalt","lumwpop20"))
ids <- cbind(emm=vot$emm, ord=vot$ord, vot[,sel.c])
vot <- vot[,-sel.c]
vot <- vot[,moveme(colnames(vot), "ord first")]

## Import municipal electoral histories for ids
elhis <- data.frame()
for (yr in seq(1994,2024,3)){
    #yr <- 2009
    tmp <- read.csv(file = paste0(dd, "v-hats-etc/mun/dipfed-municipio-vhat-", yr, ".csv"))
    tmp[1,]
    elhis <- rbind(elhis, tmp)
}
## keep selected vars only
sel.c <- grep("yr|ife|vhat|alpha|beta", colnames(elhis))
elhis <- elhis[, sel.c]
## compute oth vars
elhis <- within(elhis, vhat.oth <- 1 - vhat.pan - vhat.pri - vhat.left)
elhis <- elhis[, moveme(colnames(elhis), "vhat.oth after vhat.left")]
## alpha and beta to ids
tmp <- elhis[which(duplicated(elhis$ife)==FALSE), c("ife","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")]
ids <- merge(x = ids, y = tmp, by = "ife", all.x = TRUE, all.y = FALSE)
## vhats to vot
## function to complete missing ifes
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
source( paste(pth, "inegi2ife.r", sep = "/") )
rm(pth)
elhis$inegi <- ife2inegi(elhis$ife)
## use info from elecRetrns/ancillary/mun-yrs to deduce state cycle from federal yr
elhis$edon <- as.integer(elhis$inegi/1000)
elhis$cycle <- elhis$yr
sel <- which(elhis$edon==1)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==1],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019, 2021, 2024
                              to = 4:19)
sel <- which(elhis$edon==2)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==2],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==3)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==3],
                              from = c(1979, 1982, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2015, 2018, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==4)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==4],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==5)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==5],
                              from = c(1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012,       2018, 2021, 2024),
                           ##state els 1981, 1984, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2009, 2013, 2017, 2018, 2021, 2024),
                              to = c(4:14,16:18)) ## skip 15 that will be a manual duplicate of 16 below
sel <- which(elhis$edon==6)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==6],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==7)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==7],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1995, 1998, 2001, 2004, 2007, 2010, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==8)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==8],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==9)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==9],
                              from = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 11:19)
sel <- which(elhis$edon==10)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==10],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019, 2022, 2025),
                              to = 4:19)
sel <- which(elhis$edon==11)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==11],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==12)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==12],
                              from = c(1979, 1982, 1985, 1988, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1993, 1996, 1999, 2002, 2005, 2008, 2012, 2015, 2018, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==13)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==13],
                              from = c(1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2021, 2024),
                           ##state els 1981, 1984, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2016, 2020, 2024),
                              to = 4:17)
sel <- which(elhis$edon==14)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==14],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1992, 1995, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==15)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==15],
                              from = c(1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1981, 1984, 1987, 1990, 1993, 1996, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==16)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==16],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2011, 2015, 2018, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==17)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==17],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==18)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==18],
                              from = c(1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1981, 1984, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2014, 2017, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==19)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==19],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==20)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==20],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==21)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==21],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2018, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==22)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==22],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==23)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==23],
                              from = c(1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006,       2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1981, 1984, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2010, 2013, 2016, 2018, 2021, 2024),
                              to = c(4:12,14:19)) ## skip 13 that will be a manual duplicate of 14 below
sel <- which(elhis$edon==24)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==24],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==25)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==25],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==26)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==26],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==27)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==27],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==28)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==28],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024),
                              to = 4:19)
sel <- which(elhis$edon==29)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==29],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==30)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==30],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2004, 2007, 2010, 2013, 2017, 2021, 2024),
                              to = 4:18)
sel <- which(elhis$edon==31)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==31],
                              from = c(1982, 1985, 1988, 1991, 1994,       1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1981, 1984, 1987, 1990, 1993, 1995, 1998, 2001, 2004, 2007, 2010, 2012, 2016, 2018, 2021, 2024),
                              to = c(4:8,10:19))  ## skip 9 that will be a manual duplicate of 8 below
sel <- which(elhis$edon==32)
elhis$cycle[sel] <- mapvalues(elhis$cycle[elhis$edon==32],
                              from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024),
                           ##state els 1979, 1982, 1985, 1988, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024),
                              to = 4:19)
## duplicate cycle 16 in yucatán and make it cycle 15
sel <- which(elhis$edon==5 & elhis$cycle==16)
tmp <- elhis[sel,]
tmp$cycle <- 15
elhis <- rbind(elhis, tmp)
## duplicate cycle 14 in quintana roo and make it cycle 13
sel <- which(elhis$edon==23 & elhis$cycle==14)
tmp <- elhis[sel,]
tmp$cycle <- 13
elhis <- rbind(elhis, tmp)
## duplicate cycle 8 in yucatán and make it cycle 9
sel <- which(elhis$edon==31 & elhis$cycle==8)
tmp <- elhis[sel,]
tmp$cycle <- 9
elhis <- rbind(elhis, tmp)
## build emm for merging
elhis$edo <- mapvalues(x=elhis$edon,
                          from = 1:32,
                          to = c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac"))
tmp <- sub('.*(?=.{3}$)', '', elhis$inegi, perl=TRUE) ## last three characters of inegi
elhis$emm <- paste0(elhis$edo, "-0", elhis$cycle, ".", tmp) ## adds zero heading cycle
elhis$emm <- sub("-0([1-9][0-9])", "-\\1", elhis$emm)       ## drop heading zero in emm when followed by non-zero and another digit
elhis <- within(elhis, edo <- alphahat.pan <- alphahat.pri <- alphahat.left <- betahat.pan <- betahat.left <- NULL)
##
vot[1000,]
elhis[, c("emm","vhat.pan","vhat.pri","vhat.left")][2,]
vot <- merge(x = vot, y = elhis[, c("emm","vhat.pan","vhat.pri","vhat.left")], by = "emm", all.x = TRUE, all.y = FALSE)

## Save data
getwd()
save.image(file = "tmp.RData")

######################
## read saved image ##
######################
library(DataCombine) # easy lags
rm(list = ls())
##
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
wd <- "/home/eric/Desktop/MXelsCalendGovt/reelec/data"
setwd(wd)
load(file = "tmp.RData")

## turnout
p18 <- read.csv(file = paste0(dd, "../../censos/data/pob18/p18mu-from-se-level-projection-aggregates.csv"))
##p18 <- read.csv(file = paste0(dd, "../../censos/data/pob18/p18mu-for-municipal-elecs.csv"))
colnames(p18) <- gsub("y","p18_",colnames(p18))
p18[1,]
p18$yr <- 1995; p18$p18 <- p18$p18_1995
tmp2 <- p18
tmp <- tmp2; tmp$yr <- 1996; tmp$p18 <- tmp$p18_1996; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 1997; tmp$p18 <- tmp$p18_1997; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 1998; tmp$p18 <- tmp$p18_1998; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 1999; tmp$p18 <- tmp$p18_1999; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2000; tmp$p18 <- tmp$p18_2000; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2001; tmp$p18 <- tmp$p18_2001; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2002; tmp$p18 <- tmp$p18_2002; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2003; tmp$p18 <- tmp$p18_2003; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2004; tmp$p18 <- tmp$p18_2004; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2005; tmp$p18 <- tmp$p18_2005; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2006; tmp$p18 <- tmp$p18_2006; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2007; tmp$p18 <- tmp$p18_2007; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2008; tmp$p18 <- tmp$p18_2008; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2009; tmp$p18 <- tmp$p18_2009; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2010; tmp$p18 <- tmp$p18_2010; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2011; tmp$p18 <- tmp$p18_2011; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2012; tmp$p18 <- tmp$p18_2012; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2013; tmp$p18 <- tmp$p18_2013; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2014; tmp$p18 <- tmp$p18_2014; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2015; tmp$p18 <- tmp$p18_2015; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2016; tmp$p18 <- tmp$p18_2016; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2017; tmp$p18 <- tmp$p18_2017; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2018; tmp$p18 <- tmp$p18_2018; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2019; tmp$p18 <- tmp$p18_2019; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2020; tmp$p18 <- tmp$p18_2020; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2021; tmp$p18 <- tmp$p18_2021; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2022; tmp$p18 <- tmp$p18_2022; p18 <- rbind(p18, tmp)
tmp <- tmp2; tmp$yr <- 2023; tmp$p18 <- tmp$p18_2023; p18 <- rbind(p18, tmp)
rm(tmp,tmp2)
p18 <- p18[, c("edon","ife","inegi","mun","yr","p18")]
p18 <- within(p18, inegi.yr <- paste0(inegi, ".", yr))
p18 <- p18[, c("inegi.yr","p18")]
## merge
vot <- vot[order(vot$ord),]; ids <- ids[order(ids$ord),]
tmp <- ids[, c("ord","inegi","yr")]
tmp <- within(tmp, inegi.yr <- paste0(inegi, ".", yr))
tmp <- tmp[, c("ord","inegi.yr")]
tmp <- merge(x = tmp, y = p18, by = "inegi.yr", all.x = TRUE, all.y = FALSE)
dim(tmp)
tmp[1:10,]
## cbind
tmp <- tmp[order(tmp$ord),]
vot$p18 <- tmp$p18
vot[1,]
##
##
## get federal lisnom for comparison
## prep object that will receive all fed lisnoms
ln <- data.frame()
##
y <- 1991; d <- read.csv(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/dip",y,".csv"))
## assign secciones in fed-unused municipios
table(d$nota.emm)
d$ife[grep("ife=10017" , d$nota.emm)] <- 10017
d$ife[grep("ife=18004" , d$nota.emm)] <- 18004
## drop cols
d <- d[, c("ife","lisnom")]; d$yr <- y
## agg
d$lisnom <- ave(d$lisnom, as.factor(d$ife), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$ife)==FALSE,]
## paste to data
ln <- rbind(ln, d)
##
y <- 1994; d <- read.csv(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/dip",y,".csv"))
## assign secciones in fed-unused municipios
table(d$nota.emm)
d$ife[grep("ife=1010" , d$nota.emm)] <- 1010
d$ife[grep("ife=1011" , d$nota.emm)] <- 1011
d$ife[grep("ife=15122", d$nota.emm)] <- 15122
d$ife[grep("ife=23008", d$nota.emm)] <- 23008
d$ife[grep("ife=3005" , d$nota.emm)] <- 3005
## drop cols
d <- d[, c("ife","lisnom")]; d$yr <- y
## agg
d$lisnom <- ave(d$lisnom, as.factor(d$ife), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$ife)==FALSE,]
## paste to data
ln <- rbind(ln, d)
##
y <- 1997; d <- read.csv(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/dip",y,".csv"))
## assign secciones in fed-unused municipios
table(d$nota.emm)
d$ife[grep("ife=12076", d$nota.emm)] <- 12076
d$ife[grep("ife=2005" , d$nota.emm)] <- 2005
d$ife[grep("ife=29045", d$nota.emm)] <- 29045
d$ife[grep("ife=29046", d$nota.emm)] <- 29046
d$ife[grep("ife=29047", d$nota.emm)] <- 29047
d$ife[grep("ife=29048", d$nota.emm)] <- 29048
d$ife[grep("ife=29049", d$nota.emm)] <- 29049
d$ife[grep("ife=29050", d$nota.emm)] <- 29050
d$ife[grep("ife=29051", d$nota.emm)] <- 29051
d$ife[grep("ife=29052", d$nota.emm)] <- 29052
d$ife[grep("ife=29053", d$nota.emm)] <- 29053
d$ife[grep("ife=29054", d$nota.emm)] <- 29054
d$ife[grep("ife=29055", d$nota.emm)] <- 29055
d$ife[grep("ife=29056", d$nota.emm)] <- 29056
d$ife[grep("ife=29057", d$nota.emm)] <- 29057
d$ife[grep("ife=29058", d$nota.emm)] <- 29058
d$ife[grep("ife=29059", d$nota.emm)] <- 29059
d$ife[grep("ife=29060", d$nota.emm)] <- 29060
d$ife[grep("ife=4011" , d$nota.emm)] <- 4011
## drop cols
d <- d[, c("ife","lisnom")]; d$yr <- y
## agg
d$lisnom <- ave(d$lisnom, as.factor(d$ife), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$ife)==FALSE,]
## paste to data
ln <- rbind(ln, d)
##
y <- 2000; d <- read.csv(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/dip",y,".csv"))
## assign secciones in fed-unused municipios
table(d$nota.emm)
d$ife[grep("ife=4011" , d$nota.emm)] <- 4011
## drop cols
d <- d[, c("ife","lisnom")]; d$yr <- y
## agg
d$lisnom <- ave(d$lisnom, as.factor(d$ife), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$ife)==FALSE,]
## paste to data
ln <- rbind(ln, d)
##
y <- 2003; d <- read.csv(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/dip",y,".csv"))
## assign secciones in fed-unused municipios
table(d$nota.emm)
d$ife[grep("ife=15123", d$nota.emm)] <- 15123
d$ife[grep("ife=15124", d$nota.emm)] <- 15124
d$ife[grep("ife=30211", d$nota.emm)] <- 30211
d$ife[grep("ife=30212", d$nota.emm)] <- 30212
d$ife[grep("ife=4011" , d$nota.emm)] <- 4011
d$ife[grep("ife=7112" , d$nota.emm)] <- 7112
d$ife[grep("ife=7113" , d$nota.emm)] <- 7113
d$ife[grep("ife=7114" , d$nota.emm)] <- 7114
d$ife[grep("ife=7115" , d$nota.emm)] <- 7115
d$ife[grep("ife=7116" , d$nota.emm)] <- 7116
d$ife[grep("ife=7117" , d$nota.emm)] <- 7117
d$ife[grep("ife=7118" , d$nota.emm)] <- 7118
## drop cols
d <- d[, c("ife","lisnom")]; d$yr <- y
## agg
d$lisnom <- ave(d$lisnom, as.factor(d$ife), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$ife)==FALSE,]
## paste to data
ln <- rbind(ln, d)
##
y <- 2006; d <- read.csv(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/dip",y,".csv"))
## assign secciones in fed-unused municipios
table(d$nota.emm)
d$ife[grep("ife=12080", d$nota.emm)] <- 12080
d$ife[grep("ife=14125", d$nota.emm)] <- 14125
d$ife[grep("ife=15125", d$nota.emm)] <- 15125
d$ife[grep("ife=4011" , d$nota.emm)] <- 4011
d$ife[grep("ife=7112" , d$nota.emm)] <- 7112
d$ife[grep("ife=7113" , d$nota.emm)] <- 7113
d$ife[grep("ife=7114" , d$nota.emm)] <- 7114
d$ife[grep("ife=7115" , d$nota.emm)] <- 7115
d$ife[grep("ife=7116" , d$nota.emm)] <- 7116
d$ife[grep("ife=7117" , d$nota.emm)] <- 7117
d$ife[grep("ife=7118" , d$nota.emm)] <- 7118
## ## any duplicate casilla?
## table(dup=duplicated(paste(d$edon, d$seccion, d$casilla, sep="-")))
## with(d[duplicated(paste(d$edon, d$seccion, d$casilla, sep="-"))==TRUE,], table(dup.only=casilla, edon=edon))
## drop cols
d <- d[, c("ife","lisnom")]; d$yr <- y
## agg
d$lisnom <- ave(d$lisnom, as.factor(d$ife), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$ife)==FALSE,]
## paste to data
ln <- rbind(ln, d)
##
y <- 2009; d <- read.csv(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/dip",y,".csv"))
## assign secciones in fed-unused municipios
table(d$nota.emm)
d$ife[grep("ife=12077", d$nota.emm)] <- 12077
d$ife[grep("ife=12078", d$nota.emm)] <- 12078
d$ife[grep("ife=12079", d$nota.emm)] <- 12079
d$ife[grep("ife=12080", d$nota.emm)] <- 12080
d$ife[grep("ife=12081", d$nota.emm)] <- 12081
d$ife[grep("ife=14125", d$nota.emm)] <- 14125
d$ife[grep("ife=23009", d$nota.emm)] <- 23009
d$ife[grep("ife=4011" , d$nota.emm)] <- 4011
d$ife[grep("ife=7112" , d$nota.emm)] <- 7112
d$ife[grep("ife=7113" , d$nota.emm)] <- 7113
d$ife[grep("ife=7114" , d$nota.emm)] <- 7114
d$ife[grep("ife=7115" , d$nota.emm)] <- 7115
d$ife[grep("ife=7116" , d$nota.emm)] <- 7116
d$ife[grep("ife=7117" , d$nota.emm)] <- 7117
d$ife[grep("ife=7118" , d$nota.emm)] <- 7118
## drop cols
d <- d[, c("ife","lisnom")]; d$yr <- y
## agg
d$lisnom <- ave(d$lisnom, as.factor(d$ife), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$ife)==FALSE,]
## paste to data
ln <- rbind(ln, d)
##
y <- 2012; d <- read.csv(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/dip",y,".csv"))
## assign secciones in fed-unused municipios
table(d$nota.emm)
d$ife[grep("ife=23009", d$nota.emm)] <- 23009
d$ife[grep("ife=23010", d$nota.emm)] <- 23010
d$ife[grep("ife=4011" , d$nota.emm)] <- 4011
d$ife[grep("ife=7112" , d$nota.emm)] <- 7112
d$ife[grep("ife=7113" , d$nota.emm)] <- 7113
d$ife[grep("ife=7114" , d$nota.emm)] <- 7114
d$ife[grep("ife=7115" , d$nota.emm)] <- 7115
d$ife[grep("ife=7116" , d$nota.emm)] <- 7116
d$ife[grep("ife=7117" , d$nota.emm)] <- 7117
d$ife[grep("ife=7118" , d$nota.emm)] <- 7118
d$ife[grep("ife=7121" , d$nota.emm)] <- 7121
d$ife[grep("ife=7122" , d$nota.emm)] <- 7122
d$ife[grep("ife=7123" , d$nota.emm)] <- 7123
## drop cols
d <- d[, c("ife","lisnom")]; d$yr <- y
## agg
d$lisnom <- ave(d$lisnom, as.factor(d$ife), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$ife)==FALSE,]
## paste to data
ln <- rbind(ln, d)
##
y <- 2015; d <- read.csv(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/dip",y,".csv"))
## assign secciones in fed-unused municipios
table(d$nota.emm)
d$ife[grep("ife=23009", d$nota.emm)] <- 23009
d$ife[grep("ife=23010", d$nota.emm)] <- 23010
d$ife[grep("ife=23011", d$nota.emm)] <- 23011
d$ife[grep("ife=7112" , d$nota.emm)] <- 7112
d$ife[grep("ife=7113" , d$nota.emm)] <- 7113
d$ife[grep("ife=7114" , d$nota.emm)] <- 7114
d$ife[grep("ife=7115" , d$nota.emm)] <- 7115
d$ife[grep("ife=7116" , d$nota.emm)] <- 7116
d$ife[grep("ife=7117" , d$nota.emm)] <- 7117
d$ife[grep("ife=7118" , d$nota.emm)] <- 7118
d$ife[grep("ife=7121" , d$nota.emm)] <- 7121
d$ife[grep("ife=7122" , d$nota.emm)] <- 7122
d$ife[grep("ife=7123" , d$nota.emm)] <- 7123
## drop cols
d <- d[, c("ife","lisnom")]; d$yr <- y
## agg
d$lisnom <- ave(d$lisnom, as.factor(d$ife), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$ife)==FALSE,]
## paste to data
ln <- rbind(ln, d)
##
y <- 2018; d <- read.csv(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/dip",y,".csv"))
## assign secciones in fed-unused municipios
table(d$nota.emm)
d$ife[grep("ife=7119", d$nota.emm)] <- 7119
d$ife[grep("ife=7120", d$nota.emm)] <- 7120
d$ife[grep("ife=7121", d$nota.emm)] <- 7121
d$ife[grep("ife=7122", d$nota.emm)] <- 7122
d$ife[grep("ife=7123", d$nota.emm)] <- 7123
## drop cols
d <- d[, c("ife","lisnom")]; d$yr <- y
## agg
d$lisnom <- ave(d$lisnom, as.factor(d$ife), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$ife)==FALSE,]
## paste to data
ln <- rbind(ln, d)
##
y <- 2021; d <- read.csv(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/dip",y,".csv"))
## assign secciones in fed-unused municipios
table(d$nota.emm)
d$ife[grep("ife=17034", d$nota.emm)] <- 17034
d$ife[grep("ife=17035", d$nota.emm)] <- 17035
d$ife[grep("ife=17036", d$nota.emm)] <- 17036
## drop cols
d <- d[, c("ife","lisnom")]; d$yr <- y
## agg
d$lisnom <- ave(d$lisnom, as.factor(d$ife), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$ife)==FALSE,]
## paste to data
ln <- rbind(ln, d)
##
## OJO 28feb2025: need final dip results (shouldn't change lisnom, but file name will)
y <- 2024; d <- read.csv(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/dip",y,"-pre-trife.csv"))
## assign secciones in fed-unused municipios
table(d$nota.emm)
## drop cols
d <- d[, c("ife","lisnom")]; d$yr <- y
## agg
d$lisnom <- ave(d$lisnom, as.factor(d$ife), FUN=function(x) sum(x, na.rm=TRUE))
d <- d[duplicated(d$ife)==FALSE,]
## paste to data
ln <- rbind(ln, d)
rm(d,y)

##
## merge to vot
t <- ids[,c("ord","emm","ife","yr")] ## will receive fed lisnom for merge
t$lisnom <- NA
##table(t$yr)
##
sel.l <- which(ln$yr ==       1994    )
sel.t <- which( t$yr %in% 1993  : 1995)
if (length(sel.l) > 0 & length(sel.t) > 0){
    t2 <- t[sel.t,]; t2$yr <-     1994    ; t2$lisnom <- NULL ## subset and drop lisnom to void duplic names in merge
    t2 <- merge(x=t2, y=ln[sel.l,], by = c("ife","yr"), all.x=TRUE, all.y=FALSE)
    t2 <- t2[order(t2$ord),]
    t2$lisnom -> t$lisnom[sel.t] ## return
}
##
sel.l <- which(ln$yr ==       1997    )
sel.t <- which( t$yr %in% 1996  : 1998)
if (length(sel.l) > 0 & length(sel.t) > 0){
    t2 <- t[sel.t,]; t2$yr <-     1997    ; t2$lisnom <- NULL ## subset
    t2 <- merge(x=t2, y=ln[sel.l,], by = c("ife","yr"), all.x=TRUE, all.y=FALSE)
    t2 <- t2[order(t2$ord),]
    t2$lisnom -> t$lisnom[sel.t] ## return
}
##
sel.l <- which(ln$yr ==       2000    )
sel.t <- which( t$yr %in% 1999  : 2001)
if (length(sel.l) > 0 & length(sel.t) > 0){
    t2 <- t[sel.t,]; t2$yr <-     2000    ; t2$lisnom <- NULL ## subset
    t2 <- merge(x=t2, y=ln[sel.l,], by = c("ife","yr"), all.x=TRUE, all.y=FALSE)
    t2 <- t2[order(t2$ord),]
    t2$lisnom -> t$lisnom[sel.t] ## return
}
##
sel.l <- which(ln$yr ==       2003    )
sel.t <- which( t$yr %in% 2002  : 2004)
if (length(sel.l) > 0 & length(sel.t) > 0){
    t2 <- t[sel.t,]; t2$yr <-     2003    ; t2$lisnom <- NULL ## subset
    t2 <- merge(x=t2, y=ln[sel.l,], by = c("ife","yr"), all.x=TRUE, all.y=FALSE)
    t2 <- t2[order(t2$ord),]
    t2$lisnom -> t$lisnom[sel.t] ## return
}
##
sel.l <- which(ln$yr ==       2006    )
sel.t <- which( t$yr %in% 2005  : 2007)
if (length(sel.l) > 0 & length(sel.t) > 0){
    t2 <- t[sel.t,]; t2$yr <-     2006    ; t2$lisnom <- NULL ## subset
    t2 <- merge(x=t2, y=ln[sel.l,], by = c("ife","yr"), all.x=TRUE, all.y=FALSE)
    t2 <- t2[order(t2$ord),]
    t2$lisnom -> t$lisnom[sel.t] ## return
}
##
sel.l <- which(ln$yr ==       2009    )
sel.t <- which( t$yr %in% 2008  : 2010)
if (length(sel.l) > 0 & length(sel.t) > 0){
    t2 <- t[sel.t,]; t2$yr <-     2009    ; t2$lisnom <- NULL ## subset
    t2 <- merge(x=t2, y=ln[sel.l,], by = c("ife","yr"), all.x=TRUE, all.y=FALSE)
    t2 <- t2[order(t2$ord),]
    t2$lisnom -> t$lisnom[sel.t] ## return
}
##
sel.l <- which(ln$yr ==       2012    )
sel.t <- which( t$yr %in% 2011  : 2013)
if (length(sel.l) > 0 & length(sel.t) > 0){
    t2 <- t[sel.t,]; t2$yr <-     2012    ; t2$lisnom <- NULL ## subset
    t2 <- merge(x=t2, y=ln[sel.l,], by = c("ife","yr"), all.x=TRUE, all.y=FALSE)
    t2 <- t2[order(t2$ord),]
    t2$lisnom -> t$lisnom[sel.t] ## return
}
##
sel.l <- which(ln$yr ==       2015    )
sel.t <- which( t$yr %in% 2014  : 2016)
if (length(sel.l) > 0 & length(sel.t) > 0){
    t2 <- t[sel.t,]; t2$yr <-     2015    ; t2$lisnom <- NULL ## subset
    t2 <- merge(x=t2, y=ln[sel.l,], by = c("ife","yr"), all.x=TRUE, all.y=FALSE)
    t2 <- t2[order(t2$ord),]
    t2$lisnom -> t$lisnom[sel.t] ## return
}
##
sel.l <- which(ln$yr ==       2018    )
sel.t <- which( t$yr %in% 2017  : 2019)
if (length(sel.l) > 0 & length(sel.t) > 0){
    t2 <- t[sel.t,]; t2$yr <-     2018    ; t2$lisnom <- NULL ## subset
    t2 <- merge(x=t2, y=ln[sel.l,], by = c("ife","yr"), all.x=TRUE, all.y=FALSE)
    t2 <- t2[order(t2$ord),]
    t2$lisnom -> t$lisnom[sel.t] ## return
}
##
sel.l <- which(ln$yr ==       2021    )
sel.t <- which( t$yr %in% 2020  : 2022)
if (length(sel.l) > 0 & length(sel.t) > 0){
    t2 <- t[sel.t,]; t2$yr <-     2021    ; t2$lisnom <- NULL ## subset
    t2 <- merge(x=t2, y=ln[sel.l,], by = c("ife","yr"), all.x=TRUE, all.y=FALSE)
    t2 <- t2[order(t2$ord),]
    t2$lisnom -> t$lisnom[sel.t] ## return
}
##
sel.l <- which(ln$yr ==       2024    )
sel.t <- which( t$yr %in% 2023  : 2025)
if (length(sel.l) > 0 & length(sel.t) > 0){
    t2 <- t[sel.t,]; t2$yr <-     2024    ; t2$lisnom <- NULL ## subset
    t2 <- merge(x=t2, y=ln[sel.l,], by = c("ife","yr"), all.x=TRUE, all.y=FALSE)
    t2$lisnom -> t$lisnom[sel.t] ## return
    t$lisnom
}
## sort
t <- t[order(t$ord),]
## empty fed lisnom in vot
vot$lisnom.fed <- t$lisnom
## San Pedro Mixtepec dis 26 has huge mismatch in fed lisnom, use mun lisnom
sel <- which(ids$ife==20317)
vot$lisnom.fed[sel] <- vot$lisnom[sel]
##
## use fed lisnom to fill in missings
sel <- which(is.na(vot$lisnom)==TRUE & is.na(vot$lisnom.fed)==FALSE)
vot$lisnom[sel] <- vot$lisnom.fed[sel]
## ##
## ## compare
## tmp <- (vot$lisnom.fed - vot$lisnom)*100/ vot$lisnom.fed
## summary(tmp)
## summary(vot$lisnom)
## summary(vot$lisnom.fed)
## sel <- which(tmp < -100)
## sel <- which(tmp > 10 & tmp <20)
## sel <- which(ids$edon==7 & ids$yr==2012)
## cbind(yr=ids$yr[sel], ife=ids$ife[sel], vot[sel, c("emm", "efec","lisnom","lisnom.fed")], tmp=tmp[sel])
## write.csv(tmp2, file="/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/tmp.csv", row.names=FALSE)
## ## tot > lisnom
## sel <- which(vot$efec > .95 * vot$lisnom)
## cbind(yr=ids$yr[sel], ife=ids$ife[sel], vot[sel, c("emm", "efec","lisnom","lisnom.fed")], tmp=tmp[sel])
##
## turnout relative to lisnom
#within(vot, tot <- efec + nr + nul) # recompute tot
## vot <- within(vot, turn.ln <- tot / lisnom)
vot <- within(vot, turn.ln <- efec / lisnom)
summary(vot$turn.ln)
## cases where turnout exceeds 1: will increase lisnom to leave it at .98
sel <- which(ids$yr > 1996 & vot$turn.ln > .98)
vot$lisnom[sel] <- vot$efec[sel] / .98
vot$turn.ln[sel] <- .98
## ## used to debug
## sel <- which(ids$yr > 1996 & vot$turn.ln > .98)
## cbind(yr=ids$yr[sel], ife=ids$ife[sel], vot[sel, c("emm", "efec","lisnom","lisnom.fed")], turn.ln=vot$turn.ln[sel])
## sel <- which(ids$edon==31 & ids$yr==2001)
## write.csv(vot[sel, c("emm", "efec","lisnom","lisnom.fed")], file="/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/data/casillas/tmp.csv", row.names=FALSE)

## when lisnom complete, use it for alternative turnout
table(miss.ln=is.na(vot$lisnom), ids$yr)
table(miss.18=is.na(vot$p18), ids$yr)
table(vot$p18==0, ids$yr)
table(vot$p18<0, ids$yr)
##
## ## p18 still below efec in many cases
## vot$turn.18 <- vot$efec / vot$p18
## summary(vot$turn.18)
## with(vot, table(p18<lisnom))
## with(vot, table(turn.18 > 1))
##

## alternative to interaction
vot <- within(vot, {
    #dinomorena     <- as.numeric(dincmorena==0 & dincballot==0) ## omited
    dimorena       <- as.numeric(dincmorena==1 & dincballot==0)
    diballnomorena <- as.numeric(dincmorena==0 & dincballot==1)
    diballmorena   <- as.numeric(dincmorena==1 & dincballot==1)
    ##
    diprd       <- as.numeric(dincprd==1 & dincballot==0)
    diballnoprd <- as.numeric(dincprd==0 & dincballot==1)
    diballprd   <- as.numeric(dincprd==1 & dincballot==1)
    ##
    #dinopri     <- as.numeric(dincpri==0 & dincballot==0) ## omited
    dipri       <- as.numeric(dincpri==1 & dincballot==0)
    diballnopri <- as.numeric(dincpri==0 & dincballot==1)
    diballpri   <- as.numeric(dincpri==1 & dincballot==1)
    ##
    #dinopan     <- as.numeric(dincpan==0 & dincballot==0) ## omited
    dipan       <- as.numeric(dincpan==1 & dincballot==0)
    diballnopan <- as.numeric(dincpan==0 & dincballot==1)
    diballpan   <- as.numeric(dincpan==1 & dincballot==1)
})



#################################################
## Drop municipios with many missing elections ##
#################################################
sel.r <- which(ids$inegi %in%
             c(20472 # missing 2001-2004-2007 
             , 20130 # missing 2013 2018
               ))
if (length(sel.r)>0){
    vot <- vot[-sel.r,]
    ids <- ids[-sel.r,]
}

## string with all party labels (for lucardi-rosas incumbent model)
vot <- within(vot, {l1 <- l2 <- l3 <- l4 <- l5 <- l6 <- l7 <- l8 <- ""})
sel.r <- which(vot$pan>0);    vot$l1[sel.r] <- "pan"
sel.r <- which(vot$pri>0);    vot$l2[sel.r] <- "pri"
sel.r <- which(vot$prd>0);    vot$l3[sel.r] <- "prd"
sel.r <- which(vot$pvem>0);   vot$l4[sel.r] <- "pvem"
sel.r <- which(vot$pt>0);     vot$l5[sel.r] <- "pt"
sel.r <- which(vot$mc>0);     vot$l6[sel.r] <- "mc"
sel.r <- which(vot$morena>0); vot$l7[sel.r] <- "morena"
sel.r <- which(vot$oth>0);    vot$l8[sel.r] <- "oth"
vot <- within(vot, labs <- paste(l1,l2,l3,l4,l5,l6,l7,l8, sep="-"))
vot <- within(vot, l1 <- l2 <- l3 <- l4 <- l5 <- l6 <- l7 <- l8 <- NULL)

#############################################################
## Prepare different specifications of the DV for analysis ##
#############################################################
colnames(vot)
colnames(vot)[grep("ball", colnames(vot))]
with(vot, table(dincballotpan, diballpan))
ids[1,]
##         ## vot has raw vote shares
res <- vot ## residual v - vhat
vo4 <- vot ## pan pri left oth shares
lnr  <- vo4 ## pan/pri left/pri oth/pri ratios
##

## duplicate vot for pan pri left oth breakdown
vot <- vot[order(vot$ord),]; ids <- ids[order(ids$ord),]
table(vot$emm==ids$emm)
table(vot$ord==ids$ord)
vo4 <- vot
## consolidate left
vo4$left <- NA
vo4$left[ids$yr<2015]                  <- vot$prd[ids$yr<2015]
vo4$prd[ids$yr<2015]                   <- 0
vo4$left[ids$yr>=2015 & ids$yr<2018]   <- vo4$prd[ids$yr>=2015 & ids$yr<2018] + vo4$morena[ids$yr>=2015 & ids$yr<2018]
vo4$prd[ids$yr>=2015 & ids$yr<2018]    <- 0
vo4$morena[ids$yr>=2015 & ids$yr<2018] <- 0
vo4$left[ids$yr>=2018]                 <- vo4$morena[ids$yr>=2018]
vo4$morena[ids$yr>=2018]               <- 0
vo4 <- within(vo4, oth <- 1 - pan - pri - left)
vo4 <- within(vo4, prd <- morena <- pvem <- pt <- mc <- NULL)
vo4 <- vo4[, moveme(colnames(vo4), "left after pri")]
##
vo4$dincballotleft <- NA
vo4$dincballotleft[ids$yr<2015]                  <- vot$dincballotprd[ids$yr<2015]
vo4$dincballotprd[ids$yr<2015]                   <- 0
vo4$dincballotleft[ids$yr>=2015 & ids$yr<2018]   <- vo4$dincballotprd[ids$yr>=2015 & ids$yr<2018] + vo4$dincballotmorena[ids$yr>=2015 & ids$yr<2018]
vo4$dincballotprd[ids$yr>=2015 & ids$yr<2018]    <- 0
vo4$dincballotmorena[ids$yr>=2015 & ids$yr<2018] <- 0
vo4$dincballotleft[ids$yr>=2018]                 <- vo4$dincballotmorena[ids$yr>=2018]
vo4$dincballotmorena[ids$yr>=2018]               <- 0
vo4 <- within(vo4, dincballototh <- dincballototh + dincballotprd + dincballotmorena + dincballotpvem + dincballotpt + dincballotmc)
vo4 <- within(vo4, dincballototh <- as.numeric(dincballototh > 0))
vo4 <- within(vo4, dincballotprd <- dincballotmorena <- dincballotpvem <- dincballotpt <- dincballotmc <- NULL)
vo4 <- vo4[, moveme(colnames(vo4), "dincballotleft after dincballotpri")]
##
vo4$dgovprd <- vo4$dgovprd + vo4$dgovmorena
colnames(vo4)[grep("dgovprd", colnames(vo4))] <- "dgovleft"
vo4 <- within(vo4, dgovoth <- as.numeric((dgovpvem + dgovmc) > 0))
vo4 <- within(vo4, dgovmorena <- dgovpvem <- dgovmc <- NULL)
vo4 <- vo4[, moveme(colnames(vo4), "dgovoth after dgovleft")]
##
colnames(vo4)[grep("dpresmorena", colnames(vo4))] <- "dpresleft"
##
vo4$dincleft <- NA
vo4$dincleft[ids$yr<2015]                  <- vot$dincprd[ids$yr<2015]
vo4$dincprd[ids$yr<2015]                   <- 0
vo4$dincleft[ids$yr>=2015 & ids$yr<2018]   <- vo4$dincprd[ids$yr>=2015 & ids$yr<2018] + vo4$dincmorena[ids$yr>=2015 & ids$yr<2018]
vo4$dincprd[ids$yr>=2015 & ids$yr<2018]    <- 0
vo4$dincmorena[ids$yr>=2015 & ids$yr<2018] <- 0
vo4$dincleft[ids$yr>=2018]                 <- vo4$dincmorena[ids$yr>=2018]
vo4$dincmorena[ids$yr>=2018]               <- 0
vo4 <- within(vo4, dincoth <- dincprd + dincmorena + dincpvem + dincpt + dincmc)
vo4 <- within(vo4, dincoth <- as.numeric(dincoth > 0))
vo4 <- within(vo4, dincprd <- dincmorena <- dincpvem <- dincpt <- dincmc <- NULL)
vo4 <- vo4[, moveme(colnames(vo4), "dincleft after dincpri")]
##
vo4 <- within(vo4, {
    dimorena <- dimorena + diprd
    diballmorena <- diballmorena + diballprd
    diballnomorena <- as.numeric((diballnomorena + diballnoprd) > 0)
})
sel.c <- grep("di(ball)?(no)?morena", colnames(vo4))
tmp <- colnames(vo4)[sel.c]
tmp <- sub("di(ball)?(no)?morena", "di\\1\\2left", tmp)
colnames(vo4)[sel.c] <- tmp
vo4 <- within(vo4, diprd <- diballprd <- diballnoprd <- NULL)
rm(tmp,sel.c)
##
vo4$dcoalleft <- NA
vo4$dcoalleft[ids$yr<2015]                <- vo4$dcoalprd[ids$yr<2015]
vo4$dcoalleft[ids$yr>=2015 & ids$yr<2018] <- vo4$dcoalprd[ids$yr>=2015 & ids$yr<2018] + vo4$dcoalmor[ids$yr>=2015 & ids$yr<2018]
vo4$dcoalleft[ids$yr>=2015 & ids$yr<2018] <- as.numeric(vo4$dcoalleft[ids$yr>=2015 & ids$yr<2018]>0)
vo4$dcoalleft[ids$yr>=2018]               <- vo4$dcoalmor[ids$yr>=2018]
vo4 <- within(vo4, dcoalprd <- dcoalmor <- dcoalpve <- dcoalpt <- dcoalmc <- NULL)
vo4 <- vo4[, moveme(colnames(vo4), "dcoalleft after dcoalpri")]

## zeroes problematic for ratio-to-pri-vote DV...
table(pri.null=vo4$pri==0)
sel <- which(vo4$pri==0)
data.frame(emm=vo4$emm[sel], yr=ids$yr[sel], efec=vo4$efec[sel])
## cases before 2017 pri cand prob out and pri=0 artificial --- make these and other cases pri=.03
vo4$pri[sel] <- .03092784
## make sums = 1
vo4[sel, c("pan","pri","left","oth")] <- vo4[sel, c("pan","pri","left","oth")] / apply(X=vo4[sel, c("pan","pri","left","oth")], 1, sum ) 
## ## Make these cases NAs to drop them from regressions
## vo4[sel, c("pan","pri","left","oth")] <- NA

## Deal with zeroes as Aitchison (p. 268)
## If delta=.00005 is the maximum rounding error and unit u has C zeroes and D-C non-zeroes add
## .00005 (C+1)(D-C) / D^2
## to zero categories and subtract
## .00005 (C+1)C / D^2
## to each non-zero categories.
## In practice, adding something in [delta/5, 2*delta] will do. 
##
## turnout never zero, no need to manipulate
table(vo4$turn.ln > 0 & vo4$turn.ln < 1)
##
v4 <- vo4[, c("pan","pri","left","oth")] # take vote columns for manipulation
v4[v4 < 0.00005] <- 0 ## make votes below .005% zero
tmp <- function(x){
    C <- length(x[x==0])             ## how many zeroes in row
    if (is.na(sum(x))==TRUE) C <- 0  ## excluding NAs
    return(C)
}
C <- apply(v4, 1, tmp)
table(C)
## C = 3 zeroes
## plus  <- .0001
## minus <- .0001 * 3
plus  <- .00005 * 4 * 1 / 16
minus <- .00005 * 4 * 3 / 16
sel.r <- which(C==3)
for (i in 1:nrow(v4[sel.r,])){
    sel.plus  <- which(v4[sel.r,][i,]==0)
    sel.minus <- which(v4[sel.r,][i,]>0)
    v4[sel.r,][i, sel.plus]  <- v4[sel.r,][i, sel.plus]  + plus
    v4[sel.r,][i, sel.minus] <- v4[sel.r,][i, sel.minus] - minus
}
## C = 2
## plus  <- .0001
## minus <- .0001
plus  <- .00005 * 3 * 2 / 16
minus <- .00005 * 3 * 2 / 16
sel.r <- which(C==2)
for (i in 1:nrow(v4[sel.r,])){
    sel.plus  <- which(v4[sel.r,][i,]==0)
    sel.minus <- which(v4[sel.r,][i,]>0)
    v4[sel.r,][i, sel.plus]  <- v4[sel.r,][i, sel.plus]  + plus
    v4[sel.r,][i, sel.minus] <- v4[sel.r,][i, sel.minus] - minus
}
## C = 1
## plus  <- .0001 * 3
## minus <- .0001
plus  <- .00005 * 2 * 3 / 16
minus <- .00005 * 2 * 1 / 16
sel.r <- which(C==1)
for (i in 1:nrow(v4[sel.r,])){
    sel.plus  <- which(v4[sel.r,][i,]==0)
    sel.minus <- which(v4[sel.r,][i,]>0)
    v4[sel.r,][i, sel.plus]  <- v4[sel.r,][i, sel.plus]  + plus
    v4[sel.r,][i, sel.minus] <- v4[sel.r,][i, sel.minus] - minus
}
## check
v4[v4 <=0] ## must be empty
rm(i,plus,minus,sel.r,sel.minus, sel.plus)
##
## generate log ratios (pri is denom)
lnr <- vo4
lnr[,c("pan","left","oth")] <- log(v4[,-2] / v4$pri)
lnr$pri <- NULL
lnr$turn.ln <- log(lnr$turn.ln / (1 - lnr$turn.ln))
## ## check
## plot(lnr$turn.ln[ids$yr > 1996])
## plot(lnr$pan [ids$yr > 1996])
## plot(lnr$left[ids$yr > 1996])
## plot(lnr$oth [ids$yr > 1996])
## ## debug
## sel.r <- which(lnr$left > 2000)
## ids$emm[sel.r]
## vot[sel.r,]
##
## ## re-specify vhats as ratio while keeping names
## lnr <- within(lnr, {
##     vhat.oth  <- (1 - vhat.pan - vhat.pri - vhat.left) / vhat.pri
## })
## lnr <- within(lnr, {
##     vhat.left <- vhat.left / vhat.pri
##     vhat.pan  <- vhat.pan  / vhat.pri
## })
## lnr <- within(lnr, vhat.pri <- NULL)
rm(C, tmp, v4)

## residual DVs
res <- vo4
res <- within(res, {
    pan    <- pan  - vhat.pan
    pri    <- pri  - vhat.pri
    left   <- left - vhat.left
    oth    <- oth  - (1 - vhat.pan - vhat.pri - vhat.left)
    ## turn.ln <- turn.ln - vhat.turn
})

## USE ids$cycle TO SIMPLIFY LAGGING AND DELTAS INSTEAD OF COMMENTED FUNCTION
## ##
## ## function to simplify lagging and deltas
## inegi.cycle.fr.emm <- function(emm){
##     library(plyr)
##     #emm <- vot$emm[1:1000] # debug
##     tmp <- data.frame(emm=emm)
##     tmp$edo <- sub("^([a-z]{2,3})[-][0-9]{2}[.][0-9]{3}$", "\\1", emm)
##     tmp$edon <- mapvalues(
##         x=tmp$edo,
##         from = c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic",
##                  "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac"),
##         to = 1:32)
##     tmp$inegi <- sub("^[a-z]+[-][0-9]+[.]([0-9]{3})$", "\\1", emm)
##     tmp$inegi <- as.numeric(tmp$edon)*1000 + as.numeric(tmp$inegi)
##     tmp$cycle <- sub("^[a-z]+[-]([0-9]+)[.][0-9]{3}", "\\1", emm)
##     tmp$cycle <- as.numeric(tmp$cycle)
##     return(tmp[, c("inegi","cycle")])
##     #head(tmp)
## }


#################################
## lag variables by one period ##
#################################
ids <- ids[order(ids$emm),] # sort mun-chrono
vot <- vot[order(vot$emm),]
vo4 <- vo4[order(vo4$emm),]
lnr <- lnr[order(lnr$emm),]
## check
table(ids$emm == vot$emm)
table(ids$emm == vo4$emm)
table(ids$emm == lnr $emm)



#########################################################
## xsts lag all cols except emm (emm must be column 1) ##
#########################################################
lagall <- function(dat=NA, slideBy=-1){
    ##dat <- vot; slideBy=-1 # debug
    cols  <- colnames(dat)[-1:-2] ## get colnames except emm ord
    cols2 <- paste0(cols, "2") ## rename cols except emm
    colnames(dat)[-1:-2] <- cols2
    cols2 -> colnames(dat)[-1:-2]
    dat <- cbind(dat, ids[, c("cycle", "inegi")]) ## add cycle and inegi from ids
    ##dat <- cbind(dat, inegi.cycle.fr.emm(dat$emm)) ## add cycle and inegi from emm
    dat <- dat[order(dat$ord),]       ## sort prior to sliding
    for (i in 1:length(cols2)){
        dat <- slide(dat, Var = cols2[i], NewVar = cols[i], TimeVar = "cycle", GroupVar = "inegi", slideBy = slideBy)
    }
    drop.c <- which(colnames(dat) %in% cols2)
    dat <- dat[, -drop.c] # drop columns with "2" variables
    dat$inegi <- dat$cycle <- NULL # remove added inegi and cycle
    dat <- dat[order(dat$emm),]    # sort
    return(dat)
}
##
votlag <- lagall(vot)
vo4lag <- lagall(vo4)
lnrlag  <- lagall(lnr)
reslag <- lagall(res)
##
sel <- which(is.na(votlag$pan)==TRUE)
table(ids$status[sel], ids$yr[sel], useNA = "ifany")

## ## con missing period, usar lag-2nd dif para no perder dos obs
## sel.r <- which(ids$emm %in% c("cps-19.034", # in 2024
##                               "cps-17.058",
##                               "cps-19.125", # in 2024
##                               "oax-10.005",
##                               "oax-10.025",
##                               "oax-10.066",
##                               "oax-10.075",
##                               "oax-10.089",
##                               "oax-10.141",
##                               "oax-10.143",
##                               "oax-10.171",
##                               "oax-10.232",
##                               "oax-10.327",
##                               "oax-10.339",
##                               "oax-10.377",
##                               "oax-10.441",
##                               "oax-10.505",
##                               "oax-10.557",
##                               "oax-13.007",
##                               "oax-13.053",
##                               "oax-16.130",
##                               "pue-17.175",
##                               "ver-16.064",
##                               "zac-10.039"))
## ## second difs
## tmp1 <- lagall(vot, slideBy=-2)
## tmp2 <- lagall(vo4, slideBy=-2)
## tmp3 <- lagall(lnr , slideBy=-2)
## tmp4 <- lagall(res, slideBy=-2)
## ##
## votlag[sel.r,] <- tmp1[sel.r,]
## vo4lag[sel.r,] <- tmp2[sel.r,]
## lnrlag [sel.r,] <- tmp3[sel.r,]
## reslag[sel.r,] <- tmp4[sel.r,]

## duplicate vot for lucardi-rosas selection criteria
sel.c <- which(colnames(vot) %in% c("ord", "emm", "win", "part2nd", "mg", "win.prior", "run.prior", "mg.prior", "dincballot", "dincballotpan", "dincballotpri", "dincballotprd", "dincballotmorena"))
luro <- vot[, sel.c]
luro <- luro[order(luro$ord),]; ids <- ids[order(ids$ord),]; votlag <- votlag[order(votlag$ord),]
luro$labs.prior <- votlag$labs ## add prior cycle's party labels for l+r incumbent model
dpostref <- as.numeric(ids$yr >= ids$yr1st)
luro$yr <- ids$yr
luro$dpostref <- dpostref
vot    <- within(vot   , win.prior <- run.prior <- mg.prior <- NULL)
votlag <- within(votlag, win.prior <- run.prior <- mg.prior <- NULL)
vo4    <- within(vo4   , win.prior <- run.prior <- mg.prior <- NULL)
vo4lag <- within(vo4lag, win.prior <- run.prior <- mg.prior <- NULL)
lnr     <- within(lnr   , win.prior <- run.prior <- mg.prior <- NULL)
lnrlag  <- within(lnrlag, win.prior <- run.prior <- mg.prior <- NULL)
res    <- within(res   , win.prior <- run.prior <- mg.prior <- NULL)
reslag <- within(reslag, win.prior <- run.prior <- mg.prior <- NULL)
rm(alt,elhis,ife2inegi,ife2mun,inegi2ife,inegi2mun,sel,sel.c,yr,dpostref) ## clean

## get 2020 census indicators
## generate pob in localidades < 10k hab
my_fun <- function(path){
    tmp <- read.csv(path)
    tmp <- tmp[, c("ENTIDAD","MUN","NOM_MUN","LOC","NOM_LOC","POBTOT")]
    colnames(tmp) <- c("edon","inegi","municipio","loc","localidad","ptot")
    tmp <- tmp[(tmp$inegi>0 & tmp$loc>0),]
    tmp$drural <- as.numeric(tmp$ptot < 10000)  ## poblados with less 10k considered rural
    tmp$inegi <- tmp$edon*1000 + tmp$inegi
    tmp$prural <- tmp$ptot * tmp$drural
    tmp$prural <- ave(tmp$prural, as.factor(tmp$inegi), FUN=function(x) sum(x, na.rm=TRUE))
    tmp$ptot   <- ave(tmp$ptot,   as.factor(tmp$inegi), FUN=function(x) sum(x, na.rm=TRUE))
    tmp <- tmp[duplicated(tmp$inegi)==FALSE,]
    tmp <- within(tmp, {
        ruralsh <- round(prural / ptot, 3);
        edon <- loc <- localidad <- drural <- NULL;
    })
    return(tmp)
}
##
rur2020 <- data.frame()
for (i in 1:9){
    path <- paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/2020censo/localidades/poblacion/ITER_0", i, "CSV20.csv")
    tmp2 <- my_fun(path)
    rur2020 <- rbind(rur2020, tmp2)
}
for (i in 10:32){
    path <- paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/2020censo/localidades/poblacion/ITER_", i, "CSV20.csv")
    tmp2 <- my_fun(path)
    rur2020 <- rbind(rur2020, tmp2)
}
##head(rur2020)
##
## sum san quintin to ensenada (san quintinn won't elect ayuntamiento until 2024)
sel.r <- which(rur2020$inegi %in% c(2001, 2006))
rur2020$ruralsh[sel.r] <- round(sum(rur2020$prural[sel.r]) / sum(rur2020$ptot[sel.r]), 3)
##
## paste ruralsh in ids (not a time-varying quantity)
ids <- merge(x = ids, y = rur2020[,c("inegi","municipio","ruralsh")], by = "inegi", all.x = TRUE, all.y = FALSE)
rm(sel.r, ruralsh)
##
## add p5li
p5li <- read.csv("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/data/censo2020-mu.csv")
p5li$p5lish <- round(p5li$P5_HLI / p5li$P_5YMAS, 3)
ids2 <- merge(x = ids, y = p5li[,c("inegi","p5lish")], by = "inegi", all.x = TRUE, all.y = FALSE)
ids2 <- ids2[-which(duplicated(ids2$ord)),] ## quick fix to a bug: some obs are duplicated by merge ???
ids <- ids2
rm(p5li,rur2020,ids2)
rm(i,ln,my_fun,p18,path,sel.l,sel.t,t,t2,tmp2)

##
## sort
ids    <-    ids[order (   ids$ord) ,]
votlag <- votlag[order (votlag$ord) ,]
vo4lag <- vo4lag[order (vo4lag$ord) ,]
lnrlag <- lnrlag[order (lnrlag$ord) ,]
reslag <- reslag[order (reslag$ord) ,]
vot    <-    vot[order (   vot$ord) ,]
vo4    <-    vo4[order (   vo4$ord) ,]
lnr    <-    lnr[order (   lnr$ord) ,]
res    <-    res[order (   res$ord) ,]


###################
## Drop pre-1997 ##
###################
sel.r <- which(ids$yr < 1997)
ids <- ids[-sel.r,]
vot <- vot[-sel.r,]
vo4 <- vo4[-sel.r,]
lnr  <- lnr [-sel.r,]
res <- res[-sel.r,]
votlag <- votlag[-sel.r,]
vo4lag <- vo4lag[-sel.r,]
lnrlag  <- lnrlag [-sel.r,]
reslag <- reslag[-sel.r,]

## ###########################################################################
## ## check that NAs in current are all appointed municipios, and drop them ##
## ###########################################################################
## sel.r <- which(is.na(vot$pan)==TRUE)# & ids$yr > 1996)
## table(ids$status[sel.r], useNA = "ifany")
## table(ids$yr[sel.r], useNA = "ifany")
## data.frame(emm=ids$emm[sel.r], yr=ids$yr[sel.r])
## ids    <- ids   [-sel.r,]
## vot    <- vot   [-sel.r,]
## vo4    <- vo4   [-sel.r,]
## lnr     <- lnr    [-sel.r,]
## res    <- res   [-sel.r,]
## votlag <- votlag[-sel.r,]
## vo4lag <- vo4lag[-sel.r,]
## lnrlag  <- lnrlag [-sel.r,]
## reslag <- reslag[-sel.r,]
## rm(sel.r)
## #####################################################
## ## Drop 1st obs here, missing previous 3 so no lag ##
## #####################################################
## sel.r <- which(ids$emm=="oax-10.232")
## ids    <- ids   [-sel.r,]
## vot    <- vot   [-sel.r,]
## vo4    <- vo4   [-sel.r,]
## lnr     <- lnr    [-sel.r,]
## res    <- res   [-sel.r,]
## votlag <- votlag[-sel.r,]
## vo4lag <- vo4lag[-sel.r,]
## lnrlag  <- lnrlag [-sel.r,]
## reslag <- reslag[-sel.r,]


######################################################################################################################################
## ################################################################################################################################ ##
## ## OJO 31jul2024: esto tira indebidamente casos de sonora 1997... creo que podría deberse a un drop<1997 antes hacer los lags ## ##
## ################################################################################################################################ ##
######################################################################################################################################

## missing periods generan NAs en la serie del municipio donde ocurren
summary(lnr   $pan)
summary(lnrlag$pan)
## check that NAs in lags are all new municipios
sel.r <- which(is.na(lnrlag$pan)==TRUE)
table(ids$status[sel.r], useNA = "ifany")
## drop them
ids    <- ids    [-sel.r,]
vot    <- vot    [-sel.r,]
vo4    <- vo4    [-sel.r,]
lnr    <- lnr    [-sel.r,]
res    <- res    [-sel.r,]
votlag <- votlag [-sel.r,]
vo4lag <- vo4lag [-sel.r,]
lnrlag <- lnrlag [-sel.r,]
reslag <- reslag [-sel.r,]

################################
## deltas for cross-temp regs ##
################################
table(colnames(vot)==colnames(votlag))
table(colnames(vo4)==colnames(vo4lag))
table(colnames (lnr)==colnames (lnrlag))
table(colnames(res)==colnames(reslag))
## check order
table(votlag$emm == vot$emm)
table(vo4lag$emm == vo4$emm)
table(lnrlag $emm == lnr $emm)
table(reslag$emm == res$emm)
##
##data.frame(vot=colnames(vot), lag=colnames(votlag))
deltas <- function(dat=NA, datlag=NA){
    ## sort all
    dat    <- dat   [order(dat   $ord),   ]
    datlag <- datlag[order(datlag$ord),]
    emm <- dat[, c("emm","ord")] ## save emm and ord to re-paste later
    ## keep numeric cols only
    dat    <- dat   [, sapply(dat, class)    %in% c('numeric', 'integer')]
    datlag <- datlag[, sapply(datlag, class) %in% c('numeric', 'integer')]
    ##  subtract
    datdelta <- dat - datlag
    ## add emm again
    datdelta <- cbind(emm=emm$emm, datdelta)
    return(datdelta)
    ## sel.c <- setdiff(2:ncol(vot), grep("win|race.prior|govpty|winlast", colnames(vot))) ## ignore non-numeric vars in 1st diff
    ## delta <- vot
    ## for (i in sel.c){
    ##     delta[, i] <- vot[, i] - votlag[, i]
    ## }
    ## rm(i,sel.c,inegi.cycle.fr.emm)
}
##
votdelta <- deltas(dat=vot, datlag=votlag)
vo4delta <- deltas(dat=vo4, datlag=vo4lag)
lnrdelta  <- deltas(dat= lnr, datlag= lnrlag)
resdelta <- deltas(dat=res, datlag=reslag)
##
table(votdelta$dincpan, useNA = "always")
table(votdelta$dincpri, useNA = "always")
table(votdelta$dincprd, useNA = "always")
table(lnrdelta$dincleft, useNA = "always")
##
table(vot$emm == votlag$emm)
table(vo4$emm == vo4lag$emm)
table( lnr$emm ==  lnrlag$emm)
table(res$emm == reslag$emm)
table(vot$emm ==    ids$emm)
##
rm(deltas)

## Save data
save.image(file = "ay-mu-vote-analysis.RData")


######################
## read saved image ##
######################
#source("/home/eric/Desktop/MXelsCalendGovt/elecReturns/code/ay.r") ## slow!!
## check two warnings
## Warning messages:
## 1: In eval(ei, envir) : NAs introduced by coercion
## 2: In `[<-.data.frame`(`*tmp*`, sel7, , value = list(V1 = c("prd",  :
##   provided 9 variables to replace 7 variables

library(DataCombine) # easy lags
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
x

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
    dv2 <- sub("^[Dl]o?g?[(]?([a-z.]+)[)]?", "\\1", dv) ## drop log if any
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

## models
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
############################
## error correction model ##
############################
colnames(lnrecm)
tmpp <- c("dconcgo", "dconcdf", "dincballot", "mg", "as.factor(trienio)"); tmp <- mylm(dv="Dturn.ln", data=lnrecm, subset="yr>1999", predictors = tmpp); summary(tmp)

## replicate lucardi rosas
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

##
tmp <- luro[luro$yr>=2021,]
nrow(tmp)
table(tmp$dincballot) / nrow(tmp)


tmp <- luro[luro$dselleft==1 & luro$yr>=2018,] 
nrow(tmp)
table(incballot=tmp$dincballot, tmp$dincballotmorena) / nrow(tmp)
x


###############
## ######### ##
## ## PAN ## ##
## ######### ##
###############
##
ls()
tmp <- luro[luro$dselpan==1,] # subset
##########################
## restrict time period ##
##########################
sel.r <- which(tmp$yr > 1997 & tmp$yr < 2018)
tmp <- tmp[sel.r,]
dim(tmp)
## generalize party-specific variables
tmp$dwin <- tmp$dpanwin
tmp$mg   <- tmp$mgpan ## magpan is lagged +/- mg conditional of incumbency status
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
rdpan <- lm(dwin ~ dneg + dnegxincball + dnegxmg + dnegxmgxincball + dpos + dposxincball + dposxmg + dposxmgxincball - 1,
           data = tmp) ## controlando reforma
summary.lm(rdpan)
##
## plot
##png("../plots/pan-luro97-23-lpm.png")
##pdf("../plots/pan-luro97-23-lpm.pdf")
plot(x = c(-.1,.1), y = c(0,1), type = "n", main = "PAN \n LPM 1997-2023", xlab = expression("Margin"[t]), ylab = expression("Pr(win)"[t+1]))
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
#
tmp.bak <- fit1jags
tmp.bak -> fit1jags
fit1jags <- update(fit1jags, 10000) # continue updating to produce 10000 new draws per chain
traceplot(fit1jags) # visually check posterior parameter convergence
#
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

 




