# This is R code written by H.J. Hummel to calculate the full role census
# It was originally retrieved from: "https://www.uni-due.de/hummell/rolecensus/f.rc.r".

# f.rc.r

f.rc <- function(mat=NULL,REGE=F,HUSO=F,MES=F, CHECK=T)
{
# Calculates a rolecensus/node-level triadcensus according to H.J. Hummell/W. Sodeur (1987)
# and R. Burt (1990) in the sequence of Burt; cf.
# http://www.uni-duisburg-essen.de/hummell/rolecensus/
# Graphical description of 36 "triadic positional types" of rolecensus
# in the notation von R. Burt:
# http://www.uni-duisburg-essen.de/hummell/pdf/RoleCensus.pdf
#
# Input: Binary quadrat. matrix with diagonal entries to 0
# <REGE=T> transforms rolecensus into binary values (checks only for
# presence/absence of rolecensus types a la REGE)
# Author: H.J. Hummell
# 30.06.2013 14:51:12

TEXT0 <- 'Calculates a triad census on the level of nodes\n'
TEXT1 <- 'according to: H.J.Hummell & W.Sodeur(1987) "Positionenzensus" and Ronald Burt(1990) "Rolecensus"\n'
TEXT2 <- 'Info: http://www.uni-duisburg-essen.de/hummell/rolecensus/ \n'
TEXT3 <- 'Ordering of the 36 different "triadic positions" according to R.Burt\n'
TEXT4 <- 'Info: http://www.uni-duisburg-essen.de/hummell/pdf/RoleCensus.pdf\n'
TEXT5 <- 'Parameters: <mat=NULL>, <REGE=F>, <HUSO=F>, <MES=F>, <CHECK=T>\n'
TEXT6 <- '<REGE=T> transforms rolecensus into binary values (checks only for presence/absence of role types a la REGE)\n'
TEXT7 <- '<HUSO=T> gives Hummell/Sodeurs original sequence of "triadic positions"\n'
TEXT8 <- 'Info: http://www.uni-duisburg-essen.de/hummell/rolecensus/PositionenZensus.jpg\n'
TEXT9 <- '<MES=T> gives the sequence according to Solomon Messing from the <triads> package\n'
TEXT10 <- 'Rather slow routine!\n'
AUT <- 'H.J.Hummell; 30.06.2013.\n'

if (length(mat)==0)
{
cat(TEXT0)+
cat(TEXT1)+
cat(TEXT2)+
cat(TEXT3)+
cat(TEXT4)+
cat(TEXT5)+
cat(TEXT6)+
cat(TEXT7)+
cat(TEXT8)+
cat(TEXT9)+
cat(TEXT10)+
cat(AUT)
return(cat('\n\n'))
}

if (HUSO==TRUE & MES==TRUE) {return(cat("<HUSO> and <MES> cannot both be true!\n\n\a"))}

# CHECK Soziomatrix
if (CHECK==TRUE)
{
if (!is.matrix(mat)) {return(cat("Not a matrix!\n\n\a"))}
g1 <- dim(mat)[1]
g2 <- dim(mat)[2]
if ( g1 != g2 ) {return(cat("Matrix is not quadratic!\n\n\a"))}
l0 <- length(which(mat==0))
l1 <- length(which(mat==1))
l <- l0+l1
if(l != g1*g2) {return(cat("Not a binary matrix!\n\n\a"))}
D <- sum(abs(diag(mat)))
if (D != 0) {return(cat("Diagonal values are not zero!\n\n\a"))}
}
# END CHECK


x <- mat
g <- dim(x)[1]
z <- matrix(0,g,36)
for (i in(1:g))
{
for (j in(1:g))
{
for (k in(1:g))
{
#################################################################################################
#
# Mapping of 64 (labeled) triads to 36 different "triadic postions" of triad-members i,j,k
# according to H.J.Hummell & W. Sodeur, 1987, p. 186 (Abbildung 3)
# http://www.uni-duisburg-essen.de/hummell/rolecensus/Triads2TriadicPositions.pdf
# For numbering of 36 "triadic positions" see Hummell/Sodeur, 1987, p. 188 (Abbildung 5)
# http://www.uni-duisburg-essen.de/hummell/rolecensus/PositionenZensus.jpg
#
if ((i!=j) & (i!=k) & (j!=k))
{
TRIAD <- (x[i,j] + 2*x[i,k] +  4*x[j,i] +  8*x[j,k] + 16*x[k,i] + 32*x[k,j] + 1)
if (TRIAD < 1 | TRIAD > 64) {return(cat('Error in calculating TRIAD numbers!\n\n\a'))}
################## triads no. 1 to 16
if (TRIAD ==  1) {z[i, 1]<-z[i, 1]+1;z[j, 1]<-z[j, 1]+1;z[k, 1]<-z[k, 1]+1;next() }
if (TRIAD ==  2) {z[i, 2]<-z[i, 2]+1;z[j, 4]<-z[j, 4]+1;z[k, 8]<-z[k, 8]+1;next() }
if (TRIAD ==  3) {z[i, 2]<-z[i, 2]+1;z[j, 8]<-z[j, 8]+1;z[k, 4]<-z[k, 4]+1;next() }
if (TRIAD ==  4) {z[i, 3]<-z[i, 3]+1;z[j,12]<-z[j,12]+1;z[k,12]<-z[k,12]+1;next() }

if (TRIAD ==  5) {z[i, 4]<-z[i, 4]+1;z[j, 2]<-z[j, 2]+1;z[k, 8]<-z[k, 8]+1;next() }
if (TRIAD ==  6) {z[i, 5]<-z[i, 5]+1;z[j, 5]<-z[j, 5]+1;z[k,27]<-z[k,27]+1;next() }
if (TRIAD ==  7) {z[i, 6]<-z[i, 6]+1;z[j, 9]<-z[j, 9]+1;z[k,19]<-z[k,19]+1;next() }
if (TRIAD ==  8) {z[i, 7]<-z[i, 7]+1;z[j,13]<-z[j,13]+1;z[k,30]<-z[k,30]+1;next() }

if (TRIAD ==  9) {z[i, 8]<-z[i, 8]+1;z[j, 2]<-z[j, 2]+1;z[k, 4]<-z[k, 4]+1;next() }
if (TRIAD == 10) {z[i, 9]<-z[i, 9]+1;z[j, 6]<-z[j, 6]+1;z[k,19]<-z[k,19]+1;next() }
if (TRIAD == 11) {z[i,10]<-z[i,10]+1;z[j,10]<-z[j,10]+1;z[k,16]<-z[k,16]+1;next() }
if (TRIAD == 12) {z[i,11]<-z[i,11]+1;z[j,14]<-z[j,14]+1;z[k,23]<-z[k,23]+1;next() }

if (TRIAD == 13) {z[i,12]<-z[i,12]+1;z[j, 3]<-z[j, 3]+1;z[k,12]<-z[k,12]+1;next() }
if (TRIAD == 14) {z[i,13]<-z[i,13]+1;z[j, 7]<-z[j, 7]+1;z[k,30]<-z[k,30]+1;next() }
if (TRIAD == 15) {z[i,14]<-z[i,14]+1;z[j,11]<-z[j,11]+1;z[k,23]<-z[k,23]+1;next() }
if (TRIAD == 16) {z[i,15]<-z[i,15]+1;z[j,15]<-z[j,15]+1;z[k,34]<-z[k,34]+1;next() }
################# triads no. 17 to 32
if (TRIAD == 17) {z[i, 4]<-z[i, 4]+1;z[j, 8]<-z[j, 8]+1;z[k, 2]<-z[k, 2]+1;next() }
if (TRIAD == 18) {z[i, 6]<-z[i, 6]+1;z[j,19]<-z[j,19]+1;z[k, 9]<-z[k, 9]+1;next() }
if (TRIAD == 19) {z[i, 5]<-z[i, 5]+1;z[j,27]<-z[j,27]+1;z[k, 5]<-z[k, 5]+1;next() }
if (TRIAD == 20) {z[i, 7]<-z[i, 7]+1;z[j,30]<-z[j,30]+1;z[k,13]<-z[k,13]+1;next() }

if (TRIAD == 21) {z[i,16]<-z[i,16]+1;z[j,10]<-z[j,10]+1;z[k,10]<-z[k,10]+1;next() }
if (TRIAD == 22) {z[i,17]<-z[i,17]+1;z[j,21]<-z[j,21]+1;z[k,28]<-z[k,28]+1;next() }
if (TRIAD == 23) {z[i,17]<-z[i,17]+1;z[j,28]<-z[j,28]+1;z[k,21]<-z[k,21]+1;next() }
if (TRIAD == 24) {z[i,18]<-z[i,18]+1;z[j,31]<-z[j,31]+1;z[k,31]<-z[k,31]+1;next() }

if (TRIAD == 25) {z[i,19]<-z[i,19]+1;z[j, 9]<-z[j, 9]+1;z[k, 6]<-z[k, 6]+1;next() }
if (TRIAD == 26) {z[i,20]<-z[i,20]+1;z[j,20]<-z[j,20]+1;z[k,20]<-z[k,20]+1;next() }
if (TRIAD == 27) {z[i,21]<-z[i,21]+1;z[j,28]<-z[j,28]+1;z[k,17]<-z[k,17]+1;next() }
if (TRIAD == 28) {z[i,22]<-z[i,22]+1;z[j,32]<-z[j,32]+1;z[k,24]<-z[k,24]+1;next() }

if (TRIAD == 29) {z[i,23]<-z[i,23]+1;z[j,11]<-z[j,11]+1;z[k,14]<-z[k,14]+1;next() }
if (TRIAD == 30) {z[i,24]<-z[i,24]+1;z[j,22]<-z[j,22]+1;z[k,32]<-z[k,32]+1;next() }
if (TRIAD == 31) {z[i,25]<-z[i,25]+1;z[j,29]<-z[j,29]+1;z[k,25]<-z[k,25]+1;next() }
if (TRIAD == 32) {z[i,26]<-z[i,26]+1;z[j,33]<-z[j,33]+1;z[k,35]<-z[k,35]+1;next() }
################# triads no. 33 to 48
if (TRIAD == 33) {z[i, 8]<-z[i, 8]+1;z[j, 4]<-z[j, 4]+1;z[k, 2]<-z[k, 2]+1;next() }
if (TRIAD == 34) {z[i,10]<-z[i,10]+1;z[j,16]<-z[j,16]+1;z[k,10]<-z[k,10]+1;next() }
if (TRIAD == 35) {z[i, 9]<-z[i, 9]+1;z[j,19]<-z[j,19]+1;z[k, 6]<-z[k, 6]+1;next() }
if (TRIAD == 36) {z[i,11]<-z[i,11]+1;z[j,23]<-z[j,23]+1;z[k,14]<-z[k,14]+1;next() }

if (TRIAD == 37) {z[i,19]<-z[i,19]+1;z[j, 6]<-z[j, 6]+1;z[k, 9]<-z[k, 9]+1;next() }
if (TRIAD == 38) {z[i,21]<-z[i,21]+1;z[j,17]<-z[j,17]+1;z[k,28]<-z[k,28]+1;next() }
if (TRIAD == 39) {z[i,20]<-z[i,20]+1;z[j,20]<-z[j,20]+1;z[k,20]<-z[k,20]+1;next() }
if (TRIAD == 40) {z[i,22]<-z[i,22]+1;z[j,24]<-z[j,24]+1;z[k,32]<-z[k,32]+1;next() }

if (TRIAD == 41) {z[i,27]<-z[i,27]+1;z[j, 5]<-z[j, 5]+1;z[k, 5]<-z[k, 5]+1;next() }
if (TRIAD == 42) {z[i,28]<-z[i,28]+1;z[j,17]<-z[j,17]+1;z[k,21]<-z[k,21]+1;next() }
if (TRIAD == 43) {z[i,28]<-z[i,28]+1;z[j,21]<-z[j,21]+1;z[k,17]<-z[k,17]+1;next() }
if (TRIAD == 44) {z[i,29]<-z[i,29]+1;z[j,25]<-z[j,25]+1;z[k,25]<-z[k,25]+1;next() }

if (TRIAD == 45) {z[i,30]<-z[i,30]+1;z[j, 7]<-z[j, 7]+1;z[k,13]<-z[k,13]+1;next() }
if (TRIAD == 46) {z[i,31]<-z[i,31]+1;z[j,18]<-z[j,18]+1;z[k,31]<-z[k,31]+1;next() }
if (TRIAD == 47) {z[i,32]<-z[i,32]+1;z[j,22]<-z[j,22]+1;z[k,24]<-z[k,24]+1;next() }
if (TRIAD == 48) {z[i,33]<-z[i,33]+1;z[j,26]<-z[j,26]+1;z[k,35]<-z[k,35]+1;next() }
################# triads no. 49 to 64
if (TRIAD == 49) {z[i,12]<-z[i,12]+1;z[j,12]<-z[j,12]+1;z[k, 3]<-z[k, 3]+1;next() }
if (TRIAD == 50) {z[i,14]<-z[i,14]+1;z[j,23]<-z[j,23]+1;z[k,11]<-z[k,11]+1;next() }
if (TRIAD == 51) {z[i,13]<-z[i,13]+1;z[j,30]<-z[j,30]+1;z[k, 7]<-z[k, 7]+1;next() }
if (TRIAD == 52) {z[i,15]<-z[i,15]+1;z[j,34]<-z[j,34]+1;z[k,15]<-z[k,15]+1;next() }

if (TRIAD == 53) {z[i,23]<-z[i,23]+1;z[j,14]<-z[j,14]+1;z[k,11]<-z[k,11]+1;next() }
if (TRIAD == 54) {z[i,25]<-z[i,25]+1;z[j,25]<-z[j,25]+1;z[k,29]<-z[k,29]+1;next() }
if (TRIAD == 55) {z[i,24]<-z[i,24]+1;z[j,32]<-z[j,32]+1;z[k,22]<-z[k,22]+1;next() }
if (TRIAD == 56) {z[i,26]<-z[i,26]+1;z[j,35]<-z[j,35]+1;z[k,33]<-z[k,33]+1;next() }

if (TRIAD == 57) {z[i,30]<-z[i,30]+1;z[j,13]<-z[j,13]+1;z[k, 7]<-z[k, 7]+1;next() }
if (TRIAD == 58) {z[i,32]<-z[i,32]+1;z[j,24]<-z[j,24]+1;z[k,22]<-z[k,22]+1;next() }
if (TRIAD == 59) {z[i,31]<-z[i,31]+1;z[j,31]<-z[j,31]+1;z[k,18]<-z[k,18]+1;next() }
if (TRIAD == 60) {z[i,33]<-z[i,33]+1;z[j,35]<-z[j,35]+1;z[k,26]<-z[k,26]+1;next() }

if (TRIAD == 61) {z[i,34]<-z[i,34]+1;z[j,15]<-z[j,15]+1;z[k,15]<-z[k,15]+1;next() }
if (TRIAD == 62) {z[i,35]<-z[i,35]+1;z[j,26]<-z[j,26]+1;z[k,33]<-z[k,33]+1;next() }
if (TRIAD == 63) {z[i,35]<-z[i,35]+1;z[j,33]<-z[j,33]+1;z[k,26]<-z[k,26]+1;next() }
if (TRIAD == 64) {z[i,36]<-z[i,36]+1;z[j,36]<-z[j,36]+1;z[k,36]<-z[k,36]+1;next() }
}
}
}
}

# All "triadic positions" are counted 6 times
z <- z/6

if (HUSO==F)
{
# Reordering of 36 "triadic positions" in the sequence of Hummell/Sodeur
# into the sequence of R. Burt, 1990
# http://www.uni-duisburg-essen.de/hummell/pdf/RoleCensus.pdf
v1 <- c(1, 2, 3, 4, 6, 8, 9,21,22,31,23,24)
v2 <- c(26,28,29, 5,10, 7,32,34,33,35,25,30)
v3 <- c(36,27,11,12,13,14,16,18,19,15,20,17)
v<- cbind(v1,v2,v3)
zz <- matrix(0,g,36)
for (i in (1:36))
{
zz[,v[i]] <- z[,i]
}
#

z <- zz}

if(MES==TRUE)
{
v1 <- c( 1, 2, 7, 3,10, 5,25,12,18,15, 6,14)
v2 <- c(26,19,28,24,36,31,33,34, 4,11,20, 8)
v3 <- c(22,17,35,21,29,32, 9,13,16,23,30,27)
v<- cbind(v1,v2,v3)
zz <- matrix(0,g,36)
for (i in (1:36))
{
zz[,v[i]] <- z[,i]
}
#
z <- zz
}

if (REGE==TRUE)
{
z[z>0] <- 1
}

return(z)
}
###


