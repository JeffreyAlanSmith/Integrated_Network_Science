concor <- function(mat, conv = 1, depth){
   # Arguments:
   # mat: input matrix
   # depth: level in which network should be partitioned into
   # equivalent sets
   
   xc <- cor(mat)
   zcoors <- which(colSums(mat) == 0)
      
 
  allcases <- 1:nrow(xc)
  tpart <- matrix(1, nrow(xc),depth + 1)

  tpart[zcoors, 1] <- 0
  
  d <- 0
  while(d < depth){ # depth loop
  newpart <- matrix(NA, nrow(xc), 1)
   d <- d + 1
   b <- 0 
   grps <- unique(tpart[!(allcases %in% zcoors), d])

   ngrps <- length(grps) # number of groups to be split up */
   
   
   while (b < ngrps) { # bredth loop: number of parts at this depth,
                      #   because it's a binary tree, this will always be 2^d
     b <- b + 1
      bi <- which(tpart[, d] == grps[b]) # extract the relevant part of the orig. correlation matrix 
      xc_bd <- xc[bi, bi]
      
      pv_bd <- coritter(xc_bd, conv); # new partition vector, for the split on gpr(db) 
      
      #rename the values to make them unique across partitions
       maxpv <- max(tpart); # largest value currently in use in the ttl part. matrix
       bi_1 <- which(pv_bd == 1)
       bi_0 <- which(pv_bd == 0)

       tpart[bi[bi_1],d+1] <- rep(maxpv + 1, length(bi_1))      
       tpart[bi[bi_0],d+1] <- rep(maxpv + 2, length(bi_0))
   
      }
  }

for (p in 1:ncol(tpart)){

partition <- tpart[, p]
part.scaled <- as.numeric(factor(partition, 
                                 labels = rank(unique(partition))))
tpart[, p] <- part.scaled
 }

return(tpart)
}


##################################################################################################33

coritter=function(xc,conv){;
   i=0;
   
   repeat
   {   
      i=i+1;
      
      if (length(xc)>1){
         xc=cor(xc);
         xc[is.na(xc)==TRUE]=1
      }
      
      axc=na.omit(unique(as.numeric((round(abs(xc),conv)))))
      
      
      if (i==30){
         if (axc!=0) {
            print('Program has used 30 itterations.')
            print('There may be a convergence problem')
            print(axc)
         }
         axc=1;
      }
      axc.flag=sum(axc%in% c(1,0))
      
      if (axc.flag==length(axc))
      {break}
   } 
   
   xc=round(xc,0);
   
   if (length(xc)>1){
      all=1:nrow(xc);
      oneblk=which(xc[1,]==1);
      partvec=matrix(0,nrow(xc),1);
      partvec[oneblk]=1;
   } 
   
   if (length(xc)==1){
      partvec=1
   }
   
   return(partvec); #/* returns the 0/1 flag for which block a given row/column belongs to. */
}

