tau_stat_function <- function(network, weight.vector){ 
  # Arguments:
  # network: network of interest, in network format
  # weight.vector: vector of triads to sum up for tau statistic
  
  tmat <- t(as.matrix(summary(network ~ triadcensus(0:15))))
  tceno <- covariance.expectedvalue(tcen = tmat, 
                                    network = network)
  weight.vector <- as.matrix(weight.vector)
  
  tau <- tau.function(weighting.vector = weight.vector, 
                 observed.triads = as.matrix(tceno[, 1]),
                 expected.values = as.matrix(tceno[,4]),
                 covariance.matrix = tceno[, 7:ncol(tceno)])
  
  tau_list <- list(tau = tau, 
                   data.frame(observed.triads = as.matrix(tceno[,1]),
                              expected.triads = as.matrix(tceno[,4]), 
                              weight.vector = weight.vector))
  
  return(tau_list)
}

tau.function <- function(weighting.vector, 
                         observed.triads, 
                         expected.values, 
                         covariance.matrix){

  tau <- (t(weighting.vector) %*% observed.triads - t(weighting.vector) %*% expected.values) / (sqrt(t(weighting.vector) %*% covariance.matrix %*% weighting.vector))

    return (tau)
}



##########################################################################


covariance.expectedvalue<-function(tcen,network){
tcen=t(tcen)
 ntri=sum(tcen);
# print ntri;
 msize=network[[2]][[1]]
# print msize;

 #dyad census program *
 mu=c(0,0,1,0,0,0,1,1,0,0,2,1,1,1,2,3);
 au=c(0,1,0,2,2,2,1,1,3,3,0,2,2,2,1,0);
 nu=c(3,2,2,1,1,1,1,1,0,0,1,0,0,0,0,0);

gm2=(msize-2);
mm=(1/gm2)*(mu%*%tcen);
aa=(1/gm2)*(au%*%tcen);
nn=(1/gm2)*(nu%*%tcen);


#  /* formulas for calculating p(u) */
 # /* formulas for p(u) taken from Holland and Leinhardt, 1975 */
 # /* formulas for cov(t), var(t) taken from AJS 76:p501  */

pu=matrix(rep(0,16),c(16,1))
g2=(((msize^2)-msize))/2;

#   /* denominators of the tables */

d1=g2*(g2-1)*(g2-2);
d2=g2*(g2-1)*(g2-2)*(g2-3)*(g2-4)*(g2-5);
d3=g2*(g2-1)*(g2-2)*(g2-3)*(g2-4);

 #/* print d1 d2 d3; */

#/* nn = # of nulls, mm= # of muts, aa=# of asy */
#/* numerators for p(u) */

pu[1,1]=nn*(nn-1)*(nn-2);
pu[2,1]=3*aa*((nn)*(nn-1));
pu[3,1]=3*mm*((nn)*(nn-1));
pu[4,1]=.75*nn*((aa)*(aa-1));
pu[5,1]=.75*nn*((aa)*(aa-1));
pu[6,1]=(1.5)*nn*((aa)*(aa-1));
pu[7,1]=3*mm*aa*nn;
pu[8,1]=3*mm*aa*nn;
pu[9,1]=.75*(aa*(aa-1)*(aa-2));
pu[10,1]=.25*(aa*(aa-1)*(aa-2));
pu[11,1]=3*nn*(mm*(mm-1));
pu[12,1]=.75*mm*((aa*(aa-1)));
pu[13,1]=.75*mm*((aa*(aa-1)));
pu[14,1]=(1.5)*mm*((aa*(aa-1)));
pu[15,1]=3*aa*((mm)*(mm-1));
pu[16,1]=mm*(mm-1)*(mm-2);

pu=pu/d1;
stcen=sum(tcen);
evt=stcen*(pu);
evtcen=stcen*(pu);

 #/* program for calculating Po(u,v) under U|MAN distribution */

pouv=matrix(rep(0,16^2),c(16,16));

pouv[1,1]=nn*(nn-1)*(nn-2)*(nn-3)*(nn-4)*(nn-5);
pouv[2,1]=3*aa*(nn*(nn-1)*(nn-2)*(nn-3)*(nn-4));
pouv[3,1]=3*mm*(nn*(nn-1)*(nn-2)*(nn-3)*(nn-4));
pouv[4,1]=0.75*aa*(aa-1)*(nn*(nn-1)*(nn-2)*(nn-3));
pouv[5,1]=0.75*aa*(aa-1)*(nn*(nn-1)*(nn-2)*(nn-3));
pouv[6,1]=1.5*aa*(aa-1)*(nn*(nn-1)*(nn-2)*(nn-3));
pouv[7,1]=3*mm*aa*nn*((nn-1)*(nn-2)*(nn-3));
pouv[8,1]=3*mm*aa*nn*((nn-1)*(nn-2)*(nn-3));
pouv[9,1]=0.75*aa*(aa-1)*(aa-2)*(nn*(nn-1)*(nn-2));
pouv[10,1]=0.25*aa*(aa-1)*(aa-2)*(nn*(nn-1)*(nn-2));
pouv[11,1]=3*(mm*(mm-1))*(nn*(nn-1)*(nn-2)*(nn-3));
pouv[12,1]=0.75*mm*(aa*(aa-1))*(nn*(nn-1)*(nn-2));
pouv[13,1]=0.75*mm*(aa*(aa-1))*(nn*(nn-1)*(nn-2));
pouv[14,1]=1.5*mm*(aa*(aa-1))*(nn*(nn-1)*(nn-2));
pouv[15,1]=3*(mm*(mm-1))*aa*(nn*(nn-1)*(nn-2));
pouv[16,1]=mm*(mm-1)*(mm-2)*nn*(nn-1)*(nn-2);

pouv[2,2]=9*aa*(aa-1)*(nn*(nn-1)*(nn-2)*(nn-3));
pouv[3,2]=9*mm*aa*nn*((nn-1)*(nn-2)*(nn-3));
pouv[4,2]=2.25*aa*(aa-1)*(aa-2)*(nn*(nn-1)*(nn-2));
pouv[5,2]=2.25*aa*(aa-1)*(aa-2)*(nn*(nn-1)*(nn-2));
pouv[6,2]=4.50*aa*(aa-1)*(aa-2)*(nn*(nn-1)*(nn-2));
pouv[7,2]=9*mm*aa*(aa-1)*(nn*(nn-1)*(nn-2));
pouv[8,2]=9*mm*aa*(aa-1)*(nn*(nn-1)*(nn-2));
pouv[9,2]=2.25*aa*(aa-1)*(aa-2)*(aa-3)*(nn*(nn-1));
pouv[10,2]=0.75*aa*(aa-1)*(aa-2)*(aa-3)*(nn*(nn-1));
pouv[11,2]=9*(mm*(mm-1))*aa*(nn*(nn-1)*(nn-2));
pouv[12,2]=2.25*mm*(aa*(aa-1)*(aa-2))*(nn*(nn-1));
pouv[13,2]=2.25*mm*(aa*(aa-1)*(aa-2))*(nn*(nn-1));
pouv[14,2]=4.50*mm*(aa*(aa-1)*(aa-2))*(nn*(nn-1));
pouv[15,2]=9*(mm*(mm-1))*(aa*(aa-1))*(nn*(nn-1));
pouv[16,2]=3*(mm*(mm-1)*(mm-2))*aa*(nn*(nn-1));

pouv[3,3]=9*mm*(mm-1)*(nn*(nn-1)*(nn-2)*(nn-3));
pouv[4,3]=2.25*mm*(aa*(aa-1))*(nn*(nn-1)*(nn-2));
pouv[5,3]=2.25*mm*(aa*(aa-1))*(nn*(nn-1)*(nn-2));
pouv[6,3]=4.50*mm*(aa*(aa-1))*(nn*(nn-1)*(nn-2));
pouv[7,3]=9*(mm*(mm-1))*aa*(nn*(nn-1)*(nn-2));
pouv[8,3]=9*(mm*(mm-1))*aa*(nn*(nn-1)*(nn-2));
pouv[9,3]=2.25*mm*(aa*(aa-1)*(aa-2))*(nn*(nn-1));
pouv[10,3]=0.75*mm*(aa*(aa-1)*(aa-2))*(nn*(nn-1));
pouv[11,3]=9*mm*(mm-1)*(mm-2)*(nn*(nn-1)*(nn-2));
pouv[12,3]=2.25*(mm*(mm-1))*(aa*(aa-1))*(nn*(nn-1));
pouv[13,3]=2.25*(mm*(mm-1))*(aa*(aa-1))*(nn*(nn-1));
pouv[14,3]=4.50*(mm*(mm-1))*(aa*(aa-1))*(nn*(nn-1));
pouv[15,3]=9*(mm*(mm-1)*(mm-2))*aa*(nn*(nn-1));
pouv[16,3]=3*mm*(mm-1)*(mm-2)*(mm-3)*(nn*(nn-1));

pouv[4,4]=(9/16)*(aa*(aa-1)*(aa-2)*(aa-3))*(nn*(nn-1));
pouv[5,4]=(9/16)*(aa*(aa-1)*(aa-2)*(aa-3))*(nn*(nn-1));
pouv[6,4]=(9/8)*(aa*(aa-1)*(aa-2)*(aa-3))*(nn*(nn-1));
pouv[7,4]=2.25*mm*(aa*(aa-1)*(aa-2))*(nn*(nn-1));
pouv[8,4]=2.25*mm*(aa*(aa-1)*(aa-2))*(nn*(nn-1));
pouv[9,4]=(9/16)*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4))*nn;
pouv[10,4]=(3/16)*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4))*nn;
pouv[11,4]=2.25*(mm*(mm-1))*(aa*(aa-1))*(nn*(nn-1));
pouv[12,4]=(9/16)*mm*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
pouv[13,4]=(9/16)*mm*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
pouv[14,4]=(9/8)*mm*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
pouv[15,4]=2.25*(mm*(mm-1))*(aa*(aa-1)*(aa-2))*(nn);
pouv[16,4]=0.75*(mm*(mm-1)*(mm-2))*(aa*(aa-1))*(nn);

pouv[5,5]=(9/16)*(aa*(aa-1)*(aa-2)*(aa-3))*(nn*(nn-1));
pouv[6,5]=(9/8)*(aa*(aa-1)*(aa-2)*(aa-3))*(nn*(nn-1));
pouv[7,5]=2.25*mm*(aa*(aa-1)*(aa-2))*(nn*(nn-1));
pouv[8,5]=2.25*mm*(aa*(aa-1)*(aa-2))*(nn*(nn-1));
pouv[9,5]=(9/16)*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4))*nn;
pouv[10,5]=(3/16)*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4))*nn;
pouv[11,5]=2.25*(mm*(mm-1))*(aa*(aa-1))*(nn*(nn-1));
pouv[12,5]=(9/16)*mm*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
pouv[13,5]=(9/16)*mm*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
pouv[14,5]=(9/8)*mm*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
pouv[15,5]=2.25*(mm*(mm-1))*(aa*(aa-1)*(aa-2))*(nn);
pouv[16,5]=0.75*(mm*(mm-1)*(mm-2))*(aa*(aa-1))*(nn);

pouv[6,6]=(9/4)*(aa*(aa-1)*(aa-2)*(aa-3))*(nn*(nn-1));
pouv[7,6]=4.50*mm*(aa*(aa-1)*(aa-2))*(nn*(nn-1));
pouv[8,6]=4.50*mm*(aa*(aa-1)*(aa-2))*(nn*(nn-1));
pouv[9,6]=(9/8)*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4))*nn;
pouv[10,6]=(3/8)*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4))*nn;
pouv[11,6]=4.50*(mm*(mm-1))*(aa*(aa-1))*(nn*(nn-1));
pouv[12,6]=(9/8)*mm*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
pouv[13,6]=(9/8)*mm*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
pouv[14,6]=(9/4)*mm*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
pouv[15,6]=4.50*(mm*(mm-1))*(aa*(aa-1)*(aa-2))*(nn);
pouv[16,6]=1.50*(mm*(mm-1)*(mm-2))*(aa*(aa-1))*(nn);

pouv[7,7]=9*(mm*(mm-1))*(aa*(aa-1))*(nn*(nn-1));
pouv[8,7]=9*(mm*(mm-1))*(aa*(aa-1))*(nn*(nn-1));
pouv[9,7]=(9/4)*mm*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
pouv[10,7]=(3/4)*mm*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
pouv[11,7]=9*(mm*(mm-1)*(mm-2))*aa*(nn*(nn-1));
pouv[12,7]=2.25*(mm*(mm-1))*(aa*(aa-1)*(aa-2))*(nn);
pouv[13,7]=2.25*(mm*(mm-1))*(aa*(aa-1)*(aa-2))*(nn);
pouv[14,7]=4.50*(mm*(mm-1))*(aa*(aa-1)*(aa-2))*(nn);
pouv[15,7]=9*(mm*(mm-1)*(mm-2))*(aa*(aa-1))*nn;
pouv[16,7]=3*(mm*(mm-1)*(mm-2)*(mm-3))*aa*nn;

pouv[8,8]=9*(mm*(mm-1))*(aa*(aa-1))*(nn*(nn-1));
pouv[9,8]=(9/4)*mm*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
pouv[10,8]=(3/4)*mm*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
pouv[11,8]=9*(mm*(mm-1)*(mm-2))*aa*(nn*(nn-1));
pouv[12,8]=2.25*(mm*(mm-1))*(aa*(aa-1)*(aa-2))*(nn);
pouv[13,8]=2.25*(mm*(mm-1))*(aa*(aa-1)*(aa-2))*(nn);
pouv[14,8]=4.50*(mm*(mm-1))*(aa*(aa-1)*(aa-2))*(nn);
pouv[15,8]=9*(mm*(mm-1)*(mm-2))*(aa*(aa-1))*nn;
pouv[16,8]=3*(mm*(mm-1)*(mm-2)*(mm-3))*aa*nn;

pouv[9,9]=(9/16)*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4)*(aa-5));
pouv[10,9]=(3/16)*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4)*(aa-5));
pouv[11,9]=2.25*(mm*(mm-1))*(aa*(aa-1)*(aa-2))*(nn);
pouv[12,9]=(9/16)*mm*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4));
pouv[13,9]=(9/16)*mm*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4));
pouv[14,9]=(9/8)*mm*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4));
pouv[15,9]=(9/4)*(mm*(mm-1))*(aa*(aa-1)*(aa-2)*(aa-3));
pouv[16,9]=(3/4)*(mm*(mm-1)*(mm-2))*(aa*(aa-1)*(aa-2));

pouv[10,10]=(1/16)*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4)*(aa-5));
pouv[11,10]=0.75*(mm*(mm-1))*(aa*(aa-1)*(aa-2))*(nn);
pouv[12,10]=(3/16)*mm*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4));
pouv[13,10]=(3/16)*mm*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4));
pouv[14,10]=(3/8)*mm*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4));
pouv[15,10]=(3/4)*(mm*(mm-1))*(aa*(aa-1)*(aa-2)*(aa-3));
pouv[16,10]=(1/4)*(mm*(mm-1)*(mm-2))*(aa*(aa-1)*(aa-2));

pouv[11,11]=9*(mm*(mm-1)*(mm-2)*(mm-3))*(nn*(nn-1));
pouv[12,11]=(9/4)*(mm*(mm-1)*(mm-2))*(aa*(aa-1))*nn;
pouv[13,11]=(9/4)*(mm*(mm-1)*(mm-2))*(aa*(aa-1))*nn;
pouv[14,11]=(9/2)*(mm*(mm-1)*(mm-2))*(aa*(aa-1))*nn;
pouv[15,11]=9*(mm*(mm-1)*(mm-2)*(mm-3))*aa*nn;
pouv[16,11]=3*(mm*(mm-1)*(mm-2)*(mm-3)*(mm-4))*nn;

pouv[12,12]=(9/16)*(mm*(mm-1))*(aa*(aa-1)*(aa-2)*(aa-3));
pouv[13,12]=(9/16)*(mm*(mm-1))*(aa*(aa-1)*(aa-2)*(aa-3));
pouv[14,12]=(9/8)*(mm*(mm-1))*(aa*(aa-1)*(aa-2)*(aa-3));
pouv[15,12]=(9/4)*(mm*(mm-1)*(mm-2))*(aa*(aa-1)*(aa-2));
#*look1=pouv[15,12];
#*print look1;
pouv[16,12]=(3/4)*(mm*(mm-1)*(mm-2)*(mm-3))*(aa*(aa-1));

pouv[13,13]=(9/16)*(mm*(mm-1))*(aa*(aa-1)*(aa-2)*(aa-3));
pouv[14,13]=(9/8)*(mm*(mm-1))*(aa*(aa-1)*(aa-2)*(aa-3));
pouv[15,13]=(9/4)*(mm*(mm-1)*(mm-2))*(aa*(aa-1)*(aa-2));
 #*look2=pouv[15,13];
 #*print look2;
pouv[16,13]=(3/4)*(mm*(mm-1)*(mm-2)*(mm-3))*(aa*(aa-1));

pouv[14,14]=(9/4)*(mm*(mm-1))*(aa*(aa-1)*(aa-2)*(aa-3));
pouv[15,14]=(9/2)*(mm*(mm-1)*(mm-2))*(aa*(aa-1)*(aa-2));
pouv[16,14]=(3/2)*(mm*(mm-1)*(mm-2)*(mm-3))*(aa*(aa-1));

pouv[15,15]=9*(mm*(mm-1)*(mm-2)*(mm-3))*(aa*(aa-1));
pouv[16,15]=3*(mm*(mm-1)*(mm-2)*(mm-3)*(mm-4))*(aa);

pouv[16,16]=mm*(mm-1)*(mm-2)*(mm-3)*(mm-4)*(mm-5);
pouv=symmetrize(pouv,rule="lower")

#  /* program for p2(u,v) under U|MAN distribution  */

p2uv=matrix(rep(0,16^2),c(16,16));

p2uv[1,1]=nn*(nn-1)*(nn-2)*(nn-3)*(nn-4);
p2uv[2,1]=2*aa*(nn*(nn-1)*(nn-2)*(nn-3));
p2uv[3,1]=2*mm*(nn*(nn-1)*(nn-2)*(nn-3));
p2uv[4,1]=.25*(aa*(aa-1))*(nn*(nn-1)*(nn-2));
p2uv[5,1]=.25*(aa*(aa-1))*(nn*(nn-1)*(nn-2));
p2uv[6,1]=.5*(aa*(aa-1))*(nn*(nn-1)*(nn-2));
p2uv[7,1]=mm*aa*(nn*(nn-1)*(nn-2));
p2uv[8,1]=mm*aa*(nn*(nn-1)*(nn-2));

p2uv[11,1]=(mm*(mm-1))*(nn*(nn-1)*(nn-1));
    
p2uv[2,2]=aa*(nn*(nn-1)*(nn-2))*((4*aa)+nn-7);
p2uv[3,2]=4*mm*aa*(nn*(nn-1)*(nn-2));
p2uv[4,2]=.5*(aa*(aa-1))*(nn*(nn-1))*(aa+nn-4);
p2uv[5,2]=.5*(aa*(aa-1))*(nn*(nn-1))*(aa+nn-4);
p2uv[6,2]=(aa*(aa-1))*(nn*(nn-1))*(aa+nn-4);
p2uv[7,2]=mm*aa*(nn*(nn-1))*((2*aa)+nn-4);
p2uv[8,2]=mm*aa*(nn*(nn-1))*((2*aa)+nn-4);
p2uv[9,2]=(3/4)*(aa*(aa-1)*(aa-2))*(nn*(nn-1));
p2uv[10,2]=(1/4)*(aa*(aa-1)*(aa-2))*(nn*(nn-1));
p2uv[11,2]=2*(mm*(mm-1))*aa*(nn*(nn-1));
p2uv[12,2]=.5*mm*(aa*(aa-1))*(nn*(nn-1));
p2uv[13,2]=.5*mm*(aa*(aa-1))*(nn*(nn-1));
p2uv[14,2]=mm*(aa*(aa-1))*(nn*(nn-1));
p2uv[15,2]=(mm*(mm-1))*aa*(nn*(nn-1));
    
p2uv[3,3]=mm*(nn*(nn-1)*(nn-2))*((4*mm)+nn-7);
p2uv[4,3]=.5*mm*(aa*(aa-1))*(nn*(nn-1));
p2uv[5,3]=.5*mm*(aa*(aa-1))*(nn*(nn-1));
p2uv[6,3]=mm*(aa*(aa-1))*(nn*(nn-1));
p2uv[7,3]=mm*aa*(nn*(nn-1)*((2*mm)+nn-4));
p2uv[8,3]=mm*aa*(nn*(nn-1)*((2*mm)+nn-4));
p2uv[11,3]=2*(mm*(mm-1))*(nn*(nn-1))*(mm+nn-4);
p2uv[12,3]=.25*mm*(aa*(aa-1))*(nn*(nn-1));
p2uv[13,3]=.25*mm*(aa*(aa-1))*(nn*(nn-1));
p2uv[14,3]=.5*mm*(aa*(aa-1))*(nn*(nn-1));
p2uv[15,3]=2*(mm*(mm-1))*aa*(nn*(nn-1));
p2uv[16,3]=(mm*(mm-1)*(mm-2))*(nn*(nn-1));

p2uv[4,4]=(1/16)*(aa*(aa-1)*(aa-2))*nn*(aa+(4*nn)-7);
p2uv[5,4]=(1/16)*(aa*(aa-1)*(aa-2))*nn*(aa+(4*nn)-7);
p2uv[6,4]=(1/8)*(aa*(aa-1)*(aa-2))*nn*(aa+(4*nn)-7);
p2uv[7,4]=(1/4)*mm*(aa*(aa-1))*nn*(aa+(2*nn)-4);
p2uv[8,4]=(1/4)*mm*(aa*(aa-1))*nn*(aa+(2*nn)-4);
p2uv[9,4]=(3/8)*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
p2uv[10,4]=(1/8)*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
p2uv[11,4]=(1/4)*(mm*(mm-1))*(aa*(aa-1))*nn;
p2uv[12,4]=(1/4)*mm*(aa*(aa-1)*(aa-2))*nn;
p2uv[13,4]=(1/4)*mm*(aa*(aa-1)*(aa-2))*nn;
p2uv[14,4]=(1/2)*mm*(aa*(aa-1)*(aa-2))*nn;
p2uv[15,4]=(1/2)*(mm*(mm-1))*(aa*(aa-1))*nn;
p2uv[5,5]=(1/16)*(aa*(aa-1)*(aa-2))*nn*(aa+(4*nn)-7);
p2uv[6,5]=(1/8)*(aa*(aa-1)*(aa-2))*nn*(aa+(4*nn)-7);
p2uv[7,5]=(1/4)*mm*(aa*(aa-1))*nn*(aa+(2*nn)-4);
p2uv[8,5]=(1/4)*mm*(aa*(aa-1))*nn*(aa+(2*nn)-4);
p2uv[9,5]=(3/8)*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
p2uv[10,5]=(1/8)*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
p2uv[11,5]=(1/4)*(mm*(mm-1))*(aa*(aa-1))*nn;
p2uv[12,5]=(1/4)*mm*(aa*(aa-1)*(aa-2))*nn;
p2uv[13,5]=(1/4)*mm*(aa*(aa-1)*(aa-2))*nn;
p2uv[14,5]=(1/2)*mm*(aa*(aa-1)*(aa-2))*nn;
p2uv[15,5]=(1/2)*(mm*(mm-1))*(aa*(aa-1))*nn;
p2uv[6,6]=(1/4)*(aa*(aa-1)*(aa-2))*nn*(aa+(4*nn)-7);
p2uv[7,6]=(1/2)*mm*(aa*(aa-1))*nn*(aa+(2*nn)-4);
p2uv[8,6]=(1/2)*mm*(aa*(aa-1))*nn*(aa+(2*nn)-4);
p2uv[9,6]=(3/4)*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
p2uv[10,6]=(1/4)*(aa*(aa-1)*(aa-2)*(aa-3))*nn;
p2uv[11,6]=(1/2)*(mm*(mm-1))*(aa*(aa-1))*nn;
p2uv[12,6]=(1/2)*mm*(aa*(aa-1)*(aa-2))*nn;
p2uv[13,6]=(1/2)*mm*(aa*(aa-1)*(aa-2))*nn;
p2uv[14,6]=mm*(aa*(aa-1)*(aa-2))*nn;
p2uv[15,6]=(mm*(mm-1))*(aa*(aa-1))*nn;
p2uv[7,7]=(mm*(aa*(aa-1))*(nn*(nn-1))) + ((mm*(mm-1))*aa*(nn*(nn-1)))+ ((mm*(mm-1))*(aa*(aa-1))*nn);
p2uv[8,7]=(mm*(aa*(aa-1))*(nn*(nn-1))) + ((mm*(mm-1))*aa*(nn*(nn-1)))+ ((mm*(mm-1))*(aa*(aa-1))*nn);
p2uv[9,7]=(3/4)*mm*(aa*(aa-1)*(aa-2))*nn;
p2uv[10,7]=(1/4)*mm*(aa*(aa-1)*(aa-2))*nn;
p2uv[11,7]=(mm*(mm-1))*aa*nn*(mm+(2*nn)-4);
p2uv[12,7]=(1/4)*mm*(aa*(aa-1))*nn*(aa+(2*mm)-4);
p2uv[13,7]=(1/4)*mm*(aa*(aa-1))*nn*(aa+(2*mm)-4);
p2uv[14,7]=(1/2)*mm*(aa*(aa-1))*nn*(aa+(2*mm)-4);
p2uv[15,7]=(mm*(mm-1))*aa*nn*(mm+(2*aa)-4);
p2uv[16,7]=(mm*(mm-1)*(mm-2))*aa*nn;

p2uv[8,8]=(mm*(aa*(aa-1))*(nn*(nn-1))) + ((mm*(mm-1))*aa*(nn*(nn-1)))+ ((mm*(mm-1))*(aa*(aa-1))*nn);
p2uv[9,8]=(3/4)*mm*(aa*(aa-1)*(aa-2))*nn;
p2uv[10,8]=(1/4)*mm*(aa*(aa-1)*(aa-2))*nn;
p2uv[11,8]=(mm*(mm-1))*aa*nn*(mm+(2*nn)-4);
p2uv[12,8]=(1/4)*mm*(aa*(aa-1))*nn*(aa+(2*mm)-4);
p2uv[13,8]=(1/4)*mm*(aa*(aa-1))*nn*(aa+(2*mm)-4);
p2uv[14,8]=(1/2)*mm*(aa*(aa-1))*nn*(aa+(2*mm)-4);
p2uv[15,8]=(mm*(mm-1))*aa*nn*(mm+(2*aa)-4);
p2uv[16,8]=(mm*(mm-1)*(mm-2))*aa*nn;

p2uv[9,9]=(9/16)*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4));
p2uv[10,9]=(3/16)*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4));
     
p2uv[12,9]=(3/8)*mm*(aa*(aa-1)*(aa-2)*(aa-3));
p2uv[13,9]=(3/8)*mm*(aa*(aa-1)*(aa-2)*(aa-3));
p2uv[14,9]=(3/4)*mm*(aa*(aa-1)*(aa-2)*(aa-3));
p2uv[15,9]=(3/4)*(mm*(mm-1))*(aa*(aa-1)*(aa-2));
    p2uv[10,10]=(1/16)*(aa*(aa-1)*(aa-2)*(aa-3)*(aa-4));
    
p2uv[12,10]=(1/8)*mm*(aa*(aa-1)*(aa-2)*(aa-3));
p2uv[13,10]=(1/8)*mm*(aa*(aa-1)*(aa-2)*(aa-3));
p2uv[14,10]=(1/4)*mm*(aa*(aa-1)*(aa-2)*(aa-3));
p2uv[15,10]=(1/4)*(mm*(mm-1))*(aa*(aa-1)*(aa-2));
    p2uv[11,11]=(mm*(mm-1)*(mm-2))*nn*(mm+(4*nn)-7);
p2uv[12,11]=(1/2)*(mm*(mm-1))*(aa*(aa-1))*nn;
p2uv[13,11]=(1/2)*(mm*(mm-1))*(aa*(aa-1))*nn;
p2uv[14,11]=(mm*(mm-1))*(aa*(aa-1))*nn;
p2uv[15,11]=4*(mm*(mm-1)*(mm-2))*aa*nn;
p2uv[16,11]=2*(mm*(mm-1)*(mm-2)*(mm-3))*nn;

p2uv[12,12]=(1/16)*mm*(aa*(aa-1)*(aa-2))*(aa+(4*mm)-7);
p2uv[13,12]=(1/16)*mm*(aa*(aa-1)*(aa-2))*(aa+(4*mm)-7);
p2uv[14,12]=(1/8)*mm*(aa*(aa-1)*(aa-2))*(aa+(4*mm)-7);

p2uv[15,12]=(1/2)*(mm*(mm-1))*(aa*(aa-1))*(mm+aa-4);

p2uv[16,12]=(1/4)*(mm*(mm-1)*(mm-2))*(aa*(aa-1));

p2uv[13,13]=(1/16)*mm*(aa*(aa-1)*(aa-2))*(aa+(4*mm)-7);
p2uv[14,13]=(1/8)*mm*(aa*(aa-1)*(aa-2))*(aa+(4*mm)-7);

p2uv[15,13]=(1/2)*(mm*(mm-1))*(aa*(aa-1))*(mm+aa-4);

p2uv[16,13]=(1/4)*(mm*(mm-1)*(mm-2))*(aa*(aa-1));

p2uv[14,14]=(1/4)*mm*(aa*(aa-1)*(aa-2))*(aa+(4*mm)-7);
p2uv[15,14]=(mm*(mm-1))*(aa*(aa-1))*(mm+aa-4);
p2uv[16,14]=(1/2)*(mm*(mm-1)*(mm-2))*(aa*(aa-1));

p2uv[15,15]=(mm*(mm-1)*(mm-2))*aa*(mm+(4*aa)-7);
p2uv[16,15]=2*(mm*(mm-1)*(mm-2)*(mm-3))*aa;

p2uv[16,16]=mm*(mm-1)*(mm-2)*(mm-3)*(mm-4);

p2uv=symmetrize(p2uv,rule="lower")

pouv=pouv/d2;
p2uv=p2uv/d3;

covtu=matrix(rep(0,16^2),c(16,16));
vartu=matrix(rep(0,16),c(16,1));

  for (a in 1:16){
     for (b in a:16){
     cvtp1= (-1*stcen)*(pu[b,1])*(pu[a,1]);
     cvtp2= 3*(msize-3)*stcen*(p2uv[b,a]-pouv[b,a]);
     cvtp3= (stcen*stcen-1)*(pouv[b,a]-((pu[b,1])*(pu[a,1])));
     covtu[b,a]=cvtp1+cvtp2+cvtp3;
}
}


#vartu=j(16,1,0);

  for ( m in 1:16){
    #/* vt forumula from AJS  */
   v2p1= (stcen*pu[m,1]*(1-pu[m,1]));
   v2p2= 3*(msize-3)*stcen*(p2uv[m,m]-pouv[m,m]);
   v2p3= (stcen*stcen-1)*(pouv[m,m]-pu[m,1]^2);
    #/* print v2p1 v2p2 v2p3; */
   vartu[m,1]=v2p1+v2p2+v2p3;
  }

 cvmat=covtu+t(covtu);
 cvmatd=diag(cvmat);
 
diag(cvmat)=diag(cvmat)-cvmatd;
 #vardi=diag(vartu);
vardi=vartu 
diag(cvmat)=diag(cvmat)+vardi;

# /* program to calc the standardized diff from chance
 #      stddif=(actual-Expected)/sqrt(var)   */

 stddif=(tcen-evtcen)/(sqrt(vartu));
 tpcnt=tcen/stcen;
 tceno=cbind(tcen,tpcnt,pu,evt,vartu,stddif);

#if prnt=1 then do;
 # mattrib tceno rowname=({'003' '012' '102' '021D' '021U' '021C' '111D' '111U'
  #         '030T' '030C' '201' '120D' '120U' '120C' '210' '300'})
   #        colname=({t tpcnt pu evt vartu stddif})
    #       format=best6.4
     #      label='Triad Census';
#  print tceno;

 
   #rownames(cvmat)=c("003", "012", "102", "021D", "021U", "021C", "111D", "111U",
    #       "030T", "030C", "201", "120D", "120U", "120C", "210", "300")


 tceno=cbind(tceno,cvmat);

return(tceno);
}
