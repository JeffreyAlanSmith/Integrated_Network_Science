proportion_function=function(communities, attribute){ #inputs are community 
#membership and the attribute vector
#Here we calculate the proportion that fall into each category 
#found in attribute. We first do a table and then find the proportion 
#in each category. This is done for each community (using the tapply 
#option over the communities).

dat=tapply(factor(attribute),communities,function(x) {y=table(x);y/sum(y)})

do.call(rbind, dat) #We then output it as a matrix using a do.call 
#command and rbind option
} 
