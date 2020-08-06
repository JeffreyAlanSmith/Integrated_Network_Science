prop_same_function=function(alt.attr, ego.attr){#inputs are alter attribute and ego attribute
same=sum(ego.attr==alt.attr) #taking ego attribute and comparing to alter attributes, summing up 
#number of times they match
same/(length(alt.attr)) #calculating proportion of ego-alter pairs that match
} #note that any ego with no alters or with missing data for the alter attributes will return an NA
