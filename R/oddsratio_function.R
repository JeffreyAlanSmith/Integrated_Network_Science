oddsratio_function = function(egoalter_tab, attribute) {
#The inputs are:
#egoalter_tab =table of interest;
#attribute=vector representing attribute of interest in the sample 

#We first calculate the number of dyads that match and mismatch on the attribute
#based on the oberved data. 

#We calculate the number matching by summing up over the diagonal of the 
#the table (as these are cases where ego and alter have the same value).
match=sum(diag(egoalter_tab)) 

#We now calculate the number mismatching by taking the total number of 
#dyads and subtracting the number that match (calculated above).
notmatch=sum(egoalter_tab)-match

#Now we calculate our chance expectations, defined as what would happen if we 
#randomly paired all respondents from the data (defined in the input attribute vector), and 
#calculated how many of those pairs were the same (and different) on the 
#attribute of interest.

#We first do a table on the attribute, to see how many people fall into each category.
freq=table(attribute) 

#We next calculate the total number of random pairings, assuming we pair all respondents 
#with all respondents. This is simply the number of respondents times the number of 
#respondents minus 1, as we assume we are not pairing people with themself.
total_dyads=sum(freq)*(sum(freq)-1)

#We now calculate the number of random pairings expected to match by chance.
#Formally, we take the number in each category (e.g., number of men) and multiply that 
#number by itself (minus 1 as we again assume people are not paired with themself), showing
#how many pairs would be the same if we paired all respondents with all 
#respondents. Remember that R will multiply things elment-wise, so the following 
#bit of code will take the first value in freq and multiply it by the first element 
#in freq-1. We sum up the values to get the total number of dyads that are expected to match.
match_chance=sum(freq*(freq-1))

#We now calculate the number of dyads not matching by chance as the difference
#between the total number of dyads and those matching by chance.
notmatch_chance = total_dyads-match_chance 

#And finally we can calculate the odds ratio of observed odds of matching to 
#odds of matching by chance
or=(match*notmatch_chance)/(notmatch*match_chance) 

or
} 
