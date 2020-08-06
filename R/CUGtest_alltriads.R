
CUGtest_alltriads=function(dat, mode, cmode, reps)

  {
triads_to_count=c("003",  "012",  "102", "021D", "021U", "021C",
                  "111D", "111U", "030T", "030C", "201",  "120D",
                   "120U", "120C", "210", "300")

cug_list=list()

#looping over each triad type
for (x in 1:length(triads_to_count)){
  
  cug_results=cug.test(dat, 
                       FUN=count_triads_function, 
                       mode=mode, 
                       cmode=cmode, 
                       reps=reps, 
                       FUN.args=list(triad_names=triads_to_count[x]))
  
  cug_list[[x]]=c(obs=as.numeric(cug_results$obs.stat),
                  mean_random=mean(cug_results$rep.stat), 
                  prop_random_lte_obs=cug_results$plteob)
}

#making table of results
cug_table=do.call(rbind, cug_list)
cug_table=data.frame(triads=triads_to_count, cug_table)

cug_table
}

