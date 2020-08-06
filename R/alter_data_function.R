alter_data_function=function(egonets,netsize,var_names,egoID){
#The inputs are: 
#egonets=ego network data frame, 
#netsize=vector of ego network size, 
#var_names=names of variables to put on alter data frame (assumed to be var.name1, var.name2, for each alter)
#egoID=name of id of ego on ego network data frame

overall_list=list() #creating empty list to hold output for each variable

egonets_noisolates=egonets[netsize>0,] #taking out the isolates as they have no alters
netsize_updated=netsize[netsize>0] #redefining network size after taking out isolates

for (p in 1:length(var_names)){ #running over each variable name

var_list=list()
alter_list=list()

for (x in 1:nrow(egonets_noisolates)){ #running over each ego
alter_nums=rep(1:netsize_updated[x],times=1) #getting alter id number
#Now we grab the alter columns for that ego and put those values in a larger list so we can stack them later on 
#into one big vector (one for each variable of interest)
var_list[[x]]=egonets_noisolates[x,paste(rep(var_names[p],each=netsize_updated[x]),alter_nums,sep="")]
alter_list[[x]]=alter_nums
  }

var=unlist(var_list) #stacking all alter values into one long column
overall_list[[p]]=var
 }

dat=data.frame(do.call(data.frame,overall_list)) #putting all new variables together
colnames(dat)=var_names #putting useful column names on data frame

dat=cbind(rep(egonets_noisolates[,egoID], netsize_updated), unlist(alter_list), dat) #adding egoID and 
#alterID to data frame
colnames(dat)[1:2]=c(egoID,"alterID")
dat
}
