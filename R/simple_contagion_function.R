


simple_contagion_function=function(net,initial_seeds,time_periods
,prob_infection,prob_recovery) {

#start sim here for multiple outputs of cumulative proportions for each simulated network
cumulative.prop.list=list()

N=gorder(net)
ids=1:N

results.list=list()

#now let's randomly select the nodes that will be our seeds for this run

currently.infected=initial_seeds
currently.susceptible=ids[!(ids%in% currently.infected)] #everyone not infected is susceptible

#putting together the infected and susceptible people in our results list
current.list=list(infected=currently.infected,susceptible=currently.susceptible)
results.list[[1]]=current.list #putting the initial data in the first slot


#Run over each time period to see if "disease" spreads from infected to direct social contacts
for (time in 2:time_periods){
  
  #identify nearest neighbors of those who are infected
  #order=number of steps from the infected
  possible.infections=neighborhood(net,order=1, currently.infected)
  
  #if no infections, then stop simulation. 
  if(length(possible.infections)==0){break}
  
  #A person cannot infect someone who is already infected, thus we remove all individuals 
  #that are already infected from the possible infection list, by using function created
  #below:
  #x=possible infected
  #infected=list of infected people
  
  #Apply this function over our list of possible infections, removing those 
  #who are already infected
  possible.infections=lapply(possible.infections,remove.function,infected=currently.infected)
   
  #Run over each person's ego network and then see which of the neighbors gets infected
  new.infections.list=list()#set up list for new infections
  
  #Loop start at 1 to possible length of infection
  for (i in 1:length(possible.infections)){
    
    # Take first infected person, simulate attempt of infection onto each neighbor
    possible.infections.ego=possible.infections[[i]] #first, grab the neighbors of 1st infected person
    size.ego=length(possible.infections.ego) #see how many neighbors there are
    
    if(size.ego>0){ #if there is at least one neighbor then see if anyone gets infected in this time period 
      
      #simulate the infection process with binomial dist
      #1=success: infection, 0=failure, no infection
      #n=number of ego
      #size=1 trial
      #prob=probability of success of infection
      sim=rbinom(size.ego,size=1,prob=prob_infection)
      
      
      new.infections.list[[i]]=possible.infections.ego[which(sim==1)] #here show which 
      #cases were infected, subset possible infections by which were infected in simulation above
      ##which: shows the indices that are TRUE, or infected since 1=success, 
      ##from the possible.infections.ego and put into new.infections.list
    }
  }
  
  #make a vector of the new infections, making sure no one is listed more than once
  #unlist turns the list into a vector
  #unique makes sure any person is only listed once 
  new.infections=unique(unlist(new.infections.list)) 
  
  #now we want to see which of the currently infected recover by the end of the period
  #use rbinom for probability of recovering
  #number of draws is number of infected
  sim.recovered=rbinom(length(currently.infected),size=1,prob=prob_recovery) 
  
  if (sum(sim.recovered)>0) #remove the recovered only if anyone actually recovered
  {
    #now remove those who are recovered from the list of infected
    currently.infected=currently.infected[-which(sim.recovered==1)] #identifying which recovered using
    # a which statement, then removing them from infected using - sign.
  }
  
  #updating the list of infected and susceptible 
  currently.infected=c(currently.infected,new.infections)
  currently.susceptible=ids[!(ids%in% currently.infected)] #everyone not infected is susceptible
  
  current.list=list(infected=currently.infected,susceptible=currently.susceptible)
  
  results.list[[time]]=current.list
 }
 


cumulative_prop=cumulative_function(results.list,N=N)

  list(results=results.list,cumulative_prop=cumulative_prop)

}#end function  
  


  remove.function=function(x,infected){ 
    x[!(x%in%infected)] #here we want to remove all ids of people who are already infected
  } 



#Let's write a little function to calculate the cumulative
#proportion who adopt after each time period. The inputs are the 
#list produced by simple_contagion_function and N, the size of the network

cumulative_function=function(results,N){
cumulative_prop=NA

for (time in 1:length(results)){ 
  cumulative_prop[time]=length(results[[time]]$infected)/N #calculating 
  #proportion adopting for each time period
 }
cumulative_prop
}




