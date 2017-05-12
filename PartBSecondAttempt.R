#declare a vector with probabilities of SUCCESS
servers <- c(0.80, 0.60, 0.40, 0.20, 0)
successes <- 0;
failures <- 0;
numberOfExperiments <- 1000000;

#Following function takes in the probability of failure and based on that returns 0 or 1
#0 means fail and 1 means success
simulateBernoulliTrial  <- function(probOfSuccess) {
  result <- rbinom(1, 1, probOfSuccess)
  return(result);
}

#function that provides a random server from the remaining unused servers
getAnotherServer <- function(previouslyFailedServerOne, previouslyFailedServerTwo) {
  anotherServer <- sample(1:5, 1);
  #the following loop is to ensure that the previously failed server doesn't get returned 
  while(anotherServer == previouslyFailedServerOne || anotherServer == previouslyFailedServerTwo){
    anotherServer <- sample(1:5, 1);
  }
  return(anotherServer);
}

#run loop and simulate a server request using binomial experiment
i <- 1;
while(i <= numberOfExperiments){
  #randomly select a server to send the request to
  firstServer <- sample(1:5, 1);
  #print(cat("First Server is ", firstServer));
  
  #send the request and check if failed first time
  if(!simulateBernoulliTrial(servers[firstServer])){
    
    #choose another server and send a request. 
    secondServer <- getAnotherServer(firstServer, 0);
    #print(cat("Second Server is", secondServer))
    
    #send the request again and check if failed again
    if(!simulateBernoulliTrial(servers[secondServer])){
      
      #print(cat("Second Server failed", secondServer));
      #choose another server and send a request. 
      thirdServer <- getAnotherServer(firstServer, secondServer);
      #print(cat("Final Server", thirdServer))
      
      #Send the request again and if successful mark success
      if(simulateBernoulliTrial(servers[thirdServer])){
        #print("Success")
        successes <- successes + 1;
      } else{
        #print("Fail")
        failures <- failures + 1;
      }
      
      #the experiment has been concluded so add 1
      i <- i+1;
    }
  } 
}

print(cat("Total Successes ", successes));
print(cat("Total Failures ", failures));



