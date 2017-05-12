#declare a vector with probabilities of FAILURE
servers <- c(0.20, 0.40, 0.60, 0.80, 1)
successes <- 0;
failures <- 0;
numberOfExperiments <- 1000000;

#Following function takes in the probability of failure and based on that returns 0 or 1
#0 means fail and 1 means success
simulateBernoulliTrial <- function(probOfFailure) {
  result <- sample(0:1, 1, replace=F,prob=c(probOfFailure, (1-probOfFailure)))
  return(result);
}

#function that provides another server when the request is failed twice
getAnotherServer <- function(existingServer) {
  anotherServer <- sample(1:5, 1);
  #the following loop is to ensure that the existing server is not returned 
  while(anotherServer == existingServer){
    anotherServer = sample(1:5, 1);
  }
  return(anotherServer);
}

#run loop and simulate a server request using binomial experiment
i <- 1;
while(i <= numberOfExperiments){
  #randomly select a server to send the request to
  selectedServer <- sample(1:5, 1);
  
  #send the request and check if failed first time
  if(!simulateBernoulliTrial(servers[selectedServer])){
    #print(cat("Selected Server is ", selectedServer));
    #send the request again and check if failed again
    if(!simulateBernoulliTrial(servers[selectedServer])){
      
      #print(cat("Server failed twice", selectedServer));
      
      #choose another server and send a request. 
      anotherServer <- getAnotherServer(selectedServer);
      #print(cat("New Server", anotherServer))
      
      #Send the request again and if successful mark success
      if(simulateBernoulliTrial(servers[anotherServer])){
        successes = successes + 1;
      } else{
        failures = failures + 1;
      }
      
      #the experiment has been concluded so add 1
      i = i+1;
    }
  } 
}

print(cat("Total Successes ", successes));
print(cat("Total Failures ", failures));



