#declare a vector with probabilities of FAILURE
servers <- c(0.20, 0.40, 0.60, 0.80, 1)
successes <- 0;
failures <- 0;
numberOfExperiments <- 100000;

#set seed
#set.seed(0)

#Following function will give an indicator random variable telling whether the request has been accepted
# or rejcted on basis of the selected server's probability. 0 means rejected 
generateIndicator <- function(probOfFailure) {
  #print(cat("probOfFailure ", probOfFailure));
  result <- sample(0:1, 1, replace=F,prob=c(probOfFailure, (1-probOfFailure)))
  return(result);
}

#function that provides another server when the request is failed twice
getAnotherServer <- function(existingServerOne, existingServerTwo) {
  anotherServer <- sample(1:5, 1);
  #the following loop is to ensure that the existing server is not returned 
  while(anotherServer == existingServerOne || anotherServer == existingServerTwo){
    anotherServer = sample(1:5, 1);
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
  if(!generateIndicator(servers[firstServer])){
    
    #choose another server and send a request. 
    secondServer <- getAnotherServer(firstServer, 0);
    #print(cat("Second Server is", secondServer))
    
    #send the request again and check if failed again
    if(!generateIndicator(servers[secondServer])){
      
      #print(cat("Second Server failed", selectedServer));
      #choose another server and send a request. 
      thirdServer <- getAnotherServer(firstServer, secondServer);
      #print(cat("Final Server", thirdServer))
      
      #Send the request again and if successful mark success
      if(generateIndicator(servers[thirdServer])){
        #print("Success")
        successes = successes + 1;
      } else{
        #print("Fail")
        failures = failures + 1;
      }
      
      #the experiment has been concluded so add 1
      i = i+1;
    }
  } 
}

print(cat("Total Successes ", successes));
print(cat("Total Failures ", failures));



