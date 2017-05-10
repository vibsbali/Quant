#declare a vector with probabilities of SUCCESS
servers <- c(0.80, 0.60, 0.40, 0.20, 0)
successes <- 0;
failures <- 0;
numberOfExperiments <- 1000000;

#Following function will replicate sending the request twice 
tryTwice <- function(probOfSuccess) {
  result <- rbinom(2, 1, probOfSuccess)
  return(result);
}

#function that provides another server when the request is failed twice
getAnotherServer <- function(existingServer) {
  anotherServer <- sample(1:5, 1);
  #the following loop is to ensure that the previously failed server doesn't get returned 
  while(anotherServer == existingServer){
    anotherServer <- sample(1:5, 1);
  }
  return(anotherServer);
}

#run loop and simulate a server request using binomial experiment
i <- 1;
while(i <= numberOfExperiments){
  #randomly select a server to send the request to
  selectedServer <- sample(1:5, 1);
  
  result <- tryTwice(servers[selectedServer])
  #check if both times the server rejected the request
  if(result[1] == 0 && result[2] == 0) {
      
      #Server failed twice, Choose another server and send a request. 
      secondServer <- getAnotherServer(selectedServer);
      
      #Send the request again and if successful 
      if(rbinom(1, 1,  servers[secondServer])){
        successes <- successes + 1;
      } else{
        failures <- failures + 1;
      }
      
      #the experiment has been concluded so add 1
      i = i+1;
    }
  }
  


print(cat("Total Successes ", successes));
print(cat("Total Failures ", failures));



