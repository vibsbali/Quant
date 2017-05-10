#declare a vector with probabilities of success
phone <- c(1, 0.0, 0.5)
successes <- 0;
failures <- 0;
numberOfExperiments <- 1000000;


#Following function will give an indicator random variable telling whether the call went through
# or failed on basis of the selected phone's probability of failure. 0 means the call did not go through 
callTwice <- function(probOfSuccess) {
  result <- rbinom(2, 1, probOfSuccess)
  return(result);
}

#run loop and simulate phone call using binomial experiment
i <- 1;
while(i <= numberOfExperiments){
  #randomly select a phone to send the request to
  selectedPhone <- sample(1:3, 1);
  #print(cat("Selected phone is ", selectedPhone));
  
  #call twice
  results <- callTwice(phone[selectedPhone]);
      #print(results)
      #Check if it was the phone that is the reliable
      if((results[1] == 1) && (results[2] == 1)) {
        
        if(selectedPhone == 1){
          successes = successes + 1;  
        } else {
        failures = failures + 1;
      }
      
      #the experiment has been concluded so add 1
      i = i+1;
      }
  }
   


print(cat("Total Successes ", successes));
print(cat("Total Failures ", failures));



