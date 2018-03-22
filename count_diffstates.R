# Create matrix with different state labels for rows and columns
# The goal is to count state transitions among states.


genTransMat = function(stateList,timeList = NULL,discrete = TRUE){
  if(discrete){
    return( genTransMatDis(stateList) )
  }
  else{
    return( genTransMatCont(stateList,timeList) )
  }
}

###################################################################
# Test the examples
# Example 1: continuous time where repeating state in a sequence is not allowed
# timeSeries_1 = c(1,1,2,3,2,1,4,5,1)
# transmat_1 = genTransMat(timeSeries_1,FALSE)

# Example 2: continuous time where the smallest state is not 1.
# timeSeries_2 = c(2,3,4,7,5,2)
# transmat_2 = genTransMat(timeSeries_2,FALSE)

# Example 2: increasing by one each time discrete time step
timeSeries_2 = list()
timeSeries_2[[1]] = c(1,2,3,4,5)
transmat_2 = genTransMat(timeSeries_2)
print(transmat_2)

# Example 3: discrete time where repeating state in a sequence is allowed
timeSeries_3 = list()
timeSeries_3[[1]] = c(1,1,2,3,2,1,4,5,3)
transmat_3 = genTransMat(timeSeries_3)
print(transmat_3)

timeSeries_4 = list()
timeSeries_4[[1]] = c(1,2,3,6,8)
timeSeries_4[[2]] = c(1,2,3,6,8,10,11,12)
timeSeries_4[[3]] = c(10,11,12)
transmat_4 = genTransMat(timeSeries_4)
print(transmat_4)