# Create matrix with different state labels for rows and columns
# The goal is to count state transitions among states.

counttrans1 = function(timeSeries,type='continuous'){
  #N = max(timeSeries)
  m = length(unique(timeSeries))
  
  if (type=='continuous'){
    ind = which(diff(timeSeries)!=0)
    if (length(ind)!=0){
      timeSeries = c(timeSeries[ind],tail(timeSeries))
    }
  }
  
  transmat = matrix(0,nrow=m,ncol=m)
  
  sortRowNames = rownames(transmat) = sort(unique(timeSeries))
  sortColNames = colnames(transmat) = sort(unique(timeSeries))
  timeSeries = as.character(timeSeries)
  
  for (i in 1:(length(timeSeries)-1)){
    obsLocation = c( match(timeSeries[i],sortRowNames), match(timeSeries[i+1],sortColNames) )
    transmat[obsLocation[1],obsLocation[2]] = transmat[obsLocation[1],obsLocation[2]] + 1
  }
  return(transmat)
}

###################################################################
# Test the examples
# Example 1: continuous time where repeating state in a sequence is not allowed
timeSeries_1 = c(1,1,2,3,2,1,4,5,1)
transmat_1 = counttrans1(timeSeries_1)

# Example 2: continuous time where the smallest state is not 1.
timeSeries_2 = c(2,3,4,7,5,2)
transmat_2 = counttrans1(timeSeries_2)

# Example 3: discrete time where repeating state in a sequence is allowed
timeSeries_3 = c(1,1,2,3,2,1,4,5,3)
transmat_3 = counttrans1(timeSeries_3,'discrete')
