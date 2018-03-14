# Create matrix with different state labels for rows and columns
# The goal is to count state transitions among states.

counttrans1 = function(ts,type='continuous'){
  N = max(ts)
  m = length(unique(ts))
  if (type=='continuous'){
    ind = which(diff(ts)!=0)
    if (length(ind)!=0){
      ts = c(ts[ind],ts[length(ts)])
    }
  }
  transmat = matrix(0,nrow=m,ncol=m)
  rownames(transmat) = unique(ts)
  colnames(transmat) = unique(ts)
  ts = as.character(ts)
  for (i in 1:(length(ts)-1)){
    transmat[ts[i],ts[i+1]] = transmat[ts[i],ts[i+1]] + 1
  }
  return(transmat)
}

###################################################################
# Test the examples
# Example 1: continuous time where repeating state in a sequence is not allowed
ts = c(1,1,2,3,2,1,4,5,1)
transmat = counttrans1(ts)

# Example 2: continuous time where the smallest state is not 1.
ts = c(2,3,4,7,5,2)
transmat = counttrans(ts)

# Example 3: discrete time where repeating state in a sequence is allowed
ts = c(1,1,2,3,2,1,4,5,1)
transmat = counttrans1(ts,'discrete')
