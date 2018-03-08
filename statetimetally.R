# This function takes time series whose components are states and times
# and return the amount of time spent in each state.
# This function is mainly used in the setting of obtaining an
# CTMC matrix.

statetimetally = function(ts,cumtseq){
  stally = numeric(max(ts))
  ind = which(diff(ts)!=0)
  ts1 = c(ts[ind],ts[length(ts)])
  tseq1 = c(cumtseq[ind],cumtseq[length(cumtseq)])
  diffseq = diff(tseq1)
  print(ts1)
  print(tseq1)
  print(diffseq)
  for (i in 1:(length(ts1)-1)){
    stally[ts1[i+1]] = stally[ts1[i+1]] + diffseq[i]
  }
  stally[ts1[1]] = stally[ts1[1]]+tseq1[1]
  return(stally)
} 


# Example to test the function
ts = c(1,1,2,3,2,1)   #states
tseq = c(2,3,1,4,5,7) #time spent in each associated state
cumtseq= cumsum(tseq)

result = statetimetally(ts,cumtseq)
print(result)
