# Fit continuous-Markov chains on an arbitrary time-series data
# and compute the expected time spent from any state before absorption

statetimetally = function(ts,tseq){
  stally = numeric(max(ts))
  if (any(diff(ts)<= 0) == T){
    cumtseq = cumsum(tseq)
  }
  ind = which(diff(ts)!=0)
  ts1 = c(ts[ind],ts[length(ts)])
  tseq1 = c(cumtseq[ind],cumtseq[length(cumtseq)])
  diffseq = diff(tseq1)
  for (i in 1:(length(ts1)-1)){
    stally[ts1[i+1]] = stally[ts1[i+1]] + diffseq[i]
  }
  stally[ts1[1]] = stally[ts1[1]]+tseq1[1]
  return(stally)
} 

# This function creates a transition matrix recording state transitions.
counttrans = function(ts,type='continuous'){
  N = max(ts)
  if (type=='continuous'){
    ind = which(diff(ts)!=0)
    if (length(ind)!=0){
        ts = c(ts[ind],ts[length(ts)])
    }
    else {
      break
    }
  }
  transmat = matrix(0,N,N)
  for (i in 1:(length(ts)-1)){
    transmat[ts[i],ts[i+1]] = transmat[ts[i],ts[i+1]] + 1
  }
  return(transmat)
}


# Function for creating a CTMC matrix given states, times, and the number of states.
trainCTMC = function(ts,tseq,N){
  transmat = counttrans(ts,'continuous')
  timespent = statetimetally(ts,tseq)
  transratemat = transmat
  for (i in 1:nrow(transmat)){
    transratemat[i,] = transratemat[i,]/timespent[i]
    transratemat[i,i] = - sum(transratemat[i,])
  }
  return(transratemat)
}


# Function for compute expected time spent before absorption
# ds: dead-end state(s) (absorbing state(s))
expected_time = function(transratemat,ds){
  m = nrow(transratemat)
  #transratemat = trainCTMC(ts,cumtseq,m)
  A = transratemat[-ds,-ds]
  M = solve(-A)
  E = M%*%rep(1,m-length(ds))
  return(E)
}

###########################################################################
# Example 
ts = c(1,1,2,3,2,1,4,5,1)   # states
tseq = c(2,3,1,4,5,7,8,1,1) # time spent in each associated state

# Train CTMC
N = max(ts)
transratemat = trainCTMC(ts,tseq,N)

# Supply absorbing states.
ds = c(1,2)

# Compute expected time to get from state i to any absorbing state in ds.
exp_time = expected_time(transratemat,ds)
print(exp_time)
