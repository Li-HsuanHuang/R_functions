# Goal: Fitting continuous-time Markov chains so that it will apply to any number of 
# states, regardless of whether the minimum is 1.

# Count state transitions
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

# Obtain time spent in each state
statetimetally1 = function(ts,tseq){
  stally = numeric(length(unique(ts)))
  if (any(diff(ts)<= 0) == T){
    cumtseq = cumsum(tseq)
  }
  else {
    cumtseq = tseq
  }
  ind = which(diff(ts)!=0)
  ts1 = c(ts[ind],ts[length(ts)])
  tseq1 = c(cumtseq[ind],cumtseq[length(cumtseq)])
  diffseq = diff(tseq1)
  #print(ts1)
  #print(tseq1)
  names(stally) = unique(ts1)
  ts1 = as.character(ts1)
  #print(ts1)
  for (i in 1:(length(ts1)-1)){
    stally[ts1[i+1]] = stally[ts1[i+1]] + diffseq[i]
  }
  stally[ts1[1]] = stally[ts1[1]]+tseq1[1]
  return(stally)
} 

# Obtain transition-rate matrix
trainCTMC1 = function(ts,tseq){
  transmat = counttrans1(ts)
  timespent = statetimetally1(ts,tseq)
  transratemat = transmat
  for (i in 1:nrow(transmat)){
    transratemat[i,] = transratemat[i,]/timespent[i]
    transratemat[i,i] = - sum(transratemat[i,])
  }
  return(transratemat)
}

# Function for compute expected time spent before absorption
# ds: dead-end state.
expected_time = function(transratemat,ds){
  m = nrow(transratemat)
  #transratemat = trainCTMC(ts,cumtseq,m)
  A = transratemat[-ds,-ds]
  M = solve(-A)
  E = M%*%rep(1,m-length(ds))
  return(list(M = M,E = E))
}

###########################################################
# Examples
# Example 1
ts = c(1,1,2,3,2,1,4,5,1)   #states
tseq = c(2,3,1,4,5,7,8,1,1) #times
transratemat = trainCTMC1(ts,tseq)
ds = unique(ts)[1]
r = expected_time(transratemat,ds = ds)
r$M  # expected time to go from any state to another before absorption
r$E  # expected time to go from any state to absorbing state.

# Example 2
ts = c(1,1,2,3,2,1)
tseq = c(1,1,1,1,1,1)
transratemat = trainCTMC1(ts,tseq)
ds = unique(ts)[3]
r = expected_time(transratemat, ds = ds)

# Example 3
ts = c(2,3,4,7,5,2)
tseq = c(1,1,1,1,1,1)
transratemat = trainCTMC1(ts,tseq)
ds = which(unique(ts)==min(ts)) # smallest state/absoring state
r = expected_time(transratemat,ds = ds)

