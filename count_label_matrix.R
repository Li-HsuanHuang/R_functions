# Goal: Create a count transition matrix given different time series.

# Create count matrix
#     labs: labels for row and column names, 
#     N : length of labs
counttrans = function(ts,type='continuous',N,labs){
  if (type=='continuous'){
    ind = which(diff(ts)!=0)
    if (length(ind)!=0){
      ts = c(ts[ind],ts[length(ts)])
    }
  }
  transmat = matrix(0,nrow=N,ncol=N)
  colnames(transmat) = labs 
  rownames(transmat) = labs
  ts = as.character(ts)
  for (i in 1:(length(ts)-1)){
    transmat[ts[i],ts[i+1]] = transmat[ts[i],ts[i+1]] + 1
  }
  return(transmat)
}


#############################################################
# Experiment with time series data
# Consider partial ALS states observed data
# Each row corresponds to a different time series 
statesData = read.csv("Astates.csv")
statesData = trunc(statesData)

# N : number of time series we want
N = 30
A = statesData[1:N,]
t = apply(A,1,function(x) unique(x[!is.na(x)]))
t = unlist(t)
ustates = unique(t)
num = length(ustates)
counts = matrix(0,nrow = num,ncol = num)
rownames(counts) = ustates
colnames(counts) = ustates
timespent = numeric(num)
names(timespent) = ustates
for (i in 1:20){
  states = as.numeric(statesData[i,!is.na( statesData[i,])])
  if (length(states) > 1){
    counts = counts + counttrans(states,type = 'continuous',num,ustates)
  }
}



