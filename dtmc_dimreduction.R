# Goal: Dimensionality reduction of DTMC.
# Requirement: Each row sums to 1.

# Consider a 4 x 4 random DTMC. We want to reduce the matrix size to 2 x 2.
# This is particularly useful in data sets such as ALS where the states close among each other do not
# matter as much, e.g., states 3 and 4.

# Create a random 4 x 4 matrix whose states are 1, 2, 3, and 4.
r1 = rep(0.25,4)
r2 = c(1/3,1/3,0,1/3)
r3 = c(0,1/2,1/2,0)
r4 = c(0,3/5,0,2/5)
#r4 = c(0,0,0,1)
M = matrix(c(r1,r2,r3,r4),nrow=4,ncol=4,byrow = T)
evec = Re(eigen(t(M))$vectors[,1])
pivec = abs(evec)/sum(abs(evec))
print(pivec)
print(t(M))
# we can check that pivec%*% t(M) = pivec

# Now halve the columns to two, i.e, add first two columns and 
# and add last two columns 

# Mhat 4 x 2
newvec = pivec
newvec[1:2] = pivec[1:2]/sum(pivec[1:2])
newvec[3:4] = pivec[3:4]/sum(pivec[3:4])
M1 = M*newvec

W = matrix(0,nrow=2,ncol=2)
# A way to deal with 4 x 4 case 
for (i in c(1,3)){
  for (j in rep(c(1,3),2)){
    W[(i+1)/2,(j+1)/2]=sum(M1[i:(i+1),j:(j+1)])
  }
}

print(W)
