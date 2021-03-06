library(numbers)

# Consider a square matrix of size N x N. 

# The smaller the divisor, the bigger the new matrix, not the same as the original N x N matrix.
d = divisors(N)
d = d[-c(1,length(d))]

new_mat = function(A,p){
 N = nrow(A)
 newmat = matrix(0,nrow=N/p,ncol=N/q)
 for (i in 1:(N/p)){
    for (j in 1:(N/q)){
      newmat[i,j] = sum(A[(p*(i-1)+1):(p*i),(p*(j-1)+1):(p*j)])
    }
 }
 return(newmat)
}

# Create a list to store all new matrices.
Mat = {}
k = 1
for (j in d){
   Mat[[k]] = new_mat(A,j)
   k = k + 1
}

print(Mat)

# Create transition matrices from Mat
B = abs(Mat[[1]])/rowSums(Mat[[1]])
evec = Re(eigen(t(B))$vectors[,1])
pivec = abs(evec)/sum(abs(evec))
print(pivec)
