# Goal of this code: To compute expected visits to states before absorption.

# Consider a probability transition matrix P.
# State 1 is the absorbing state.

row1 = c(1,0,0,0,0)
row2 = c(1/2,0,1/2,0,0)
row3 = c(0,1/2,0,1/2,0)
row4 = c(0,0,1/2,0,1/2)
row5 = c(0,0,0,1,0)
P = matrix(c(row1,row2,row3,row4,row5),nrow=5,byrow=T)

# Q is substochastic matrix
Q = P[2:5,2:5]
I = diag(4)

# M = (I-Q)^(-1)
# Starting at a state i, the i-th row of the matrix tells us the expected number
# of visits to another state before absorption.
M = solve(I-Q)

# The expected number of visits to get from state i to absoprtion is 
visits = M%*%c(1,1,1,1)
