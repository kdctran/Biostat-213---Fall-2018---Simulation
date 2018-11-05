set.seed(213)

N<-100
cust.wait <- numeric(N)
Tp <- numeric(N)
depart1 <-numeric(N)
depart2 <-numeric(N)
Timepast <-numeric(N)
i<-0

# Initialize variables
t <- 0			  #time starts at 0
T <- 9			  #time open is 9 hours
A <- 0        #arrival time
D1 <- 0       #departure time server 1
D2 <- 0       #departure time server 2
lambda <- 10  #rate of poisson process
Na <- 0       #number of arrivals
Nd <- 0       #number of departures
n <- 0        #number of customer at time t
c1 <- 0       #number of customer served by server 1 by time t
c2 <- 0       #number of customer served by server 2 by time t
Td1 <- 9999   #service completion time server 1
Td2 <- 9999   #service completion time server 2

# Generate first arrival time
u1 <- runif(1)
case <- 0  #number of events (arrivals?) at time t
while (case == 0) {
  u2 <- runif(1)
  lt <- 1 / ((t - 4)^2 + .5)  #intensity function lambda(t)
  lambda.t <- lt/lambda
  
  if (u2 <= lambda.t) { 
    u1 <- runif(1)
    T0 <- t  - 1/lambda*log(u1)
    Ta <- T0
    case <- case + 1
  }
}

# Case 1: someone arrives before someone departs (servers idle or serving)
if (Ta == min(Ta, Td1, Td2)) {
  t <- Ta				#updating time variable
  Na <- Na + 1			#add additional arrival at time ta
  n <- n + 1			#add additional customer
  A[Na] <- t			#output data (customer Na arrived at time t
  
  # Generate next arrival
  case <- 0  #number of events (arrivals?) at time t
  while (case == 0) {
    u2 <- runif(1)
    lt <- 1 / ((t - 4)^2 + .5)  #intensity function lambda(t)
    lambda.t <- lt/lambda
    
    if (u2 <= lambda.t) { 
      u1 <- runif(1)
      T0 <- t  - 1/lambda*log(u1)
      Ta <- T0
      case <- case + 1
    }
  }
  
  # If no customer already in the store 
  if (n == 0 & )
}

