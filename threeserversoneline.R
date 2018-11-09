## intensity rate in which the customers arrive ##
lambda.t <- function(time){		# the banks open from 8am to 5pm, so we set 	
  if(time >= 4 && time <= 5){	# the time from 0 to 9 and the rate is 4/hr 
    lambda <- 6		# but increase to 6 between 4 and 5.
  } else { lambda <- 4 }
  return(lambda)
}

generate.Ta <- function(ta, t) {
  Tt <- t - (1/lambda.t(t))*log(runif(1))	# next arrival time
  
  if (ta < 4) {		# ta is less than 4
    ta <- Tt
    if (ta > 4) { 
      ta <- 4+((ta-4)*4/6) 
    }	#if greater than 4, change rate	
  } else if (4 <= ta && ta <= 5) {	# 4 <= ta <= 5
    ta <- Tt
    if (ta > 5) { 
      ta <- 5 + ((ta-5)*6/4) 
    }	#if greater than 5 , change rate
  } else { 
    ta <- Tt
  }
  
  if ( ta > 9)  ta <- 9999	# To insure that nobody can enter the system
  
  return(ta)
}	

t = 0
ta <- t <- t - (1 / lambda.t(t)) * log(runif(1))

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
i1 <- 0       #index of customer at server 1
i2 <- 0       #index of customer at server 2
i3 <- 0       #index of customer at server 3
Na <- 0       #number of arrivals
Nd <- 0       #number of departures
n <- 0        #number of customer at time t
c1 <- 0       #number of customer served by server 1 by time t
c2 <- 0       #number of customer served by server 2 by time t
c3 <- 0       #number of customer served by server 2 by time t
Td1 <- 9999   #service completion time server 1
Td2 <- 9999   #service completion time server 2
Td3 <- 9999   #service completion time server 3

set.seed(213)

# Generate first arrival time
Ta <- t - (1 / lambda.t(t)) * log(runif(1))

while (t < T) {
  
  # Case 1: someone arrives before someone departs (servers idle or serving)
  
  if (Ta == min(Ta, Td1, Td2, Td3)) {
    t <- Ta				#updating time variable
    Na <- Na + 1			#add additional arrival at time ta
    A[Na] <- t			#output data (customer Na arrived at time t
    
    # Generate next arrival
    Ta <- generate.Ta(Ta, t)
    
    # If no customer in the store 
    if (n == 0 & Td1 == 9999 & Td2 == 9999 & Td3 == 9999) {
      n <- 1
      i1 <- Na
      Y1 <- rgamma(1,1,10)
      Td1 <- t + Y1	
    }
    
    #If there's a customer already in a store being serviced by server 1
    if (n == 1 & Td1 != 9999 & Td2 == 9999 & Td3 == 9999) {
      n <- 2
      i2 <- Na
      Y2 <- rgamma(1,1,10)
      Td2 <- t + Y2	
    }
    
    #If there's a customer already in a store being serviced by server 2
    if (n == 1 & Td1 == 9999 & Td2 != 9999 & Td3 == 9999) {
      n <- 2
      i1 <- Na
      Y1 <- rgamma(1,1,10)
      Td1 <- t + Y1	
    }
    
    #If there's a customer already in a store being serviced by server 3
    if (n == 1 & Td1 == 9999 & Td2 == 9999 & Td3 != 9999) {
      n <- 2
      i1 <- Na
      Y1 <- rgamma(1,1,10)
      Td1 <- t + Y1	
    }
    
    #If server 1 & 2 are busy
    if (n == 2 & Td1 != 9999 & Td2 != 9999 & Td3 == 9999) {
      n <- 3
      i3 <- Na
      Y3 <- rgamma(1,1,10)
      Td3 <- t + Y3	
    }
    
    #If server 1 & 3 are busy
    if (n == 2 & Td1 != 9999 & Td2 == 9999 & Td3 != 9999) {
      n <- 3
      i2 <- Na
      Y2 <- rgamma(1,1,10)
      Td2 <- t + Y2	
    }
    
    #If server 2 & 3 are busy
    if (n == 2 & Td1 == 9999 & Td2 != 9999 & Td3 != 9999) {
      n <- 3
      i1 <- Na
      Y1 <- rgamma(1,1,10)
      Td1 <- t + Y1	
    }
    
    #If all servers are busy
    if (n > 2 & Td1 != 9999 & Td2 != 9999 & Td3 != 9999) {
      n <- n + 1
    }
  }
  
  #Case 2: someone departs from server 1
  #SS = (n, i1, i2, i3) and Td1 < Ta and Td1 <= Td2, Td3
  if (Td1 < Ta & Td1 <= min(Td2, Td3)) {
    t <- Td1
    c1 <- c1 + 1
    Nd <- Nd + 1
    D[i1] <- t
    if (n == 1) {
      n <- 0
      i1 <- 0
      Td1 <- 9999
    }
  
    if (n == 2) {
      n <- 1
      i1 <- 0
      Td1 <- 9999
    }
    
    if (n == 3) {
      n <- 2
      i1 <- 0
      Td1 <- 9999
    }
    
    if (n > 3) {
      n <- n - 1
      m = max(i1, i2, i3)
      i1 <- m + 1
      Y1 < rgamma(1, 1, 10)
      Td1 <- t + Y1
    }
  }
  
  #Case 3: someone departs from server 2
  t <- Td2
  c2 <- c2 + 1
  Nd <- Nd + 1
  D[i2] <- t
  if (n == 1) {
    n <- 0
    i2 <- 0
    Td2 <- 9999
  }
  
  if (n == 2) {
    n <- 1
    i2 <- 0
    Td2 <- 9999
  }
  
  if (n == 3) {
    n <- 2
    i2 <- 0
    Td2 <- 9999
  }
  
  if(n > 3){
    n <- n - 1
    m <- max(i1, i2, i3)
    i2 <- m + 1
    Y2 <- rgamma(1, 1, 10)
    Td2 <- t + Y2
  }
  
  #Case 4: someone departs from server 3
  t <- Td3
  c3 <- c3 + 1
  Nd <- Nd + 1
  D[i3] <- t
  if (n == 1) {
    n <- 0
    i3 <- 0
    Td3 <- 9999
  }
  
  if (n == 2) {
    n <- 1
    i3 <- 0
    Td3 <- 9999
  }
  
  if (n == 3) {
    n <- 2
    i3 <- 0
    Td3 <- 9999
  }
  
  if (n > 3) {
    n <- n - 1
    m <- max(i1, i2, i3)
    i3 <- m + 1
    Y3 <- rgamma(1, 1, 10)
    Td3 <- t + Y3
  }
  
  #Case 5: store is closed 
}
