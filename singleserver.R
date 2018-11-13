# One-teller problem

## subroutine ##
## intensity rate in which the customers arrive ##
lambda.t <- function (time) {		# the banks open from 8am to 5pm, so we set 	
  if (time >= 4 && time <= 5) {	# the time from 0 to 9 and the rate is 4/hr 
    lambda <- 6		# but increase to 6 between 4 and 5.
  } else {lambda <- 4}
  list(lamt = lambda)
}

### subroutine ###
### Generate Tt which will be assigned to ta: next arrival time ###

generate.Tt <- function(ta, t) {
  Tt <- t- (1/lambda.t(t)$lamt)*log(runif(1))	# next arrival time
  
  if (ta < 4) {		# ta is less than 4
    ta <- Tt
    if (ta > 4) {ta <- 4 + ((ta - 4) * 4 / 6)}	#if greater than 4, change rate	
  } else if (4 <= ta && ta <= 5) {	# 4 <= ta <= 5
    ta <- Tt
    if (ta > 5) {ta <- 5 + ((ta - 5) * 6 / 4)}	#if greater than 5 , change rate
  } else { 
    ta <- Tt
  }
  if ( ta > 9)  ta <- 9999	# To insure that nobody can enter the system after 
  return(ta)
}	
set.seed(213)
N<-100
cust.wait <- numeric(N)
Tp <- numeric(N)
depart <-numeric(N)

Timepast <-numeric(N)
i<-0

# lt = function(t){1/ ((t -4)^2 + .5)}
# plot(lt(1:15), type='l')

for (i in 1:1) {		#starting the loop and initializing
  t<-0				#time starts at 0
  T<-9				#time is 8 hours
  A <- 0      #arrival time
  D <- 0      #departure time
  Na <-0      #number of arrivals
  Nd <-0      #number of departures
  n  <-0      #number of customer at time t
  Td <- 9999

  #generate arrival times
  Ta <- t - (1/lambda.t(t)$lamt)*log(runif(1))
  print(Ta)
  
  while(t<T)
  {
    #Case 1: somebody arrives before somebody departs and store is open
    if (Ta <= Td & Ta <= T)
    {
      t <- Ta				#updating time variable
      Na <- Na + 1			#add additional arrival at time ta
      n <- n + 1			#add additional customer
      A[Na] <- t			#output data (customer Na arrived at time t)
      Ta <- generate.Tt(Ta, t)
      print(paste("next Ta = ", Ta))

      if (n==1)		#if n=1 we need to generate Y and reset
      {		    #td=t+Y because system was empty and td=infty
        Y <- rgamma(1,1,10)
        Td <- t + Y
      }
    }

    #CASE 2: somebody departs before somebody arrives
    if (Td < Ta & Td <=T)
    {
      t <- Td
      n <- n-1
      Nd <- Nd + 1
      D[Nd] <-t
      if (n == 0) 
      {Td <- 9999 
      }
      if (n > 0)  #there is customer in queue
      {	
        Y <-  rgamma(1,1,10)
        Td <- t + Y
      }
    }
    
    #Case 3: store is closed but there is still customer
    if (min(Ta,Td) > T & n > 0)
    {
      while (n >0)
      {
        t <- Td
        n<- n-1
        Nd<- Nd + 1
        D[Nd] <-  t
        if (n >0)
        {
          Y <- rgamma(1,1,10)
          Td <-t + Y
        }
      }
    }
    
    #Case 4: close and no more customer
    if (min(Ta,Td) > T & n==0)
    {
      Tp<-max(t-T,0)
      Timepast[i] <-Tp
    }
    if(min(Ta, Td) >= T) break
  }
  
  cust.wait[i] <- sum(D-A)/Na
  depart[i] <-t
  i <- i+1
}

table <- cbind(A, D)
table