## subroutine ##
## intensity rate in which the customers arrive ##
lambda.t <- function (time) {		# the banks open from 8am to 5pm, so we set 	
  if (time >= 4 && time <= 5) {	# the time from 0 to 9 and the rate is 4/hr 
    lambda <- 6		# but increase to 6 between 4 and 5.
  } else {lambda <- 4}
  list(lamt = lambda)
}
## determine which of the 3 tellers to go to, each with 1/3 probability
rand.teller <- function () {
  sample(c(1, 2, 3), size=1, replace=TRUE, prob=c(1/3, 1/3, 1/3))
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
  A1 <- numeric()	  # the arrival time of customer n
  A2 <- numeric()	  # the arrival time of customer n at server 2
  A3 <- numeric() 	# the arrival time of customer n at server 3
  D1 <- numeric()	  # the departure time of customer n
  D2 <- numeric()	  # the departure time of customer n at server 2
  D3 <- numeric() 	# the departure time of customer n at server 3
  D <- 0      #departure time
  Na1 <-0      #number of arrivals
  Na2 <-0      #number of arrivals
  Na3 <-0      #number of arrivals
  Nd1 <-0      #number of departures
  Nd2 <-0      #number of departures
  Nd3 <-0      #number of departures
  Nd <-0      #number of departures
  n1  <-0      #number of customer at time t
  n2  <-0      #number of customer at time t
  n3  <-0      #number of customer at time t
  Td1 <- 9999
  Td2 <- 9999
  Td3 <- 9999
  
  #generate arrival times
  Ta <- t - (1/lambda.t(t)$lamt)*log(runif(1))
  print(Ta)
  #select teller with equal 1/3 probability
  teller <- rand.teller()
  print(teller)
  
  while(t<T) {
    if (teller == 1) {
      #Case 1: somebody arrives before somebody departs and store is open
      if (Ta <= Td1 & Ta <= T)
      {
        t <- Ta				#updating time variable
        Na1 <- Na1 + 1			#add additional arrival at time ta
        n1 <- n1 + 1			#add additional customer
        A1[Na1] <- t			#output data (customer Na arrived at time t)
        Ta <- generate.Tt(Ta, t)
        teller <- rand.teller()
        
        if (n1==1)		#if n=1 we need to generate Y and reset
        {		    #td=t+Y because system was empty and td=infty
          Y1 <- rgamma(1,1,10)
          Td1 <- t + Y1
        }
      }
      
      #CASE 2: somebody departs before somebody arrives
      else if (Td1 < Ta & Td1 <=T)
      {
        t <- Td1
        n1 <- n1-1
        Nd1 <- Nd1 + 1
        D1[Nd1] <-t
        if (n1 == 0) 
        {Td1 <- 9999 
        }
        if (n1 > 0)  #there is customer in queue
        {	
          Y1 <-  rgamma(1,1,10)
          Td1 <- t + Y1
        }
      }
      
      #Case 3: store is closed but there is still customer
      else if (min(Ta,Td1) > T & n1 > 0)
      {
        while (n1 >0)
        {
          t <- Td1
          n1<- n1-1
          Nd1<- Nd1 + 1
          D1[Nd1] <-  t
          if (n1 >0)
          {
            Y1 <- rgamma(1,1,10)
            Td1 <-t + Y1
          }
        }
      }

      if(min(Ta, Td1) >= T) break
    }
    
    else if (teller == 2) {
      #Case 1: somebody arrives before somebody departs and store is open
      if (Ta <= Td2 & Ta <= T)
      {
        t <- Ta				#updating time variable
        Na2 <- Na2 + 1			#add additional arrival at time ta
        n2 <- n2 + 1			#add additional customer
        A2[Na2] <- t			#output data (customer Na arrived at time t)
        Ta <- generate.Tt(Ta, t)
        teller <- rand.teller()
        
        if (n2==1)		#if n=1 we need to generate Y and reset
        {		    #td=t+Y because system was empty and td=infty
          Y2 <- rgamma(1,1,10)
          Td2 <- t + Y2
        }
      }
      
      #CASE 2: somebody departs before somebody arrives
      else if (Td2 < Ta & Td2 <=T)
      {
        t <- Td2
        n2 <- n2-1
        Nd2 <- Nd2 + 1
        D2[Nd2] <-t
        if (n2 == 0) 
        {Td2 <- 9999 
        }
        if (n2 > 0)  #there is customer in queue
        {	
          Y2 <-  rgamma(1,1,10)
          Td2 <- t + Y2
        }
      }
      
      #Case 3: store is closed but there is still customer
      else if (min(Ta,Td2) > T & n2 > 0)
      {
        while (n2 >0)
        {
          t <- Td2
          n2<- n2-1
          Nd2<- Nd2 + 1
          D2[Nd2] <-  t
          if (n2 >0)
          {
            Y2 <- rgamma(1,1,10)
            Td2 <-t + Y2
          }
        }
      }
      
      if(min(Ta, Td2) >= T) break
    }
    
    else if (teller == 3) {
      #Case 1: somebody arrives before somebody departs and store is open
      if (Ta <= Td3 & Ta <= T)
      {
        t <- Ta				#updating time variable
        Na3 <- Na3 + 1			#add additional arrival at time ta
        n3 <- n3 + 1			#add additional customer
        A3[Na3] <- t			#output data (customer Na arrived at time t)
        Ta <- generate.Tt(Ta, t)
        teller <- rand.teller()
        
        if (n3==1)		#if n=1 we need to generate Y and reset
        {		    #td=t+Y because system was empty and td=infty
          Y3 <- rgamma(1,1,10)
          Td3 <- t + Y3
        }
      }
      
      #CASE 2: somebody departs before somebody arrives
      else if (Td3 < Ta & Td3 <=T)
      {
        t <- Td3
        n3 <- n3-1
        Nd3 <- Nd3 + 1
        D3[Nd3] <-t
        if (n3 == 0) 
        {Td3 <- 9999 
        }
        if (n3 > 0)  #there is customer in queue
        {	
          Y3 <-  rgamma(1,1,10)
          Td3 <- t + Y3
        }
      }
      
      #Case 3: store is closed but there is still customer
      else if (min(Ta,Td3) > T & n3 > 0)
      {
        while (n3 >0)
        {
          t <- Td3
          n3<- n3-1
          Nd3<- Nd3 + 1
          D3[Nd3] <-  t
          if (n3 >0)
          {
            Y3 <- rgamma(1,1,10)
            Td3 <-t + Y3
          }
        }
      }
      
      if(min(Ta, Td3) >= T) break
    }
  }
  
  # cust.wait[i] <- sum(D-A)/Na
  # depart[i] <-t
  # i <- i+1
}

length(A1)
length(D1)
