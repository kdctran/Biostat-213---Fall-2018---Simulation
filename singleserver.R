# One-teller problem
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
  T<-8				#time is 8 hours
  A <- 0      #arrival time
  D <- 0      #departure time
  lambda<-10  #rate of poisson process
  Na <-0      #number of arrivals
  Nd <-0      #number of departures
  u1 <-runif(1)
  #t <- t -1/lambda*log(u1)
  n  <-0      #number of customer at time t
  Td <- 9999
  
  while(t<T)
  {
    #generate arrival times
    case <-0  #number of events (arrivals?) at time t
    while (case==0)
    {
      u2 <-runif(1)
      lt <- 1/ ((t -4)^2 + .5)  #intensity function lambda(t)

      lambda.t <- lt/lambda
      if (u2 <= lambda.t)
      { u1 <- runif(1)
      T0 <- t  - 1/lambda*log(u1)
      Ta <- T0
      case<- case + 1
      }
    }
    # print(Ta)
    # print(case)
    
    #Case 1
    
    if (Ta <= Td & Ta <= T)
    {
      t <- Ta				#updating time variable
      Na <- Na + 1			#add additional arrival at time ta
      n <- n + 1			#add additional customer
      A[Na] <- t			#output data (customer Na arrived at time t
      
      case <-0
      while (case==0)
      {
        u2 <- runif(1)
        lt <- 1/ ((t -4)^2 + .5)
        
        if (u2 <=  (lt/ lambda))
        { u1 <- runif(1)
        Tt <-  t  - (1/lambda)*log(u1)   #generate time of next arrival
        Ta <- Tt			 #update time	
        case<- case + 1
        }
      }
      
      if (n==1)		#if n=1 we need to generate Y and reset
      {		    #td=t+Y because system was empty and td=infty
        Y <- rgamma(1,1,10)
        Td <- t + Y		
      }
    }

    #CASE 2
    
    if (Td < Ta & Td <=T)
    {
      t <- Td
      n <- n-1
      Nd <- Nd + 1
      D[Nd] <-t
      if (n == 0) 
      {Td <- 99999 
      }
      if (n > 0)
      {	
        Y <-  rgamma(1,1,10)
        Td <-t + Y
      }
    }
    
    #Case 3
    
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
    
    #Case 4
    
    if (min(Ta,Td) > T & n==0)
    {
      Tp<-max(t-T,0)
      Timepast[i] <-Tp
    }
  }
  
  cust.wait[i] <- sum(D-A)/Na
  depart[i] <-t
  i <- i+1
}
