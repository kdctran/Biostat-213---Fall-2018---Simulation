# stackexchange example
# integrate (0 to pi/2) of x^2 cosx
# transform the integral from (0 to pi/2) to (0 to 1) 
# which tranforms the function to 1/x^2 sinx

f<-function(x) x^2 * cos(x)
mean(f(runif(100000,0,pi/2)))*(pi/2)
integrate(f,0,pi/2)

## OR

mc.integral = function(FUN, n.iter = 1000, interval){
  
  # take a sample using 'runif'
  x = runif(n.iter, interval[1], interval[2])
  
  # apply the user-defined function
  y = FUN(x)
  
  # calculate
  mean(y)*(interval[2] - interval[1])
}

FUN = function(x){x^2 * cos(x)}
integ = mc.integral(FUN, interval = c(0, pi/2))
print(integ)

g <- function(x) exp(-x^2 / 2) / sqrt(2 * pi)
a = 2
b = 5
c = min(g(a:b))

##-----------------------------##
n = 1e6
x <- runif(n)

## integrate (0 to 1) (1 - x^2)^(3/2) dx
f <- function(x) (1 - x^2)^(3/2)
I <- sum(f(x)) / n

integrate(f, 0, 1) # check answer

## integrate (0 to inf) x / (1 + x^2) dx
f<- function(x) x / (1 + x^2)
integrate(f, 0, Inf)

## integrate (-inf to inf) e^(x^2) dx

f<- function(x) exp(x^2)
integrate(f, lower = -Inf, upper = Inf)

## integrate (0 to inf) (0 to pi) e^(-(2x + y)) dydx

