#################################################################
#This code comes from the supplementary materials in Kruschke (2015). *Doing
#Bayesian data analysis: A tutorial with R, JAGS, and Stan* (Ch. 7). Academic Press.
#The code can be found here: 
#https://sites.google.com/site/doingbayesiandataanalysis/exercises
# It was adapted by Nicole Cruz and Julia Haaf
#################################################################

# Specify the data, to be used in the likelihood function.
myData <- c(rep(0,6), rep(1,14))

# Define the Bernoulli likelihood function, p(D|theta).
# The argument theta could be a vector, not just a scalar.
likelihood <- function( theta , data ) {
  z = sum( data )
  N = length( data )
  pDataGivenTheta = theta^z * (1-theta)^(N-z)
  # The theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # The likelihood for theta > 1 or for theta < 0 is zero:
  pDataGivenTheta[ theta > 1 | theta < 0 ] = 0
  return( pDataGivenTheta )
}

# Define the prior density function. 
prior <- function( theta, alpha , beta ) {
  pTheta = dbeta( theta , alpha , beta )
  # The theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # The prior for theta > 1 or for theta < 0 is zero:
  pTheta[ theta > 1 | theta < 0 ] = 0
  return( pTheta )
}

# Define the relative probability of the target distribution, 
# as a function of vector theta. For our application, this
# target distribution is the unnormalized posterior distribution.
targetRelProb <- function( theta , data, alpha , beta ) {
  targetRelProb =  likelihood( theta , data ) * prior( theta, alpha , beta )
  return( targetRelProb )
}

# Specify the length of the trajectory, i.e., the number of jumps to try:
NSamples <- 10000 # arbitrary large number
# Initialize the vector that will store the results:
theta <- rep( 0 , NSamples )
# Specify where to start the trajectory:
theta[1] <- 0.01 # arbitrary value, ideally between 0 and 1
# Specify the burn-in period:
burnIn <- 50 # The first samples are removed as they are typically used to tune the algorithm
# Initialize accepted, rejected counters, just to monitor performance:
nAccepted <- 0
nRejected <- 0

# Now generate the random walk. The 't' index is time or trial in the walk.
# Specify seed to reproduce same random walk:
# set.seed(47405) # comment out to create different graphs

# Specify standard deviation of proposal distribution:
proposalSD <- c(0.02,0.2,2.0)[2] # vary the proposal standard deviation

# Specify priors
alpha <- 1
beta <- 1

for ( t in 1:(NSamples - 1) ) {
	currentPosition <- theta[t]
	
	# Use the proposal distribution to generate a proposed jump.
	proposedJump <- rnorm( 1 , mean=0 , sd=proposalSD )
	
	# Compute the probability of accepting the proposed jump.
	probAccept <- min( 1,
		targetRelProb( currentPosition + proposedJump , myData, alpha , beta )
		/ targetRelProb( currentPosition , myData, alpha , beta ) )
	
	# Generate a random uniform value from the interval [0,1] to
	# decide whether or not to accept the proposed jump.
	if ( runif(1) < probAccept ) {
		# accept the proposed jump
		theta[ t+1 ] <- currentPosition + proposedJump
		# increment the accepted counter, just to monitor performance
		if ( t > burnIn ) { nAccepted = nAccepted + 1 }
	} else {
		# reject the proposed jump, stay at current position
		theta[ t+1 ] <- currentPosition
		# increment the rejected counter, just to monitor performance
		if ( t > burnIn ) { nRejected = nRejected + 1 }
	}
}

# Extract the post-burnIn portion of the trajectory.
acceptedTheta <- theta[ (burnIn+1) : length(theta) ]

# End of Metropolis algorithm.

#-----------------------------------------------------------------------
# Display the chain.

layout( matrix(1:4,nrow=2) )
par(mar=c(3,4,2,1),mgp=c(2,0.7,0))

# Posterior histogram:
hist(acceptedTheta, xlim=c(0,1) , xlab=bquote(theta), freq = F, breaks = 50,
     , cex.main=1.5, main=bquote( list( "Posterior Distribution", "Prpsl.SD" == .(proposalSD) 
                                        , "Eff.Sz." == .(round(coda::effectiveSize(acceptedTheta),1)) ) ) )

# Autocorrelation
acf(acceptedTheta, cex = 1.2, main = "")

# Trajectory, a.k.a. trace plot, end of chain:
idxToPlot = (NSamples-100):NSamples
plot( theta[idxToPlot] , idxToPlot , main="End of Chain" , cex.main=1.5,
      xlab=bquote(theta) , xlim=c(0,1) , ylab="Step in Chain" ,
      type="o" , pch=20 , col="skyblue" , cex.lab=1.2 )
# Display proposal SD and acceptance ratio in the plot.
text( 0.0 , NSamples , adj=c(0.0,1.1) , cex=1.3 ,
      labels = bquote( frac(N[acc],N[pro]) == 
                       .(signif( nAccepted/length(acceptedTheta) , 3 ))))

# Trajectory, a.k.a. trace plot, beginning of chain:
idxToPlot = 1:100
plot( theta[idxToPlot] , idxToPlot , main="Beginning of Chain" , cex.main=1.5,
      xlab=bquote(theta) , xlim=c(0,1) , ylab="Step in Chain" ,
      type="o" , pch=20 , col="skyblue" , cex.lab=1.2 )
# Indicate burn in limit (might not be visible if not in range):
if ( burnIn > 0 ) {
  abline(h=burnIn,lty="dotted")
  text( 0.5 , burnIn+3 , "Burn In" , adj=c(0.5,1.1) )
}

#------------------------------------------------------------------------
