# The function for computing the Ichimoku cloud
# http://www.quantf.com/blog/fotis-papailias/ichimoku-clouds-r-code-trading/938
ichimoku <- function(data,pars)
{

    #     data <- x

# REMEMBER THAT THE DATA SHOULD BE IN ORDER
#
# HIGH, LOW and CLOSE
#
# ==========================================

# Number of observations
Nobs <- NROW(data)

# Get the three parameters
p1 <- pars[1]
p2 <- pars[2]
p3 <- pars[3]

# The maximum of these should be p3, check
if ((p1 > p2) | (p1 > p3) | (p2 > p3))
{
stop("parameters should enter in ascending order")
}
# Set the max
maxp <- p3

# You will leave out maxp observations
cloud.lines <- matrix(0,nrow=Nobs-maxp,ncol=5)
colnames(cloud.lines) <- c("Tenkan","Kijun","SenkouA","SenkouB","Chikou")

# Run a loop to make the computations
for (i in seq(maxp+1,Nobs,1))
{
# Compute the cloud lines
tenkan <- (max(data[seq(i-p1,i,1),1])+min(data[seq(i-p1,i,1),2]))/2
kijun <- (max(data[seq(i-p2,i,1),1])+min(data[seq(i-p2,i,1),2]))/2
senkouA<- (tenkan+kijun)/2
senkouB<- (max(data[seq(i-p3,i,1),1])+min(data[seq(i-p3,i,1),2]))/2
chikou <- data[i,3]

# Save in appropriate places
cloud.lines[(i-maxp),] <- c(tenkan,kijun,senkouA,senkouB,chikou)
}

# OK, now align them correctly: SenkouA and SenkouB are moved p2 periods forward
# while Chikou is moved p2 periods backward…
A1 <- rbind(cloud.lines[,1:2],matrix(NA,p2,2))
A2 <- rbind(matrix(NA,p2,2),cloud.lines[,3:4])
A3 <- c(cloud.lines[(p2+1):(Nobs-maxp),5],matrix(NA,2*p2,1))
new.cloud.lines <- cbind(A1,A2,A3)
colnames(new.cloud.lines) <- colnames(cloud.lines)

# Align the data as well
new.data <- rbind(data[(maxp+1):Nobs,],matrix(NA,p2,3))
colnames(new.data) <- colnames(data)

# OK, return everything
return(list(data=new.data,cloud.lines=new.cloud.lines))
# return(new.cloud.lines)
}
