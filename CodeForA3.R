library(ggplot2)
library(tidyverse)
library(RColorBrewer)

# Constants
less.than.value <- 4499 # Hours used x-intercept
mu <- 4800 # mean in Hour
sigma <- 400 # sigma
rnorm.axis <- mu * 2 # Number of values to be randomly generated 


# The lifetime of a particular type of TV follows a normal distribution with mean =
# 4800 hours and equal std =400 hours.
# (a) Find the probability that a single randomly-chosen TV will last 
# less than 4,500 hours. Use R to assist with your computations.
# (b) Find the probability that the mean lifetime of a random sample
# of 16 TVs is less than 4,500 hours. Use R to assist with your computations.
# (c) Compare answers from (a) and (b).

# probability of vectors
# (a) Find the probability that a single randomly-chosen TV will last 
# less than 4,500 hours. Use R to assist with your computations.
# So the probability (success) of a TV lasting less than 4,500 hours is;
# https://www.theanalysisfactor.com/understanding-odds-and-probability/
p.aTV <- pnorm(less.than.value, mu, sigma) # Success
p.aTV


# Create random vectors of a normal distribution using mean, standard deviation 
# and x end and assign to variable named TV_Life_Time
# and then convert to a data.frame for use with ggplot
TV <- data.frame(x = rnorm(rnorm.axis, mu, sigma), y=rnorm(rnorm.axis, mu, sigma))

# Add to ggplot clustering
nClusters <- 7  # Cluster it
kMeans <- kmeans(TV, centers = nClusters)
TV$Cluster <- as.factor(kMeans$cluster)
plot <- ggplot(TV, aes(x=x, y=y)) +
               stat_density2d(aes(fill = Cluster, colour = Cluster,
                   alpha = ..level..),
               geom = "polygon")

# Creat a plot that shows where less than 4,500 sits on a 
# normal distribution
plotTV <- ggplot(TV, aes(x=x)) + 
  geom_density(alpha= 0.2, fill='red', size=1) +
  labs(title=('TV Life Time - Normal Distribution')) +
  #scale_x_continuous(
   # breaks = c(4, 4.5, mean.scaled, 5, 5.5, 6),
    #labels = c('4000', '4500', 'Mean', '5000','5500', '6000'),
    #name = "TV Life Time (Hours)") +
  scale_y_continuous(name = 'Density') + #, labels = scales::percent_format()) +
  theme_minimal()
plotTV 
# vertical line spliting the area of the normal distribution
# at which the probability become 1 - p
plotTV + geom_vline(xintercept = less.than.value.scaled)

#  geom_text(aes(y=1,label=aTV)) +


###

###
### https://stackoverflow.com/questions/3494593/shading-a-kernel-density-plot-between-two-points
###


# function that approximates the density at the provided values
approxdens <- function(x) {
  dens <- density(x)
  f <- with(dens, approxfun(x, y))
  f(x)
}

probsl <- c(0.00, p.aTV) # lower
#probsu <- c(0.885, 1.00) # upper
#probch <- c(aTV, p.bTV) # change from lower

TV2 <- TV %>%
  mutate(dy = approxdens(x),                         # calculate density
         p = percent_rank(x),                        # percentile rank 
         pcatl = as.factor(cut(p, breaks = probsl,   # percentile category 
                              include.lowest = TRUE))) #based on probs
         #pcatch = as.factor(cut(p, breaks = probch,  # percentile category 
         #                    include.lowest = TRUE))) # based on probs


# lower tail geom_ribbon 
ggplot(TV2 , aes(x, dy)) +
  geom_ribbon(aes(ymin = 0, ymax = dy, fill = pcatl)) +
  geom_line() +
  labs(title=('TV Life Time - Normal Distribution')) +
  scale_x_continuous(
    breaks = c(4000, less.than.value, mu, 5000, 5500, 6000),
    labels = c('4000', less.than.value, 'Mean', '5000','5500', '6000'),
    name = "TV Life Time (Hours)") +
  scale_y_continuous(name = 'Density') +
  scale_fill_brewer(palette = 4, guide = 'none') +
  theme_bw()

###

###

###

# Central limit theorum 
# n = 16
# Sampling distribution of the sample mean, we have the population mean 4800
# Sample Error of the mean
# trial histogram first
# (b) Find the probability that the mean lifetime of a random sample
# of 16 TVs is less than 4,500 hours. Use R to assist with your computations.

# Take a random sample of 16 TV's from a sample distribution of the sample mean
# equal to the population mean then find the mean of this subset.

# find standard error of the bTV
# if multiple samples were take then
# the standard error of bTV could be done
bTV <- sample(TV$x, 16, replace=FALSE)

# Find the probability of the sample distribution of the sample mean for n= 16
# using the 16 sampled TV's and samples standard deviation and mean
sd.bTV <- sd(bTV)
mean.bTV <- mean(bTV)
# mean 4800
# sd 400
se <- std/sqrt(1)
z <- (4500-4800)/se
pnorm(z)

SE <- std/sqrt(16)
Z <- (4500- 4800)/SE
pnorm(Z)

p.bTV <- pnorm(less.than.value, meanbTV, sd.bTV, lower.tail = TRUE)

pbinom(1, 16, p.bTV)

probsl <- c(0.00, p.aTV) # lower
#probsu <- c(0.885, 1.00) # upper
probch <- c(p.aTV, p.bTV)

TV2 <- TV %>%
  mutate(dy = approxdens(x),                         # calculate density
         p = percent_rank(x),                        # percentile rank 
         pcatl = as.factor(cut(p, breaks = probsl,   # percentile category lower 
                               include.lowest = TRUE)), #based on probs
         pcatch = as.factor(cut(p, breaks = probch,  # percentile category change
                                include.lowest = TRUE))) # based on probs


# pcatl lower tail geom_ribbon
# pcatch deviation from probability of population based on sample 16
ggplot(TV2 , aes(x, dy)) +
  geom_ribbon(aes(ymin = 0, ymax = dy, fill = pcatl)) +
  geom_ribbon(aes(ymin = 0, ymax = dy, fill = pcatch)) +
  geom_line() +
  labs(title=('TV Life Time - Normal Distribution')) +
  scale_x_continuous(
    breaks = c(4000, less.than.value, mu, 5000, 5500, 6000),
    labels = c('4000', less.than.value, 'Mean', '5000','5500', '6000'),
    name = "TV Life Time (Hours)") +
  scale_y_continuous(name = 'Density') +
  scale_fill_brewer(palette = 4, guide = 'none') +
  theme_bw()


# Beta endorphins are morphine like substances produced by the body. 
# They create a sense of well-being. It has been proposed that Beta 
# endorphins increase with exercise. Test this hypothesis using the 
# data in beta.csv which has Beta endorphin levels for 10 people 
# measured for each person pre- and post exercise. Using this sample, 
# test if Beta endorphins increase with exercise. Adopt a 5% risk of 
# committing a type I error.
# 1. Enter the data into R.
# 2. Perform some exploratory data analysis procedures.
# 3. Perform an appropriate significance test.
# 4. State any assumptions needed to support the validity of the procedure and where possible comment on the adequacy of these assumptions.
# 5. If you were conducting this experiment, what would you try to do to minimise confounding?
#    Hint: use R to assist your calculations.