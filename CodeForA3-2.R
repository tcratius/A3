library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(reshape2)

# Constants
less.than.value <- 4499 # Hours used x-intercept
mu <- 4800 # mean in Hour
sigma <- 400 # sigma
rnorm.axis <- mu * 2 # Number of values to be randomly generated 
n.TV <- 16
std <- sigma/n.TV 

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

p.aTV2 <- 1- pnorm(less.than.value, mu, sigma)

per.aTV2 <- round(p.aTV2*100, 2)
per.aTV <- round(p.aTV*100, 2) # probability of population of TV lifetime is less than 4500
per.aTV100 <- 1
# Create random vectors of a normal distribution using mean, standard deviation 
# and x end and assign to variable named TV_Life_Time
# and then convert to a data.frame for use with ggplot
TV <- data.frame(x = rnorm(rnorm.axis, mu, sigma))

# Add to ggplot clustering
nClusters <- 7  # Cluster it
kMeans <- kmeans(TV, centers = nClusters)
TV$Cluster <- as.factor(kMeans$cluster)
plot <- ggplot(TV, aes(x=x, y=y)) +
               stat_density2d(aes(fill = Cluster, colour = Cluster,
                   alpha = ..level..),
               geom = "polygon")


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
#probch <- c(y, y) # change from lower

TV2 <- TV %>%
  mutate(dy = approxdens(x),                         # calculate density
         p = percent_rank(x),                        # percentile rank 
         pcatl = as.factor(cut(p, breaks = probsl,   # percentile category 
                              include.lowest = TRUE))) #based on probs
         #probch = as.factor(cut(p, breaks = probch,  # percentile category 
          #                   include.lowest = TRUE))) # based on probs


# lower tail geom_ribbon 
ggplot(TV2 , aes(x, dy)) +
  geom_ribbon(aes(ymin = 0, ymax = dy, fill = pcatl)) +
  geom_line() +
  labs(title=('TV Life Time - Population vs Sample'), 
       fill='% Pop of TV\'s') +
  scale_x_continuous(
    breaks = c(4000, less.than.value, mu, 5000, 5500, 6000),
    labels = c('4000', less.than.value, 'Mean', '5000','5500', '6000'),
    name = "TV Life Time (Hours)") +
  scale_y_continuous(name = 'Density') +
  scale_fill_brewer(palette = 4, labels = c(per.aTV, per.aTV2) ) +
  theme_minimal()

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

SE <- sigma/sqrt(16) # Standard Error = Population mean/sqrt(16TV's)
Z <- (4500- 4800)/SE 
p.bTV <-pnorm(Z) 
per.bTV <- round(p.bTV*100, 2)

# create shade for normal distribution
probsl <- c(0.00, p.aTV) # lower
#probsu <- c(0.885, 1.00) # upper
probch <- c(0.00, p.bTV)

# creates the x and y values to shade in under the curve 
TV3 <- TV %>%
  mutate(dy = approxdens(x),                         # calculate density
         p = percent_rank(x),                        # percentile rank 
         pcatl = as.factor(cut(p, breaks = probsl,   # percentile category lower 
                               include.lowest = TRUE)), #based on probs
         pcatch = as.factor(cut(p, breaks = probch,  # percentile category change
                                include.lowest = TRUE))) # based on probs


# pcatl lower tail geom_ribbon showing the population probabilty less than 4500 hours
# pcatch lower tail geom_ribbon form probability of the 16 sample TV being less 4500 hours

plotTV2 <- ggplot(TV3 , aes(x, dy)) +
              geom_ribbon(aes(ymin = 0, ymax = dy, fill = pcatl)) +
              geom_ribbon(aes(ymin = 0, ymax = dy, fill = pcatch)) +
              geom_line() +
              labs(title=('TV Life Time - Population vs Sample'), 
                   fill='% Pop vs\nSample 16 TV\'s') +
              scale_x_continuous(
                breaks = c(4000, less.than.value, mu, 5000, 5500, 6000),
                labels = c('4000', less.than.value, 'Mean', '5000','5500', '6000'),
                name = 'TV Life Time (Hours)') +
              scale_y_continuous(name = 'Density') +
              scale_fill_brewer(palette = 5, labels = c(per.bTV, per.aTV)) +
              labs(col=scale_fill_brewer(palette=5)) +
              theme_minimal()
plotTV2


# Beta endorphins are morphine like substances produced by the body. 
# They create a sense of well-being. It has been proposed that Beta 
# endorphins increase with exercise. Test this hypothesis using the 
# data in beta.csv which has Beta endorphin levels for 10 people 
# measured for each person pre- and post exercise. Using this sample, 
# test if Beta endorphins increase with exercise. Adopt a 5% risk of 
# committing a type I error.

# 1. Enter the data into R.
beta <- read.csv(choose.files())

# 2. Perform some exploratory data analysis procedures.

df.beta <- data.frame(beta)
# beta.sub <- df.beta$subject
# beta.pre <- df.beta$pre
# beta.post <- df.beta$post
# beta.dif <- df.beta$dif

# two row for box plot
par(mfrow=c(1, 2))

# Q-Q plot with qqline
qqnorm(df.beta$pre)
qqline(df.beta$pre)

qqnorm(df.beta$post)
qqline(df.beta$post)

qqnorm(df.beta$dif)
qqline(df.beta$dif)

# Histogram
hist(df.beta$pre) # positive skew
hist(df.beta$post) # postive skew
hist(df.beta$dif) # positive skew

# Boxplot
boxplot(df.beta$pre)
boxplot(df.beta$post)
boxplot(df.beta$dif)

# remove outlier from beta.post
boxplot.stats(df.beta$post)
df.beta$post[df.beta$post %in% boxplot.stats(df.beta$post)$out] <- NA
boxplot(df.beta$post)

# remove outlier(s) from beta.dif
boxplot.stats(df.beta$dif)
df.beta$dif[df.beta$dif %in% boxplot.stats(df.beta$dif)$out] <- NA
boxplot(df.beta$dif)

# na.omit
df.beta <- na.omit(df.beta)

# 3. State the hypothesis
#    a) Ho Beta endorphins are equal pre and post exercise (Pre = Post)
#    b) H1 alternative Pre <= Post
# 4. Perform an appropriate significance test. This p-value is obviously 
#    much smaller than the significance level of 0.05 and so we reject the 
#    null hypothesis.
# 5. State any assumptions needed to support the validity of the procedure and 
#    where possible comment on the adequacy of these assumptions.

cat('State the hypothesis\n  a) Ho Beta endorphins are equal pre and post exercise',
    '(Pre = Post)\n  b) H1 alternative Pre <= Post\n')           

# comparing mean's for approximate equal variance
mean.pre <- mean(df.beta$pre)
mean.post <- mean(df.beta$post)
std.pre <- sd(df.beta$pre)
std.post <- sd(df.beta$post)
rnorm.axis.pre <- 1000
rnorm.axis.post <- 1000

cat('Comparisons of Pre and Post workout are: Pre = ', signif(mean.pre), 'and Post = ', signif(mean.post),
    'Shows there is\nunequal variance. Therefore, tstatistic can not be calculated.', 
    'Due to this assumption\n\"Two independent samples t-test will be used instead\"\n')

# t-test for unequal variance
result.ttest.pre.post <- t.test(df.beta$pre, df.beta$post, conf.level=0.95,
                         var.equal = F, alternative = 'less') 
result.ttest.pre.post

par(mfrow=c(1, 1))

# create a data.frame for both pre and post graphs
x.pre <- (x = rnorm(rnorm.axis.pre, mean.pre, std.pre))
x.post <- (x = rnorm(rnorm.axis.post, mean.post, std.post))


cat('The graph below was created using the pre exercise mean', signif(mean.pre, 4),
    'and the post exercise mean', signif(mean.post,4))


# Add both pre and post values to a data.frame and melt using reshape2
x.pre.post <- data.frame(x.pre, x.post)
plot.pp <- melt(x.pre.post)

# plot the graph using geom_density increase the x axis limits and 
# colour using colour brewer
ggplot(plot.pp ,aes(x=value, fill=variable)) + 
  geom_density(alpha=0.25) +
  labs(title=('Variance between Beta levels:\nPre & Post Exercise'), 
       fill='Exercise') +
  scale_x_continuous(name=('x - Pre & Post')) +
  scale_fill_brewer(palette = 5, labels=c('Pre', 'Post')) +
  expand_limits(x=c(-5,45)) +
  theme_minimal()

# 6. If you were conducting this experiment, what would you try to do to minimise confounding?
#    Hint: use R to assist your calculations.
