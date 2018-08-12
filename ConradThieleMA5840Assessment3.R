library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(CatEncoders)
library(lawstat)
library(MASS)
library(dplyr)

# Constants

less.than.value <- 4500 # Hours used x-intercept
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

# p.aTV2 <- 1- pnorm(less.than.value, mu, sigma)

# Convert to percentage
per.aTV <- round(p.aTV*100, 2) 
p.aTV2 <- 1-pnorm(less.than.value, mu, sigma)
per.aTV2 <- round(p.aTV2*100, 2)

# probability of population of TV lifetime is less than 4500
# Create random vectors of a normal distribution using mean, standard deviation 
# and x and assign to variable named TV
# and then convert to a data.frame for use with ggplot
TV <- data.frame(x = rnorm(rnorm.axis, mu, sigma))


###
# https://stackoverflow.com/questions/3494593/shading-a-kernel-density-plot-between-two-points
# function that approximates the density of x and y using only x value
### 
approxdens <- function(x) {
  dens <- density(x)
  f <- with(dens, approxfun(x, y))
  f(x)
}

# Assign p.aTV to probl to be used in following function and plot
probsl <- c(0.00, p.aTV) # lower
# probsu <- c(0.885, 1.00) # upper
# probch <- c(y, y) # change from lower

TV2 <- TV %>%
  mutate(dy = approxdens(x),                         # calculate density
         p = percent_rank(x),                        # percentile rank 
         pcatl = as.factor(cut(p, breaks = probsl,   # percentile category 
                              include.lowest = TRUE))) #based on probs
         #probch = as.factor(cut(p, breaks = probch,  # percentile category 
         #                   include.lowest = TRUE))) # based on probs
###
# Creates and plots a normal distribution of the population of a particular TV life time
# in hours.  It takes the arguments (x, dy) from TV2, see mutate above, to shade in the
# lower tail using geom_ribbon. Geom_line produces the normal distributed line and the 
# rest are Aesthetics.
###
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
  theme_classic()

###
# (b) Find the probability that the mean lifetime of a random sample
# of 16 TVs is less than 4,500 hours. Use R to assist with your computations.
#
# The popultation mean 4800 is equal to the all the Sampling distribution of the sample 
# mean, therefore the population mean can be used to find the standard error of the 
# sample mean
###
n.TV <- 16
SE.TV <- sigma/sqrt(n.TV) # Standard Error = Population mean/sqrt(16TV's)
Z.TV <- (less.than.value - mu)/SE.TV 
p.bTV <- pnorm(Z.TV) 
per.bTV <- round(p.bTV*100, 2)

# create shade for normal distribution
probsl <- c(0.00, p.aTV) # lower tail
probch <- c(0.00, p.bTV) # lower tail

# creates the x and y values to shade in under the curve 
TV.mutate <- TV %>%
  mutate(dy = approxdens(x),                         # calculate density
         p = percent_rank(x),                        # percentile rank 
         pcatl = as.factor(cut(p, breaks = probsl,   # percentile category lower 
                               include.lowest = TRUE)), #based on probs
         pcatch = as.factor(cut(p, breaks = probch,  # percentile category change
                                include.lowest = TRUE))) # based on probs

###
# pcatl lower tail geom_ribbon showing the population probabilty less than 4500 hours
# pcatch lower tail geom_ribbon form probability of the 16 sample TV being less 4500 hours
###
plotTV2 <- ggplot(TV.mutate , aes(x, dy)) +
              geom_ribbon(aes(ymin = 0, ymax = dy, fill = pcatl)) +
              geom_ribbon(aes(ymin = 0, ymax = dy, fill = pcatch)) +
              geom_line() +
              labs(title=('TV Life Time - Population vs Sample'), 
                   fill='% Pop vs\nSample 16 TV\'s') +
              scale_x_continuous(
                breaks = c(4000, less.than.value, mu, 5000, 5500, 6000),
                labels = c('4000', less.than.value, 'mu', '5000','5500', '6000'),
                name = 'TV Life Time (Hours)') +
              scale_y_continuous(name = 'Density') +
              scale_fill_brewer(palette = 5, labels = c(per.bTV, per.aTV)) +
              labs(col=scale_fill_brewer(palette=5)) +
              coord_cartesian(ylim=c(0, 0.00005)) +
              theme_classic()
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
View(beta)
str(beta)

df.beta <- data.frame(beta) # create data.frame

# Hist Q-Qplot
par(mfrow=c(1,2)) # two columns for plotting
hist(df.beta$pre, xlab="Pre"); qqnorm(df.beta$pre); qqline(df.beta$pre)
hist(df.beta$post, xlab="Pre"); qqnorm(df.beta$post); qqline(df.beta$post)

# Boxplot of pre and post
boxplot(df.beta$pre, xlab= "Pre")
boxplot(df.beta$post, xlab= "Post")

# remove outlier from beta.post
boxplot.stats(df.beta$post)
df.beta$post[df.beta$post %in% boxplot.stats(df.beta$post)$out] <- NA
df.beta.par <- na.omit(df.beta) # na.omit removes outliers
par(mfrow=c(1,1))
boxplot(df.beta.par)

mean.pre <- mean(df.beta.par$pre)
mean.post <- mean(df.beta.par$post)
std.pre <- sd(df.beta.par$pre)
std.post <- sd(df.beta.par$post)

rnorm.axis.pre <- 1000
rnorm.axis.post <- 1000
# create a data.frame for both pre and post graphs
x.pre <- (x = rnorm(rnorm.axis.pre, mean.pre, std.pre))
x.post <- (x = rnorm(rnorm.axis.post, mean.post, std.post))

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
  theme_classic()

# 4. Perform an appropriate significance test. This p-value is obviously 
#    much smaller than the significance level of 0.05 and so we reject the 
#    null hypothesis.

####################################################################
# Difference = symmetry
# Differnce != symmetry
# symmetry.test(df.beta.par$pre - 
#                df.beta.par$post, boot = FALSE, side="both")
# Pre beta = symmetry
# Pre beta != symmetry
# symmetry.test(df.beta.par$pre,boot = FALSE, side="both")

# Post beta = symmetry
# Post beta != symmetry
#symmetry.test(df.beta.par$post, boot = FALSE, side="both")
##################################################################

# Pre Beta endorphins = Post Beta endorphins
# Pre Beta endorphins < Post Beta endorphins
welsh.pre.less.post <- t.test(df.beta.par$pre, 
                              df.beta.par$post, 
                              conf.level=0.95,
                              var.equal = FALSE, alternative = 'less') 
welsh.pre.less.post

###########################################################

df.beta.non.par <- data.frame(beta)

# Difference = symmetry
# Differnce != symmetry
symmetry.test(df.beta.non.par$pre - 
                df.beta.non.par$post, boot = FALSE, side="both")
# Pre beta = symmetry
# Pre beta != symmetry
symmetry.test(df.beta.non.par$pre,boot = FALSE, side="both")

# Post beta = symmetry
# Post beta != symmetry
symmetry.test(df.beta.non.par$post, boot = FALSE, side="both")

# Pre Beta endorphins = Post Beta endorphins
# Pre Beta endorphins < Post Beta endorphins
kruskal.test(df.beta.non.par$pre, df.beta.non.par$post)
kruskal.test(df.beta$pre, df.beta$post)

# Pre Beta endorphins = Post Beta endorphins
# Pre Beta endorphins < Post Beta endorphins
wilcox.pre.less.post <- wilcox.test(df.beta.non.par$pre,
                                      df.beta.non.par$post, 
                             alternative="less", paired=TRUE,
                             conf.level=0.95 )

wilcox.pre.less.post

##############################################################

# 5. State any assumptions needed to support the validity of the procedure and 
#    where possible comment on the adequacy of these assumptions.
# 
#    Population of different scores is normally distributed: from looking
#    at the Q-Q plot, it is reasonable to assume normality
#    The two samples are dependent: Yes, the dependent variable difference (diff)
#    is dependent on the two independent variables pre and post (exercise)
#    Data values are obtained by independent random sampling: For the tests
#    it has been assumed that simple random sampling has been performed.
#    Ideally there would be a treatment and control group.
#    Equal variance or standard deviations in the groups: There is
#    definitely not equal variance in the variables pre and post


# 6. If you were conducting this experiment, what would you try to do to
#    minimise confounding?
#    Hint: use R to assist your calculations.
#    Average of control and trial groups

# observed pre and post beta levels
# 
cor.test(df.beta.non.par$post, df.beta.non.par$pre) #pearsons
cor(df.beta.non.par$post, df.beta.non.par$pre, method = "spearman" )
cor(df.beta.non.par$post, df.beta.non.par$pre, method = "kendall" )

lm.beta <- lm(subject ~ post + pre, data=df.beta)
summary(lm.beta)



###################################################
plot.beta <- ggplot(data=df.beta, aes(x=pre, y=pre, 
                                                    col= factor(subject),
                                                    group=1))
plot.beta + geom_point() + geom_line() + 
  geom_label(aes(label = subject))

plot.beta <- ggplot(data=df.beta, aes(x=pre, y=post, 
                                                    col= factor(subject),
                                                    group=1))
plot.beta + geom_violin(scale = "area") + 
  geom_label(aes(label = subject), nudge_x = 0,
             nudge_y = 1)

plot.beta + geom_point() + 
  geom_label(aes(label = subject), nudge_x = 0,
             nudge_y = 1 )

check_overlap = FALSE
plot.beta
# Dataset: PlantGrowth

library(datasets)
library(dplyr)

data(PlantGrowth)
View(PlantGrowth)
str(PlantGrowth)


# These data are obtained from an experiment to compare plant
# yields under a control and two other treatments. Test if there 
# is a difference between the control group and treatment 2 on mean plant yields.

# remove contents of weight by factor name "trt1" 
PlantGrowth.ctrl.trt2 <- PlantGrowth[which(!PlantGrowth$group %in% 'trt1'), ]

# remove the factor "trt1" from the level
PlantGrowth.ctrl.trt2$group <-  levels(droplevels(PlantGrowth.ctrl.trt2$group))
                                               
# 2. Perform some exploratory data analysis procedures.
par(mfrow=c(1,2))
# boxplot 
boxplot(weight ~ group, data=PlantGrowth.ctrl.trt2)
# linear model using the least square method 
PlantGrowth.lm = lm(weight ~ group, data=PlantGrowth.ctrl.trt2) 
# Standard residual - difference between the observed value of the dependent variable (y)
PlantGrowth.stdres = rstandard(PlantGrowth.lm) 

# Plot the residual
qqnorm(PlantGrowth.stdres, 
       ylab = "Standard Residual",
       xlab = "normal scores",
       main = "Plant Growth Treatment 2 vs Control")
qqline(PlantGrowth.stdres)

# convert to table to find the number of factor for each level in the group.
# Display the results
pg.table <- table(as.matrix(PlantGrowth.ctrl.trt2))
count.factors <- as.data.frame(pg.table)
count.factors # give the n of sample in this case both ctrl and trt2  n = 10

n.pg <- 10 # from count.factors

# Subset group 'ctrl' and 'trt2'
PlantGrowth.sub.ctrl <-  subset(PlantGrowth.ctrl.trt2, group=='ctrl', 
                                    select=c(group,weight))

PlantGrowth.sub.trt2 <-  subset(PlantGrowth.ctrl.trt2, group=='trt2', 
                                    select=c(group,weight))
qqnorm(PlantGrowth.sub.ctrl$weight)
qqline(PlantGrowth.sub.ctrl$weight)

qqnorm(PlantGrowth.sub.trt2$weight)
qqline(PlantGrowth.sub.trt2$weight)

# Summarise the mean, standard deviation, and standard error for 'ctrl' and 'trt2' 
PlantGrowth.summary.ctrl <-  PlantGrowth.sub.ctrl %>%
  group_by(group == 'ctrl') %>%
  summarise(sum= sum(weight),mean = mean(weight), std = sd(weight), SE = sd(weight)/sqrt(n.pg))
PlantGrowth.summary.ctrl

PlantGrowth.summary.trt2 <-  PlantGrowth.sub.trt2 %>%
  group_by(group == 'trt2') %>%
  summarise(sum = sum(weight),mean = mean(weight), std = sd(weight), SE = sd(weight)/sqrt(n.pg))
PlantGrowth.summary.trt2
# 3. Perform an appropriate significance test.
# independent 2-group t-test
# t.test(y~x) # where y is numeric and x is a binary factor

# Ho = ctrl = trt2
# Ha = ctrl < trt2
t.test(PlantGrowth.ctrl.trt2$weight ~ PlantGrowth.ctrl.trt2$group, 
       conf.level=0.95, var.equal=T, alternative="less")

# Ho = ctrl = trt2
# Ha = ctrl > trt2
t.test(PlantGrowth.ctrl.trt2$weight ~ PlantGrowth.ctrl.trt2$group, 
       conf.level=0.95, var.equal=T, alternative="greater")

# Ho = ctrl = trt2
# Ha = ctrl != trt2
t.test(PlantGrowth.ctrl.trt2$weight ~ PlantGrowth.ctrl.trt2$group, 
       conf.level=0.95, var.equal=T, alternative="two.sided")

symmetry.test(PlantGrowth.sub.ctrl$weight)
symmetry.test(PlantGrowth.sub.trt2$weight)

# Ho = ctrl = trt2
# Ha = ctrl != trt2
wilcox.test(PlantGrowth.ctrl.trt2$weight ~ PlantGrowth.ctrl.trt2$group, 
            conf.level=0.95, var.equal=T, alternative="two.sided")

# Ho = ctrl = trt2
# Ha = ctrl < trt2
wilcox.test(PlantGrowth.ctrl.trt2$weight ~ PlantGrowth.ctrl.trt2$group, 
            conf.level=0.95, var.equal=T, alternative="less")

# Ho = ctrl = trt2
# Ha = ctrl > trt2
wilcox.test(PlantGrowth.ctrl.trt2$weight ~ PlantGrowth.ctrl.trt2$group, 
            conf.level=0.95, var.equal=T, alternative="greater")

# Ho = ctrl = trt2
# Ha = ctrl != trt2
kruskal.test(group ~ weight, data = PlantGrowth.ctrl.trt2)
# At .05 significance level, we conclude that the weight for "ctrl" and 
# "trt2" could be identical populations.

# 4. State any assumptions needed to support the validity of the procedure and where possible 
# comment on the adequacy of these assumptions.

# Population is appoximately normally distributed.
# The two populations have the same variance
# The two samples are independent of each other.
# Randomly sampling of the population were performed.

# i.e.
# The data are a simple random sample. We assume that the cabbages grown are a 
# representative sample of the entire population of cabbages of the same cultivar type.
# Observations come from a population that is normally distributed. The Q-Q plot of 
# the sample data, shown above, indicates that this is a reasonable assumption.