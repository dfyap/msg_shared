#############
# Functions #
#############

# Congruity function
cong.fnc <- function(dat) {mean(dat$StoryDirection == dat$direction)}

# Permutation function
library(permute)
WithinSubsCongPermute <- function(dat, cong.fnc, n.reps=10000){
  PermHelper <- function(dat) {
    CTRL <- how(blocks = dat$Sub) 
    shuff.inx <- shuffle(nrow(dat), control = CTRL) # shuffle indices within Sub
    dat$StoryDirection <- dat$StoryDirection[shuff.inx]
    return(cong.fnc(dat)) # calculate mean congruence across all Sub
  }
  replicate(n=n.reps, expr=PermHelper(dat))  
}

# Bootstrap function to calculate congruity
cong.fnc <- function(dat) {mean(dat$StoryDirection == dat$direction)}

BootSingleDirection <- function(dat, cong.fnc, story.direction, n.reps=1000) {
  
  # First figure out how many samples to draw for each subject
  n.samps.per.subj <- aggregate(X ~ Sub, data=dat,
                                subset=(StoryDirection == story.direction),
                                FUN=length)
  counts <- n.samps.per.subj$X
  names(counts) <- n.samps.per.subj$Sub
  
  BootSampHelper <- function() {
    # Sample with replacement from within each group
    # Take the same number of observations as there 
    # are for each story direction.
    # Be sure to only look at subjects who had > 0 gestures in this cond.
    resamp.dat <- stratifiedDT(subset(dat, Sub %in% n.samps.per.subj$Sub),
                               group = 'Sub',
                               size = counts,
                               replace = TRUE)
    
    # Get the bootstrapped congruence with this story direction
    resamp.dat$StoryDirection <- story.direction
    c.resamp <- cong.fnc(resamp.dat)
    return(c.resamp)
  }
  
  replicate(n=n.reps, expr=BootSampHelper())
}

# Oneway permutation functions
GroupVar <- function(dat) {
  group.means <- by(dat, dat$Input.Lang, cong.fnc)
  return(var(group.means))
}

WithinSubsCongVarPermute <- function(dat, n.reps=10000){
  PermHelper <- function(dat) {
    CTRL <- how(blocks = dat$Sub)
    shuff.inx <- shuffle(nrow(dat), control = CTRL) # shuffle indices within Sub
    dat$Input.Lang <- dat$Input.Lang[shuff.inx]
    return(GroupVar(dat)) # calculate mean congruence across all Sub
  }
  replicate(n=n.reps, expr=PermHelper(dat))  
}

###################
# Simulated data? #
###################

# 1. create a dataframe that has chance congruity (.25 in 4 directions)
# Columns needed: storydirection, gdirection (up, down, left, right)
a<- rep(c('up','down','left','right'), each = 200) # storydirection
b <- rep(c('up','down','left','right'), each = 50) # equal prop of gdirection in each storydirection
c <- rep(b, 4) # do the same for all four storydirections
Sub <- rep(1:10,each=5, 16) # create a sample that gestures at chance
d <- as.data.frame(cbind(Sub,a,c))
table(d$a,d$c,d$Sub)
colnames(d)[1] <- "Sub"
colnames(d)[2] <- "StoryDirection"
colnames(d)[3] <- "direction" #gest direction
cong.fnc(d) # mean congruity of simulated data is .25

perm.stat <- WithinSubsCongPermute(d, cong.fnc)
summary(perm.stat) # mean of permuted dist. is .25
hist(perm.stat)
real.stat <- cong.fnc(d); real.stat; abline(v=real.stat, col='red')
mean(perm.stat > real.stat)

# 2. now, simulate a 100% congruity sample (cong = 1)
a<- rep(c('up','down','left','right'), each = 200) # storydirection
b <- rep(c('up','down','left','right'), each = 200) 
Sub <- rep(1:10,each=20, 4) # create a sample that gestures at 100% congruity for 10 Ss
d <- as.data.frame(cbind(Sub,a,b))
table(d$a,d$b,d$Sub)
colnames(d)[1] <- "Sub"; colnames(d)[2] <- "StoryDirection"; colnames(d)[3] <- "direction"
cong.fnc(d) # mean congruity of simulated data is 1.0

perm.stat <- WithinSubsCongPermute(d, cong.fnc)
summary(perm.stat) # mean of permuted dist is .25
hist(perm.stat, xlim=c(0,1)); abline(v=cong.fnc(d), col='red')
mean(perm.stat > cong.fnc(d))

########################
# Actual data analyses #
########################
# 1. four-direction omnibus (collapses direction, language and clause type)
# 2. Single-directional bootstrapping analyses
# 3. One-way permutation test of language type
# 4. Pairwise permutation tests of language type
# 5. Permutation tests within each language type
# 6. Pairwise permutation tests of clause type (target vs. non-target)
# 7. permutation tests within each clause type

library(lme4)
# library(devtools)
# install_github(repo='analysis_helpers', username='gbrookshire', subdir='analysisHelpers')
# library("analysisHelpers")
library(plotrix) # polar plots
# library(permute)

msg <- read.csv("~/Desktop/msgclause.csv") # recoded gesture labels according to clause direction
msg$StoryDirection[msg$StoryDirection == 'left.mov'] <- 'left'
# rename 'gesture' to 'storyGesture' to avoid confusing storyGesture for clauseGesture
names(msg)[names(msg) == 'gesture'] <- 'storyGesture' 
# exclude non-gestures and emblems
msg <- msg[which(msg$storyGesture != "" 
                 & msg$storyGesture != "ADAPTOR"
                 & msg$storyGesture != 'ENUMERATING'
                 & msg$storyGesture != 'EMBLEM'
                 & msg$storyGesture != "NA "),] 
msg$storyGesture <- factor(msg$storyGesture)

#########
# MSG 1 #
#########
msg1 <- msg[msg$experiment=='MSG1',] # selecting only MSG1 

###################################################
# 1. four-direction omnibus: only include up-,down-,left- and right-gestures
p <- subset(msg1, direction %in% c('UP','DOWN','LEFT','RIGHT')) 
p$direction <- factor(tolower(p$direction)) 
cong.fnc(p) # real/observed congruity
perm.stat <- WithinSubsCongPermute(p, cong.fnc)
summary(perm.stat) # permuted congruity
hist(perm.stat, xlim=c(0,1)); abline(v=cong.fnc(p), col='red')
mean(perm.stat > cong.fnc(p))

###################################################
# 2. Single-direction bootstrap analyses 
d <- subset(msg, direction %in% c('up', 'down', 'right', 'left')
            & experiment == 'MSG1')
d$direction <- factor(d$direction)
# Compute the stats!
k <- 1000
directions <- c('up', 'down', 'left', 'right')
c.emp <- sapply(directions,
                FUN=function(drct) cong.fnc(subset(d, StoryDirection == drct)))
c.boot <- lapply(directions,
                 FUN=function(drct) BootSingleDirection(d, cong.fnc, drct, k))

# Plot the results
pdf(file="~/Desktop/msgplots/MSG1bootstrap.pdf")
boxplot(c.boot, range = 0, boxwex = 0.3, staplewex = 0.1, lty = 1,
        names = directions, ylab = 'Prop. congruent', xlab = 'Story direction',
        ylim=c(0,.8), yaxt ='n', ylab ='n')
axis(2, at=seq(0,1,.1), las=2)
points(1:4, c.emp, pch=19, col='red')
dev.off()

# descriptives from boxplot
round(c.emp,2) # empirical congruity levels 
round(sapply(c.boot, FUN=mean),2) # chance congruity

# p = count(empirical_congruence > observed_congruence) / K
sum((c.boot[[1]]) > c.emp[1]) / k # up story
sum((c.boot[[2]]) > c.emp[2]) / k # down story
sum((c.boot[[3]]) > c.emp[3]) / k # left story
sum((c.boot[[4]]) > c.emp[4]) / k # right story

###########################################
# 3. one-way perm. of language type in MSG1
b <- d[d$experiment == 'MSG1',]
emp <- GroupVar(b); emp
permuted <- WithinSubsCongVarPermute(b, 10000)
mean(emp < permuted)
hist(permuted,xlim=c(-.004,.01))
abline(v = emp,col='red')

#################################################
# 4a. Comparing LSL and MSL (pairwise comparison)
p <- subset(msg1, direction %in% c('UP','DOWN','LEFT','RIGHT')
            & Input.Lang %in% c('LSL','MSL'))
p$direction <- as.factor(tolower(p$direction)) 

# create function to calculate test statistic: difference between means of 
# congruity between 'a' group and 'b' group
label.mean.diff <- function(dat) {
  # e.g., group a = LSL, group b = MSL
  # still restricted that it only calculates subsets of input language - what about target-nt?
  cong.fnc(dat[dat$Input.Lang=='MSL',]) - cong.fnc(dat[dat$Input.Lang=='LSL',])
}
label.mean.diff(p)
perm.stat <- WithinSubsCongPermute(p, label.mean.diff)
summary(perm.stat) 
hist(perm.stat, xlim=c(-.2,.2)); abline(v=label.mean.diff(p), col='red')
mean(perm.stat > label.mean.diff(p)) # p < .0001

# 4b. Comparing LSL and NSL
p <- subset(msg1, direction %in% c('UP','DOWN','LEFT','RIGHT')
            & Input.Lang %in% c('LSL','NSL'))
p$direction <- as.factor(tolower(p$direction)) 

# create function to calculate test statistic: difference between means of 
# congruity between 'a' group and 'b' group
label.mean.diff <- function(dat) {
  # e.g., group a = LSL, group b = MSL
  # still restricted that it only calculates subsets of input language - what about target-nt?
  cong.fnc(dat[dat$Input.Lang=='LSL',]) - cong.fnc(dat[dat$Input.Lang=='NSL',])
}
label.mean.diff(p)
perm.stat <- WithinSubsCongPermute(p, label.mean.diff)
summary(perm.stat) 
hist(perm.stat, xlim=c(-.2,.2)); abline(v=label.mean.diff(p), col='red')
mean(perm.stat > label.mean.diff(p))

# 4c. Comparing NSL and MSL
p <- subset(msg1, direction %in% c('UP','DOWN','LEFT','RIGHT')
            & Input.Lang %in% c('NSL','MSL'))
p$direction <- as.factor(tolower(p$direction)) 
label.mean.diff <- function(dat) {
  cong.fnc(dat[dat$Input.Lang=='NSL',]) - cong.fnc(dat[dat$Input.Lang=='MSL',])
}
label.mean.diff(p)
perm.stat <- WithinSubsCongPermute(p, label.mean.diff)
summary(perm.stat) 
hist(perm.stat, xlim=c(-.2,.2)); abline(v=label.mean.diff(p), col='red')
mean(perm.stat > label.mean.diff(p)) # p < .0001

###################################################
# 5a. Permutating only LSL stories (all directions)
p <- subset(msg1,direction %in% c('UP','DOWN','LEFT','RIGHT')
            & Input.Lang == 'LSL')
p$direction <- as.factor(tolower(p$direction))
cong.fnc(p)
perm.stat <- WithinSubsCongPermute(p, cong.fnc)
summary(perm.stat) 
hist(perm.stat, xlim=c(0,1)); abline(v=cong.fnc(p), col='red')
mean(perm.stat > cong.fnc(p))

# 5b. Permutating only MSL stories
p <- subset(msg1,direction %in% c('UP','DOWN','LEFT','RIGHT')
            & Input.Lang == 'MSL')
p$direction <- as.factor(tolower(p$direction))
cong.fnc(p)
perm.stat <- WithinSubsCongPermute(p, cong.fnc)
summary(perm.stat) 
hist(perm.stat, xlim=c(0,1)); abline(v=cong.fnc(p), col='red')
mean(perm.stat > cong.fnc(p)) # p< .0001

# 5c. Permutating only NSL stories
p <- subset(msg1,direction %in% c('UP','DOWN','LEFT','RIGHT')
            & Input.Lang == 'NSL')
p$direction <- as.factor(tolower(p$direction))
cong.fnc(p)
perm.stat <- WithinSubsCongPermute(p, cong.fnc)
summary(perm.stat) 
hist(perm.stat, xlim=c(0,1)); abline(v=cong.fnc(p), col='red')
mean(perm.stat > cong.fnc(p)) # p < .0001

#####################################
# 6. Pairwise comparison between Target and Non-target clauses
p <- subset(msg1, direction %in% c('UP','DOWN','LEFT','RIGHT') 
            & Target.NonTarget %in% c('Target','NonTarget'))
p$direction <- as.factor(tolower(p$direction)) 
label.mean.diff <- function(dat) {
  cong.fnc(dat[dat$Target.NonTarget=='Target',]) - cong.fnc(dat[dat$Target.NonTarget=='NonTarget',])
}
label.mean.diff(p)
perm.stat <- WithinSubsCongPermute(p, label.mean.diff)
summary(perm.stat) 
hist(perm.stat, xlim=c(-.2,.2)); abline(v=label.mean.diff(p), col='red')
mean(perm.stat > label.mean.diff(p)) # p < .0001

####################################
# 7a. Permutating only Target clauses
p <- subset(msg1,direction %in% c('UP','DOWN','LEFT','RIGHT')
            & Target.NonTarget == 'Target')
p$direction <- as.factor(tolower(p$direction))
cong.fnc(p)
perm.stat <- WithinSubsCongPermute(p, cong.fnc)
summary(perm.stat) 
hist(perm.stat, xlim=c(0,1)); abline(v=cong.fnc(p), col='red')
mean(perm.stat > cong.fnc(p))

# 7b. Permutating only Non-target clauses
p <- subset(msg1,direction %in% c('UP','DOWN','LEFT','RIGHT')
            & Target.NonTarget == 'NonTarget')
p$direction <- as.factor(tolower(p$direction))
cong.fnc(p)
perm.stat <- WithinSubsCongPermute(p, cong.fnc)
summary(perm.stat) 
hist(perm.stat, xlim=c(0,1)); abline(v=cong.fnc(p), col='red')
mean(perm.stat > cong.fnc(p))
