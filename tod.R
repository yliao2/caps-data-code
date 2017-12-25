#function.path = "D:/caps/function_code.R"
#source(file = function.path)
load("tod.RData")
#time of day screening
#use function: obs.rank (create)
#level set plot for TOD (alpha = 0.05)
#-------------------------------
time <- "shift12"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:12, sep = ""))
alpha <- 0.05


#create separate data sets based on the setting "Line 8-11"
#generate data: 0-2, 2-4, 4-6,... 20-22, 22-24
rank.time.dt(data = tod, tag = tag, time = time)


#create separate data sets based on setting "Line 5-9"
#combine the results into "out"
lvl.set.time(tag = tag, alpha = alpha)
out <- NULL
for (i in 1:12) {out <- rbind(out, get(paste(tag[i], ".lvl", sep = "")))}



hline.dat <- data.frame(shift12 = out$time, hl = out$thres)

point.dat <- data.frame(rbind(s1[which(s1$p>=out$thres[1]), ],
                              s2[which(s2$p>=out$thres[2]), ],
                              s3[which(s3$p>=out$thres[3]), ],
                              s4[which(s4$p>=out$thres[4]), ],
                              s5[which(s5$p>=out$thres[5]), ],
                              s6[which(s6$p>=out$thres[6]), ],
                              s7[which(s7$p>=out$thres[7]), ],
                              s8[which(s8$p>=out$thres[8]), ],
                              s9[which(s9$p>=out$thres[9]), ],
                              s10[which(s10$p>=out$thres[10]), ],
                              s11[which(s11$p>=out$thres[11]), ],
                              s12[which(s12$p>=out$thres[12]), ]))

ggplot(data = tod, mapping = aes(x = Logmile, y = p)) +
  geom_segment(aes(xend = Logmile, yend = 0)) + 
  geom_hline(aes(yintercept=hl), data=hline.dat, col = "darkgray") + 
  geom_point(data = point.dat, aes(x = Logmile, y = p), col = 4) + 
  facet_grid(shift12~.)




#KS tests: on level sets & rank for TOD data (% crashes vs. size of alpha)
#------------------------------------------------------------------------

#2-hour period
time <- "shift12"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:12, sep = ""))
rank.time.dt(data = tod, tag = tag, time = time)

alpha = seq(0, 1, by = 0.01)
lvl.set.time(tag = tag, alpha = alpha)
ks1 <- origin.pair(ref = tag, compare = tag, sig = 0.3, opt = FALSE)
#------------------------------------------------------------------------

#4-hour period
time <- "shift6"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:6, sep = ""))
rank.time.dt(data = tod, tag = tag, time = time)

alpha = seq(0, 1, by = 0.01)
lvl.set.time(tag = tag, alpha = alpha)
ks2 <- origin.pair(ref = tag, compare = tag, sig = 0.3, opt = FALSE)
#-------------------------------------------------------------------------

#8-hour period
time <- "shift3"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:3, sep = ""))
rank.time.dt(data = tod, tag = tag, time = time)

alpha = seq(0, 1, by = 0.01)
lvl.set.time(tag = tag, alpha = alpha)
ks3 <- origin.pair(ref = tag, compare = tag, sig = 0.3, opt = FALSE)
#-------------------------------------------------------------------------

#12-hour period
time <- "shift2"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:2, sep = ""))
rank.time.dt(data = tod, tag = tag, time = time)

alpha = seq(0, 1, by = 0.01)
lvl.set.time(tag = tag, alpha = alpha)
ks4 <- origin.pair(ref = tag, compare = tag, sig = 0.3, opt = FALSE)
#-------------------------------------------------------------------------





#segments binnings with size 1 and 0.5
#-------------------------------------------------------------
#2-hour period
time <- "shift12"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:12, sep = ""))
xlim <- c(0, ceiling(max(tod$Logmile))+1)

#2-hour period
#binnings using level sets with size 1
#-------------------------------------------------------------
brks <- seq(0, xlim[2], by = 1)
bin2rank.time(data = tod, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))
ks.tod12bin1 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)

#2-hour period
#binnings using level sets with size 0.5
#-------------------------------------------------------------
brks <- seq(0, xlim[2], by = 0.5)
bin2rank.time(data = tod, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))
ks.tod12bin0.5 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)
#-------------------------------------------------------------
#-------------------------------------------------------------

#4-hour period
time <- "shift6"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:6, sep = ""))
xlim <- c(0, ceiling(max(tod$Logmile))+1)

#4-hour period
#binnings using level sets with size 1
#-------------------------------------------------------------
brks <- seq(0, xlim[2], by = 1)
bin2rank.time(data = tod, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))
ks.tod6bin1 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)

#4-hour period
#binnings using level sets with size 0.5
#-------------------------------------------------------------
brks <- seq(0, xlim[2], by = 0.5)
bin2rank.time(data = tod, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))
ks.tod6bin0.5 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)
#-------------------------------------------------------------
#-------------------------------------------------------------

#8-hour period
time <- "shift3"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:3, sep = ""))
xlim <- c(0, ceiling(max(tod$Logmile))+1)

#8-hour period
#binnings using level sets with size 1
#-------------------------------------------------------------
brks <- seq(0, xlim[2], by = 1)
bin2rank.time(data = tod, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))
ks.tod3bin1 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)

#8-hour period
#binnings using level sets with size 0.5
#-------------------------------------------------------------
brks <- seq(0, xlim[2], by = 0.5)
bin2rank.time(data = tod, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))
ks.tod3bin0.5 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)
#-------------------------------------------------------------
#-------------------------------------------------------------

#12-hour period
time <- "shift2"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:2, sep = ""))
xlim <- c(0, ceiling(max(tod$Logmile))+1)

#12-hour period
#binnings using level sets with size 1
#-------------------------------------------------------------
brks <- seq(0, xlim[2], by = 1)
bin2rank.time(data = tod, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))
ks.tod2bin1 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)

#12-hour period
#binnings using level sets with size 0.5
#-------------------------------------------------------------
brks <- seq(0, xlim[2], by = 0.5)
bin2rank.time(data = tod, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))
ks.tod2bin0.5 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)
#-------------------------------------------------------------
#-------------------------------------------------------------

#=============================================================




#use "may", "jun", "jul" as reference to test other days
#test % matched segments under level sets
#--------------------------------------------------------

#2-hour periods
time <- "shift12"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:12, sep = ""))
rank.time.dt(data = tod, tag = tag, time = time)
lvl.set.time(tag = tag, alpha = alpha)

tod3.mod <- data2pred(ref = tag[3], compare = tag[-3])
tod5.mod <- data2pred(ref = tag[5], compare = tag[-5])
tod6.mod <- data2pred(ref = tag[6], compare = tag[-6])
tod7.mod <- data2pred(ref = tag[7], compare = tag[-7])
tod8.mod <- data2pred(ref = tag[8], compare = tag[-8])
tod10.mod <- data2pred(ref = tag[10], compare = tag[-10])
tod11.mod <- data2pred(ref = tag[11], compare = tag[-11])

par(mfrow=c(4,2),family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
for (i in c(3,5,6,7,8,10,11))
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = toupper(tag[i]), xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste("tod",i,".mod",sep=""))$alpha, 
           y = get(paste("tod",i,".mod",sep=""))[,-1], col = 1:11, type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], col = 1:11, lty = 1, bty = "n")
}
#-------------------------------------------------------------

#4-hour periods
time <- "shift6"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:6, sep = ""))
rank.time.dt(data = tod, tag = tag, time = time)
lvl.set.time(tag = tag, alpha = alpha)

tod1.mod <- data2pred(ref = tag[1], compare = tag[-1])

par(family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
plot.new()
plot.window(xlim = range(alpha), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = toupper(tag[1]), xlab = expression(alpha), ylab = "Matched rate")
matlines(x = tod1.mod$alpha, 
         y = tod1.mod[,-1], col = 1:5, type = "s")
box(which = "plot")
legend("bottomright", tag[-1], col = 1:5, lty = 1, bty = "n")
#--------------------------------------------------------------

#8-hour periods
time <- "shift3"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:3, sep = ""))
rank.time.dt(data = tod, tag = tag, time = time)
lvl.set.time(tag = tag, alpha = alpha)

tod1.mod <- data2pred(ref = tag[1], compare = tag[-1])
tod2.mod <- data2pred(ref = tag[2], compare = tag[-2])
tod3.mod <- data2pred(ref = tag[3], compare = tag[-3])

par(mfrow = c(2,2), family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
for (i in 1:3)
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = toupper(tag[i]), xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste("tod",i,".mod",sep=""))$alpha, 
           y = get(paste("tod",i,".mod",sep=""))[,-1], col = 1:2, type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], col = 1:2, lty = 1, bty = "n")
}
#--------------------------------------------------------------

#12-hour periods
time <- "shift2"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:2, sep = ""))
rank.time.dt(data = tod, tag = tag, time = time)
lvl.set.time(tag = tag, alpha = alpha)

tod1.mod <- data2pred(ref = tag[1], compare = tag[-1])
tod2.mod <- data2pred(ref = tag[2], compare = tag[-2])

par(mfrow = c(1,2), family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
for (i in 1:2)
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = toupper(tag[i]), xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste("tod",i,".mod",sep=""))$alpha, 
           y = get(paste("tod",i,".mod",sep=""))[,-1], type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], lty = 1, bty = "n")
}
#--------------------------------------------------------------




#test % matched segments under level sets
#2-hour periods
#size 1, "18:00-20:00" as model ----------------------------------------------------
time <- "shift12"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:12, sep = ""))

bin2rank.time(data = tod, time = time, tag = tag, brks = brks)

lvl.set.time(tag = tag, alpha = alpha)
tod10.mod <- data2pred(ref = tag[10], compare = tag[-10], var = "breaks")

par(family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
for (i in 10)
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = tag[i], xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste("tod",i,".mod",sep=""))$alpha,
           y = get(paste("tod",i,".mod",sep=""))[,-1], col = 1:11, type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], col = 1:11, lty = 1, bty = "n") 
}

#4-hour periods
#size 1, "08:00-12:00" as model ----------------------------------------------------
time <- "shift6"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:6, sep = ""))

bin2rank.time(data = tod, time = time, tag = tag, brks = brks)

lvl.set.time(tag = tag, alpha = alpha)
tod3.mod <- data2pred(ref = tag[3], compare = tag[-3], var = "breaks")

par(family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
for (i in 3)
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = tag[i], xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste("tod",i,".mod",sep=""))$alpha,
           y = get(paste("tod",i,".mod",sep=""))[,-1], col = 1:5, type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], col = 1:5, lty = 1, bty = "n") 
}

#8-hour periods
#size 1, "08:00-16:00", "16:00-24:00" as model -------------------------------------
time <- "shift3"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:3, sep = ""))

bin2rank.time(data = tod, time = time, tag = tag, brks = brks)

lvl.set.time(tag = tag, alpha = alpha)
tod2.mod <- data2pred(ref = tag[2], compare = tag[-2], var = "breaks")
tod3.mod <- data2pred(ref = tag[3], compare = tag[-3], var = "breaks")

par(mfrow=c(1,2), family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
for (i in 2:3)
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = tag[i], xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste("tod",i,".mod",sep=""))$alpha,
           y = get(paste("tod",i,".mod",sep=""))[,-1], col = 1:2, type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], col = 1:2, lty = 1, bty = "n") 
}

#12-hour periods
#size 1, "12:00-24:00" as model ----------------------------------------------------
time <- "shift2"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:2, sep = ""))

bin2rank.time(data = tod, time = time, tag = tag, brks = brks)

lvl.set.time(tag = tag, alpha = alpha)
tod2.mod <- data2pred(ref = tag[2], compare = tag[-2], var = "breaks")

par(family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
for (i in 2)
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = tag[i], xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste("tod",i,".mod",sep=""))$alpha,
           y = get(paste("tod",i,".mod",sep=""))[,-1], type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], lty = 1, bty = "n") 
}
