#function.path = "D:/caps/function_code.R"
#source(file = function.path)
load("dow.RData")
#day of week screening
#use function: obs.rank (create)
#level set plot for DOW (alpha = 0.1)
#-------------------------------
time <- "DayOfWeekUSA"
week <- obs.rank(2015, 71, time)
tag <- c("sun","mon","tue","wed","thu","fri","sat")
alpha <- 0.05



#generate data: sun, mon, tue, wed, thu, fri, sat
#------------------------------------------------
rank.time.dt(data = week, tag = tag, time = time)



#create separate data sets based on setting "Line 8-11"
#combine the results into "out"
#-------------------------------------------------------
lvl.set.time(tag = tag, alpha = alpha)
out <- rbind(sun.lvl, mon.lvl, tue.lvl, wed.lvl, thu.lvl, fri.lvl, sat.lvl)



#plot: distributions of crashes given different time
#-------------------------------------------------------------
hline.dat <- data.frame(DayOfWeekUSA = out$time, hl = out$thres)

point.dat <- data.frame(rbind(sun[which(sun$p>=out$thres[1]), ],
                              mon[which(mon$p>=out$thres[2]), ],
                              tue[which(tue$p>=out$thres[3]), ],
                              wed[which(wed$p>=out$thres[4]), ],
                              thu[which(thu$p>=out$thres[5]), ],
                              fri[which(fri$p>=out$thres[6]), ],
                              sat[which(sat$p>=out$thres[7]), ]))

ggplot(data = week, mapping = aes(x = Logmile, y = p)) +
  geom_segment(aes(xend = Logmile, yend = 0)) + 
  geom_hline(aes(yintercept=hl), data=hline.dat, col = "darkgray") + 
  geom_point(data = point.dat, aes(x = Logmile, y = p), col = 4) + 
  facet_grid(DayOfWeekUSA~.)


#KS tests: on level sets & rank for DOW data (% crashes vs. size of alpha)
#------------------------------------------------------------------------
#tag <- c("sun","mon","tue","wed","thu","fri","sat")

alpha = seq(0, 1, by = 0.01)
lvl.set.time(tag = tag, alpha = alpha)
ks1 <- origin.pair(ref = tag, compare = tag, sig = 0.3, opt = FALSE)
#------------------------------------------------------------------------






#segments binnings with size 1 and 0.5
#-------------------------------------------------------------
time <- "DayOfWeekUSA"
tag <- c("sun","mon","tue","wed","thu","fri","sat")
xlim <- c(0, ceiling(max(week$Logmile))+1)

#binnings using level sets with size 1
#-------------------------------------------------------------
brks <- seq(0, xlim[2], by = 1)
bin2rank.time(data = week, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))
ks.dowbin1 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)

#binnings using level sets with size 0.5
#-------------------------------------------------------------
brks <- seq(0, xlim[2], by = 0.5)
bin2rank.time(data = week, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))
ks.dowbin0.5 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)
#-------------------------------------------------------------







#use "sun" as reference to test other days
#test % matched segments under level sets
#--------------------------------------------------------
tag <- c("sun","mon","tue","wed","thu","fri","sat")
rank.time.dt(data = week, tag = tag, time = time)
lvl.set.time(tag = tag, alpha = alpha)
dow.mod <- data2pred(ref = tag[1], compare = tag[-1])

par(family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
plot.new()
plot.window(xlim = range(alpha), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "Sunday", xlab = expression(alpha), ylab = "Matched rate")
matlines(x = dow.mod$alpha, y = dow.mod[,-1], col = 1:6, type = "s")
box(which = "plot")
legend("bottomright", tag[-1], col = 1:6, lty = 1, bty = "n")
#--------------------------------------------------------




#use "wed" and "thu" as reference to test other days
#test % matched segments under level sets
#size 1--------------------------------------------------------
tag <- c("sun","mon","tue","wed","thu","fri","sat")
xlim <- c(0, ceiling(max(week$Logmile))+1)
brks <- seq(0, xlim[2], by = 1)
bin2rank.time(data = week, time = time, tag = tag, brks = brks)

lvl.set.time(tag = tag, alpha = alpha)
dow4.mod <- data2pred(ref = tag[4], compare = tag[-4], var = "breaks")
dow5.mod <- data2pred(ref = tag[5], compare = tag[-5], var = "breaks")

par(mfrow=c(1,2),family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
for (i in 4:5)
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = tag[i], xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste("dow",i,".mod",sep=""))$alpha,
           y = get(paste("dow",i,".mod",sep=""))[,-1], col = 1:6, type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], col = 1:6, lty = 1, bty = "n") 
}
#size 0.5, "tue"-"thu" as model --------------------------------
brks <- seq(0, xlim[2], by = 0.5)
bin2rank.time(data = week, time = time, tag = tag, brks = brks)

lvl.set.time(tag = tag, alpha = alpha)
dow3.mod <- data2pred(ref = tag[3], compare = tag[-3], var = "breaks")
dow4.mod <- data2pred(ref = tag[4], compare = tag[-4], var = "breaks")
dow5.mod <- data2pred(ref = tag[5], compare = tag[-5], var = "breaks")

par(mfrow=c(2,2),family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
for (i in 3:5)
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = tag[i], xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste("dow",i,".mod",sep=""))$alpha,
           y = get(paste("dow",i,".mod",sep=""))[,-1], col = 1:6, type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], col = 1:6, lty = 1, bty = "n") 
}