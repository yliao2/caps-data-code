
#mon of year screening
#use function: obs.rank (create)
#level set plot for moy (alpha = 0.05)
#-------------------------------
time <- "Month"
month <- obs.rank(2015, 71, time)
tag <- c("jan","feb","mar","apr","may","jun","jul", "aug",
         "sep","oct","nov","dec")
alpha <- 0.05


#create separate data sets based on the setting "Line 5-9"
#generate data: jan ~ dec
rank.time.dt(data = month, tag = tag, time = time)


#create separate data sets based on setting "Line 5-9"
#combine the results into "out"
lvl.set.time(tag = tag, alpha = alpha)
out <- rbind(jan.lvl, feb.lvl, mar.lvl, apr.lvl, may.lvl, jun.lvl, 
             jul.lvl, aug.lvl, sep.lvl, oct.lvl, nov.lvl, dec.lvl)



hline.dat <- data.frame(Month = out$time, hl = out$thres)

point.dat <- data.frame(rbind(jan[which(jan$p>=out$thres[1]), ],
                              feb[which(feb$p>=out$thres[2]), ],
                              mar[which(mar$p>=out$thres[3]), ],
                              apr[which(apr$p>=out$thres[4]), ],
                              may[which(may$p>=out$thres[5]), ],
                              jun[which(jun$p>=out$thres[6]), ],
                              jul[which(jul$p>=out$thres[7]), ],
                              aug[which(aug$p>=out$thres[8]), ],
                              sep[which(sep$p>=out$thres[9]), ],
                              oct[which(oct$p>=out$thres[10]), ],
                              nov[which(nov$p>=out$thres[11]), ],
                              dec[which(dec$p>=out$thres[12]), ]))

ggplot(data = month, mapping = aes(x = Logmile, y = p)) +
  geom_segment(aes(xend = Logmile, yend = 0)) + 
  geom_hline(aes(yintercept=hl), data=hline.dat, col = "darkgray") + 
  geom_point(data = point.dat, aes(x = Logmile, y = p), col = 4) + 
  facet_grid(Month~.)


#tests for surveillance plot setting for MOY data
#(size of alpha vs. cumulative % crashes: largest -> smallest, in rank order)
#----------------------------------------------------------------------------
#time <- "Month"
#week <- obs.rank(2015, 71, time)
#tag <- tag <- c("jan","feb","mar","apr","may","jun","jul", "aug", 
#                "sep","oct","nov","dec")
#alpha <- 0.05

out1 <- NULL
for (var in tag)
{out1 <- rbind(out1, get(var))}

pair.data(data = out1, time = time, 
          tag = tag, var = "p", alpha = 0.05)

#ks test results for surveillance plot (jan ~ dec):
# same:
#  jan - jun, jul
#  feb - mar ~ dec
#  mar - apr, may, jul ~ oct, dec
#  apr - may, aug ~ oct, dec
#  may - jun ~ sep, nov, dec
#  jun - jul, sep, nov
#  jul - aug, sep, nov, dec
#  aug - sep, oct, dec
#  sep - oct ~ dec
#  oct - dec
#  nov - dec
# diff: (the rest)
#
# break (winter & summer) vs. non-break
#--------------------------------------


#level set setting for MOY data (% crashes vs. size of alpha)
#------------------------------------------------------------
#tag <- c("jan","feb","mar","apr","may","jun","jul", "aug", 
#         "sep","oct","nov","dec")
#step 1: create a list of level set results given "tag"
alpha = seq(0, 1, by = 0.01)
lvl.set.time(tag = tag, alpha = alpha)
out2 <- rbind(jan.lvl,feb.lvl,mar.lvl,apr.lvl,may.lvl,jun.lvl,jul.lvl,
              aug.lvl,sep.lvl,oct.lvl,nov.lvl,dec.lvl)


#step 2: rename tag to .lvl data
#compare every combination of .lvl data
tag <- c("jan.lvl","feb.lvl","mar.lvl","apr.lvl","may.lvl","jun.lvl",
         "jul.lvl","aug.lvl","sep.lvl","oct.lvl","nov.lvl","dec.lvl")
pair.data(data = data.frame(out2), time = "time", 
          tag = tag, var = "pct.events", alpha = 0.05)

#same: feb - jul, mar - may, mar - sep, may - sep
#-------------------------------------------------


#only consider top 10% of segments
#---------------------------------
#find % matched segments for every combination of "tag"
#(ex. jan - feb, jan - mar, jan - apr, ...)
#-------------------------------------------------------

alpha <- 0.05
tag <- c("jan","feb","mar","apr","may","jun","jul", "aug", 
         "sep","oct","nov","dec")
lvl.set.time(tag = tag, alpha = alpha)
out3 <- rbind(jan.lvl,feb.lvl,mar.lvl,apr.lvl,may.lvl,jun.lvl,jul.lvl,
              aug.lvl,sep.lvl,oct.lvl,nov.lvl,dec.lvl)
pair.match(tag = tag, alpha = alpha) %>% arrange(-match)
