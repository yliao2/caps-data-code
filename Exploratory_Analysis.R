install.packages("tidyverse")
install.packages("dpl")
install.packages("tibble")
install.packages("purrr")
install.packages("stringr")
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("chron")
install.packages("lubridate")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(readxl)
library(gridExtra)
library(chron)
library(lubridate)

#File path..
path.root <- "D:/University of Alabama/Spring-2017/Research-Crash/MD Porter Files/"
file.CrashReports1     <- "CrashReports1.csv"
file.CrashReports2     <- "CrashReports2.csv" 
file.HighwayCrash2015  <- "HighwayCrash-1-2015.csv"
file.Route_Discrepancy <- "Route_Discrepancy.csv"
file.blankRoute        <-  "blankRoute.csv"

#Load Data files
df.CrashReports1    <- read.csv(paste0(path.root,file.CrashReports1))
df.CrashReports2    <- read.csv(paste0(path.root,file.CrashReports2))
df.HighwayCrash2015 <- read.csv(paste0(path.root,file.HighwayCrash2015))
df.Route_Discrepancy <- read.csv(paste0(path.root,file.Route_Discrepancy))
df.blankRoute   <- read.csv(paste0(path.root,file.blankRoute))  

#Data Processing.. 
#For Highway
str(df.HighwayCrash2015)
#Change to factor
df.HighwayCrash2015$AHTD_Distr <- as.factor(df.HighwayCrash2015$AHTD_Distr)
df.HighwayCrash2015$NumberLane <- as.factor(df.HighwayCrash2015$NumberLane)

str(df.CrashReports2)
head(df.CrashReports2)

#For Crash
#Change to Data n Time Data type
df.CrashReports2$CrashDate = as.Date(df.CrashReports2$CrashDate, "%Y-%m-%d")
df.CrashReports2$CrashTime = chron(times=df.CrashReports2$CrashTime)

#Day wise Crashdata
b2015 = 
  filter(df.CrashReports2, format(CrashDate, '%Y') == 2015) %>% 
  mutate(dow = weekdays(CrashDate, abbr=TRUE),
         dow = factor(dow, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", 
                                    "Sat", "Sun"))) %>% 
  select(CrashDate, dow, everything(), -ID) %>% 
  arrange(CrashDate)

head(b2015)


#All Funtion definations

#-- Highway factor summaries
my_sum <- function(df, varname){
  df %>% group_by_(varname) %>% 
    summarize(n=n(),
              total = sum(CrashCount),   avg=round(mean(CrashCount),digits=2), 
              totalf = sum(FatalCount),  avgf=round(mean(FatalCount),digits=2),
              pfatal =  round(totalf/total, digits=2)) %>%
    mutate(ptotal = round(total/sum(total), digits=2), ptotalf = round(totalf/sum(totalf),digits=2), pn=round(n/sum(n),digits=2)) 
    
    
}
my_sum(df.HighwayCrash2015,"AHTD_Distr")

#-- Categorical Plot
cat_plot <- function(x, varname, title, legend=FALSE){
  if(missing(title)) title = varname
  x = my_sum(x, varname) %>% 
    gather( type, value, ptotal, ptotalf)
  pp = ggplot(x, aes_string(x=varname, y='value', fill='type')) + 
    geom_col(position="dodge")+
    geom_errorbar(aes(ymax=pn, ymin=pn), color="black") +
    geom_text(aes(label=ifelse(type=="ptotal", total, totalf)),
              position=position_dodge(.9), vjust = -.5, size=3.5) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
    labs(y="proportion", title=title, fill="") + 
    scale_fill_brewer( palette="Set2",
                       breaks= c("ptotal", "ptotalf"),
                       labels= c("% of total", "% of fatals") 
    ) +  
    theme(axis.title.x = element_blank())   # remove x-axis label
  # caption="2015 data") 
  if(!legend) pp = pp + guides(fill=FALSE) # remove legend
  pp
}


#Call Categorical plots
p1 = cat_plot(filter(df.HighwayCrash2015, Rural_Urba !=4), "Rural_Urba", legend=TRUE) 
p2 = cat_plot(filter(df.HighwayCrash2015, MedianType != 6), "MedianType") 
p3 = cat_plot(df.HighwayCrash2015, "NumberLane")
p4 = cat_plot(df.HighwayCrash2015 , "AHTD_Distr")
p5 = cat_plot(df.HighwayCrash2015 , "FuncClass")
p6 = cat_plot(df.HighwayCrash2015 , "RouteSign")
p7 = cat_plot(df.HighwayCrash2015 , "APHN")
p8 = cat_plot(df.HighwayCrash2015 , "Access")
p9 = cat_plot(df.HighwayCrash2015 , "TypeOperat")

#arrange grid..
grid.arrange(p3,p4,p5,p6,p7,p8,p9,p2,p1, ncol=2)

#-- DOW analysis
dtime = count(b2015, CrashDate) %>% 
  mutate(dow = weekdays(CrashDate, abbr=TRUE)) %>%
  mutate(dow = factor(dow, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) 

ggplot(dtime, aes(CrashDate,n, color=dow, shape=dow)) + 
  geom_smooth(se=FALSE) +
  geom_point() + ylab('# per day') + xlab("Date") + scale_color_brewer(palette="Dark2")

#Weeend effects

 head(tow)
 hour(as.POSIXct.Date(b2015$CrashTime))

tow = b2015 %>% 
  mutate(CrashTime = hour(as.POSIXct.Date(b2015$CrashTime)))  %>% 
  mutate(tod = floor(CrashTime)) %>% 
  count(dow, tod) %>% 
  ungroup %>% complete(dow, tod, fill=list(n=0))   # check for zero counts


ggplot(tow, aes(tod, n)) + xlab("time of day") + ylab("count")+
  # geom_line(data=count(tow, tod, wt=n), aes(tod,nn/7), color="red", alpha=.6) + # overall trend line
  # geom_col() + 
  geom_point() + geom_line() +
  # facet_grid(~dow)
  facet_wrap(~dow)

#-- Make Surveillance Plots
s = df.HighwayCrash2015 %>% select(HighwayKey, CrashCount, FatalCount) %>% 
  arrange(desc(CrashCount)) %>% 
  mutate(cum_crash=cumsum(CrashCount), cum_fatal = cumsum(FatalCount),
         count=row_number(),
         pcrash = cum_crash/max(cum_crash),
         pfatal = cum_fatal/max(cum_fatal)) # %>%   filter(count<300)
head(s)
par(mar=c(5,4,4,4)) 
plot(s$count, s$pcrash, typ='n', col="black",lwd=2,las=1,
     xlab='size of hotspot (# of highway points)', 
     ylab='proportion of events in hotspot',
     main='Surveillance Plot' #,ylim=c(0,1)
     ,col.axis='grey50',cex.axis=.8
     ,ylim=c(0,.6),xlim=c(0,300), xaxt='n')
#grid()  # add grid

abline(h=seq(0,1, by=.10), col="lightgray")
abline(v=seq(0, 1000, by=50), col="lightgray")
box()
axis(1, at=seq(0,1000, by=50),col.axis='grey50',cex.axis=.8,tcl=-.25,las=1)
lines(s$count, s$pcrash, col="black", lwd=2)
axis(4, at=axTicks(4), labels=round(axTicks(4)*max(s$cum_crash)),
     col.axis='grey50',cex.axis=.8,tcl=-.55,las=1)
mtext("# of events in hotspot",side=4,line=2.7)
lines(s$count, s$pfatal, col="red", lwd=2)
legend("topleft", c("all crashes", "fatal crashes"), col=1:2, lwd=2,
       bty='n',inset=0, cex=.85)


#Crash events

filter(df.CrashReports2, CrashDate >= as.Date("2015-01-01")) %>% 
  count(CrashDate) %>% 
  ggplot(., aes(CrashDate,n)) + geom_smooth(se=TRUE) +
  geom_point() + ylab('# crashes per day') + xlab("Date")

#
head(df.blankRoute)
str(df.blankRoute)

ggplot(df.blankRoute, aes(YearMonth, Count)) + xlab("Year-Month") + ylab("count")+
geom_col() 
