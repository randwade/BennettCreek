library(Rainmaker)

#######################################################################################

######################################################################################
setwd("~/R/Bennett/county precip and flow")
Q <- read.csv("11518007-stage-discharge.csv")
P <- read.csv("11518000-Bennett_Ck_Rain_forRM.csv")
P$rain <- P$rain*25 #inverse cm/in conversion, accounting for rounding

Q2 <- RMprep(Q, prep.type = 1, date.type = 1,dates.in = "default", dates.out = "pdate", cnames.in = "",
             cnames.new = "discharge",tz="MST7MDT" )
P2 <- RMprep(P, prep.type = 1, date.type = 1,dates.in = "default", dates.out = "pdate", cnames.in = "rain",
             cnames.new = "rain",tz="MST7MDT" )

event.list<- RMevents(df=P2, ieHr = .25, rainthresh = 3)

events <- event.list$storms2  # get the storms that surpass rainthresh
RMevents.plot(df=P2, 
              date="pdate", 
              rain="rain", 
              df.events=events, 
              sdate="StartDate", 
              edate="EndDate", 
              depth="rain", 
              plot.buffer=0.1, 
              site.name="Bennett Creek Culvert")

# Create the plot
RMevents.plotQ(
  df = P2,
  dfQ = Q2,
  date = "pdate",
  Qdate = "pdate",
  rain = "rain",
  Q = "Discharge",
  df.events = event.list$storms2,
  sdate = "StartDate",
  edate = "EndDate",
  erain = "rain",
  plot.buffer = 0.1,
  logy = "", # Use "y" for log scale or "" for linear scale
  site.name = "Bennett Creek Culvert",
  SampleInfo = FALSE # Change this to TRUE if you want to plot sample start and end times
)
