##
## Author: Tomaz Kastrun
## Date: 12.01.2017
## twitter: @tomaz_tsql
## blog: http://tomaztsql.wordpress.com


setwd("C:/DataTK")
library(ggplot2)
library(dplyr)
#install.packages("debug")
library(debug)


# Primer
# full graph
d <- data.frame(val=c(2,3,4,3,4,3,4,3,2,1), year=c(2015,2016,2017,2015,2016,2015,2015,2016,2017,2015))
ggplot(data=d, aes(x=year, y=val, fill=year)) + geom_bar(stat="identity") + guides(fill=FALSE)



# year 0 begining
png("barplot000.png")
d <- data.frame(val=c(0,0,0,0,0,0,0,0,0,0), year=c(2015,2016,2017,2015,2016,2015,2015,2016,2017,2015))
ggplot(data=d, aes(x=year, y=val, fill=year)) + geom_bar(stat="identity") + guides(fill=FALSE) 
dev.off()


# year 2015
png("barplot001.png")
d <- data.frame(val=c(2,0,0,3,0,3,4,0,0,1), year=c(2015,2016,2017,2015,2016,2015,2015,2016,2017,2015))
ggplot(data=d, aes(x=year, y=val, fill=year)) + geom_bar(stat="identity") + guides(fill=FALSE)
dev.off()

# year 2015 + 2016
png("barplot002.png")
d <- data.frame(val=c(2,3,0,3,4,3,4,3,0,1), year=c(2015,2016,2017,2015,2016,2015,2015,2016,2017,2015))
ggplot(data=d, aes(x=year, y=val, fill=year)) + geom_bar(stat="identity") + guides(fill=FALSE)
dev.off()

# year 2015 + 2016 + 2017
png("barplot003.png")
d <- data.frame(val=c(2,3,4,3,4,3,4,3,2,1), year=c(2015,2016,2017,2015,2016,2015,2015,2016,2017,2015))
ggplot(data=d, aes(x=year, y=val, fill=year)) + geom_bar(stat="identity") + guides(fill=FALSE)
dev.off()

system("magick -delay 150 -loop 0 *.png barplot.gif")

file.remove(list.files(pattern=".png"))


plot.bar <- function(df_plot, xvar, yvar, fill)
{
  
  ggplot(data=df_plot, aes(x=xvar, y=yvar, fill=xvar)) + geom_bar(stat="identity") + guides(fill=FALSE)
  
}

plot.bar(d, d$year, d$val, d$year)


################################################
####
## Replacing Changing values in data.frame
####
################################################

# dataset
d <- data.frame(val=c(2,3,4,3,4,3,4,3,2,1), year=c(2015,2016,2017,2015,2016,2015,2015,2016,2017,2015))


#number of steps - based on values in X-axis
x_unique <- unique(d$year)
nof_steps <- length(x_unique)

first_val <- 2017


#set the values of y-axis to zero, based on x-axis ordered ASC
d1 <- d %>%
      arrange(year) %>%
      filter (year<=first_val) %>%
      mutate (new_val = val) %>%
      select (year, new_val)
      
d2 <- d %>%
      arrange(year) %>%
      filter (year>first_val) %>%
      mutate (new_val = 0) %>%
      select (year,new_val)

dfinal <- union_all(d1, d2)
dfinal

dfinal <- data.frame(dfinal)
colnames(dfinal)[1] <- "x"
colnames(dfinal)[2] <- "y"


# final function for plotting graph

plot.bar.2 <- function(df_plot, xvar, yvar, fill)
{
  require(ggplot2)
  require(dplyr)
  attach(df_plot)
  #number of steps - based on values in X-axis
  x_unique <- unique(df_plot$xvar)   #xvar = year
  nof_steps <- as.integer(length(df_plot$x_unique))

  for (i in 1:nof_steps) 
  {
    x <- as.integer(x_unique[i])
    
    d1 <- df_plot %>%
          arrange(xvar) %>%
          filter(xvar<=x) %>%
          mutate(new_val = val) %>%
          select(xvar, new_val)
    
    d2 <- df_plot %>%
          arrange(xvar) %>%
          filter(xvar>x) %>%
          mutate(new_val = 0) %>%
          select(xvar,new_val)
    
    dfinal <- union_all(d1, d2)
    dfinal <- data.frame(dfinal)
    colnames(dfinal)[1] <- "x"
    colnames(dfinal)[2] <- "y"
    
    name <- paste('barplot00',i,'.png',sep="")
    png(name)
    ggplot(data=dfinal, aes(x=x, y=y, fill=x)) + geom_bar(stat="identity") + guides(fill=FALSE)
    dev.off()
    rm(d1,d2,x,dfinal) 
  }

  system("magick -delay 150 -loop 0 *.png GeomBar_plot.gif")
  file.remove(list.files(pattern=".png"))
  rm(x_unique, nof_steps)   
  detach(df_plot)
}


plot.bar.2(d, d$year, d$val, d$year)

#traceback()



