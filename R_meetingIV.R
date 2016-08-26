## Vectors
########################
a <- c(1,0.2,300) # numeric vector
b <- c("one","two","three") # character vector
c <- c(TRUE,TRUE,FALSE) #logical vector
a
b
c

## Access vectors:
a[1]
a[c(2,3)] # 2nd and 4th elements of vector

## Matricse
########################
# All columns in a matrix must have same mode (data type)
# generates 5 x 4 numeric matrix
y<-matrix(1:12, nrow=3,ncol=4)
y

## Access Metrices:
y[,3]
y[2,]
y[1:2,2:3]

### data.frame
########################
# Columns can have different modes (data types)
z <- data.frame(a,b,c)
names(z) <- c("count","type","coolness") # variable names 

## Access data.frames:
z[1:2] # columns 3,4,5 of data frame
z[c("count","type")] # columns ID and Age from data frame
z$type # variable x1 in the data frame
z[1:2,2:3]

### Lists
#######################
# An ordered component of objects
myshoping <- list(fruits = c("kiwi","grapes"), snacks = c("soreen", "bugles"))
myshoping
mylist <- list(name="Johan", mynumbers=a, mymatrix=y, age=34)

## Access a list
mylist
mylist[[2]]
mylist[["name"]] # 2nd component of the list

### Factors
#######################
## A nominal or categorial variable (no order) 
gender <- c(rep("male",20), rep("female", 30))
gender <- factor(gender)
summary(gender) 

## Access a factor:
# Same as vectors
gender[3]

## An ordinal variable in R - (category with Order) in this case male < female
### Another example could be bad < good < great
gender <- ordered(gender, levels = c("male","female"))
gender


### Usefull functions
###############################################
length(mylist) # number of elements or components
str(mylist)    # structure of an object
class(y)  # class or type of an object
names(mylist)  # names

c(a,b)       # combine objects into a vector
cbind(a,b) # combine objects as columns
rbind(a,b) # combine objects as rows

ls()       # list current objects
rm(a) # delete an object

z
b
which(z=="one") # Logic testing


### Loading data
#################################################
d <- read.table("ufo_data.txt", sep = "\t", fill = T, header = T, as.is = T)

### Keep only rows which start with 7 i.e valied juli dates
d <- d[strtrim(d$Date,1)==7,]

### Fix dates-times
# Duration
d[,"date"] <- as.POSIXct(strptime(d$Date, format = "%m/%d/%y %H:%M"))
d$Date <- NULL
summary(d$date)

d[1,]


# Posted
unique(d$Posted)
d[,"posted"] <- as.POSIXct(d$Posted, format = "%m/%d/%y")
d$Posted <- NULL

### Shapes
d$Shape <- gsub(" ", "", d$Shape)
sort(table(d$Shape))
d <- d[!d$Shape=="",]
d <- d[!d$Shape=="10minutes",]

### Functions and loops
unique(d$Duration)
grep("seconds",d$Duration, value = T)
#### 

text <- "20 seconds "
rm.text <- "second"
clean.number <- function(text, rm.text){
  out <- sub(rm.text, "", text)
  out <- sub("s","",out)
  out <- gsub(" ","", out)
  return(out)
}

clean.number("20 seconds ", "seconds")

for(i in 1:nrow(d)){
  if(grepl("second", d[i,"Duration"])){
    out <- clean.number(d[i,"Duration"], "second")
    d[i,"duration_min"] <- as.numeric(out) / 60
  }
  if(grepl("minute", d[i,"Duration"])){
    out <- clean.number(d[i,"Duration"], "minute")
    d[i,"duration_min"] <- as.numeric(out) 
  }
  if(grepl("hour", d[i,"Duration"])){
    out <- clean.number(d[i,"Duration"], "hour")
    d[i,"duration_min"] <- as.numeric(out) * 60 
  }
}
### Warnings tells us this is not a perfect solution but good enough for now!
## Here is an example of times converted
d[1:10,c("Duration","duration_min")]


## Exploration
### Do people think they have seen UFOs longer if they dont report it straight away?
#### Calculate difference between post date and observation date
d[,"time_diff"] <- difftime(d[,"posted"],d[,"date"],unit = "days") 
plot(d$time_diff,d$duration_min)
model <- lm(duration_min~time_diff, data = d)
abline(model)
summary(model)

### Do different shaped UFOs stay on the night sky for different amount of time?
boxplot(duration_min~Shape, data = d, las = 2, width = table(d$Shape), ylab = "Duration in min.", main = "Shape of UFO vs Duration of observation!")

### Examine specific stories
subset(d, Shape == "Light" & duration_min > 100, select = Summary)

write.table(d, "ufo_data_clean.txt", row.names = F, quote = F, sep = "\t")
