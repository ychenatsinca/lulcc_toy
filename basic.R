1+2            # calculator
log(3.14)
x=1+2          # one plus two assigned to x
x=c(1,3,5,7)   # assign a vector
blsmean(x)        # use the build in function mean() to calculate the average of x
log(x)         # use function log() to calculate the natural log of xg

factorial(4) 
sin(pi)
x.vec <- c(2:5)
exp(x.vec) 
matrix(c(1:6), nrow = 2, ncol=3)
weight = c(50, 45, 67, 53)
mean(weight)
sd(weight)

## show objects
objects() # show all objects used currently
ls()     # show all objects used currently

rm(x, x.vec) # delete x and x.vec  



getwd() # show your current working directory
setwd("/cloud/project/Rdata")
getwd()

x.vec <- c(1, 2, 3, 4, 5)
log(x=x.vec)

log(x=x.vec,base = 2)

## data frame
# assign values to vectors 
 id.vec <- c(1, 2, 3, 4)
age.vec <- c(35, 55, 45, 25)
sex.vec <- c("Male", "Male", "Female", "Female")
disease.vec <- c("Yes", "No", "No", "Yes")

# create a data.frame:x.df based on vectors 
x.df  <-  data.frame(
  id  = id.vec,
  age = age.vec,
  sex = sex.vec,
  disease = disease.vec
)

# the structure of the object
str(x.df)


## array()
a.vec <- 1:24
a.vec
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
b.array <- array(a.vec, dim = c(4, 3, 2),
                 dimnames = list(c("x1", "x2", "x3", "x4"),
                                 c("y1", "y2", "y3"),
                                 c("z1", "z2")))
# show b.array
b.array


#list 

my.list <- list(a=a.vec, b=b.array, c=x.df)
my.list$a
my.list$c


# if-else condition

#
x <- c(1:5)
x
if (x[2] >= 3) {
    print("x is greater than 3")
    print("or x is equal to 3")
  } else{
    print("x is less than 3")
    print("x < 3")
  }

# for loop
for (a.vec in 5:8)  print(a.vec)


#for loop + if-else condition

for ( i in 1:length(x)) {
      print(paste("Xi:",i,sep=""))
      if (x[i] >= 3) {
         print("x is greater than 3")
         print("or x is equal to 3")
      } else{
         print("x is less than 3")
         print("x < 3")
  } #end if-else
}# end for 
