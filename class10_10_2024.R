vignette(package = 'dplyr')
vignette(package = "car")


#functions ####
round(3.48595, 3)
round(x = 4.96023, digits = 2)
ceiling(3.456)
?ceiling
floor(56.29387485)

# Alt -, assignment operator
#data types ####

x <- 4
#pass number to r assumes it is numeric
class(x)
x <- as.integer(x)#coerce to int
class(x)
class(4.592)
as.integer(4.592)#truncates number
#can always convert int to numeric
#logical

z <- TRUE
class(z)


#can you convert integer to a numeric
y <- 10
class(as.numeric(y))

#convert numeric to integer
class(2)

#convert numeric to character
number <- 10
class(10)
class(as.character(number))
#can convert logical to numeric
class(as.numeric(z))
logVec <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
class(logVec)
class(as.numeric(logVec))
print(as.numeric(logVec))
nums <- c(23, 0, 0, 56, 2, 3, 0)
as.logical(nums)
#convert log to character
as.character(logVec)

#convert char to logical, cannot do!, only if char value is TRUE or FALSE
t <- c("tree", "car", "boat", "bike", "road")
as.logical(t)



#object types
#vectors ####
#collection of elements same data type, one dimension only
vec1 <- c(1, 2, 3)#c means combine
length(vec1)
vec2 <- 1:10
class(vec1)
vec3 <- c(2, 1, 3, TRUE, FALSE)
class(vec3)
vec3
vec5 <- c(2,3, "tree")
class(vec5)

j <- 0:10
k <-  seq(0, 10, 1)
#normal distribution
x <-  20
u <-  2
sd <- 1
dnorm(x,u,sd)
pnorm(3, u, sd)#returns AUC to the left of 3


?qqnorm
#qqnorm()compares quantiles of standard normal distribution with
#quantiles of sample data


#matrix- 2D, one data type, 2 dimensions
mat1<- matrix(1:25, nrow = 5, ncol = 5)
mat1
class(mat1)
mode(mat1)

mat10<- matrix(1:25, nrow = 5, ncol = 5, byrow = TRUE)
mat10
mat4 <- matrix(paste0("0", 1:25), nrow = 5, ncol = 5)
mat4
dim(mat1)
mode(mat4)
#array - stack of matrices
array1 <- array(1:75, dim = c(5,5,3))
array1
#data frame - 2D, different data types, columns are vectors(same data type)
#enter data as a series of columns
df1 <- data.frame(numbers = 1:3, animals = c("dog", "cat", "lion"),
                   is_wild = c(FALSE, FALSE, TRUE))
df1
class(df1)
#list - most flexible, can be anything
list2 <- list(vec1, mat4, df1)
list2
list2[[2]][3]#[[]] to get into the bucket then index to extract specific element




#factor - special type of vector encodes categorical data
factor1 <- factor(x = c("A", "B", "C"))
factor1
factor5 <- factor(c("A", "A", "C", "C", "B", "B"))
factor5


stock <- read.csv("stock_data.csv")
class(stock)




#positional subsetting ####
vec1[3]
vec6 <- LETTERS[1:10]
vec6[3]
vec6
vec6[c(3,2, 1)]


mat4[2,2]
array1[3, 4, 3]
array1[2,2,2]

df1[1:2, 2]
df1[c(1,2), 2]
df1[1, 2]
df1[c(1, 3, 1, 1, 1), 2]
?save
?saveRDS
df1[df1$animals == "dog", ]
?is.na


#load dragons table
dragons_db <-  DBI::dbConnect(drv = RSQLite::SQLite(),
                              "dragons.db")

dragons <-  DBI::dbGetQuery(dragons_db,
                            "SELECT * FROM dragons;")
dragons[is.na(dragons$sex),]
dragons[!is.na(dragons$sex), ]


dragons[dragons$species == "Forest Dragon", ]
dragons[dragons$species == "Forest Dragon" | 
        dragons$species == "Mountain Dragon", ]


dragons[dragons$species %in% c("Forest Dragon",
                               "Mountain Dragon"), ]

df1
df1[df1$numbers >= 1, ]
df1[df1$numbers > 1, ]

df1[df1$is_wild, ]
df1[!df1$is_wild, ]
