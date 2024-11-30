# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # WLF 553 - Reproducible Data Science # # # # #
# # # # # # # # # # # Fall 2024 # # # # # # # # # # # #
# # # # # # # # # # # Assignment # # # # # # # # # # # 
# # # # # # # # # Base R Programming # # # # # # # # #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Task 1 ####

# Paste the numbers from 1 to 26 next to the corresponding letter of the 
# alphabet. Your result should be a vector that looks like this:
#  "1a"  "2b"  "3c"  "4d"  "5e"  "6f"  "7g"  "8h"  "9i"  "10j" "11k" "12l" "13m" 
# "14n" "15o" "16p" "17q" "18r" "19s" "20t" "21u" "22v" "23w" "24x" "25y" "26z"

# Can you do this using vectorization? If so, write the vectorized solution. If
# it's not possible to vectorize this task, answer no. 
#solution with vectorization
vec1 <- letters[1:26]

vec2 <-  1:26


list01 <-  paste0(vec1, vec2)
list01
# Regardless of whether you can use vectorization, write a loop to perform the
# task. 
empty_list <- rep(NA, length(vec1))
for (i in 1:length(vec1)){
  empty_list[i] <-  paste0(vec1[i], vec2[i])
}
empty_list

# Now write an alternative solution to perform the task using the apply family.
#mapply is multivariate version of lapply,  allows for multiple arguements
comboVec <-  mapply(FUN = paste0, vec2, vec1)
comboVec
# Task 2 ####

# Start with the number 60. Add half of it to it. Then add half of the 
# resulting value to itself. Do this 20 times. The result should be a vector 
# that looks like this:
# 60.0000     90.0000    135.0000    202.5000    303.7500    455.6250    
# 683.4375  1025.1562   1537.7344   2306.6016   3459.9023   5189.8535   
# 7784.7803  11677.1704 17515.7556  26273.6334  39410.4501  59115.6752  
# 88673.5128 133010.2692 199515.4038

# Can you do this using vectorization? If so, write the vectorized solution. If
# it's not possible to vectorize this task, answer no. 
#There is no vectorized solution because the current answer depends on the prior
# Regardless of whether you can use vectorization, write a loop to perform the
# task. 
x <-  60
result2 <- rep(NA, 20)#empty list
for (i in 1:20){
  half <- x/2 #divide x by 2
  x <-  x + half #add half to x to get new x
  result2[i] <- x #put new x into the empty list
}
result2

# Task 3 ####

# Consider the following list:
my_list <- list(matrix(1:25, nrow = 5, ncol = 5),
                matrix(1:36, nrow = 6, ncol = 6), 
                matrix(1:49, nrow = 7, ncol = 7))

# The last task consists of transposing these matrices. The function to do so
# is t().
?t


# The result should be a list that looks like this:
# [[1]]
# [,1] [,2] [,3] [,4] [,5]
# [1,]    1    2    3    4    5
# [2,]    6    7    8    9   10
# [3,]   11   12   13   14   15
# [4,]   16   17   18   19   20
# [5,]   21   22   23   24   25
# 
# [[2]]
# [,1] [,2] [,3] [,4] [,5] [,6]
# [1,]    1    2    3    4    5    6
# [2,]    7    8    9   10   11   12
# [3,]   13   14   15   16   17   18
# [4,]   19   20   21   22   23   24
# [5,]   25   26   27   28   29   30
# [6,]   31   32   33   34   35   36
# 
# [[3]]
# [,1] [,2] [,3] [,4] [,5] [,6] [,7]
# [1,]    1    2    3    4    5    6    7
# [2,]    8    9   10   11   12   13   14
# [3,]   15   16   17   18   19   20   21
# [4,]   22   23   24   25   26   27   28
# [5,]   29   30   31   32   33   34   35
# [6,]   36   37   38   39   40   41   42
# [7,]   43   44   45   46   47   48   49

# Can you do this using vectorization? If so, write the vectorized solution. If
# it's not possible to vectorize this task, answer no. 
#vectorized solution
transposed_mat1 <- t(my_list[[1]])
transposed_mat2 <- t(my_list[[2]])
transposed_mat3 <- t(my_list[[3]])
List_transposed_mats <-list(transposed_mat1, transposed_mat2, transposed_mat3)
List_transposed_mats
# Regardless of whether you can use vectorization, write a loop to perform the
# task. 


transposedMats <- list()
for (i in 1:length(my_list)){
  transposedMats[[i]] <- t(my_list[[i]])
 
}
transposedMats

# Now write an alternative solution to perform the task using the apply family.


transposed_mat_list <- lapply(my_list, t)
