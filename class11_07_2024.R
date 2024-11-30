library(tidyverse)
library(readxl)
#load data ####
#sheet1 <-  read_xlsx("Raw_data/pygmy-rabbit_extra-messy.xlsx", sheet = 1)



#returns the names of the sheets
# In R programming, the `::` is the namespace operator. It allows you to specify which package a function comes from, which helps:
#   
#   1. Avoid naming conflicts between functions
# 2. Explicitly call a function from a specific package
# 3. Use a function without loading the entire package with `library()`
# 
# Examples:
#   - `readxl::excel_sheets()` means "use the excel_sheets() function from the readxl package"
# - `dplyr::filter()` means "use the filter() function from the dplyr package"
# 
# This is especially useful when multiple packages have functions with the same name.




#from readxl package used to list the names of sheets in an excel workbook. 
#returns a character vector of sheet names
readxl::excel_sheets("Raw_data/pygmy-rabbit_extra-messy.xlsx")

#sheets

length(readxl::excel_sheets("Raw_data/pygmy-rabbit_extra-messy.xlsx"))
my_list <- list()

my_file <- "Raw_data/pygmy-rabbit_extra-messy.xlsx"

#2 ways of loading different spreadsheets

for (i in 1:length(readxl::excel_sheets(my_file))){
  my_list[[i]] <-  read_xlsx(my_file, sheet = i)
}

my_list

my_other_list <- lapply(excel_sheets(my_file), FUN = function(x){
  read_xlsx(my_file, sheet=x)
})

my_other_list
sheet1 <- read_xlsx(my_file, sheet = 1)
# Store the file path as a string
file_path <- "Raw_data/pygmy-rabbit_extra-messy.xlsx"

# Get sheet names
sheet_names <- excel_sheets(file_path)

dim(my_list[[1]][, -1])
#make columns into row and rows into columns
test <- as.data.frame(matrix(ncol = nrow(my_list[[1]]),
                            nrow = ncol(my_list[[1]])))

test


colnames(test) <-  my_list[[1]]$Question


test[1,] <-  as.vector(my_list[[1]][,2])[[1]]

#only for the first sheet
for (i in 2:ncol(my_list[[1]])){
  test[i-1,] <- as.vector(my_list[[1]][, i][[1]])
}


transposed_list <- list()

for (j in 1:length(my_list)){
  transposed_list[[j]] <- as.data.frame(matrix(ncol = nrow(my_list[[j]]),
                               nrow = ncol(my_list[[j]])))
  colnames(transposed_list[[j]]) <-  my_list[[j]]$Question
  for (i in 2:ncol(my_list[[j]])){
    transposed_list[[j]][i-1,] <- as.vector(my_list[[j]][, i][[1]])
  }

}
transposed_list


lapply(transposed_list, ncol)
colnames(transposed_list[[8]])
