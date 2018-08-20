# roundGrade
# fcn: rounds elements in the vector on a 7 step scale
# input: grades - a vector 
# output: gradeRounded - a vector
# test: grades <- c(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0,-1,-2,-3,-4)
roundGrade <- function(grades) {
  gradesRounded <- grades
  for( i in 1:length(grades)){
    if(grades[c(i)] >= 12) gradesRounded[i] <- 12
    else if(grades[c(i)] >= 10 && grades[c(i)] < 12) gradesRounded[i] <-10
    else if(grades[c(i)] >=  7 && grades[c(i)] < 10) gradesRounded[i] <-7
    else if(grades[c(i)] >=  4 && grades[c(i)] < 7) gradesRounded[i] <-4
    else if(grades[c(i)] >=  2 && grades[c(i)] < 4) gradesRounded[i] <-2
    else if(grades[c(i)] >=  0 && grades[c(i)] < 2) gradesRounded[i] <-0
    else gradesRounded[i] <- -3
  }
  return(gradesRounded)
}

# computeFinalGrades
# fcn: if any grade is -3 the final grade is -3, 
# else drop the lowest grade and calculate the mean of M-1 highest scores
# pass that vector to roundGrades
# the final vector is the scaled mean grade for each student
# input: grades - an NxM matrix with grades on the 7 step scale with N students on M assignments 
# output: gradesFinal - a vector of length n of the final grades for each of the N students
# test: matrix(c(-3,0,1, 12,12,12, 3,3,3, 4,4,4), nrow=3, ncol=4)
computeFinalGrades <- function(grades) {
  gradesFinal <- c(1:nrow(grades))
  
  # one assignment (M=1), finalgrade = grade of that assignment
  if(ncol(grades) == 1) gradesFinal <- grades 
  
  else if(ncol(grades) > 1){    # more than one assignment (M > 1): 
    for(n in 1 : nrow(grades)){  # for each student
      lowestgrade <- grades[n,1] 
      sum <- final <- marked <- 0
      print("Next Student")
      
      for(m in 1: ncol(grades)){ # look at each assignment
        # if any of the assignments is a -3, then the final grade is -3
        print("Next Assignment: Contents")
        print(grades[n,m])
        
        if(grades[n,m] == -3){
          gradesFinal[c(n)] == -3
          marked <- 1
        }
        
        # else: drop lowest grade
        else{
          if(lowestgrade > grades[n,m]) lowestgrade <- grades[n,m]
          sum <- sum + grades[n,m]
        }
      }
      # final grade is mean of M-1 highest grades 
      if(marked == 0){
        sum <- sum - lowestgrade
        final <- sum/(ncol(grades)-1) 
      }
    }
    # rounded/scaled as defined by roundGrade
    gradesFinal <- roundGrade(gradesFinal)
  }
  
  return(gradesFinal)
}

gradesPlot <- function(grades) {
  # Insert your code here
}

#' main script
#' le put codez here