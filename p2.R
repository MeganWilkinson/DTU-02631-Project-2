
#' @author Megan Coleman
# roundGrade
# fcn: rounds elements in the vector on a 7 step scale
# input: grades - a vector 
# output: gradeRounded - a vector
# test: grades <- c(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0,-1,-2,-3,-4)
# roundGrade
# fcn: rounds elements in the vector on the Danish 7 step grading scale 
# (12,10,7,4,02,00)
# I chose to round to the nearesst number, with .5 rounding up because this is a grading system
# input: grades - a vector of grades of the length of number of students 
# each element represents a students grade in the class
# output: gradeRounded - a vector of grades rounded according to the 7 step scale, of the length of number of students
# each element represents a students rounded (on the 7 step scale) grade in the class
# test: grades <- c(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0,-1,-2,-3,-4)
roundGrade <- function(grades) {
  gradesRounded <- grades
  for( i in 1:length(grades)){
    if(grades[c(i)] >= 11) gradesRounded[i] <- 12
    else if(grades[c(i)] >= 8.5 && grades[c(i)] < 11) gradesRounded[i] <-10
    else if(grades[c(i)] >=  5.5 && grades[c(i)] < 8.5) gradesRounded[i] <-7
    else if(grades[c(i)] >=  3 && grades[c(i)] < 5.5) gradesRounded[i] <-4
    else if(grades[c(i)] >=  1 && grades[c(i)] < 3) gradesRounded[i] <-2
    else if(grades[c(i)] >=  -1.5 && grades[c(i)] < 1) gradesRounded[i] <-0
    else gradesRounded[i] <- -3
  }
  return(gradesRounded)
}

#' @author Megan Wilkinson
#' @description rounds a matrix holding the grades for the assignments of students.
#'              The grades are rounded based on the 7-step grading scale.
#' @param data An NxM matrix where N=(number of students) and M=(number of assignments).
#' @return An NxM matrix holding the rounded grades.
roundAll <- function(data) {
    # go through the matrix rows and round each students' grades using a helper method
    for (i in 1:nrow(data)) {
        data[i,] <- roundGrade(data[i,])
    }
    return(data)
}

#' @author Megan Coleman
# computeFinalGrades
# fcn: if any grade is -3 the final grade is -3, 
# else drop the lowest grade and calculate the mean of M-1 highest scores
# pass that vector to roundGrades
# the final vector is the scaled mean grade for each student
# input: grades - an NxM matrix with grades on the 7 step scale with N students on M assignments 
# output: gradesFinal - a vector of length n of the final grades for each of the N students
# each element represents a students final grade
# test: matrix(c(-3,0,1, 12,12,12, 3,3,3, 4,4,4), nrow=3, ncol=4) ; expected result: -3,7,7
computeFinalGrades <- function(grades) {
  gradesFinal <- c(1:nrow(grades))
  
  if(ncol(grades) == 1) gradesFinal <- grades  # one assignment (M=1), finalgrade = grade of that assignment
  
  else if(ncol(grades) > 1){    # more than one assignment (M > 1): 
    for(n in 1 : nrow(grades)){  # for each student
      lowestgrade <- grades[n,1] 
      sum <- final <- marked <- 0
      
      for(m in 1: ncol(grades)){ # look at each assignment
        if(grades[n,m] == -3){  # if any assignment= -3, final grade= -3
          gradesFinal[c(n)] == -3
          marked <- 1
        }
        else{ # else: drop lowest grade
          if(lowestgrade > grades[n,m]) lowestgrade <- grades[n,m]
          sum <- sum + grades[n,m]
        }
      }
      # final grade is mean of M-1 highest grades 
      if(marked == 0){
        sum <- sum - lowestgrade
        gradesFinal[n] <- sum/(ncol(grades)-1)
      }
      else gradesFinal[n] <- -3
    }
    gradesFinal <- roundGrade(gradesFinal)   # rounded/scaled as defined by roundGrade
  }
  return(gradesFinal)
}

#' @author Megan Wilkinson
#' @description This function displays two plots:
#'   1. “Final grades”: A bar plot of the number of students who have received each of possible 
#'   final grades on the 7-step-scale (computed using the function computeFinalGrades).
#'   2. “Grades per assignment”: A plot with the assignments on the x-axis and the grades
#'   on the y-axis. The x-axis must show all assignments from 1 to M, and the y-axis
#'   must show all grade −3 to 12. The plot must contain:
#'     1. Each of the given grades marked by a dot.
#'     2. The average grade of each of the assignments plotted as a barplot
#' @param grades An NxM matrix where N=number of students and M=number of assignments
#' @return two plots
gradesPlot <- function(grades) {
  ## FINAL GRADES PLOT
  finalGrades <- computeFinalGrades(grades)
  counts <- table(finalGrades)
  barplot(counts,
          main="Final Grades",
          xlab="Grades on the 7-Step Scale",
          ylab="Number of Students")

  ## GRADES PER ASSIGNMENT PLOT
  clrs <- rainbow(ncol(grades))
  avgs <- apply(grades, 2, mean)
  rand <- runif(nrow(grades), min=-0.1, max=0.1)
  #plot(grades[,1]+rand, rep(1, nrow(grades))+rand, xlim =range(grades) , ylim=c(1,ncol(grades)), xlab="Grades on the 7-Step Scale", ylab="Assignment", main="Grades of Each Assignment", col=clrs[1])
  barplot(avgs, horiz=TRUE, xlim=c(-3,12), xlab="Grades on the 7-Step Scale", ylab="Assignment", main="Grades of Each Assignment", xpd=FALSE)
  for (i in 1:ncol(grades)) {
      points(grades[,i]+rand, rep(i,nrow(grades))+rand, col=clrs[i]) # rep(i,nrow(grades))+rand
  }
}

#' @author Megan Wilkinson
#' @description load a csv with error handling.
#' 
#'  @param filename The name, as a string, of the file you want to load
#'  @return A matrix with the data in filename, otherwise returns NULL 
dataLoad <- function(filename) {
  dataframe <- NULL
  data <- NULL

  tryCatch({
      dataframe <- read.csv(filename)
      data <- data.matrix(dataframe)
    },
    warning = function(w) {
      cat("File loaded with the warning:")
      cat(w)
    },
    error = function(e) {
      cat("Failure to load file")
      data <- NULL
    }
  )
  
  return(dataframe, data)
}

#' MAIN SCRIPT
#' @author Megan Wilkinson
main.menu <- "This program provides a way to process grades for students using the 7-step grading scale.\nGrades must be input via CSVs (comma seperated values).\nThe CSV must have data in this format: StudentID,Name,Assignment1,Assignment2,Assignment3,...,Assignment N\n"
options.menu <- "\n1. Load new data\n2. Check for data errors\n3. Generate plots\n4. Display list of grades\n5. Display menu\n6. Quit\n"


dframe <- NULL # holds the student name and id in a data.frame
data <- NULL # holds grade data in a matrix
firstFilename <- ""

cat(main.menu)

firstFileEntered <- FALSE
## get the first set of data
while (!firstFileEntered) {
  firstFilename <- readline("Enter the name of first CSV to begin:")
  if (file.exists(firstFilename)) {
    cat("File load was successful!\n")
    firstFileEntered <- TRUE
  } else {
    cat("Error in loading the file. Please try again.")
  }
  
}

## read in the data from the filename
dframe <- read.csv(firstFilename)
data <- data.matrix(dframe)
dframe <- dframe[,1:2]
data <- roundAll(data[,3:ncol(data)])
cat(paste("Number of students:", nrow(data), "\nNumber of assignments:", ncol(data), sep = " "))

cat(options.menu)
done <- FALSE

## execute until user ends the program
while (!done) {
  action <- suppressWarnings(as.numeric(readline("Enter the number of which action you would like to perform:")))
  if (is.na(action)) { # handle NaNs
    cat("Invalid input. Please input a number from 1 to 6.\n")
  } else if (action == 1) { # Load new data
    filename <- readline("Enter the name of the file you would like to load:")
    if (file.exists(filename)) {
      dframe <- read.csv(filename)
      data <- data.matrix(dframe)
      dframe <- dframe[,1:2]
      data <- roundAll(data[,3:ncol(data)])
      cat("File load was successful!\n")
      cat(paste("Number of students:", nrow(data), "\nNumber of assignments:", ncol(data), sep = " "))
    } else {
      cat("Error in loading the file.")
    }
  } else if (action == 2) { # Check for data errors
    ## Check for duplicate student id
    duplicates <- data.frame(table(dframe$StudentID))
    cat("The following are duplicated student IDs:\n")
    print(dframe[dframe$StudentID %in% duplicates$Var1[duplicates$Freq > 1],])
    ## There is no check for invalid grades (not on the 7-step scale), the rounding method should take care of it
  } else if (action == 3) { # Generate plots
    gradesPlot(data)
  } else if (action == 4) { # Display list of grades
    ## convert our matrices to dataframes
    finalGrades <- data.frame(FinalGrades=computeFinalGrades(data))
    dframedata <- data.frame(data)
    
    ## combine the dataframes
    output <- cbind(dframe, dframedata, finalGrades) 
    print(output)
  } else if (action == 5) { # Print menu
    cat(options.menu)
  } else if (action == 6) { # Quit
    done <- TRUE
  } else { # handle invalid numbers
    cat("Invalid input. Please input a number from 1 to 6.\n")
  }
}