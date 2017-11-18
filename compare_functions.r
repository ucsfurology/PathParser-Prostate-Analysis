vars <- c("glcombined", 
          "pathmgnpos", "pathecepos", "pathsvipos", 
          "pathnodes_dissected", "pathnodes_positive", "pathnodes_status",
          "tstagep", "nstagep")


# Function to compare 2 data types against each other
compare2 <- function(x, y, varname) {
  # x is the first data source that will be used in the comparison
  # y is the first data source that will be used in the comparison
  # varname represents variables to compare
  x_compare <- paste0(x,"_",varname)
  y_compare <- paste0(y,"_",varname)
  
  total <- sum(!is.na(pp[,x_compare] == pp[,y_compare]))
  correct <- sum(pp[,x_compare] == pp[,y_compare], na.rm=T)
  
  result <- c(
    sprintf("%.0f",total), 
    sprintf("%.1f%%", (correct/total)*100)
  )
  m <- matrix(result, 1, 2)
  colnames(m) <- c(paste(x,"vs.", y, "(n)"),paste(x,"vs.", y, "(%)"))
  rownames(m) <- varname
  return(m)
}

# Function to compare all 3 data types against each other
compare3 <- function(varname) {
  # y represents variables to compare
  p_compare <- paste0("p_",varname)
  s_compare <- paste0("s_",varname)
  u_compare <- paste0("u_",varname)
  total <- sum(!is.na(pp[,u_compare]) & !is.na(pp[,s_compare]) & !is.na(pp[,p_compare]))
  correct <- sum(pp[,u_compare] == pp[,s_compare] & pp[,u_compare] == pp[,p_compare], na.rm = T)
  result <- c(
    sprintf("%.0f",total), 
    sprintf("%.1f%%", (correct/total)*100)
  )
  m <- matrix(result, 1, 2)
  colnames(m) <- c("All 3 (n)","All 3 (%)")
  rownames(m) <- varname
  return(m)
}

# Functiomn to make wide table of comparisons
allcomparisons <- function(x) {
  cbind(
    compare2("u", "p", x),
    compare2("u", "s", x),
    compare2("p", "s", x),
    compare3(x)
  )  
}

