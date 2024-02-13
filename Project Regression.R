library(XML)
library(RODBC)
library(readxl)
library(jsonlite)
library(haven)
library(feather)
SLR <- function(Data){ 
  # Getting needed Data
  x <- Data$x
  y <- Data$y
  n <- length(x)
  xbar<-mean(x)
  ybar<-mean(y)
  # Calculating Sxx and Sxy and Syy
  Sxx<- sum(x^2) -n*(xbar^2)
  Syy <- sum(y^2) -n*(ybar^2)
  Sxy <- sum(x*y) -n*xbar*ybar
  # Calculating intercept(Beta Node) and Slope(Beta 1)
  Beta_1 <- Sxy / Sxx
  Beta_0 <- ybar - Beta_1*xbar
  #calculating sum squares (regression, error,total)
  SSR <-Beta_1^2*Sxx
  SST <-Syy
  SSE <-SST-SSR
  #calculating coefficient(correlation, determination)
  Rsquare<-as.numeric(SSR/SST)
  print(paste('dependent variable explained by an independent variable in regression model',Rsquare*100,'%'))
  if(Beta_1<0){
    r<-0-sqrt(Rsquare)
    print(paste("the model has negative correlation about",r))
  }else{
    r<-sqrt(Rsquare)
    print(paste("the model has positive correlation about",r))
  }
  #DEGREE OF FREEDOM 
  DFR=1
  DFE=n-2
  DFT= DFR+DFE
  #calculating Mean sum squares(regression,error)
  MSR<-SSR/DFR
  MSE<-SSE/DFE
  # calculate F0
  F0 <- MSR / MSE
  #anova table
  ANOVA=matrix(c(SSR,SSE,SST,DFR,DFE,DFT,MSR,MSE,"",F0,"",""),ncol=4)
  row.names(ANOVA)=c("Treatment","Error","Total")
  colnames(ANOVA)=c("Sum square","Degree of freedom","Mean sum square","F table")
  ANOVA<- as.table(ANOVA)
  # calculate f_test 
  SL <- as.numeric(readline("Enter significance level : "))
  Fc<- qf(SL, DFR, DFE)
  if (F0 > Fc) {
    print("Reject H0, There's relation between X and Y")
  } else {
    print("dont reject H0, There's no relation")
  }
  #Confidence Interval For B0 
  Confidence_Interval_of_B1 = function(C){
    t = qt(C/2,df=(n-2))
    margin =  t * sqrt(MSE/Sxx)
    lower_bound = Beta_1-margin
    upper_bound = Beta_1+margin
    CI = c(lower_bound,upper_bound)
    return(CI)
  }
  Confidence_Interval_of_B0 = function(C){
    t = qt(C/2,df=(n-2))
    margin =  t * sqrt(MSE*((1/n)+(xbar^2/Sxx)))
    lower_bound = Beta_0-margin
    upper_bound = Beta_0+margin
    CI = c(lower_bound,upper_bound) 
    return(CI)
  }
  B0 = Confidence_Interval_of_B0(SL)
  lower_bound_b0 = B0[2]
  upper_bound_b0 = B0[1]
  B1 = Confidence_Interval_of_B1(SL)
  lower_bound_b1 = B1[2]
  upper_bound_b1 = B1[1]
  Xn = as.numeric(readline("Enter the X for mean response: "))
  R_atX=Beta_0+Beta_1*Xn
  t=qt(SL/2, length(x)-2,lower.tail = F)
  CL = as.numeric((1-SL)*100)
  L=RatX-t*sqrt(MSE*(1/length(x)+(Xn-xbar)^2/Sxx))
  U=RatX+t*sqrt(MSE*(1/length(x)+(Xn-xbar)^2/Sxx))
  Xnew = as.numeric(readline("Enter the X for new observation: "))
  RatXnew=Beta_0+Beta_1*Xnew
  t=qt(SL/2, length(x)-2,lower.tail = F)
  CL = as.numeric((1-SL)*100)
  Lnew= RatXnew - t * sqrt(MSE*(1+1/length(x)+(Xnew-xbar)^2/Sxx))
  Unew= RatXnew + t * sqrt(MSE*(1+1/length(x)+(Xnew-xbar)^2/Sxx))
  #Print Functions
  message(cat("Value of Sxx is",Sxx
              ,"\nValue of Syy is",Syy
              ,"\nValue of Sxy is",Sxy
              ,"\nValue of Beta One(slope) is",Beta_1
              ,"\nValue of Beta node(intercept) is",Beta_0
              ,"\nValue of SST =",Syy
              ,"\nValue of SSR =",SSR
              ,"\nValue of SSE =",SSE
              ,'\nTarget explained by an Features in regression model',Rsquare*100,'%'
              ,"\nthe value of r = :",sqrt(Rsquare)
              ,"\n",lower_bound_b0,"<B0<",upper_bound_b0
              ,"\n",lower_bound_b1, "<B1<",upper_bound_b1
              ,"\nConfidence Interval for mean response at confidence level",CL,"%"
              ,"\n[",L, ",",U,"]"
              ,"\nConfidence Interval for new observation at confidence level",CL,"%"
              ,"\n[",Lnew, ",",Unew,"]"))
  print(ANOVA)
  plot(x,y,main = "Fitted Model",
       xlab ="Features",
       ylab="target")
  abline(a=Beta_0,b=Beta_1,col="red",lwd=3)
  abline(v=L,col="blue",lwd=3,lty="dotdash")
  abline(v=U,col="blue",lwd=3,lty="dotdash")
  abline(v = Lnew, col = "green", lwd = 3, lty = "dashed")
  abline(v = Unew, col = "green", lwd = 3, lty = "dashed")
}
MLR <- function(Data) {
  my_df <- as.matrix(Data)
  n_col <- length(colnames(my_df))
  
  big_x <- cbind(b0 = rep(1, nrow(my_df)), my_df)
  x <- big_x[, -ncol(big_x)]
  y <- subset(my_df, select = ncol(my_df))
  
  xt <- t(x)
  xtx <- xt %*% x
  xtx_inverse <- solve(xtx)
  xty <- xt %*% y
  betas <- xtx_inverse %*% xty
  y_hat <- x %*% betas
  
  k <- ncol(big_x[, -c(1, ncol(big_x))])
  p <- k + 1
  
  # Calculate y bar
  y_bar <- mean(y)
  n_row <- length(y)  # length of y
  yt <- t(y)  # y transpose
  betas_t <- t(betas)  # betas transpose
  betas_x_t <- betas_t %*% xt  # Calculate betas * (x transpose)
  SSE <- (yt %*% y) - (betas_x_t %*% y)  # Calculate SSE
  SST <- (yt %*% y) - n_row * (y_bar)^2  # Calculate SST
  SSR <- SST - SSE  # Calculate SSR
  
  # Calculate R Square
  Rsquare <- 1 - (SSE / SST)
  Rsquare_adj <- 1 - ((SSE * (n_row - 1)) / (SST * (n_row - p)))
  
  # Degree of freedom
  DFR <- k
  DFE <- n_row - p
  DFT <- DFR + DFE
  
  # Calculating Mean sum squares (regression, error)
  MSR <- SSR / DFR
  MSE <- SSE / DFE
  
  # Calculate F0
  F0 <- MSR / MSE
  
  # ANOVA table
  ANOVA <- matrix(
    c(SSR, SSE, SST, DFR, DFE, DFT, MSR, MSE, "", F0, "", ""),
    ncol = 4
  )
  row.names(ANOVA) <- c("Treatment", "Error", "Total")
  colnames(ANOVA) <- c("Sum square", "Degree of freedom", "Mean sum square", "F table")
  ANOVA <- as.table(ANOVA)
  
  SL <- as.numeric(readline("Enter significance level: "))
  
  # Calculate f test for MLR
  Fc <- qf(SL, DFR, DFE)
  
  if (F0 > Fc) {
    cat("H0: B1, B2, B3, ..., Bn = 0\n")
    cat("Ha: At least one coefficient is not equal to zero\n")
    cat("Reject H0. There is a relationship between X and Y.\n")
  } else {
    cat("H0: B1, B2, B3, ..., Bn = 0\n")
    cat("Ha: At least one coefficient is not equal to zero\n")
    cat("Do not reject H0. There is no relationship.\n")
  }
  
  diagonal_vector <- xtx_inverse[row(xtx_inverse) == col(xtx_inverse)]
  
  Confidence_Interval_of_B_vector <- function(SL) {
    t <- qt(SL / 2, df = n_row - p, lower.tail = FALSE)
    CI <- data.frame(lowers = numeric(length(betas)), uppers = numeric(length(betas)))
    
    for (beta in 1:length(betas)) {
      margin <- t * sqrt(MSE * diagonal_vector[beta])
      lower_bound <- betas[beta] - margin
      upper_bound <- betas[beta] + margin
      CI[beta, ] <- c(lower_bound, upper_bound)
    }
    
    return(CI)
  }
  
  Confidence_Interval_of_B_vector_data <- Confidence_Interval_of_B_vector(SL)
  
  # Confidence interval for mean response
  xo <- c(1)
  for (i in 1:(n_col - 1)) {
    xo[i + 1] <- as.numeric(readline(paste("Enter the X", i, "for mean response: ")))
  }
  
  x0 <- as.matrix(xo)
  y0 <- t(x0) %*% betas
  t <- qt(SL / 2, df = n_row - p, lower.tail = FALSE)
  X0t_xtx_inverse_X0 <- t(x0) %*% xtx_inverse %*% x0
  L_Mean_Response <- y0 - t * sqrt((MSE * X0t_xtx_inverse_X0))
  U_Mean_Response <- y0 + t * sqrt((MSE * X0t_xtx_inverse_X0))
  
  # Confidence interval for new observation
  xnew <- c(1)
  for (i in 1:(n_col - 1)) {
    xnew[i + 1] <- as.numeric(readline(paste("Enter the X", i, "for new observation: ")))
  }
  
  XN <- as.matrix(xnew)
  ynew <- t(XN) %*% betas
  t <- qt(SL / 2, df = n_row - p, lower.tail = FALSE)
  Xnewt_xtx_inverse_Xnew <- t(XN) %*% xtx_inverse %*% XN
  L_New_Obs <- ynew - t * sqrt((MSE * (diag(1) + Xnewt_xtx_inverse_Xnew)))
  U_New_Obs <- ynew + t * sqrt((MSE * (diag(1) + Xnewt_xtx_inverse_Xnew)))
  
  # ERROR STANDARD
  di <- (y - y_hat) / sqrt(MSE[1])
  # Bonus #
  partial_f_test <- function(Data) {
    for (i in 1:(n_col - 1)) {
      cat("H0: b", i, "= 0\n")
      cat("H1: b", i, "!= 0\n")
      
      data <- as.matrix(Data)
      newdata <- data[, -i]
      
      big_x_reduced <- cbind(b0 = rep(1, n_row), newdata)
      x_reduced <- big_x_reduced[, -ncol(big_x_reduced)]
      y_reduced <- subset(newdata, select = ncol(newdata))
      
      xt_reduced <- t(x_reduced)
      xtx_reduced <- xt_reduced %*% x_reduced
      xtx_inverse_reduced <- solve(xtx_reduced)
      xty_reduced <- xt_reduced %*% y_reduced
      betas_reduced <- xtx_inverse_reduced %*% xty_reduced
      y_hat_reduced <- x_reduced %*% betas_reduced
      betas_t_reduced <- t(betas_reduced)
      betas_x_t_reduced <- betas_t_reduced %*% xt_reduced
      SSE_reduced <- (t(y_reduced) %*% y_reduced) - (betas_x_t_reduced %*% y_reduced)
      SST_reduced <- (t(y_reduced) %*% y_reduced) - n_row * (mean(y_reduced))^2
      SSR_reduced <- SST_reduced - SSE_reduced
      
      F_node_reduced <- (SSR[1] - SSR_reduced[1]) / MSE[1]
      F_calc_reduced <- qf(SL, 1, DFE)
      
      beta_matrix <- matrix(nrow = n_col - 1, ncol = 1)
      
      if (F_node_reduced > F_calc_reduced) {
        cat(SSR_reduced, "\n")
        cat("Reject H0, then the model depends on x", i, "\n\n")
      } else {
        cat(SSR_reduced, "\n")
        cat("Failed to reject H0, then the model doesn't depend on x", i, "\n\n")
      }
    }
  }
  print_function <- function(){
    cat("Summary statistics:\n")
    
    cat("Matrix of X:\n")
    print(x)
    cat("\n")
    
    cat("Vector of y:\n")
    print(y)
    cat("\n")
    
    cat("Matrix of (XtX):\n")
    print(xtx)
    cat("\n")
    
    cat("Matrix of C (XtX)^-1:\n")
    print(xtx_inverse)
    cat("\n")
    
    cat("Beta values:\n")
    print(betas)
    cat("\n")
    
    cat("SST: ", SST, "\n")
    cat("SSR: ", SSR, "\n")
    cat("SSE: ", SSE, "\n")
    
    cat("R-squared: ", round(Rsquare * 100, 2), "%\n")
    cat("R-squared Adjusted : ", round(Rsquare_adj * 100, 2), "%\n")
    # Print ANOVA table
    cat("ANOVA table:\n")
    print(ANOVA)
    
    # Print the confidence interval for Beta
    for (i in 1:(n_col)) {
      L_b = Confidence_Interval_of_B_vector_data$lowers[i]
      U_b = Confidence_Interval_of_B_vector_data$uppers[i]
      cat(paste0("Confidence Interval for B", i-1, ": [", L_b, ", ", U_b, "]\n"))
    }
    # Print the mean response
    cat("Mean response: ", L_Mean_Response[1], " < Y0 < ", U_Mean_Response[1], "\n")
    # Print the new observation
    cat("New Observation: ", L_New_Obs[1], " < Y_new < ", U_New_Obs[1], "\n")
    # Print Standard Errors 
    # Example plot
    plot(di)
    abline(h=c(3,-3),col="red")
  }
  print_function()
  partial_f_test(Data)
}
path <- noquote(choose.files())
value_of_format <- as.numeric(readline("Which type of Data do you need? : \n1-CSV\n2-Excel\n3-Json\n4-XML\n5-SQL\n6-SAS\n7-SPSS\n8-Feather"))
Data <- switch (value_of_format,
                Data=read.csv(path),
                Data=read_excel(path),
                Data=fromJSON(path),
                Data=xmlTreeParse(file = path),
                Data=sqlQuery(con(odbcConnect(path)),"SELECT * FROM MY TABLE"),
                Data=read_sas(path),
                Data=read_spss(path),
                Data=read_feather(path),
                stop("Invalid input. one of the choices above you"))
value_of_regression <- as.numeric(readline("Which type of method do you need? :\n1-SLR(Simple Linear Regression)\n2-MLR(Multiple Linear Regression)"))
Function <- switch(value_of_regression,
                   SLR = SLR(Data),
                   MLR = MLR(Data),
                   stop("Invalid input. Please enter 1 or 2."))
