# Define the server logic
server <- function(input, output,session) {
  
  library(writexl)
  calculate_result <- function(){
    library(lubridate)
    
    # Define the two dates
    #données :
    start_date <- input$start_date
    first_payement_date <- input$first_payment_date
    perioddate = input$tenor
    
    number_of_payment<- function(number,frequence){
      if (round((number/12)*frequence, digits =0)<((number/12)*frequence)){
        return((round((number/12)*frequence, digits =0)+1))
      }else{
        return(round((number/12)*frequence, digits =0))
      }
    }
    
    Leap_Year <- function(a,i) {
      year = as.integer(format(a[i], "%Y"))
      # Determine the number of days in the year
      if (leap_year(year)) {
        days_in_year = 366
      } else {
        days_in_year = 365
      }
      
      # Print the result
      days_in_year
      return(days_in_year)
    }
    
    
    a <-seq(first_payement_date,length.out = perioddate , by = "month")
    a <-c(start_date,a)
    a
    end_date <- a[perioddate+1]
    
    # Calculate the number of months between the two dates
    months_between <-as.numeric(round(( end_date - first_payement_date) /30.44))+1
    # Print the result
    months_between
    
    loan_amount <- input$loan_amount
    
    interest_rate <- (input$interest)/100
    loan_term <- months_between
    
    interest_frequency <-input$coupon_frequency
    if (interest_frequency =="monthly" ){
      freq <-1
      mode <-12
    }else if (interest_frequency =="quarterly" ){
      freq <-3
      mode <-4
    }else if (interest_frequency =="semi annual" ){
      freq <-6
      mode <-2
    }else if (interest_frequency =="annual" ){
      freq <-12
      mode <-1
    }
    ammortisation_frequency<-input$amortization_frequency
    if (ammortisation_frequency =="monthly" ){
      paydate<-1
      loan_term1 <- loan_term
    }else if (ammortisation_frequency =="quarterly" ){
      paydate<-3
      loan_term1 <- number_of_payment(loan_term,4)
      if (interest_frequency =="quarterly" ){
        a<-seq(first_payement_date, end_date, by = "quarter")
        a <-c(start_date,a)
        paydate<-1
      }
    }else if (ammortisation_frequency =="semi annual" ){
      paydate<-6
      loan_term1 <- number_of_payment(loan_term,2)
      if (interest_frequency =="quarterly" ){
        a<-seq(first_payement_date, end_date, by = "quarter")
        a <-c(start_date,a)
        paydate<-2
      }else if (interest_frequency =="semi annual" ){
        b<-seq(first_payement_date,length.out = perioddate , by = "month")
        b[-1]
        a <-c(start_date,first_payement_date)
        for (i in 1:(length(b))){
          if (i%%6 == 0){
            a<-c(a,b[i])
          }
        } 
        if(perioddate%%6 ==0){
          a<-a[-length(a)]
        }
        paydate<-1
      }
      
    }else if (ammortisation_frequency =="annual" ){
      paydate<-12
      loan_term1 <- number_of_payment(loan_term,1)
      if (interest_frequency =="quarterly" ){
        a<-seq(first_payement_date, end_date, by = "quarter")
        a <-c(start_date,a)
        paydate<-4
      }else if (interest_frequency =="semi annual" ){
        b<-seq(first_payement_date,length.out = perioddate , by = "month")
        
        a <-c(start_date,first_payement_date)
        for (i in 1:(length(b))){
          if (i%%6 == 0){
            a<-c(a,b[i])
          }
        } 
        if(perioddate%%6 ==0){
          a<-a[-length(a)]
        }
        paydate<-2
      }else if (interest_frequency =="annual" ){
        a<-seq(first_payement_date, end_date, by = "year")
        a <-c(start_date,a)
        paydate<-1
        }
    }
    # Calculate the monthly payment
    monthly_payment <-function(l,r,m,p){
      if(input$Recalculation=="NO"){
       return ((loan_amount * interest_rate/mode) / (1 - (1 + interest_rate /mode)^(-loan_term1)))
      }else {
        return((l * r/m) / (1 - (1 + r /m)^(-p)))
      }
      } 
    # Initialize a data frame to store the amortization schedule
    schedule <- data.frame(period_number = 0:(length(a)-1),
                           number_of_days = 0,
                           payment_date = format(start_date, "%Y-%m-%d"),
                           balance = loan_amount,
                           interest_paid = 0,
                           principal_paid = 0,
                           Annuity = 0,
                           Remaining_balance = loan_amount)
    
    AC_BS <-input$AccrualBasis
    
    
    if (AC_BS == '30/360'){
      AC_BS = (30/360)*freq
      l<-loan_term1
      for (i in 2:(length(a))) {
        
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-as.numeric(days_in_month(a[i]))
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$Annuity[i] <-  monthly_payment(schedule$Remaining_balance[i-1],interest_rate,mode,l)
          l<-l-1
        }else{
          schedule$Annuity[i]<- schedule$interest_paid[i]
        }
        
        if ((schedule$Annuity[i] - schedule$interest_paid[i])<0){
          schedule$principal_paid[i] <- schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <- schedule$balance[i] +abs(schedule$Annuity[i] - schedule$interest_paid[i])
        }else {
          schedule$principal_paid[i] <- schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        }
        
      }
      true_annuity<-schedule$Annuity[length(a)]
      schedule$Annuity[length(a)] <- schedule$Remaining_balance[length(a)-1] + schedule$interest_paid[length(a)]
      schedule$principal_paid[length(a)] <- schedule$Annuity[length(a)] - schedule$interest_paid[length(a)]
      schedule$Remaining_balance[length(a)] <-schedule$balance[length(a)] - schedule$principal_paid[length(a)]
      # Print the amortization schedule
      print(schedule)
    }else if (AC_BS == '30/365'){
      AC_BS = (30/365)*freq
      l<-loan_term1
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-as.numeric(days_in_month(a[i]))
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$Annuity[i] <-  monthly_payment(schedule$Remaining_balance[i-1],interest_rate,mode,l)
          l<-l-1
        }else{
          schedule$Annuity[i]<-schedule$interest_paid[i]
        }
        if ((schedule$Annuity[i] - schedule$interest_paid[i])<0){
          schedule$principal_paid[i] <- schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <- schedule$balance[i] +abs(schedule$Annuity[i] - schedule$interest_paid[i])
        }else {
          schedule$principal_paid[i] <-schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        }
        
      }
      true_annuity<-schedule$Annuity[length(a)]
      schedule$Annuity[length(a)] <- schedule$Remaining_balance[length(a)-1] + schedule$interest_paid[length(a)]
      schedule$principal_paid[length(a)] <- schedule$Annuity[length(a)] - schedule$interest_paid[length(a)]
      schedule$Remaining_balance[length(a)] <-schedule$balance[length(a)] - schedule$principal_paid[length(a)]
      # Print the amortization schedule
      print(schedule)
    }else if (AC_BS =='n/360'){
      l<-loan_term1
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-(as.numeric(round((a[i] - a[i-1]))))
        AC_BS = ((as.numeric(round((a[i] - a[i-1]))))/ 360)*freq
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$Annuity[i] <-  monthly_payment(schedule$Remaining_balance[i-1],interest_rate,mode,l)
          l<-l-1
        }else{
          schedule$Annuity[i]<-schedule$interest_paid[i]
        }
        if ((schedule$Annuity[i] - schedule$interest_paid[i])<0){
          schedule$principal_paid[i] <- schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <- schedule$balance[i] +abs(schedule$Annuity[i] - schedule$interest_paid[i])
        }else {
          schedule$principal_paid[i] <-schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        }
        
      }
      true_annuity<-schedule$Annuity[length(a)]
      schedule$Annuity[length(a)] <- schedule$Remaining_balance[length(a)-1] + schedule$interest_paid[length(a)]
      schedule$principal_paid[length(a)] <- schedule$Annuity[length(a)] - schedule$interest_paid[length(a)]
      schedule$Remaining_balance[length(a)] <-schedule$balance[length(a)] - schedule$principal_paid[length(a)]
      # Print the amortization schedule
      print(schedule)
      
      
    }else if (AC_BS == 'n/365') {
      l<-loan_term1
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-(as.numeric(round((a[i] - a[i-1]))))
        AC_BS = ((as.numeric(round((a[i] - a[i-1]))))/ 365)*freq
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$Annuity[i] <-  monthly_payment(schedule$Remaining_balance[i-1],interest_rate,mode,l)
          l<-l-1
        }else{
          schedule$Annuity[i]<-schedule$interest_paid[i]
        }
        if ((schedule$Annuity[i] - schedule$interest_paid[i])<0){
          schedule$principal_paid[i] <- schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <- schedule$balance[i] +abs(schedule$Annuity[i] - schedule$interest_paid[i])
        }else {
          schedule$principal_paid[i] <-schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        }
        
      }
      true_annuity<-schedule$Annuity[length(a)]
      schedule$Annuity[length(a)] <- schedule$Remaining_balance[length(a)-1] + schedule$interest_paid[length(a)]
      schedule$principal_paid[length(a)] <- schedule$Annuity[length(a)] - schedule$interest_paid[length(a)]
      schedule$Remaining_balance[length(a)] <-schedule$balance[length(a)] - schedule$principal_paid[length(a)]
      # Print the amortization schedule
      print(schedule)
    }else {
      l<-loan_term1
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-(as.numeric(round((a[i] - a[i-1]))))
        AC_BS = ((as.numeric(round((a[i] - a[i-1]))))/ Leap_Year(a,i))*freq
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$Annuity[i] <-  monthly_payment(schedule$Remaining_balance[i-1],interest_rate,mode,l)
          l<-l-1
        }else{
          schedule$Annuity[i]<-schedule$interest_paid[i]
        }
        if ((schedule$Annuity[i] - schedule$interest_paid[i])<0){
          schedule$principal_paid[i] <- schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <- schedule$balance[i] +abs(schedule$Annuity[i] - schedule$interest_paid[i])
        }else {
          schedule$principal_paid[i] <-schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        }
        
      }
      true_annuity<-schedule$Annuity[length(a)]
      schedule$Annuity[length(a)] <- schedule$Remaining_balance[length(a)-1] + schedule$interest_paid[length(a)]
      schedule$principal_paid[length(a)] <- schedule$Annuity[length(a)] - schedule$interest_paid[length(a)]
      schedule$Remaining_balance[length(a)] <-schedule$balance[length(a)] - schedule$principal_paid[length(a)]
      # Print the amortization schedule
      print(schedule)
    }
    Somme_Interest = sum(schedule$interest_paid) 
    Somme_Principal_Paid = sum(schedule$principal_paid)
    Somme_Annuity = sum(schedule$Annuity)
    evaluate <- evaluation(Somme_Interest,Somme_Principal_Paid,Somme_Annuity,true_annuity)
    return(list(schedule,evaluate))
  }
  ###############################
  
  PM <- function(){
    library(lubridate)
    
    # Define the two dates
    #données :
    start_date <- input$start_date
    first_payement_date <- input$first_payment_date
    perioddate = input$tenor
    
    number_of_payment<- function(number,frequence){
      if (round((number/12)*frequence, digits =0)<((number/12)*frequence)){
        return((round((number/12)*frequence, digits =0)+1))
      }else{
        return(round((number/12)*frequence, digits =0))
      }
    }
    
    Leap_Year <- function(a,i) {
      year = as.integer(format(a[i], "%Y"))
      # Determine the number of days in the year
      if (leap_year(year)) {
        days_in_year = 366
      } else {
        days_in_year = 365
      }
      
      # Print the result
      days_in_year
      return(days_in_year)
    }
    
    
    a <-seq(first_payement_date,length.out = perioddate , by = "month")
    a <-c(start_date,a)
    a
    end_date <- a[perioddate+1]
    
    # Calculate the number of months between the two dates
    months_between <-as.numeric(round(( end_date - first_payement_date) /30.44))+1
    # Print the result
    months_between
    
    loan_amount <- input$loan_amount
    
    interest_rate <- (input$interest)/100
    loan_term <- months_between
    
    interest_frequency <-input$coupon_frequency
    if (interest_frequency =="monthly" ){
      freq <-1
      mode <-12
    }else if (interest_frequency =="quarterly" ){
      freq <-3
      mode <-4
    }else if (interest_frequency =="semi annual" ){
      freq <-6
      mode <-2
    }else if (interest_frequency =="annual" ){
      freq <-12
      mode <-1
    }
    ammortisation_frequency<-input$amortization_frequency
    if (ammortisation_frequency =="monthly" ){
      paydate<-1
      loan_term1 <- loan_term
    }else if (ammortisation_frequency =="quarterly" ){
      paydate<-3
      loan_term1 <- number_of_payment(loan_term,4)
      if (interest_frequency =="quarterly" ){
        a<-seq(first_payement_date, end_date, by = "quarter")
        a <-c(start_date,a)
        paydate<-1
      }
    }else if (ammortisation_frequency =="semi annual" ){
      paydate<-6
      loan_term1 <- number_of_payment(loan_term,2)
      if (interest_frequency =="quarterly" ){
        a<-seq(first_payement_date, end_date, by = "quarter")
        a <-c(start_date,a)
        paydate<-2
      }else if (interest_frequency =="semi annual" ){
        b<-seq(first_payement_date,length.out = perioddate , by = "month")
        b[-1]
        a <-c(start_date,first_payement_date)
        for (i in 1:(length(b))){
          if (i%%6 == 0){
            a<-c(a,b[i])
          }
        } 
        if(perioddate%%6 ==0){
          a<-a[-length(a)]
        }
        paydate<-1
      }
      
    }else if (ammortisation_frequency =="annual" ){
      paydate<-12
      loan_term1 <- number_of_payment(loan_term,1)
      if (interest_frequency =="quarterly" ){
        a<-seq(first_payement_date, end_date, by = "quarter")
        a <-c(start_date,a)
        paydate<-4
      }else if (interest_frequency =="semi annual" ){
        b<-seq(first_payement_date,length.out = perioddate , by = "month")
        
        a <-c(start_date,first_payement_date)
        for (i in 1:(length(b))){
          if (i%%6 == 0){
            a<-c(a,b[i])
          }
        } 
        if(perioddate%%6 ==0){
          a<-a[-length(a)]
        }
        paydate<-2
      }else if (interest_frequency =="annual" ){
        a<-seq(first_payement_date, end_date, by = "year")
        a <-c(start_date,a)
        paydate<-1
      }
    }
    # Calculate the monthly payment
    monthly_payment <-function(l,r,m,p){
      if(input$Recalculation=="NO"){
        return ((loan_amount * interest_rate*AC_BS) / (1 - (1 + interest_rate *AC_BS)^(-loan_term1)))
      }else {
        return((l * r*m) / (1 - (1 + r *m)^(-p)))
      }
    } 
    
    # Initialize a data frame to store the amortization schedule
    schedule <- data.frame(month = 0:(length(a)-1),
                           number_of_days = 0,
                           payment_date = format(start_date, "%Y-%m-%d"),
                           balance = loan_amount,
                           interest_paid = 0,
                           principal_paid = 0,
                           Annuity = 0,
                           Remaining_balance = loan_amount)
    AC_BS <- input$AccrualBasis
    
    
    
    if (AC_BS == '30/360'){
      AC_BS = (30/360)*freq
      l<-loan_term1
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-as.numeric(days_in_month(a[i]))
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$Annuity[i] <-  monthly_payment(schedule$Remaining_balance[i-1],interest_rate,AC_BS,l)
          l<-l-1
        }else{
          schedule$Annuity[i]<-schedule$interest_paid[i]
        }
        if ((schedule$Annuity[i] - schedule$interest_paid[i])<0){
          schedule$principal_paid[i] <- schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <- schedule$balance[i] +abs(schedule$Annuity[i] - schedule$interest_paid[i])
        }else {
          schedule$principal_paid[i] <-schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        }
        
      }
      true_annuity<-schedule$Annuity[length(a)]
      schedule$Annuity[length(a)] <- schedule$Remaining_balance[length(a)-1] + schedule$interest_paid[length(a)]
      schedule$principal_paid[length(a)] <- schedule$Annuity[length(a)] - schedule$interest_paid[length(a)]
      schedule$Remaining_balance[length(a)] <-schedule$balance[length(a)] - schedule$principal_paid[length(a)]
      # Print the amortization schedule
      print(schedule)
    }else if (AC_BS == '30/365'){ 
      AC_BS = (30/365)*freq
      l<-loan_term1
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-as.numeric(days_in_month(a[i]))
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$Annuity[i] <-  monthly_payment(schedule$Remaining_balance[i-1],interest_rate,AC_BS,l)
          l<-l-1
        }else{
          schedule$Annuity[i]<-schedule$interest_paid[i]
        }
        if ((schedule$Annuity[i] - schedule$interest_paid[i])<0){
          schedule$principal_paid[i] <- schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <- schedule$balance[i] +abs(schedule$Annuity[i] - schedule$interest_paid[i])
        }else {
          schedule$principal_paid[i] <-schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        }
        
      }
      true_annuity<-schedule$Annuity[length(a)]
      schedule$Annuity[length(a)] <- schedule$Remaining_balance[length(a)-1] + schedule$interest_paid[length(a)]
      schedule$principal_paid[length(a)] <- schedule$Annuity[length(a)] - schedule$interest_paid[length(a)]
      schedule$Remaining_balance[length(a)] <-schedule$balance[length(a)] - schedule$principal_paid[length(a)]
      # Print the amortization schedule
      print(schedule)
    }else if (AC_BS =='n/360'){
      l<-loan_term1
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-(as.numeric(round((a[i] - a[i-1]))))
        AC_BS = ((as.numeric(round((a[i] - a[i-1]))))/ 360)*freq
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$Annuity[i] <-  monthly_payment(schedule$Remaining_balance[i-1],interest_rate,AC_BS,l)
          l<-l-1
        }else{
          schedule$Annuity[i]<-schedule$interest_paid[i]
        }
        if ((schedule$Annuity[i] - schedule$interest_paid[i])<0){
          schedule$principal_paid[i] <- schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <- schedule$balance[i] +abs(schedule$Annuity[i] - schedule$interest_paid[i])
        }else {
          schedule$principal_paid[i] <-schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        }
        
      }
      true_annuity<-schedule$Annuity[length(a)]
      schedule$Annuity[length(a)] <- schedule$Remaining_balance[length(a)-1] + schedule$interest_paid[length(a)]
      schedule$principal_paid[length(a)] <- schedule$Annuity[length(a)] - schedule$interest_paid[length(a)]
      schedule$Remaining_balance[length(a)] <-schedule$balance[length(a)] - schedule$principal_paid[length(a)]
      # Print the amortization schedule
      print(schedule)
      
      
    }else if (AC_BS == 'n/365') {
      l<-loan_term1
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-(as.numeric(round((a[i] - a[i-1]))))
        AC_BS = ((as.numeric(round((a[i] - a[i-1]))))/ 365)*freq
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$Annuity[i] <-  monthly_payment(schedule$Remaining_balance[i-1],interest_rate,AC_BS,l)
          l<-l-1
        }else{
          schedule$Annuity[i]<-schedule$interest_paid[i]
        }
        if ((schedule$Annuity[i] - schedule$interest_paid[i])<0){
          schedule$principal_paid[i] <- schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <- schedule$balance[i] +abs(schedule$Annuity[i] - schedule$interest_paid[i])
        }else {
          schedule$principal_paid[i] <-schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        }
        
      }
      true_annuity<-schedule$Annuity[length(a)]
      schedule$Annuity[length(a)] <- schedule$Remaining_balance[length(a)-1] + schedule$interest_paid[length(a)]
      schedule$principal_paid[length(a)] <- schedule$Annuity[length(a)] - schedule$interest_paid[length(a)]
      schedule$Remaining_balance[length(a)] <-schedule$balance[length(a)] - schedule$principal_paid[length(a)]
      # Print the amortization schedule
      print(schedule)
    }else {
      l<-loan_term1
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-(as.numeric(round((a[i] - a[i-1]))))
        AC_BS = ((as.numeric(round((a[i] - a[i-1]))))/ Leap_Year(a,i))*freq
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$Annuity[i] <-  monthly_payment(schedule$Remaining_balance[i-1],interest_rate,AC_BS,l)
          l<-l-1
        }else{
          schedule$Annuity[i]<- schedule$interest_paid[i]
        }
        if ((schedule$Annuity[i] - schedule$interest_paid[i])<0){
          schedule$principal_paid[i] <- schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <- schedule$balance[i] +abs(schedule$Annuity[i] - schedule$interest_paid[i])
        }else {
          schedule$principal_paid[i] <-schedule$Annuity[i] - schedule$interest_paid[i]
          schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        }
        
      }
      true_annuity<-schedule$Annuity[length(a)] 
      schedule$Annuity[length(a)] <- schedule$Remaining_balance[length(a)-1] + schedule$interest_paid[length(a)]
      schedule$principal_paid[length(a)] <- schedule$Annuity[length(a)] - schedule$interest_paid[length(a)]
      schedule$Remaining_balance[length(a)] <-schedule$balance[length(a)] - schedule$principal_paid[length(a)]
      # Print the amortization schedule
      print(schedule)
    }
    Somme_Interest = sum(schedule$interest_paid) 
    Somme_Principal_Paid = sum(schedule$principal_paid)
    Somme_Annuity = sum(schedule$Annuity)
    evaluate <- evaluation(Somme_Interest,Somme_Principal_Paid,Somme_Annuity,true_annuity)
    return(list(schedule,evaluate))
    
  }
  evaluation <- function(a,b,c,l){
    evaluate <- data.frame(Somme_Interest = a ,
                           Somme_Principal_Paid = b,
                           Somme_Annuity = c,
                           Original_Last_Annuity = l
                           )
    return(evaluate)
  }
  ###############################
  
  CA <- function(){
    library(lubridate)
    
    # Define the two dates
    #données :
    start_date <- input$start_date
    first_payement_date <- input$first_payment_date
    perioddate = input$tenor
    
    number_of_payment<- function(number,frequence){
      if (round((number/12)*frequence, digits =0)<((number/12)*frequence)){
        return((round((number/12)*frequence, digits =0)+1))
      }else{
        return(round((number/12)*frequence, digits =0))
      }
    }
    
    Leap_Year <- function(a,i) {
      year = as.integer(format(a[i], "%Y"))
      # Determine the number of days in the year
      if (leap_year(year)) {
        days_in_year = 366
      } else {
        days_in_year = 365
      }
      
      # Print the result
      days_in_year
      return(days_in_year)
    }
    
    
    a <-seq(first_payement_date,length.out = perioddate , by = "month")
    a <-c(start_date,a)
    a
    end_date <- a[perioddate+1]
    
    # Calculate the number of months between the two dates
    months_between <-as.numeric(round(( end_date - first_payement_date) /30.44))+1
    # Print the result
    months_between
    
    loan_amount <- input$loan_amount
    
    interest_rate <- (input$interest)/100
    loan_term <- months_between
    
    interest_frequency <-input$coupon_frequency
    if (interest_frequency =="monthly" ){
      freq <-1
      mode <-12
    }else if (interest_frequency =="quarterly" ){
      freq <-3
      mode <-4
    }else if (interest_frequency =="semi annual" ){
      freq <-6
      mode <-2
    }else if (interest_frequency =="annual" ){
      freq <-12
      mode <-1
    }
    ammortisation_frequency<-input$amortization_frequency
    if (ammortisation_frequency =="monthly" ){
      paydate<-1
      loan_term1 <- loan_term
    }else if (ammortisation_frequency =="quarterly" ){
      paydate<-3
      loan_term1 <- number_of_payment(loan_term,4)
      if (interest_frequency =="quarterly" ){
        a<-seq(first_payement_date, end_date, by = "quarter")
        a <-c(start_date,a)
        paydate<-1
      }
    }else if (ammortisation_frequency =="semi annual" ){
      paydate<-6
      loan_term1 <- number_of_payment(loan_term,2)
      if (interest_frequency =="quarterly" ){
        a<-seq(first_payement_date, end_date, by = "quarter")
        a <-c(start_date,a)
        paydate<-2
      }else if (interest_frequency =="semi annual" ){
        b<-seq(first_payement_date,length.out = perioddate , by = "month")
        b[-1]
        a <-c(start_date,first_payement_date)
        for (i in 1:(length(b))){
          if (i%%6 == 0){
            a<-c(a,b[i])
          }
        } 
        if(perioddate%%6 ==0){
          a<-a[-length(a)]
        }
        paydate<-1
      }
      
    }else if (ammortisation_frequency =="annual" ){
      paydate<-12
      loan_term1 <- number_of_payment(loan_term,1)
      if (interest_frequency =="quarterly" ){
        a<-seq(first_payement_date, end_date, by = "quarter")
        a <-c(start_date,a)
        paydate<-4
      }else if (interest_frequency =="semi annual" ){
        b<-seq(first_payement_date,length.out = perioddate , by = "month")
        
        a <-c(start_date,first_payement_date)
        for (i in 1:(length(b))){
          if (i%%6 == 0){
            a<-c(a,b[i])
          }
        } 
        if(perioddate%%6 ==0){
          a<-a[-length(a)]
        }
        paydate<-2
      }else if (interest_frequency =="annual" ){
        a<-seq(first_payement_date, end_date, by = "year")
        a <-c(start_date,a)
        paydate<-1
      }
    }
    # Calculate the monthly payment
    monthly_payment <-function(){
      
      return((loan_amount /loan_term1))
      
    } 
    
    # Initialize a data frame to store the amortization schedule
    schedule <- data.frame(month = 0:(length(a)-1),
                           number_of_days = 0,
                           payment_date = format(start_date, "%Y-%m-%d"),
                           balance = loan_amount,
                           interest_paid = 0,
                           principal_paid = 0,
                           Annuity = 0,
                           Remaining_balance = loan_amount)
    AC_BS <- input$AccrualBasis
    
    
    
    if (AC_BS == '30/360'){
      AC_BS = (30/360)*freq
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-as.numeric(days_in_month(a[i]))
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$principal_paid[i] <-  monthly_payment()
          
        }else{
          schedule$principal_paid[i]<-0
        }
        schedule$Annuity[i] <- schedule$principal_paid[i] + schedule$interest_paid[i]
        schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
      }
      # Print the amortization schedule
      print(schedule)
    }else if (AC_BS == '30/365'){ 
      AC_BS = (30/365)*freq
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-as.numeric(days_in_month(a[i]))
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$principal_paid[i] <-  monthly_payment()
          
        }else{
          schedule$principal_paid[i]<-0
        }
        schedule$Annuity[i] <- schedule$principal_paid[i] + schedule$interest_paid[i]
        schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        
      }
      # Print the amortization schedule
      print(schedule)
    }
    else if (AC_BS =='n/360'){
      
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-(as.numeric(round((a[i] - a[i-1]))))
        AC_BS = ((as.numeric(round((a[i] - a[i-1]))))/ 360)*freq
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$principal_paid[i] <-  monthly_payment()
          
        }else{
          schedule$principal_paid[i]<-0
        }
        schedule$Annuity[i] <- schedule$principal_paid[i] + schedule$interest_paid[i]
        schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        
        
      }
      # Print the amortization schedule
      print(schedule)}
    else if (AC_BS == 'n/365') {
      
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-(as.numeric(round((a[i] - a[i-1]))))
        AC_BS = ((as.numeric(round((a[i] - a[i-1]))))/ 365)*freq
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$principal_paid[i] <-  monthly_payment()
          
        }else{
          schedule$principal_paid[i]<-0
        }
        schedule$Annuity[i] <- schedule$principal_paid[i] + schedule$interest_paid[i]
        schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        
        
      }
      # Print the amortization schedule
      print(schedule)}
    else {
      
      for (i in 2:(length(a))) {
        schedule$balance[i] <- schedule$Remaining_balance[i-1]
        schedule$number_of_days[i] <-(as.numeric(round((a[i] - a[i-1]))))
        AC_BS = ((as.numeric(round((a[i] - a[i-1]))))/ Leap_Year(a,i))*freq
        schedule$payment_date [i] <-format(a[i], "%Y-%m-%d")
        schedule$interest_paid[i] <- schedule$Remaining_balance[i-1]*interest_rate*AC_BS
        if((i-1)%%paydate == 0){
          schedule$principal_paid[i] <-  monthly_payment()
          
        }else{
          schedule$principal_paid[i]<-0
        }
        schedule$Annuity[i] <- schedule$principal_paid[i] + schedule$interest_paid[i]
        schedule$Remaining_balance[i] <-  schedule$Remaining_balance[i-1] - schedule$principal_paid[i]
        
        
      } 
      # Print the amortization schedule
      print(schedule) }
    
    return(schedule)
  }
  ###############################
  observeEvent(input$do, {
    if (input$amortization_type == "constant_due_amount1" ){
      schedule<-calculate_result()
      output$schedule <- renderTable({schedule[1]})
      output$evaluation <- renderTable({schedule[2]})
      output$downloadData <- downloadHandler(
        filename = function() {
          paste0("data-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          write_xlsx(schedule, path = file)
        }
      )
    }else if (input$amortization_type == "constant_due_amount2"){
      schedule<-PM()
      output$schedule <- renderTable({schedule[1]})
      output$evaluation <- renderTable({schedule[2]})
    }else if (input$amortization_type == "constant_amortization"){
      schedule<-CA()
      output$schedule <- renderTable({schedule})
    }
  })
  
  
  observeEvent(input$coupon_frequency, {
    if (input$coupon_frequency == "monthly") {
      updateSelectInput(session, "amortization_frequency", choices = c("monthly","quarterly","semi annual","annual"))
    } else if (input$coupon_frequency == "quarterly") {
      updateSelectInput(session, "amortization_frequency", choices = c("quarterly","semi annual","annual"))
    }else if (input$coupon_frequency == "semi annual") {
      updateSelectInput(session, "amortization_frequency", choices = c("semi annual","annual"))
    }else if (input$coupon_frequency == "annual") {
      updateSelectInput(session, "amortization_frequency", choices = c("annual"))
    }
  })
}