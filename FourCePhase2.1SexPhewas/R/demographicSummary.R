#' Function to summaryze the demographic information
#'
#' Given the data present in patientSummary it will generates
#' a \code{data.frame} with the number of patients per age group, and sex, and 
#' with the potential of getting a more detailed count aggregating also by race, and severity status.
#'
#' @param input The data.frame with the demographic data present in patientSummary.
#' @param by.age_group By default FALSE. Set it to TRUE if you do want to aggregate the counts based on the age_group.
#' @param by.race By default FALSE. Set it to TRUE if you want to aggregate the counts based on race.
#' @param by.severe By default FALSE. Set it to TRUE if you want to aggregate the counts based on severity
#' @param obfuscation By default FALSE. If your IRB require obfuscation, change it to the minimum patient count allowed.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get detailed information.
#' @return An object of class \code{data.frame} with the counts.
#' @examples
#'
#' demographicsCount <- demographicSummary(
#'               input        = patientSummary 
#'               by.age_group = TRUE,
#'               by.race      = FALSE,
#'               by.severe    = FALSE,
#'               obfuscation  = 3
#'              )
#' @export demographicSummary

demographicSummary <- function( input, by.age_group = FALSE, by.race = FALSE, by.severe = FALSE, plot = FALSE, obfuscation = FALSE, verbose = FALSE ){
  
  #check that the input data.frame contains all the required columns
  if( verbose == TRUE){
    print( 'Checking if the data.frame contains the required columns: patient_num, sex, age_group, race and severe.')
  }
  
  missingColummns <- which(! c("patient_num", "sex", "age_group", "race", "severe") %in% tolower( colnames( input ) ) )
  if( length( missingColummns ) == 0 ){
    print("All columns needed for the analysis are present.")
  }else{
    print( paste0( "Columns missing: ", paste0(tolower( colnames( input ) )[missingColummns], collapse = ", " ), "."))
    print( "Please check that the column names are not misspeled in the patientSummary file.")
    stop()
  }
  
  # Create a data.frame subset with the variables of interest
  colnames( input ) <- tolower( colnames( input ) )
  selection <- input[ , c("patient_num", "sex", "age_group", "race", "severe")]
  
  if( by.age_group == FALSE & by.race == FALSE & by.severe == FALSE ){
    
    print( "Estimating aggregate counts (patients by sex)" )
    summary <- rbind(plyr::ddply(input,
                                 plyr::.(sex),
                                 summarise,COUNT = length(sex)))
  }else if( by.age_group == TRUE & by.race == FALSE & by.severe == FALSE ){
    
    print( "Estimating aggregate counts (patients by sex and age)" )
    summary <- rbind(plyr::ddply(input,
                                 plyr::.(age_group,sex),
                                 summarise,COUNT = length(sex)))
  }else if( by.age_group == TRUE & by.race == TRUE & by.severe == FALSE ){
    
    print( "Estimating aggregate counts (patients by sex, age and race)" )
    summary <- rbind(plyr::ddply(input,
                                 plyr::.(age_group,sex, race),
                                 summarise,COUNT = length(sex)))
  }else if( by.age_group == TRUE & by.race == TRUE & by.severe == TRUE ){
    
    print( "Estimating aggregate counts (patients by sex, age, race and severity)" )
    summary <- rbind(plyr::ddply(input,
                                 plyr::.(age_group,sex, race, severe),
                                 summarise,COUNT = length(sex)))
  }else if( by.age_group == TRUE & by.race == FALSE & by.severe == TRUE ){
    
    print( "Estimating aggregate counts (patients by sex, age and severity)" )
    summary <- rbind(plyr::ddply(input,
                                 plyr::.(age_group,sex, severe),
                                 summarise,COUNT = length(sex)))
  }else if( by.age_group == FALSE & by.race == TRUE & by.severe == TRUE ){
    
    print( "Estimating aggregate counts (patients by race and severity)" )
    summary <- rbind(plyr::ddply(input,
                                 plyr::.(age_group,race, severe),
                                 summarise,COUNT = length(sex)))
  }else if( by.age_group == TRUE & by.race == FALSE & by.severe == TRUE ){
    
   print( "Estimating aggregate counts (patients by age and severity)" )
    summary <- rbind(plyr::ddply(input,
                                 plyr::.(age_group,age, severe),
                                 summarise,COUNT = length(sex)))
  }  
  
  if( verbose ==  TRUE ){
    print( "Output data.frame generated")
  }
  
  if( obfuscation != FALSE ){
    summary$COUNT <- ifelse( summary$COUNT < obfuscation, -1, summary$COUNT)
  }
  
  #output in decreasing order
  summary <- summary[ order( summary$COUNT, decreasing = TRUE), ]

    return( summary )
}
