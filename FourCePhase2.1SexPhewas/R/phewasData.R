#' Function to prepare a data.frame for the PheWAS analysis
#'
#' Given the data present in patientObservation and patientSummary it will generates
#' a unique \code{data.frame} with all the variables needed for the PheWAS analysis.
#'
#' @param demographicInput The data.frame with the demographic data present in patientSummary.
#' @param diagnosisInput The data.frame with the diagnosis data present in patientObservation.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get detailed information.
#' @return An object of class \code{data.frame} with the counts.
#' @examples
#'
#' dataAnalysis <- phewasData(
#'               demographicInput = patientSummary, 
#'               diagnosisInput   = patientObservation
#'              )
#' @export phewasData

phewasData <- function( demographicInput, diagnosisInput, verbose = FALSE ){
  
  #check that the demographicInput data.frame contains all the required columns
  if( verbose == TRUE){
    print( 'Checking if the patientSummary data.frame contains the required columns: patient_num, sex, age_group, race and severe.')
  }
  
  missingColummnsDem <- which(! c("patient_num", "sex", "age_group", "race", "severe") %in% tolower( colnames( demographicInput ) ) )
  missingColummnsDia <- which(! c("patient_num", "days_since_admission", "concept_type", "concept_code" ) %in% tolower( colnames( diagnosisInput ) ) )
  
  if( length( missingColummnsDem ) == 0 & length( missingColummnsDia) == 0 ){
    print("All columns needed for the analysis are present.")
  }else{
    print( paste0( "Columns missing in PatientSummary: ", paste0(tolower( colnames( demographicInput ) )[missingColummnsDem], collapse = ", " ), "."))
    print( paste0( "Columns missing in PatientObservation: ", paste0(tolower( colnames( diagnosisInput ) )[missingColummnsDia], collapse = ", " ), "."))
    print( "Please check that the column names are not misspeled in the original files.")
    stop()
  }
  
  #check that the observation dataset contains ICD codes
  if( verbose == TRUE){
    print( 'Checking if the patientObservation data.frame contains diagnosis codes (ICD)')
  }
  
  concept_types <- unique( diagnosisInput$concept_type )
 
  if( "DIAG-ICD10" %in% concept_types | "DIAG-ICD9" %in% concept_types ){
    print( "ICD codes are present in the diagnosis data.")
  }else{
    print( "ICD codes are not present in the diagnosis data. Please check the patientObservation.csv file and verify that it contains concept type: DIAG-ICD10 or DIAG-ICD9")
    stop()
 
  }
  
  #create a new column to determine if the diagnosis was done before or after the admission
  diagnosisInput$when <- ifelse( diagnosisInput$days_since_admission >= 0, "afterAdmission", "beforeAdmission")
  #transform column names to lower case
  colnames( diagnosisInput ) <- tolower( colnames( diagnosisInput ) )
  #select the specific columns and remove potential duplicate.
  diagnosis <- unique( diagnosisInput[ diagnosisInput$concept_type %in% c( "DIAG-ICD10", "DIAG-ICD9"), 
                                    c( "patient_num", "concept_type", "concept_code", "when")] )
  
  #transform column names to lower case
  colnames( demographicInput ) <- tolower( colnames( demographicInput ) )
  demog <- demographicInput[ , c("patient_num", "sex", "age_group", "race", "severe")]
  

  #create the phewas data set for the analysis
  dataAnalysis <- merge( demog, diagnosis, by = "patient_num")

  if( verbose ==  TRUE ){
    print( "Output data.frame generated")
  }
  
  return( dataAnalysis )
}
