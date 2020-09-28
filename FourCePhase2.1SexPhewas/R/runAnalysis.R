
#' Runs the analytic workflow for the SexPhewas project
#'
#' @keywords 4CE
#' @export

runAnalysis <- function( dir.data, dir.output, obfuscation ){
    
    ### Read the CSV input files
    files <- readInputFiles( path      = dir.data, 
                             separator = ",",
                             skip      = 1, 
                             verbose   = TRUE )
    
    ### Extract the patient summary and observation information. 
    patientSummary <- files[[1]]
    patientObservations <- files[[2]]
    
    ### Demographic Summary
    summary <- demographicSummary( input = patientSummary, 
                                   by.age_group = TRUE,
                                   by.race      = FALSE,
                                   by.severe    = FALSE, 
                                   obfuscation  = obfuscation,
    )
    head(summary)
    write.table( summary, file = paste0( dir.output, "demographicSummary.csv"), col.names = TRUE, row.names = FALSE)
    
    
    #### Example plot 
    p <- ggplot( data = summary, aes( x = age_group, y = COUNT, fill = sex ) ) +
      geom_bar( stat = "identity", position = position_dodge() ) + 
      labs( title = "Demographics", x="Age range", y = "Number of patients" ) +
      scale_fill_manual( values = c( 'darkblue','orange' ) ) +
      theme_classic()
    p

    ## Generate Sex PheWAS input 
    dataAnalysis <- phewasData(
      demographicInput = patientSummary,
      diagnosisInput   = patientObservations, 
      verbose = TRUE
    )
    
    ### Diagnosis summary
    diagnosisCount <- diagnosisSummary(
      input        = dataAnalysis, 
      by.age_group = TRUE,
      by.race      = FALSE,
      by.severe    = FALSE, 
      obfuscation  = obfuscation
    )
    
    diagnosisCountAfterAdmission <- diagnosisSummary(
      input         = dataAnalysis,
      diagnosisTime = "afterAdmission",
      by.age_group = FALSE,
      by.race      = FALSE,
      by.severe    = FALSE, 
      obfuscation  = obfuscation
    )
    
    ## Phenotype comorbidity analysis
    #To identify statistically significant phenotypes that were significantly different in females than males with COVID-19, 
    #we deployed a phenotypic category comorbidity approach (Figure 1). We first selected and classified two groups of patients, 
    #cases (females with COVID-19) versus controls (males with COVID-19). We then identify their associated phenotypes, 
    #codified based on the International Classification of Diseases 10 (ICD10)23 and we systematically examined each phenotype to 
    #find associations with sex.
    
    ### GLM
    sexPheWAS_all <- myPhewasAnalysis(input     = dataAnalysis,
                                      ageGroup  = "ALL",
                                      caco      = "sex",
                                      cases     = "female",
                                      control   = "male",
                                      correctionMethod = "bonferroni", 
                                      verbose = TRUE )
    write.table( sexPheWAS_all, file = paste0( dir.output, "sexPheWAS_all.csv"), col.names = TRUE, row.names = FALSE)
    
    ### Fisher Test
    sexFisher_all <- fisherAnalysis( input      = dataAnalysis,
                                     ageGroup  = "ALL",
                                     caco     = "sex",
                                     cases    = "female",
                                     control  = "male",
                                     correctionMethod = "bonferroni", 
                                     obfuscation  = obfuscation )
    write.table( sexFisher_all, file = paste0( dir.output, "sexFisherAll.csv"), col.names = TRUE, row.names = FALSE)
    
    save( summary, p, diagnosisCount, diagnosisCountAfterAdmission, 
          sexFisher_all, sexPheWAS_all, file=paste0(dir.output, "output.Rdata") )
    }

