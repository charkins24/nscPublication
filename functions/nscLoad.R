nscLoad <-
   function(path, file = NULL) {
     
     # error checking
     # string inputs
     if(inherits(path, 'character') == FALSE) {
       stop('path must be a character string')
     }
     
     if(inherits(file, 'character') == FALSE & inherits(file, 'NULL') == FALSE) {
       stop('file must be NULL or a character string')
     }
     
     # file exists
     k.path <-
       paste0(path, file)
     
     if(file.exists(k.path) != TRUE) {
       stop('path and file do not resolve to a file that can be loaded; please check your path and file')
     }
     
     # define require libraries
     require(magrittr)
      
     nsc <-
       readr::read_csv(k.path) %>%
       dplyr::mutate(
         HIGH_SCHOOL_GRAD_DATE = as.Date(HIGH_SCHOOL_GRAD_DATE %>% as.character(), format = '%Y%m%d')
         , ENROLLMENT_BEGIN = as.Date(ENROLLMENT_BEGIN %>% as.character(), format = '%Y%m%d')
         , ENROLLMENT_END = as.Date(ENROLLMENT_END %>% as.character(), format = '%Y%m%d')
         , GRADUATION_DATE = as.Date(GRADUATION_DATE %>% as.character(), format = '%Y%m%d')
       )
     
     return(nsc)
     
   }