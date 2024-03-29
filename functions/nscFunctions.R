#### nscLoad ####
nscLoad <-  # load the nsc file
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
        , ENROLLMENT_STATUS = ifelse(ENROLLMENT_STATUS == '', NA, ENROLLMENT_STATUS)
        , DEGREE_TITLE = ifelse(DEGREE_TITLE == '', NA, DEGREE_TITLE)
        , MAJOR = ifelse(MAJOR == '', NA, MAJOR)
      )
    
    names(nsc) <-
      gsub('/', '_', names(nsc))
    
    return(nsc)
    
  }


#### nscCitation ####
nscCitation <-  # create citation info to be used throughout nsc publication
  function(mth, yr, sem) {
    
    mth.cur <-
      simpleCap(mth)
    
    yr.cur <-
      simpleCap(yr)
    
    sem.cur <-
      if(inherits(sem, 'character')) {
        simpleCap(sem)
      } else {
        stop('sem must be a character input (eg: fall)')
      }
    
    citation <-
      list(
        date = paste0(
          mth.cur, ', ', yr.cur
          ),
        month = mth.cur,
        year = yr.cur,
        semester = sem.cur,
        citation = 
          paste0(
            'Source: ', 'NSC', ' ',
            mth.cur, ', ', yr.cur
          ))
        
    return(citation)
    
  }


#### nscEnrollFirstFall ####
nscEnrollFirstFall <-
  function(nsc.df) {
    
    # required libraries
    require(dplyr)
    
  }



#### Count of Students Enrolled in College the Fall Immediately Following Graduation From High School #### 
require(dplyr)

nsc <-
  nscLoad('Z:/Reports (recurring)/Other/National Student Clearinghouse (NSC)/Raw Data/20150505/', '10003061_10003061-34502-DETAIL-EFFDT-20150416-RUNDT-20150505.csv')

  
nsc.working <-
  nsc %>%
  mutate(
    ENROLLMENT_STATUS = ifelse(is.na(ENROLLMENT_STATUS) == TRUE, 'unk', ENROLLMENT_STATUS)
    , enroll_length = ENROLLMENT_END - ENROLLMENT_BEGIN
    , sem_fall = ENROLLMENT_BEGIN >= as.Date(paste0(format(ENROLLMENT_BEGIN, '%Y'), '-08-15'), '%Y-%m-%d') & format(ENROLLMENT_BEGIN, '%m') < 11
    , sem_first_fall = format(ENROLLMENT_BEGIN, '%Y') == format(HIGH_SCHOOL_GRAD_DATE, '%Y') & sem_fall
    )
