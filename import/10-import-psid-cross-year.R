cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(SAScii) 	# load the SAScii package (imports ascii data with a SAS script)
library(RCurl)		# load RCurl package (downloads https files)
library(dplyr)

########################## Start Program Here ######################### #

setwd(paste0(importdir, "10-psid-cross-year"))

psid_credentials <- read.csv(paste0(importdir, "10-psid-cross-year/psid_credentials.csv"), 
                             header = FALSE,
                             sep = ",")

psid_username <- psid_credentials$V1
psid_password <- psid_credentials$V2

# follow the authentication technique described on this stackoverflow post
# http://stackoverflow.com/questions/15853204/how-to-login-and-then-download-a-file-from-aspx-web-pages-with-r


# initiate and then set a curl handle to store information about this download
curl = getCurlHandle()

curlSetOpt(
  cookiejar = 'cookies.txt' , 
  followlocation = TRUE , 
  autoreferer = TRUE , 
  curl = curl
)

# connect to the login page to download the contents of the `viewstate` option
html <- 
  getURL(
    'http://simba.isr.umich.edu/u/Login.aspx' , 
    curl = curl
  )

# extract the `viewstate` string
viewstate <- 
  as.character(
    sub(
      '.*id="__VIEWSTATE" value="([0-9a-zA-Z+/=]*).*' , 
      '\\1' , 
      html
    )
  )

# extract the `eventvalidation` string
eventvalidation <- 
  as.character(
    sub(
      '.*id="__EVENTVALIDATION" value="([0-9a-zA-Z+/=]*).*' , 
      '\\1' , 
      html
    )
  )

params <- 
  list(
    'ctl00$ContentPlaceHolder1$Login1$UserName'    = psid_username ,
    'ctl00$ContentPlaceHolder1$Login1$Password'    = psid_password ,
    'ctl00$ContentPlaceHolder1$Login1$LoginButton' = 'Log In' ,
    '__VIEWSTATE'                                  = viewstate ,
    '__EVENTVALIDATION'                            = eventvalidation
  )


# analyze survey data for free (http://asdfree.com) with the r language
# panel study of income dynamics
# 1968 through 2011
# family, marriage history, childbirth & adoption history, parent identification, cross-year individual

save.psid <-
  # ..a file number, a save name, the parameters list, and the curl options
  function( file , name_category , params , curl ){
    
    # logs into the umich form
    html = postForm('http://simba.isr.umich.edu/U/Login.aspx', .params = params, curl = curl)
    
    # confirms the result's contents contains the word `Logout` because
    # if it does not contain this text, you're not logged in.  sorry.
    if ( !grepl('Logout', html) ) stop( 'no longer logged in' )
    
    # initiate a temporary file and a temporary directory
    tf <- tempfile() ; td <- tempdir()
    
    # download the file number
    file <- 
      getBinaryURL( 
        paste0( 
          "http://simba.isr.umich.edu/Zips/GetFile.aspx?file=" , 
          file 
        ) , 
        curl = curl 
      )
    
    # write the file to the temporary file on the local disk
    writeBin( file , tf )
    
    # unzip the temporary file to the temporary directory
    z <- unzip( tf , exdir = td )
    
    # figure out which file contains the data (so no readmes or technical docs)
    fns <- z[ grepl( ".txt" , tolower( z ) , fixed = TRUE ) & ! grepl( "_vdm|readme|doc|errata" , tolower( z ) ) ]
    
    # loop through all of the files and convert them to R data frames
    for (fn in fns) {
      
      # the sas importation script should have the same filename as the data, except with a .sas extension
      sas_ri <- gsub(".txt", ".sas", fn)
      if(file.exists(sas_ri)) {
        
        # read the text file directly into an R data frame with `read.SAScii`
        x <- read.SAScii( fn , sas_ri)
        
        # convert all column names to lowercase
        names( x ) <- tolower( names( x ) )
        
        # add a `one` column
        x$one <- 1
        
        # name the dataset combining the data category with the individual file name
        name<-paste0(name_category, "_", gsub(".txt","",tolower(basename(fn)))) 
        
        # copy the data.frame `x` over to whatever the `name` parameter was supposed to be
        assign( name , x )
        
        # save the renamed data.frame to an R data file (.rda)
        save( list = name , file = paste0( name , '.rda' ) )
        
        # delete the data.frame `x`
        rm( x )
        
        # delete the data.frame (name)
        rm( list = name )
        
        # clear up RAM
        gc()
      }
    }
    
    # remove the files you'd downloaded from the local disk
    file.remove( tf , z )
    
    # confirm that the function worked by returning TRUE
    TRUE
  }

save.psid( 1053 , 'ind' , params , curl )


# ############################  End  ################################## #