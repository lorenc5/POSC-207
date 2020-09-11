# py_tweet #
# author: Loren Collingwood, Stephanie DeMora
# DATE: 9/10/2020 

# search = search term hash tag, with both couble and single quotes, 
# e.g.: "'#Clicks4Kass'"
# until = YYYY-MM-DD, e.g.: "'2020-09-09'"
# since = YYYY-MM-DD, e.g.: "'2007-01-01'"
# limit = numeric, default = 10000000, 
# output = string in double and single quote for output csv file, 
# e.g.: "'Clicks.csv'",
# pfile = string of python file name, e.g.: "twitter_hist.py"   
# remove = Logical, removes created .py file, default to FALSE

py_tweet <- function(search, 
                     until,
                     since,
                     limit, 
                     output,
                     pfile = "twitter_hist.py",
                     remove = FALSE){
    
    # Set up and write out Python Script #
    fileConn<-file(pfile)
    writeLines(c("import pip",
                 "import twint",
                 "c = twint.Config()",
                 paste("c.Search = ", search, sep=" "),
                 paste("c.Until = ", until, sep=" "),
                 paste("c.Since = ", since, sep=" "),
                 paste("c.Limit = ", limit, sep=" "),
                 "c.Store_csv = True",
                 paste("c.Output = ", output, sep=" "),
                 "twint.run.Search(c)"),
               fileConn)
    close(fileConn)
    
    # Adjust for Mac/Windows #
    
    if(Sys.info()[['sysname']] == "Darwin") {
        
        py_path <- system("ls -al /usr/local/bin/python3", intern =T)
        
        py_call <- paste("/", sub('.*../../', '', py_path), sep="")
        
        system(paste(py_call, pfile, sep=" "))
        
    } else if(Sys.info()[['sysname']] == "Windows"){
        
        shell(paste("python3", pfile, sep=" "))
        
    }
    
    # Remove created python file #
    if(remove){
        
        system(paste("rm", pfile, sep=" "))
        
    }
    
}