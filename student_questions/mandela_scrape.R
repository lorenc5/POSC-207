#####################
# Loren Collingwood #
# Whitney Martinez  #
# DATE: 10/27/2020  #
#####################

# Working Directory #
setwd("~/Dropbox/collingwood_research/posc_fall_20/POSC-207/student_questions")

# Packages #
library(rvest)
library(data.table)

# Nelson Mandela Scrape 
speech_collect <- list()
title_collect <- list()

# Iterator Length -- probably can hit up to 1500 or so?
n <- 101
for (i in 1:n){ # open i loop
    
    if (i <10 ) {
        first <- "http://db.nelsonmandela.org/speeches/pub_view.asp?pg=item&ItemID=NMS00"
        second <- i
    } else if (i >9 & i < 100){
        
        first <- "http://db.nelsonmandela.org/speeches/pub_view.asp?pg=item&ItemID=NMS0"
        second <- i
    } else {
        
        first <- "http://db.nelsonmandela.org/speeches/pub_view.asp?pg=item&ItemID=NMS"
        second <- i
    }
    
    nm <- read_html(paste(first, second, sep=""))

    # Extract Text #
    text <- html_text(html_nodes(nm, "tr td")); length(text)

    speech_collect[[i]] <- as.data.frame(text[8])
    
    # Extract Title #
    title_e <- html_text(html_nodes(nm, "td font")); length(title_e)
    title_collect[[i]] <- title_e[2]
    
} # close 9 loop

# Flatten Speech and Title #
nm_speech <- data.table::rbindlist(speech_collect)
colnames(nm_speech) <- "text"

nm_title <- unlist(title_collect)

# Combine into DataFrame #
nm_dat <- data.frame(nm_title, nm_speech)


# Write out to CSV but may have some end of line issues with data like this...
write.csv(nm_dat, "mandela_speeches.csv", row.names=F)


