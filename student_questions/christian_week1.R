################################
# Loren Collingwood            #
# re: Student Questions        #
# Eminent Political Scientists #
################################

library(rtweet)
library(data.table)

# Start Holder #
hold <- list()

# Vector of Names #
nam <- c("lorenc2","kassrao", "hlw_phd", "skdreier24")

n <- length(nam)

for (i in 1:n){

    hold[[i]] <- get_timeline(nam[i], n =3000)

}

# Put into dataframe
pol_sci_df <- data.table::rbindlist(hold)
# Look at some Stuff #
table(pol_sci_df$screen_name)

write.csv(pol_sci_df, 
          "eminent_pol_sci.csv", 
          row.names=F)

