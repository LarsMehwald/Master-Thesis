######################################
# Master thesis
# Lars Mehwald
# sha1 marker
# 4 April 2016
######################################

######################################
# Creating an object to collect sha1 hashes
######################################

sha1 <- data.frame(VariableName = character(), sha1.old = character(),
                   sha1.new = character(), IsTheSame = logical(),
                   stringsAsFactors = FALSE)

# Checking the data, benchmark December 2015
sha1.KOFindex <- as.data.frame(cbind(VariableName = "KOFindex", 
                                     sha1.old = "0e475515b404c3214f9a66272d26dca7981fa53b", 
                                     sha1.new = digest(KOFindex, algo = "sha1")))
sha1.KOFindex$sha1.old <- as.character(sha1.KOFindex$sha1.old)
sha1.KOFindex$sha1.new <- as.character(sha1.KOFindex$sha1.new)
sha1.KOFindex <- cbind(sha1.KOFindex, 
                       IsTheSame = ifelse(sha1.KOFindex$sha1.old == sha1.KOFindex$sha1.new, 
                                          TRUE, 
                                          FALSE))
sha1 <- rbind(sha1, sha1.KOFindex)
rm(sha1.KOFindex)