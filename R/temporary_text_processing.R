# This code was used to preprocess the
# These ad-hoc scripts were used in support of manual corrections to
# the (very messy) original text file, where for examples some letters had been recognised as numbers.
# The raw and edited texts are available in case these semi-manual edits need to be reviewed.

# setwd('~/a_pattern_language_viz/')
# library(stringr)
# library(dplyr)
#
#
# ### CLEANING
#
# fileName <- './apatternlanguage.html'
# apl<-readLines(fileName, file.info(fileName)$size)
#
# # regex for letter next to a number
# innum<-function(x){
#   paste0('([0-9][0-9]*',x,')|(',x,'[0-9][0-9]*)')
# }
#
#
# # list of numbers that were wrongly scanned as certain letters
# letters_that_are_numbers<-matrix(c(
#   'o','0',
#   'l','1',
#   'I','1',
#   'i','1',
#   'u','11'
#   ),ncol = 2,byrow = T)
#
#
# # list of patterns where letters are in numbers; one list item per common letter for number
# repme<-lapply(letters_that_are_numbers[,1],function(pattern){
# str_match(apl,innum(pattern))[,1] %>% unique %>% .[!is.na(.)]
# })
#
#
# names(repme)<-letters_that_are_numbers[,1]
#
#
# # global lookup/replace table
# lookuptable<-lapply(1:nrow(letters_that_are_numbers),function(i){
# 		cbind(repme[[i]],
# 			   gsub(letters_that_are_numbers[i,1],letters_that_are_numbers[i,2],repme[[i]])
# 			)
#
# }) %>% do.call('rbind',.)
#
# lookuptable
#
# # replace in text
# for(i in 1:nrow(lookuptable)){
# 	apl<-gsub(lookuptable[i,1],lookuptable[i,2],apl)
# }
#
#
#
# gsub(lookuptable[i,1],lookuptable[i,2],apl)
#
#
# sink('apl_regexedalot.txt')
# writeLines(apl)
# sink()
#
#
#
#   apl<-gsub(was_becomes[1],was_becomes[2],apl)


