# Midterm 3
# SID 23596619 Qiming Zhang

# Write a function called numBracElements. Your function should take the following
# arguments
#   <chvec>: A character vector containing strings of letters, possibly 
#     with the "[" symbol
#
# and return the following
#   <num.brac>: an integer indicating how many elements of <chvec> contain the "["
#     symbol. For example: numBracElements(c('digit', '[:digit:]', '[]')) should return 2

numBracElements = function(chvec){
  num.brac = length(grep("[",chvec,fixed=T))
  return(num.brac)
}


# Write a function called maxDigits that return the maximum of all (single) digits in
# a string.  The function should return 0 if there is no digit in the
# string. Your function should take the following arguments:
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <total>: A single number (the maximum of all digits in chvec)

maxDigits = function(chvec){
  str1 = strsplit(chvec,"")[[1]]
  index1 = gregexpr("[0-9]",chvec)[[1]]
  if (length(index1) > 1){
    index2 = as.numeric(index1)
    index3 = as.numeric(str1[index2])
    total = max(index3)
  } else if (length(index1) == 1){
    if(as.numeric(index1) == -1){
      total = 0
    } else {
      index2 = as.numeric(index1)
      index3 = as.numeric(str1[index2])
      total = max(index3)
    }
  }
  return(total)
}

# Some test cases:
all.equal(maxDigits("1z3p ! 28"), 8)
all.equal(maxDigits("abcdefg"), 0)



# Write a function called hisToHer that converts every instance of 
# him in a string to her; every instance of he to she and every instance 
# of his to her. You can assume everything is lower case. Be careful not 
# to replace words that contain him/he/his (eg you don't want to
# replace the with ther). Your function should take the argument
#   <chvec>: A character vector
#
# and return
#   <herchvec>: The same character vector with the required substitutions.

hisToHer = function(chvec){
  convert1<-gsub("\\bhim\\b","her",chvec)
  convert2<-gsub("\\bhis\\b","her",convert1)
  convert3=gsub("\\bhe\\b","she",convert2)
  return(convert3)
}
# A test case
all.equal(
  hisToHer("he went to the store his mother gave him"), 
  "she went to the store her mother gave her"
)


# Write a function called mostCommonLetter that finds the most common 
# letter in a string. If there is a tie for most common letter return 
# all of the letters that were most common. Your function should 
# convert all letters in the string to *lower case* and you should 
# remove  everything other than letters. 
# Your function has the argument
#  <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and it should return
#  <letter> The most common letter or letters in the string.
# For example mostCommonLetter("aabbccccdddd") should return 
# [1] "c" "d"

mostCommonLetter = function(chvec){
  chvec = tolower(chvec)
  str1 = strsplit(chvec,"")[[1]]
  str2 = sort(str1)
  str3 = unique(str2)
  m1 = matrix(0,1,length(str3))
  for (i in 1:length(str3)){
    m1[i] = length(which(str2 == str3[i]))
  }
  m2 = matrix(max(m1),1,length(m1))
  letter = str3[which(m2 == m1)]
  return(letter)
}

mostCommonLetter=function(chvec){
  chvec=gsub("[[:punct:]]+","",chvec)
  chvec=gsub("[0-9]+","",chvec)
  chvec=unlist(strsplit(chvec,""))
  letter=unique(chvec)
  num=table(chvec)
  num=sort(num,decreasing=T)
  return(names(num[which(num==max(num))]))
}

mostCommonLetter=function(chvec){
  chvec=gsub("[[:punct:]]+","",chvec)
  chvec=gsub("[0-9]+","",chvec)
  chvec=unlist(strsplit(chvec,""))
  letter=unique(chvec)
  num=table(chvec)
  num=sort(num,decreasing=T)
  return(num)
}