# Midterm 3

# Write a function called numStarElements. Your function should take the following
# arguments
#   <chvec>: A character vector containing strings of letters, possibly 
#     with the "*" symbol
#
# and return the following
#   <num.star>: an integer indicating how many elements of <chvec> contain the "*"
#     symbol. For example: numStarElements(c('star', 'st*r', '***')) should return 2

numStarElements=function(chvec){
  num.star=length(grep("\\*",chvec))
  return (num.star)  
}


# Write a function called numDigits that counts the number of (single) digits in
# a string.  The function should return 0 if there is no digit in the
# string. Your function should take the following arguments:
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <total>: A single number (the number of digits in chvec)

# Some test cases:
# all.equal(numDigits("1z3p ! 21"), 4)
# all.equal(numDigits("abcdefg"), 0)

numDigits=function(chvec){
  if (gregexpr("[[:digit:]]",chvec)[[1]][1] != -1)
    total=length(gregexpr("[[:digit:]]",chvec)[[1]])
  else
    total=0
  return (total)
}




# Write a function called hisToTheir that converts every instance of him
# in a string to them; every instance of he to they and every instance of 
# his to their. You can assume everything is lower case. Be careful not to 
# replace words that contain him/he/his (eg. you don't want to replace the 
# with they). Your function should take the argument
#   <chvec>: A character vector
#
# and return
#   <theirchvec>: The same character vector with the required substitutions.

#A test case
all.equal(
  hisToTheir("he went to the store his mother gave him"), 
  "she went to the store her mother gave her"
)

hisToTheir=function(chvec){
  a=unlist(strsplit(chvec," "))
  b=length(a)
  a=gsub("^him$","them",a)
  a=gsub("^he$","they",a)
  a=gsub("^his$","thier",a)  
  #for (i in 1:b){
    #if (a[i]=="him") 
    #  a[i]="them"
    #else 
    #  (if (a[i]=="he"))
    #    a[i]="they"
    #else
    #  (if (a[i]=="his"))
    #    a[i]="their"
  #}
  a=paste(a, collapse=" ")
  return (a) 
}

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
mostCommonLetter=function(chvec){
  chvec=gsub("[[:punct:]]+","",chvec)
  chvec=gsub("[0-9]+","",chvec)
  chvec=unlist(strsplit(chvec,""))
  letter=unique(chvec)
  num=c()
  for (i in 1:length(letter))
    {
    num[i]=length(grep(letter[i],chvec))
    }
  names(num)=letter
  num=sort(num,decreasing=T)
  if (num[1]>num[2])
    return (names(num)[1])
  if (num[1]==num[2] & num[2]>num[3])
    return (names(num)[1:2])
  if (num[1]==num[2] & num[2]==num[3] & num[3]>num[4])
    return (names(num)[1:3])
  if (num[1]==num[2] & num[2]==num[3] & num[3]==num[4] & num[4]>num[5])
    return (names(num)[1:4]) 
}
