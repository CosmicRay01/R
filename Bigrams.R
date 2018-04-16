
#
# CST 383: R part of homework 6
#
# Text mining with bigrams.
# Dr. Bruns

###########################################################################
#
# Write code after each problem number, following the instructions
# in comments.
#
# Important: when writing the code for a problem, do not use functions
# or variables from previous problems, except as directed.
#
# I've included tests that you can do to help you understand if your
# code is right.
#
###########################################################################

# Write R code to read the document as a vector of words
# and assign it to variable 'wds'.
# test: length(wds) should give 26444, and wds[1] should be "ALICE'S".
# If wds[1] is "ADVENTURES", then remove the line 'wds = wds[-1]' below.

doc = "https://raw.githubusercontent.com/grbruns/cst383/master/alice.txt"
wds = scan(file=doc, what="character", quote="")
wds = wds[-1]

# Create a version of the vector wds with all punctation symbols removed.
# (assign to wds)

wds = gsub("[[:punct:]]", "", wds)

#@ 1
# Remove every empty word (in other words, every "" word)
# test: length(wds) should be 26384
# (assign to wds)

wds = wds[wds != ""]

#@ 2
# Convert all letters to lowercase
# 
# Hint: note that there is an R function that converts strings to lower case.
# Also, always check functions to see if they can be used on vectors.
# No need to write a loop or use apply here.
#
# (assign to wds)

wds = tolower(wds)

#@ 3
# Using your vector wds, compute the number of appearances of
# the string that appears the most in wds.
#
# test: the number should be between 1500 and 1700
#
# Hint: the word that appears the most is 'the'.
# (compute a number)

as.numeric(sort(table(wds), decreasing = TRUE)["the"])

#@ 4
# Compute all the bigrams in the document and assign them to vector variable
# 'bigrams'.  A "bigram" is a string of two words that appear in succession 
# in a document.  A single space should separate the two words.  For example, 
# in alice.txt, the first three bigrams are "alices adventures", 
# "adventures in", and "in wonderland".  When you compute
# the bigrams, do not eliminate duplicates.  
#
# test: length(bigrams) = 26383 bigrams
# test: length(unique(bigrams)) = 14703
#
# Hint: try your ideas out with a small test vector of perhaps 10 words.
# (assign to bigrams)

bigrams = c()
for (i in 1:(length(wds) - 1)){
  bigrams = c(bigrams, paste(wds[i], wds[i + 1]))
}

#@ 5
# Tabulate the bigrams, sort the result in decreasing order, and assign
# it to variable 'tbl'.  
# test: head(tbl) should give:
# bigrams
#   said the     of the said alice       in a    and the     in the 
#        210        130        115         97         80         79 
#
# (assign to tbl)

tbl = sort(table(bigrams), decreasing = TRUE)

#@ 6
# Using variable tbl, compute the fraction of bigrams that appear more than once.  
# In other words, out of the set of unique bigrams, which appear more than once
# in vector 'bigrams'?
# test: the correct answer is somewhere between 0.15 and 0.25
# (compute a value between 0 and 1)

sum(as.numeric(tbl) > 1)/length(as.numeric(tbl))

#@ 7 
# Define a function 'get_nexts' that has parameters 'w' (a string),
# and 'bigrams' (a vector of bigrams), and that returns a vector
# of strings.  The strings that are returned should be the second
# words of bigrams such that w is the first words.
#
# Example: get_nexts("art", c("art show", "to far", "tart plate", "art piece")) should
# return the vector c("show", "piece").
#
# Hint: Consider using function 'grep' to find the first word. 
# Note that grep returns an index of matches.
# (define function get_nexts)

get_nexts = function(w, bigrams){
  gsub(paste0(w," "), "", bigrams[grep(paste0("^",w), bigrams, perl = TRUE)])
}

#@ 8
# Assume a vector 's' of strings exists.  It may contain duplicates.  Compute
# an string a 's', chosen randomly but in proportion to how many times
# the string appears in vector 's'.
# For example, if s were c("a", "b", "c", "a"), then your expression would
# would return "a" 50% of the time and "b" 25% of the time.
#
# Hint: sample is your friend.  This is very easy -- you need only
# a line of code.
#
# (write an expression that computes a string in vector s)

sample(s, 1, replace = TRUE)

#@ 9
# Write a function 'nxt_word' that takes parameters 'w' and 
# 'bigrams', and returns a random successor word to 'w' following
# the ideas of the last two problems.  In other words, the word
# that is returned must follow 'w' in one of the bigrams, and
# it should be returned in proportion to how often is appears
# in bigrams as the second word following w.  
#
# Example: nxt_word("the", c("the girl", "the gift", "of night", "the girl"))
# would return "girl" about 2/3 of the time, and "gift" about 1/3 of the time.
#
# Hint: use the ideas from the last two problems (but don't call previous functions)
# (define a function nxt_word)

nxt_word = function(w, bigrams){
  sample(gsub(paste0(w," "), "", bigrams[grep(paste0("^", w, " "), bigrams, perl = TRUE)]), 1, replace = TRUE)
}

#@ 10
# You are ready to enjoy the fruits of your labor.  Write a
# function 'generate' that takes parameters 'bigrams', 'n',
# and 'first_word', and makes multiple calls to your function
# nxt_word() to create a string containing n words.  You can
# assume n >= 1 and that bigrams contains at least 1 bigram.  
# Your function should work correctly for any value of n >= 1.  
#
# Your function definition should use no preceding code except
# for function nxt_word.  The returned
# vector of strings will mimic, to some extent, how Lewis
# Carroll chose his words.
#
# test: try generate(bigrams, 20, "the").  You should get a
# string containing exactly 20 words.  The string you get should
# be different every time you make the call to generate().
#
# Hint: just use the word returned by nxt_word as the 
# first_word parameter in the next call to nxt_word.
# (define function generate)

generate = function (bigrams, n, first_word){
  sentence = first_word
  for (i in 2:n){
    first_word = nxt_word(first_word, bigrams)
    sentence = paste(sentence, first_word)
  }
  return(sentence)
}

# Notes:

# Lewis Carroll, who wrote "Alice in Wonderland", was a mathematician
# and did work in probability theory, among other things.

# Sometimes when people build bigrams they allow for special
# symbols <start> and <stop> indicating the beginning and end
# of sentences.  These symbols can then appear in the bigrams.
# For extra credit, redo your code (post new code below), with
# start and stop symbols.  Also, modify 'generate' so that the
# 'first_word' and 'n' parameters are not used.  Instead, the
# first word will be randomly chosen (with correct probability) 
# among words that follow <start>, and words will be generated
# until a <stop> is encountered.

# First I get a fresh set of words
doc_2 = "https://raw.githubusercontent.com/grbruns/cst383/master/alice.txt"
wds_2 = scan(file=doc_2, what="character", quote="")
wds_2 = wds_2[-1]

# Then I remove the punctuation excempt for punctuation marking the end of a sentence
wds_2 = gsub("[,'\"():;\\]", "", wds_2)
wds_2 = gsub("-", " ", wds_2)

# I add the <start> and <stop> symbols
wds_2 = gsub("[.?!]", "<stop>", wds_2)
for (i in 1:(length(wds_2)-1)){
  if(substr(wds_2[i], nchar(wds_2[i]) - 5, nchar(wds_2[i])) == "<stop>"){
    wds_2[i+1] = paste0("<start>", wds_2[i+1])
  }
}

# More string processing
wds_2 = wds_2[wds_2 != ""]
wds_2 = tolower(wds_2)

# Create a bigrams vector
bigrams_2 = c()
for (i in 1:(length(wds_2) - 1)){
  bigrams_2 = c(bigrams_2, paste(wds_2[i], wds_2[i + 1]))
}

# My epic generate function
generate_2 = function(bigrams){
  sentence = ""
  while(substr(sentence, nchar(sentence) - 5, nchar(sentence)) != "<stop>"){
    first = sample(bigrams[substr(bigrams, 1, 7) == "<start>"], 1)
    sentence = substr(first, 1, regexpr(" ", first) - 1)
    nxt = substr(first, regexpr(" ", first) + 1, nchar(first))
    i = 1
    while(substr(nxt, nchar(nxt) - 5, nchar(nxt)) != "<stop>" && i < 30) {
      sentence = paste(sentence, nxt)
      space = gsub(paste0(nxt," "), "", bigrams[grep(paste0("^", nxt, " "), bigrams, perl = TRUE)])
      if(length(space) == 0) {
        space = gsub("the ", "", bigrams[grep(paste0("^", "the "), bigrams, perl = TRUE)])
      }
      nxt = sample(space, 1, replace = TRUE)
      i = i + 1
    }
    sentence = paste(sentence, nxt)
  }
  return(sentence)
}

generate_2(bigrams_2)

# For more fun, try a different novel, or try a completely different
# kind of document.  You can also try using 'trigrams' (length 3 word
# sequences) instead of bigrams.

bigrams_3 = c()
for (i in 1:(length(wds_2) - 2)){
  bigrams_3 = c(bigrams_3, paste(wds_2[i], wds_2[i + 1], wds_2[i+2]))
}

generate_2(bigrams_3)
