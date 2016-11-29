rm(list=(ls(all=TRUE)))
setwd("F:\\CPEE\\Batch 15\\CSE 7306c\\Day01\\txt")

# sub(), gsub(), str_replace() (stringr) make some substitutions in a string.
# grep(), str_extract() (stringr) extract some value
# grepl(), str_detect() (stringr) detect the presence of a pattern.

# \': single quote. You don't need to escape single quote inside a double-quoted string, so we can also use "'"
# "." stands for any character.
# "[ABC]" means A,B or C.
# "[A-Z]" means any upper letter between A and Z.
# "[0-9]" means any digit between 0 and 9.
# Here is the list of metacharacters â,$ * + . ? [ ] ^ { } | ( ) \. If you need to use one of those characters, precede them with a doubled backslash.

# some classes of regular expressions
# [:digit:] Digits: 0 1 2 3 4 5 6 7 8 9.
# [:alpha:] Alphabetic characters: [:lower:] and [:upper:].
# [:upper:] Upper-case letters.
# [:lower:] Lower-case letters.
#[:punct:] Punctuation characters: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~.
# [:space:] Space characters: tab, newline, vertical tab, form feed, carriage return, and space.
# [:blank:] Blank characters: space and tab.
# [:cntrl:] Control characters

# [:alnum:] Alphanumeric characters: [:alpha:] and [:digit:].
# [:graph:] Graphical characters: [:alnum:] and [:punct:].
# [:print:] Printable characters: [:alnum:], [:punct:] and space.
# [:xdigit:] Hexadecimal digits: 0 1 2 3 4 5 6 7 8 9 A B C D E F a b c d e f.

# ? The preceding item is optional and will be matched at most once.
# * The preceding item will be matched zero or more times.
# + The preceding item will be matched one or more times.
# {n} The preceding item is matched exactly n times.
# {n,} The preceding item is matched n or more times.
# {n,m} The preceding item is matched at least n times, but not more than m times.
# $ to force the regular expression to be at the end of the string

strings <- c("a", "ab", "acb", "accb", "acccb", "accccb")
# "ac" will be matched 
grep("ac*b", strings, value = TRUE)
grep("ac+b", strings, value = TRUE)
grep("ac?b", strings, value = TRUE)
grep("ac{2}b", strings, value = TRUE)
grep("ac{2,}b", strings, value = TRUE)
grep("ac{2,3}b", strings, value = TRUE)

# to remove space characters in a string

sub('\\s', '',"regular expression", perl = TRUE)

string <- "23 mai 2000"
string2 <- "1 mai 2000"

# Pattern recognition, 2 digits with the [[:digit:]]{2} expression, the letters with [[:alpha:]]+ and the 4 digits with [[:digit:]]{4}
regexp <- "([[:digit:]]{2}) ([[:alpha:]]+) ([[:digit:]]{4})"

# The function grepl 1st one is true and the second one is false since there is only one digit in the first number.
grepl(pattern = regexp, x = string)
grepl(pattern = regexp, x = string2)
grep(pattern = regexp, x = string,value = T)

string <- "blabla 23 mai 2000 blabla 18 mai 2004"
regexp <- "([[:digit:]]{2})"
# regexpr() returns the position of the regular expression.
r <- regexpr(pattern = regexp, text = string)
regmatches(string, r)

#  gregexpr() is similar to regexpr() but the starting position of every match is returned.
r <- gregexpr(pattern = regexp, text = string)
regmatches(string, r)

string <- "23 mai 2000"
regexp <- "([[:digit:]]{2}) ([[:alpha:]]+) ([[:digit:]]{4})"
sub(pattern = regexp, replacement = "\\1", x = string) # returns the first part of the regular expression
sub(pattern = regexp, replacement = "\\2", x = string) # returns the second part
sub(pattern = regexp, replacement = "\\3", x = string) # returns the third part


# ^: matches the start of the string.
# $: matches the end of the string.
# \b: matches the empty string at either edge of a word. Dont confuse it with ^ $ which marks the edge of a string.
# \B: matches the empty string provided it is not at an edge of a word.

(strings <- c("abcd", "cdab", "cabd", "c abd"))

grep("ab", strings, value = TRUE)
grep("^ab", strings, value = TRUE)
grep("ab$", strings, value = TRUE)
grep("\\bab", strings, value = TRUE)
grep("\\Bab", strings, value = TRUE)


# .: matches any single character, as shown in the first example.
# ^ to force the regular expression to be at the beginning of the string
# [...]: a character list, matches any one of the characters inside the square brackets.
# [^...]: matches any characters except those inside the square brackets.
# \: suppress the special meaning of metacharacters in regular expression, i.e. $ * + . ? [ ] ^ { } | ( ) \, similar to its usage in escape sequences. Since \ itself needs to be escaped in R, we need to escape these metacharacters with double backslash like \\$.
# |: an operator, matches patterns on either side of the |.
# (...): grouping in regular expressions. This allows you to retrieve the bits that matched various parts of your regular expression so you can alter them or use them for building up a new string. Each group can than be refer using \\N, with N being the No. of (...) used. This is called backreference.

(strings <- c("^ab", "ab", "abc", "abd", "abe", "ab 12"))

grep("ab.", strings, value = TRUE)
grep("ab[c-e]", strings, value = TRUE)
grep("ab[^c]", strings, value = TRUE)
grep("^ab", strings, value = TRUE)
grep("\\^ab", strings, value = TRUE)
grep("abc|abd", strings, value = TRUE)
gsub("(ac) 12", "\\1 34", strings)



## references
#http://www.regular-expressions.info/rlanguage.html
#http://astrostatistics.psu.edu/su07/R/html/base/html/regex.html
#https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
