# The primary R functions for dealing with regular expressions are
#   grep, grepl:        Search for matches of a regular expression/pattern
#                       in a character vector; either return the indices
#                       into the character vector that match, the strings 
#                       that happen to match, or a TRUE/FALSE vector 
#                       indicating which elements match
#   regexpr, gregexpr:  Search a character vector for regular expression 
#                       matches and return the indices of the string where 
#                       the match begins and the length of the match
#   sub, gsub:          Search a character vector for regular expression 
#                       matches and replace that match with another string
#   regexec:            Gives you indices of parethensized sub-expressions.

setwd("F:\\CPEE\\Batch 15\\CSE 7306c\\Day01")

homicides <- readLines("homicides.txt")
homicides[1]
homicides[1000]

# grep
length(grep("iconHomicideShooting", homicides))
length(grep("iconHomicideShooting|icon_homicide_shooting", homicides))
length(grep("Cause: shooting", homicides))
length(grep("Cause: [Ss]hooting", homicides))
length(grep("[Ss]hooting", homicides))

i <- grep("[cC]ause: [Ss]hooting", homicides)
j <- grep("[Ss]hooting", homicides)
str(i)
str(j)
setdiff(i, j)
setdiff(j, i)

homicides[859]

# By default, grep returns the indices into the character vector where the regex pattern matches.

state.name
grep("^New", state.name)

# Setting value = TRUE returns the actual elements of the character vector that match.
grep("^New", state.name, value = TRUE)

# grepl returns a logical vector indicating which element matches.
grepl("^New", state.name)

# Some limitations of grep
#   The grep function tells you which strings in a character vector match a # certain pattern but it doesn't tell you exactly where the match occurs or what the match is for a more complicated regex.
#   The regexpr function gives you the index into each strin where the match begins and the length of the match for that string.
#   regexpr only gives you the first match of the string (reading left to right). 
#   gregexpr will give you all of the matches in a given string.


# Let's use the pattern
#   <dd>[F|f]ound(.*)</dd>
regexpr("<dd>[F|f]ound(.*)</dd>", homicides[1:10])
substr(homicides[1], 177, 177 + 93 - 1)

# The previous pattern was too greedy and matched too much of the string. We need to use the ? metacharacter to make the regex \lazy".
regexpr("<dd>[F|f]ound(.*?)</dd>", homicides[1:10])
substr(homicides[1], 177, 177 + 33 - 1)

# One handy function is regmatches which extracts the matches in the strings for you without you having to use substr.
r <- regexpr("<dd>[F|f]ound(.*?)</dd>", homicides[1:5])
regmatches(homicides[1:5], r)

# sub/gsub
#   Sometimes we need to clean things up or modify strings by matching a pattern and replacing it with something else. For example, how can we extract the data from this string?

x <- substr(homicides[1], 177, 177 + 33 - 1)
x
sub("<dd>[F|f]ound on |</dd>", "", x)
gsub("<dd>[F|f]ound on |</dd>", "", x)

# sub/gsub can take vector arguments
r <- regexpr("<dd>[F|f]ound(.*?)</dd>", homicides[1:5])
m <- regmatches(homicides[1:5], r)
m

gsub("<dd>[F|f]ound on |</dd>", "", m)
as.Date(d, "%B %d, %Y")


# The regexec function works like regexpr except it gives you the indices for parenthesized sub-expressions.
regexec("<dd>[F|f]ound on (.*?)</dd>", homicides[1])

regexec("<dd>[F|f]ound on .*?</dd>", homicides[1])


# Now we can extract the string in the parenthesized sub-expression.
regexec("<dd>[F|f]ound on (.*?)</dd>", homicides[1])

substr(homicides[1], 177, 177 + 33 - 1)

substr(homicides[1], 190, 190 + 15 - 1)

# Even easier with the regmatches function.
r <- regexec("<dd>[F|f]ound on (.*?)</dd>", homicides[1:2])
regmatches(homicides[1:2], r)

# Let's make a plot of monthly homicide counts
r <- regexec("<dd>[F|f]ound on (.*?)</dd>", homicides)
m <- regmatches(homicides, r)
dates <- sapply(m, function(x) x[2])
dates <- as.Date(dates, "%B %d, %Y")
hist(dates, "month", freq = TRUE)
