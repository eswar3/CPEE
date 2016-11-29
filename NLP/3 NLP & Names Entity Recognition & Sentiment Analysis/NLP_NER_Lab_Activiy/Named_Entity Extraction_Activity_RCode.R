rm(list=ls(all=TRUE))

library(NLP)
library(openNLP)
library(dplyr)
library(openNLPmodels.en)
setwd("F:\\TA\\Cognizant\\Batch-05\\Text Mining\\20160509_CSE9098 Cognizant_TM_TextProcessing_Activity_Rcode")

# Reads each line as a separate character vector.
bio <- readLines("anb-jarena-lee.txt")

print(bio)

# Combine all of these character vectors into a single character vector 
# using the paste() function, adding a space between each of them.
bio <- paste(bio, collapse = " ")
print(bio)

# Sentence and Word Annotations

# For NLP we are obligated to use the String class i.e. We need to 
#convert our bio variable to a string.
class(bio)
bio <- as.String(bio)
class(bio)

# Create annotators for words and sentences
#   Annotators are created by functions which load the underlying Java libraries. 
#   These functions then mark the places in the string where words and sentences start and end.
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

# We can apply these annotator functions to our data using the annotate() function.
bio_annotations <- annotate(bio, list(sent_ann, word_ann))

class(bio_annotations)

# We can combine the biography and the annotations to create what the NLP package calls an AnnotatedPlainTextDocument. 
# If we wishd we could also associate metadata with the object using the meta = argument.
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)

# Extract information from our document using accessor functions like sents() to get the sentences and words() to get the words. 
sents(bio_doc) %>% head(2)

words(bio_doc) %>% head(10)

# We could get just the plain text with 
as.character(bio_doc)


# Annotating people and places

# An entity is basically a proper noun, such as a person or place name
# Using a technique called named entity recognition (NER), we can extract people, places, and organizations in our sample paragraph

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")


# Create a new pipeline list to hold our annotators in the order we want to apply them
pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)

# Pass list of annotator functions to the annotate() function and apply it to the bio variable. 
bio_annotations <- annotate(bio, pipeline)

# Create an AnnotatedPlainTextDocument.
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)



# No easy way to extract names entities from documents. 
# But the function below will do the trick.
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

# Extract all of the named entities using entities(bio_doc), and specific kinds of entities using the kind = argument. 

# Get all the people.
entities(bio_doc, kind = "person")

# Get all the places.
entities(bio_doc, kind = "location")

loc = entities(bio_doc, kind = "location")
loc
unique(loc)

# Get all the organizations.
entities(bio_doc, kind = "organization")

# Applying our techniques to this paragraph shows both the power and the limitations of NLP.
#We managed to extract the every person named in the text: Jarena Lee, Richard Allen, and Joseph Lee. 
#But Jarena Lee's six children were not detected. Both New Jersey and Philadelphia were detected,
#but "Snow Hill, New Jersey" was not, perhaps because "snow" and "hill" fooled the algorithm into 
#thinking they were common nouns. 
#The Bethel African Methodist Episcopal Church was detected, but not the unnamed African American church 
#in Snow Hill; arguably "Methodists" is also an institution.


#Parts-Of-Speech POS Tagging
posTag <- Maxent_POS_Tag_Annotator()
POSTagging <- annotate(bio,posTag,bio_annotations)

POSTagging <- as.data.frame(POSTagging)
POSTagging <- POSTagging[8:nrow(POSTagging),]
library(stringr)
Vec <- c()
for (iLoop in (1:nrow(POSTagging))){
  if(POSTagging$features[[iLoop]][[1]] =="JJ"){
    AdjTerm <- substr(bio,POSTagging$start[iLoop],POSTagging$end[iLoop])
    Vec <- c(Vec,AdjTerm)
  }
}
Vec
