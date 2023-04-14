library(tidyverse)
# use this library to highlight text you are interested in
library(crayon)

# import .csv of references exported from Zotero - this should point to where you saved it:
texty<-read.csv("D:/OneDrive/OneDrive - USDA/Desktop/Disturbance_abstracts.csv",encoding="UTF-8")

# this function highlights the terms within the abstracts that are pulled out
# from the data. This allows you to see if the context of the word is what you
# are interested in. 

highlight.fun<-function(x.data,term.vector,abs.column.name="Abstract.Note",cit.num.col.name="Citation.num"){
  require(crayon)
  abst.results<-list(NULL)
  # this will extract any abstracts with terms that mactch any of the terms in the terms vector
  # this code assumes that the abstract data is stored in a column called "Abstract.Note" - change

  paste0(term.vector,collapse="|")
  x.data<-suppressWarnings(x.data[str_detect(x.data[,abs.column.name],paste0(term.vector,collapse="|")),])
  
  if(nrow(x.data)==0){
    stop("Terms not found in any of the abstracts from the specified database.")
  }
  # iterate over each row of the subset data from above:
  for(i in 1:nrow(x.data)){
    x.dat<-x.data[i,abs.column.name]
    # to remove the lookaround syntax from the word vector used later use this:
    term.vector.replacements<-str_replace_all(term.vector,'\\(\\?\\!\\[A-Za-z0-9]\\)',"")
    term.vector.replacements<-str_replace_all(term.vector.replacements,'\\(\\?\\<\\!\\[A-Za-z0-9]\\)',"")
    
    # create highlighted words in abstract text for each term in the term vector:
    for(j in 1:length(term.vector)){
      
      x.dat<-str_replace_all(x.dat,regex(term.vector[j],ignore_case=T),crayon::yellow$italic(term.vector.replacements[j]))
      
    }
    # store citation number along with modified abstract:
    x.dat<-paste(x.data[i,cit.num.col.name],": ",x.dat)
    abst.results[[i]]<-x.dat
    
  }
  return(abst.results)
  
}

# set your working directory where you wish to save everything 
# or use full location reference for the exported zotero data
setwd("D:/Adatafolder/Station carbon work/Synthesis team/Data")

# Break up table into different text vectors - in this case the vector names are based on the data
# I used so you may need to change the names for different data sets. 
Auth.text<-texty$Author
Title.text<-texty$Title

## abstract text vector:
Abst.text<-texty$Abstract.Note
# convert everything to lower case:
Abst.text<-tolower(Abst.text)

## special characters have to be removed separately for some reason ####
## these characters appeared in my data so my not apply to your data ##
Abst.text<-str_replace_all(Abst.text,c('ﬁ'),"fi")
Abst.text<-str_replace_all(Abst.text,c('ﬂ'),"fl")
Abst.text<-str_replace_all(Abst.text,c('"'),"")
Abst.text<-str_replace_all(Abst.text,c('“'),"")
Abst.text<-str_replace_all(Abst.text,c('”'),"")
Abst.text<-str_replace_all(Abst.text,c('’'),"")
Abst.text<-str_replace_all(Abst.text,c('‘'),"")
Abst.text<-str_replace_all(Abst.text,c('‘'),"")
Abst.text<-str_replace_all(Abst.text,c('-')," ")
Abst.text<-str_replace_all(Abst.text,c('‐')," ")
Abst.text<-str_replace_all(Abst.text,c('•'),"")
Abst.text<-str_replace_all(Abst.text,c('-')," ")
Abst.text<-str_replace_all(Abst.text,c('≈'),"")
Abst.text<-str_replace_all(Abst.text,c('∼'),"")
Abst.text<-str_replace_all(Abst.text,c('–')," ")
Abst.text<-str_replace_all(Abst.text,c('−')," ")
Abst.text<-str_replace_all(Abst.text,c('‐')," ")
Abst.text<-str_replace_all(Abst.text,c('æ')," ")
Abst.text<-str_replace_all(Abst.text,c('ʻ'),"")
Abst.text<-str_replace_all(Abst.text,c('‐')," ")
Abst.text<-str_replace_all(Abst.text,c(' ')," ")
Abst.text<-str_replace_all(Abst.text,c(' ')," ")
Abst.text<-str_replace_all(Abst.text,c(' ')," ")
Abst.text<-str_replace_all(Abst.text,c('à')," ")




## clean up text
## replace words with other words / phrases ####
## note the replacement words or phrases should be brackets with (?<![A-Za-z0-9]) to signify
## that the words are whole words and not part of another word. This process may be iterative
## as you may want to add more words to it once your final data is available. 
words.to.replace<-c( '(?<![A-Za-z0-9])mountain pine beetle(?![A-Za-z0-9])'='disturbance',
                     '(?<![A-Za-z0-9])greenhouse gas concentrations(?![A-Za-z0-9])'='atmosphere',
                     '(?<![A-Za-z0-9])gas concentrations(?![A-Za-z0-9])'='atmosphere',
                     '(?<![A-Za-z0-9])greenhouse gas concentration(?![A-Za-z0-9])'='atmosphere',
                     '(?<![A-Za-z0-9])gas concentration(?![A-Za-z0-9])'='atmosphere',
                     '(?<![A-Za-z0-9])atmospheric co2 concentration(?![A-Za-z0-9])'='atmosphere',
                     '(?<![A-Za-z0-9])atmospheric co2 concentrations(?![A-Za-z0-9])'='atmosphere',
                     '(?<![A-Za-z0-9])atmospheric carbon dioxide concentration(?![A-Za-z0-9])'='atmosphere',
                     '(?<![A-Za-z0-9])atmospheric carbon dioxide concentrations(?![A-Za-z0-9])'='atmosphere',
                     '(?<![A-Za-z0-9])atmospheric concentrations(?![A-Za-z0-9])'='atmosphere',
                     '(?<![A-Za-z0-9])atmospheric concentration(?![A-Za-z0-9])'='atmosphere',
                     '(?<![A-Za-z0-9])mountain pine beetles(?![A-Za-z0-9])'='disturbance',
                     '(?<![A-Za-z0-9])trees(?![A-Za-z0-9])'="tree",
                     '(?<![A-Za-z0-9])aggregated(?![A-Za-z0-9])'="aggregate",
                     '(?<![A-Za-z0-9])aggregates(?![A-Za-z0-9])'="aggregate",
                     '(?<![A-Za-z0-9])aggregation(?![A-Za-z0-9])'="aggregate",
                     '(?<![A-Za-z0-9])aggregations(?![A-Za-z0-9])'="aggregate"
                     
                    
                     
)

# replace all words above with indicated alternative:
Abst.text2<-str_replace_all(Abst.text,words.to.replace)

## words to remove ####
## use this to eliminate any words that are not relevant to your study but were not 
## eliminated previously this may be an interative process where you eliminate more
## words after your final data is available. 
remove.words<-c('fossilFuel','arbuscular',"reintroduce",'reintroduction',
                'change', 'estimate', 'data', 'usa', 'land', 'sites', 'potential', 
                'organic', 'effects', 'measurements', 'rates', 'time', 'analysis', 
                'conditions', 'inventory', 'scale', 'terrestrial', 'level', 'average', 
                'live', 'cycle', 'processes', 'range', 'energy', 'impacts', 'nitrogen', 
                'greenhouse', 'spatial', 'severity', 'density', 'plots', 'system', 'effect'
              
                
)
## now signify that those words should be treated as whole words / phrases:
rem.words<-paste0('(?<![A-Za-z0-9])',remove.words,'(?![A-Za-z0-9])')

## remove digits from text and remove words: ####
Abst.text3<-gsub('[[:digit:] ]+',' ', Abst.text2)

## loop through the remove words and take them out :
for(i in 1:length(rem.words)){
  Abst.text3<-str_replace_all(Abst.text3,rem.words[i]," ")
}

## remove punctuation marks:
Abst.text4<-str_replace_all(Abst.text3, "[^[:alnum:]]", " ") %>% str_replace_all(.,"[ ]+", " ")

## check abstract results:
# replace double spaces with single spaces:
Abst.text4<-str_replace_all(Abst.text4,'  ',' ')

words.un<-(paste(Abst.text4,collapse=" "))
words.un2<-vapply(lapply(strsplit(words.un, " "), unique), paste, character(1L), collapse = " ")
# get final list of words:
fin.words<-unlist(str_split(words.un2,' '))

# The most common value is a blank space - remove it here:
fin.words<-stringi::stri_remove_empty(fin.words)

# create dataframe to keep track of word occurrence values:
fin.data<-data.frame(final.words=fin.words,word.count=rep(NA,length(fin.words)),papers.with.word.count=rep(NA,length(fin.words)))

# get number of word occurrences and number of papers with words with this loop:
for(i in 1:nrow(fin.data)){
  fin.data$word.count[i]<-sum(str_count(Abst.text4,paste0("\\b",fin.data[i,'final.words'],'\\b')))
  fin.data$papers.with.word.count[i]<-sum(str_count(Abst.text4,paste0("\\b",fin.data[i,'final.words'],'\\b'))>0)
}
# put the dataframe in alphabetical order using final.words:
fin.data<-fin.data[order(fin.data$final.words),]

# Now look at the data to see what words need replacement or removal ####
## then go back to those sections and edit and run again
fin.data

## use this function to check that context is correct for given term within the abstracts:
term.print.function<-function(term,words.to.rep.vect,Abst.tex.or){
  
  # because there were substitutions made you need to create a full list of them so that all words are highlighted
  # correctly - here is the list
  term.vector<-c(paste0("(?<![A-Za-z0-9])",term,"(?![A-Za-z0-9])"),names(words.to.rep.vect[str_detect(words.to.rep.vect,term)]))
  
  # create dataframe with abstracts that contain term and the paper number related to that abstract:
  term.data<-data.frame(Abst.txt=Abst.tex.or[which(str_detect(Abst.tex.or, paste0(term.vector,collapse="|")))],
                        Abst.num=which(str_detect(Abst.tex.or, paste0(term.vector,collapse="|"))))
  
  # check results of list:paste0()
  # function that returns a list of abstracts which can be printed using cat() function to 
  # display highlighted terms for word and replacement words. 
  # specify different column name as Abstract.Note is now Abst.txt also respecify number.
  new.list<-highlight.fun(term.data,term.vector,abs.column.name = "Abst.txt",cit.num.col.name = "Abst.num")
  
  ## iterate through length of list and continue to next with keystroke entered into console
  for(i in 1:length(new.list)){
    print(cat(new.list[[i]]))
    readline(prompt="Press [enter] to continue")
    
  }
  
}

# For each term that may show up in contexts outside of how you would interpret them check that
# the context of the term within the abstract fits your understanding of it. In this example I 
# pick the word aggregate as it replaced a few related terms that could be about beetles aggregating
# during an infestation or it could be about soil aggregates. For the purposes of this example we 
# are interested in soil aggregates and not the other examples. 

# run the function here specifying the term you are interested in looking at and 
# the words to replace vector along with the abstract text prior to eliminating words.
# to save on typing create a variable with the term you are interested in reviewing:
term.to.check<-"aggregate"
# now input appropriate items into function and run it:
# the words.to.replace vector is included to find words in the original text
# that were later replaced with the word we are looking at here. The original Abst.text
# is used so that we can read the abstract in proper context. 
term.print.function(term.to.check,words.to.replace,Abst.text)

# subtract for word occurrence: within each abstract count the number of occurrences of the word
# that are not in the context you are expecting and put those numbers into the sum function separated
# by commas. 
sum(1,10,1,1,1,2,4,1,1,1,2,
    1,
    1)
# subtract for manuscripts. To help you remember what abstracts you have already viewed it is
# best to put the abstract (cit.num) number in the vector within the length function separating 
# each number with a comma. That way if your computer crashes you can resume from that abstract.
# it can also be helpful to move to the next line when the an abstract breaks the pattern such as 
# abstract 558 in this case which is about soil aggregates while the papers leading up to it were not.
# Using the next line allows me to see which numbers above correspond to which citation numbers below
# so if there is a problem in the middle of running this function I can pick up where I left off. 
length(c(1,23,70,85,90,411,469,470,500,516,539,
         568,
         822))

## you then adjust the word occurrence and abstracts that don't contain the term in the right context
## here - this is where using term.to.check instead of typing the word in can be helpful. 
fin.data[match(term.to.check,fin.data$final.words),"word.count"]<-
  # the number you subtract from this section comes from the sum function above:
  fin.data[match(term.to.check,fin.data$final.words),"word.count"]-27
# subtract for paper occurence
fin.data[match(term.to.check,fin.data$final.words),"papers.with.word.count"]<-
  # the number you subtract from this section comes from the length function above
  fin.data[match(term.to.check,fin.data$final.words),"papers.with.word.count"]-13

## the best approach is to delete or replace words that are clearly not appropriate for your
## study before going through the word check above. In this example with relatively few abstracts
## there are 9152 unique words. 
nrow(fin.data)

