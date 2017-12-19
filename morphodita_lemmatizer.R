### ### ### ###
#
# Lukas Lehotsky
# 19-12-2017
#
### ### ### ###

### ### ### ###
#
# functions
#
### ### ### ###

# packages helper function ----

required.packages <- function() {
  
  packages <- c("jsonlite", "tau", "XML")
  
  install.list <- names(which(sapply(packages, function(x) any(x == rownames(installed.packages())) == T) == F))
  
  if(length(install.list) > 0) {install.packages(install.list)}
  
  library(jsonlite)
  library(tau)
  library(XML)
  
}

# wait helper function ----

sys.wait <- function(min,max) {
  t <- runif(1, min = min,max = max) #random number generation
  Sys.sleep(t) #time break in between downloads to prevent downloader being classified as robot attack
} # pause between iterations function

# send request to lemmatizer and get raw output - external CURL software needed (windows version) ----

curl.dl.lemma <- function(content, curl.software, language.model) {
  
  if(file.exists(curl.software) == F) {stop("CURL software is not accessible. Download @ https://curl.haxx.se/download.html")}
  
  if(missing(language.model)) {language.model = "cs"}
  
  if(!any(c("en","cs")== language.model)) {stop("Wrong language model selection. Options are \"en\", \"cs\"")}
  
  if(language.model == "cs") {
    lang.model <- "czech-morfflex-pdt-161115"
  } else if (language.model == "en") {
    lang.model <- "english-morphium-wsj-140407"
  }
  
  file <- "temp-file.txt"
  
  write.table(content,
              file = file,
              quote = F,
              row.names = F,
              col.names = F,
              fileEncoding = "UTF-8"
  )
  
  s <- NULL
  
  s[1] <- paste0("\"",curl.software,"\"")
  s[2] <- "-F" #curl form parameter
  s[3] <- paste0("data=@\"",file,"\"") #source file
  s[4] <- "-F convert_tagset=strip_lemma_id" #strip lemma comments
  s[5] <- paste0("-F model=",lang.model)
  s[6] <- "-F output=xml" #output type
  s[7] <- "--output" #curl output parameter
  s[8] <- paste0("\"",file,"\"") # output file name
  s[9] <- " http://lindat.mff.cuni.cz/services/morphodita/api/tag"
  
  if(any(is.na(s))) {
    stop("No file to process")
  } else {
    system(command = paste(s, collapse = " "), wait = T)
  }
  
  lemmatized <- readLines(file,warn = F,encoding = "UTF-8",skipNul = T)
  
  lemmatized <- paste0(lemmatized,collapse = "")
  
  file.remove(file)
  
  return(lemmatized)
}

# convert raw output to xml/lemmatized text ----

lemma.process <- function(content, return.lemmatized) {
  
  if(missing(return.lemmatized)) {return.lemmatized = F}
  
  content.json <-
    fromJSON(content, 
             simplifyVector = T,
             flatten = T) #fetch result as a json structure - package "jsonlite" needed
  
  content.json <- fixEncoding(content.json$result)
  
  content.json <- paste0("<xml>",content.json) #prepend xml tag
  
  content.json <- paste0(content.json,"</xml>") #apend xml tag
  
  content.xml <-
    xmlTreeParse(
      content.json,
      useInternalNodes = T,
      trim = T,
      encoding = "UTF-8",
      error = NULL
    ) #extract xml results from json file  - package "XML" needed
  
  if(return.lemmatized == T){
    
    content.lemma <- xpathSApply(content.xml, "//*//token//@lemma") #extract word stems - package "XML" needed
    
    content.lemma <- unlist(content.lemma)
    
    content.lemma <- paste(content.lemma,collapse = " ")
    
    content.lemma <- fixEncoding(content.lemma)
    
    return(content.lemma)
    
  } else {
    
    return(content.xml)
  }
  
}

# extract POS tags into data frame ----

pos.tag.dataframe <- function(lemmatized.xml.source,language.model) {
  
  if(missing(language.model)) {language.model = "cs"}
  
  if(!any(c("en","cs")== language.model)) {stop("Wrong language model selection. Options are \"en\", \"cs\"")}
  
  xml.sentences <- xpathSApply(lemmatized.xml.source, "//*//sentence")
  
  sentences.df <- data.frame(stringsAsFactors = F)
  
  for(sentence in 1:length(xml.sentences)) {
    
    sentence.no <- sentence
    
    sentence <- xml.sentences[[sentence]]
    
    sentence <- xmlDoc(sentence)
    
    sentence.df <- xmlToDataFrame(sentence,stringsAsFactors = F)
    
    sentence.df <- data.frame(text = sentence.df[complete.cases(sentence.df),],stringsAsFactors = F)
    
    sentence.df$text <- fixEncoding(sentence.df$text)
    
    sentence.df$position <- 1:nrow(sentence.df)
    
    sentence.tag <- xpathSApply(sentence, "//*//token//@tag")
    
    sentence.tag <- strsplit(sentence.tag,split = "*",fixed = F)
    
    if(language.model == "cs") {
      
      sentence.tag <- t(as.data.frame(sentence.tag))
      
    } else if (language.model == "en") {
      
      sentence.tag <- sapply(sentence.tag, function(x) x[1])
    
    }
    
    sentence.tag <- unname(sentence.tag)
    
    if(language.model == "cs") {
    
      colnames(sentence.tag) <- c("POS","SubPOS","Gender","Number","Case","PossGender","PossNumber","Person","Tense","Grade","Negation","Voice","Reserve1","Reserve2","Var")
    
      }
    
    # full tag meaning explained @ http://ufal.mff.cuni.cz/pdt2.0/doc/manuals/en/m-layer/html/ch02s02s01.html
    
    sentence.lemmas <- xpathSApply(sentence, "//*//token//@lemma")
    
    sentence.lemmas <- fixEncoding(sentence.lemmas)
    
    binder <- cbind(word = sentence.df, sentence.no = sentence.no, lemma = sentence.lemmas, sentence.tag)
    
    rownames(binder)<- NULL
    
    sentences.df <- rbind(sentences.df, binder, stringsAsFactors=F)
    
  }
  
  # write.table(sentences.df,"sentences.txt",fileEncoding = "UTF-8",row.names = F)
  
  if (language.model == "en") {
    
    colnames(sentences.df)[which(colnames(sentences.df) == "sentence.tag")] <- "POS"
    
  }
  
  # implementation of the english model is simplified only to single main PoS type characteristic
  
  return(sentences.df)
  
}

# apply filtering over POS dataframe ----

pos.filter <- function(pos.tag.dataframe,conditions) {
  
  sentences.sub <- data.frame(stringsAsFactors = F)
  
  for(condition in conditions) {
    
    variable <- parse(text = paste0("pos.tag.dataframe$",condition))
    
    binder <- pos.tag.dataframe[eval(variable),]
    
    sentences.sub <- rbind(sentences.sub, binder)
  }
  
  sentences.sub <- unique(sentences.sub)
  
  sentences.sub <- sentences.sub[order(sentences.sub$sentence.no,sentences.sub$word.position),]
  
  return(sentences.sub)
  
}

# czech PoS tagger works according to description @ http://ufal.mff.cuni.cz/pdt2.0/doc/manuals/en/m-layer/html/ch02s02s01.html
# english PoS tagger is simplified - only PoS type is determined


# reconstruct sentences from filtered POS tags ----

pos.synthesizer <- function(pos.tag.dataframe, return.lemmatized) {
  
  if(missing(return.lemmatized)) {return.lemmatized = T}
  
  max(pos.tag.dataframe$sentence.no)
  
  text.output <- NULL
  
  for(sentence in 1:max(pos.tag.dataframe$sentence.no)) {
    
    sentence <- pos.tag.dataframe[pos.tag.dataframe$sentence.no == sentence,]
    
    if(return.lemmatized == T) {
      output <- sentence$lemma
    } else {
      output <- sentence$word.text
    }
    
    
    output <- paste(output, collapse = " ")
    
    text.output <- append(text.output, output)
    
  }
  text.output <- as.character(text.output)
  
  return(text.output)
}

### ### ### ###
#
# test the script and return lemmatized output directly
#
### ### ### ###

# load packages

required.packages()

# link to curl software - download @ https://curl.haxx.se/download.html

soft  <-  paste0(Sys.getenv("USERPROFILE"),"\\path_to_curl\\curl.exe")

# plug in some content

content <- "Lidovci po čtyřech letech opustili vládu. Jejich předseda Pavel Bělobrádek tak přišel o post vicepremiéra pro vědu, výzkum a inovace. Nyní proto uvažuje, jak naložit s další politickou kariérou."

# send a document for lemmatization

content <- curl.dl.lemma(content,curl.software = soft,language.model = "cs")

# extract lemmatized output of the document

content.lemmatized <- lemma.process(content,return.lemmatized = T)

content.lemmatized

### ### ### ###
#
# test the script, lemmatize, filter on PoS type, return filtered lemmatized output
#
### ### ### ###

# load packages

required.packages()

# link to curl software - download @ https://curl.haxx.se/download.html

soft  <-  paste0(Sys.getenv("USERPROFILE"),"\\path_to_curl\\curl.exe")

# plug in some content

content <- "Lidovci po čtyřech letech opustili vládu. Jejich předseda Pavel Bělobrádek tak přišel o post vicepremiéra pro vědu, výzkum a inovace. Nyní proto uvažuje, jak naložit s další politickou kariérou."

# send a document for lemmatization

content <- curl.dl.lemma(content,curl.software = soft,language.model = "cs")

# extract XML structure of the lemmatized output for future PoS processing

content.lemmatized.raw <- lemma.process(content,return.lemmatized = F)

# convert XML to data-frame

content.df.tag <- pos.tag.dataframe(lemmatized.xml.source = content.lemmatized.raw,language.model = "cs")

# filter data-frame according to particular conditions
# list of all tags and tag names @ http://ufal.mff.cuni.cz/pdt2.0/doc/manuals/en/m-layer/html/ch02s02s01.html

conditions <- c("POS ==\"N\"", 
                # "POS == \"N\" & Gender == \"F\" & Case != \"1\"", 
                "POS ==\"V\"",
                "POS ==\"A\""
                )

content.tag.filtered <- pos.filter(pos.tag.dataframe = content.df.tag, conditions = conditions)

# reconstruct the filtered data-frame into lemmatized output

content.lemmatized.filtered <- pos.synthesizer(pos.tag.dataframe = content.tag.filtered,return.lemmatized = T)


### ### ### ###
#
# run the script, lemmatize, filter on PoS type, return filtered unlemmatized output
#
### ### ### ###

# load packages

required.packages()

# link to curl software - download @ https://curl.haxx.se/download.html

soft  <-  paste0(Sys.getenv("USERPROFILE"),"\\path_to_curl\\curl.exe")

# plug in some content

content <- "Lidovci po čtyřech letech opustili vládu. Jejich předseda Pavel Bělobrádek tak přišel o post vicepremiéra pro vědu, výzkum a inovace. Nyní proto uvažuje, jak naložit s další politickou kariérou."

# send a document for lemmatization

content <- curl.dl.lemma(content,curl.software = soft,language.model = "cs")

# extract XML structure of the lemmatized output for future PoS processing

content.lemmatized.raw <- lemma.process(content,return.lemmatized = F)

# convert XML to data-frame

content.df.tag <- pos.tag.dataframe(lemmatized.xml.source = content.lemmatized.raw,language.model = "cs")

# filter data-frame according to particular conditions
# list of all tags and tag names @ http://ufal.mff.cuni.cz/pdt2.0/doc/manuals/en/m-layer/html/ch02s02s01.html

conditions <- c("POS ==\"N\"", 
                # "POS == \"N\" & Gender == \"F\" & Case != \"1\"", 
                "POS ==\"V\"",
                "POS ==\"A\""
)

content.tag.filtered <- pos.filter(pos.tag.dataframe = content.df.tag, conditions = conditions)

# reconstruct the filtered data-frame into lemmatized output

content.lemmatized.filtered <- pos.synthesizer(pos.tag.dataframe = content.tag.filtered,return.lemmatized = F)
