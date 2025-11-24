rm(list=ls())
setwd('V:/MPA/MPAPRG/exercise_10')

library(Biostrings)

# --- TASK 1 ---

SuffixArray <- function(dna_string){
  l_dna <- length(dna_string)
  unordered_strings <- c()
  
  for (i in 1:l_dna){
    unordered_strings <- c(unordered_strings, dna_string[i:l_dna])
  }
  
  suffix_array <- order(as.character(DNAStringSet(unordered_strings)))

  return(suffix_array)
}

dna_string <- DNAString("CTAATAATG")
# print(SuffixArray(dna_string))


# --- TASK 2 ---
# jednotka na piatom mieste -> patka na prvom mieste

InverseSuffixArray <- function(suffix_array){
  inverse_array <- rep(0, length(suffix_array))
  
  for (i in 1:length(suffix_array)){
    ind <- which(suffix_array == i)
    
    inverse_array[i] <- ind
  }
  
  return(inverse_array)
}

# print(InverseSuffixArray(SuffixArray(dna_string)))


# --- TASK3 ---

LCPArray <- function(text, SA, ISA){
  m <- length(SA)
  LCP <- c()
  
  LCP[1] <- -1
  LCP[m + 1] <- -1
  l <- 0
  
  for (i in 1:m){
    j <- ISA[i]
    if (j > 1){
      k <- SA[j - 1]
      while (text[k + l] == text[i + l]){
        l <- l + 1
      }
      LCP[j] <- l
      l <- max((l-1), 0)
    }
  }
  return(LCP)
}

s_a <- SuffixArray(dna_string)
i_s_a <- InverseSuffixArray(SuffixArray(dna_string))
lcp_array <- LCPArray(dna_string, s_a, i_s_a )


# --- TASK4 ---

BinarySearchSA <- function(pattern, text, SA){
  minIndex <- 1
  maxIndex <- length(text)
  
  while (minIndex < maxIndex){
    midlIndex <- floor((minIndex + maxIndex) / 2)
    
    pref_suffix <- as.character(subseq(text, SA[midlIndex], min(SA[midlIndex] + nchar(pattern) - 1, nchar(text))))
    if (as.character(pattern) <= pref_suffix){
      maxIndex <- midlIndex
      
    }else{
      minIndex <- midlIndex + 1
    }
  }
  First <- minIndex
  maxIndex <- length(text)
  while (maxIndex > minIndex){
    midlIndex <- floor((minIndex + maxIndex) / 2)
    
    pref_suffix <- as.character(subseq(text, SA[midlIndex], min(SA[midlIndex] + nchar(pattern) - 1, nchar(text))))
    if (pref_suffix <= as.character(pattern)){
      minIndex <- midlIndex + 1
    }else{
      maxIndex <- midlIndex
    }
  }
  Last <- maxIndex - 1
  if (Last < First){
    return('Pattern does not appear in text')
  }else{
    return(c(First, Last))
  }
}

pattern <- DNAString("ATG")
text <- DNAString("CTAATAATG")
sa <- SuffixArray(text)

print(BinarySearchSA(pattern, text, sa))

















