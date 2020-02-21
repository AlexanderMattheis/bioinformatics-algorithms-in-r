compiler::enableJIT(3);  # compile functions (closures), loops and more

NEEDLEMAN_WUNSCH_IMPORTED = TRUE;  # to see if this file is imported

library(rlist);  # hint: mistakenly people use often require, but require is only returning a logical value

if(!exists("ALIGNMENT_IMPORTED")) source("algorithms/alignment/Alignment.R");
if(!exists("SYMBOLS_IMPORTED")) source("system/Symbols.R");
if(!exists("TRACEBACK_IMPORTED")) source("algorithms/Traceback.R");

#########################################################
# algorithm input
#########################################################
NeedlemanWunsch.__sequenceA <- "AATCG";
NeedlemanWunsch.__sequenceB <- "ACG";

NeedlemanWunsch.__gap <- -2;
NeedlemanWunsch.__match <- 1;
NeedlemanWunsch.__mismatch <- -1;

NeedlemanWunsch.__matrixHeight <- nchar(NeedlemanWunsch.__sequenceA) + 1;
NeedlemanWunsch.__matrixWidth <- nchar(NeedlemanWunsch.__sequenceB) + 1;


#########################################################
# algorithm output
#########################################################
NeedlemanWunsch.__matrix <- NULL;
NeedlemanWunsch.__score <- NULL;
NeedlemanWunsch.__tracebacks <- NULL;
NeedlemanWunsch.__alignments <- NULL;


#########################################################
# algorithm implementation
#########################################################
NeedlemanWunsch.setInput <- function(sequenceA, sequenceB, gap, match, mismatch) {
  NeedlemanWunsch.__sequenceA <<- sequenceA;
  NeedlemanWunsch.__sequenceB <<- sequenceB;
  
  NeedlemanWunsch.__gap <<- gap;
  NeedlemanWunsch.__match <<- match;
  NeedlemanWunsch.__mismatch <<- mismatch;
  
  NeedlemanWunsch.__matrixHeight <<- nchar(NeedlemanWunsch.__sequenceA) + 1;
  NeedlemanWunsch.__matrixWidth <<- nchar(NeedlemanWunsch.__sequenceB) + 1;
}


NeedlemanWunsch.start <- function() {
  NeedlemanWunsch.__initializeMatrix();
  NeedlemanWunsch.__computeMatrixAndScore();
  NeedlemanWunsch.__computeTracebacks();
  NeedlemanWunsch.__createAlignments();
}


NeedlemanWunsch.__initializeMatrix <- function() {
  NeedlemanWunsch.__matrix <<- matrix(0, NeedlemanWunsch.__matrixHeight, NeedlemanWunsch.__matrixWidth);
  
  for (i in 2:NeedlemanWunsch.__matrixHeight) {
    NeedlemanWunsch.__matrix[i, 1] <<- NeedlemanWunsch.__matrix[i - 1, 1] + NeedlemanWunsch.__gap;
  }
  
  for (j in 2:NeedlemanWunsch.__matrixWidth) {
    NeedlemanWunsch.__matrix[1, j] <<- NeedlemanWunsch.__matrix[1, j - 1] + NeedlemanWunsch.__gap;
  }
}


NeedlemanWunsch.__computeMatrixAndScore <- function() {
  # going over each matrix cell
  for (i in 2:NeedlemanWunsch.__matrixHeight) {
    aChar <- substr(NeedlemanWunsch.__sequenceA, i-1, i-1);
    
    for (j in 2:NeedlemanWunsch.__matrixWidth) {
      bChar <- substr(NeedlemanWunsch.__sequenceB, j-1, j-1);
      
      NeedlemanWunsch.__matrix[i, j] <<- NeedlemanWunsch.__recursionFunction(aChar, bChar, i, j);
    }
  }
  
  # score is stored in the right bottom cell
  NeedlemanWunsch.__score <<- NeedlemanWunsch.__matrix[NeedlemanWunsch.__matrixHeight, NeedlemanWunsch.__matrixWidth];
}


NeedlemanWunsch.__recursionFunction <- function(aChar, bChar, i, j) {
  if (aChar == bChar) { 
    matchOrMismatch <- NeedlemanWunsch.__match; 
  } else {
    matchOrMismatch <- NeedlemanWunsch.__mismatch;
  }
  
  diagonalValue <- NeedlemanWunsch.__matrix[i - 1, j - 1] + matchOrMismatch;
  upValue <- NeedlemanWunsch.__matrix[i - 1, j] + NeedlemanWunsch.__gap;
  leftValue <- NeedlemanWunsch.__matrix[i, j - 1] + NeedlemanWunsch.__gap;
  
  return(max(diagonalValue, upValue, leftValue));
}


NeedlemanWunsch.__computeTracebacks <- function() {
  lowerRightCorner <- c(i = NeedlemanWunsch.__matrixHeight, j = NeedlemanWunsch.__matrixWidth);
  Traceback.init();
  Traceback.computeGlobalTracebackPaths(list(lowerRightCorner), NeedlemanWunsch.__neighbourFunction);
  NeedlemanWunsch.__tracebacks <<- Traceback.getTracebacks();
}


NeedlemanWunsch.__neighbourFunction <- function(currentCell) {
  neighboured <- list();
  
  left <- currentCell[["j"]] - 1;
  up <- currentCell[["i"]] - 1;
  
  # retrieve values
  if(left >= 1) { bChar <- substr(NeedlemanWunsch.__sequenceB, left, left); } else { bChar <- Symbols.EMPTY; }
  if(up >= 1) { aChar <- substr(NeedlemanWunsch.__sequenceA, up, up); } else { aChar <- Symbols.EMPTY; }
  
  currentValue <- NeedlemanWunsch.__matrix[currentCell[["i"]], currentCell[["j"]]];
  
  if(aChar == bChar) { matchOrMismatch <- NeedlemanWunsch.__match; } else { matchOrMismatch <- NeedlemanWunsch.__mismatch; }
  
  if(left >= 1 && up >= 1) { diagonalValue <- NeedlemanWunsch.__matrix[up, left]; } else { diagonalValue <- Symbols.INF; }
  if(up >= 1) { upValue <- NeedlemanWunsch.__matrix[up, currentCell[["j"]]]; } else { upValue <- Symbols.INF; }
  if(left >= 1) { leftValue <- NeedlemanWunsch.__matrix[currentCell[["i"]], left]; } else { leftValue <- Symbols.INF; }

  # check
  isMatchMismatch <- currentValue == diagonalValue + matchOrMismatch;
  isDeletion <- currentValue == upValue + NeedlemanWunsch.__gap;
  isInsertion <- currentValue == leftValue + NeedlemanWunsch.__gap;
  
  # add
  if (isMatchMismatch) {
    neighboured <- rlist::list.append(neighboured, c(i = up, j = left));
  }
  
  if (isDeletion) {
    neighboured <- rlist::list.append(neighboured, c(i = up, j = currentCell[["j"]]));
  }
  
  if (isInsertion) {
    neighboured <- rlist::list.append(neighboured, c(i = currentCell[["i"]],  j = left));
  }
  
  return(neighboured);
}


NeedlemanWunsch.__createAlignments <- function() {
  NeedlemanWunsch.__alignments <<- Alignment.createAlignments(NeedlemanWunsch.__tracebacks, 
                                                              NeedlemanWunsch.__sequenceA, 
                                                              NeedlemanWunsch.__sequenceB);
}