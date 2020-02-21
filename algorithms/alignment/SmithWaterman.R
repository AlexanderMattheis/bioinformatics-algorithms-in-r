compiler::enableJIT(3);  # compile functions (closures), loops and more

SMITH_WATERMAN_IMPORTED = TRUE;  # to see if this file is imported

library(rlist);  # hint: mistakenly people use often require, but require is only returning a logical value

if(!exists("ALIGNMENT_IMPORTED")) source("algorithms/alignment/Alignment.R");
if(!exists("TRACEBACK_IMPORTED")) source("algorithms/Traceback.R");
if(!exists("SYMBOLS_IMPORTED")) source("system/Symbols.R");

#########################################################
# algorithm input
#########################################################
SmithWaterman.__sequenceA <- "AATCG";
SmithWaterman.__sequenceB <- "AACG";

SmithWaterman.__gap <- -2;
SmithWaterman.__match <- 1;
SmithWaterman.__mismatch <- -1;

SmithWaterman.__matrixHeight <- nchar(SmithWaterman.__sequenceA) + 1;
SmithWaterman.__matrixWidth <- nchar(SmithWaterman.__sequenceB) + 1;


#########################################################
# algorithm output
#########################################################
SmithWaterman.__matrix <- NULL;
SmithWaterman.__score <- NULL;
SmithWaterman.__tracebacks <- NULL;
SmithWaterman.__alignments <- NULL;


#########################################################
# algorithm implementation
#########################################################
SmithWaterman.setInput <- function(sequenceA, sequenceB, gap, match, mismatch) {
  SmithWaterman.__sequenceA <<- sequenceA;
  SmithWaterman.__sequenceB <<- sequenceB;
  
  SmithWaterman.__gap <<- gap;
  SmithWaterman.__match <<- match;
  SmithWaterman.__mismatch <<- mismatch;
  
  SmithWaterman.__matrixHeight <<- nchar(SmithWaterman.__sequenceA) + 1;
  SmithWaterman.__matrixWidth <<- nchar(SmithWaterman.__sequenceB) + 1;
}


SmithWaterman.start <- function() {
  SmithWaterman.__initializeMatrix();
  SmithWaterman.__computeMatrixAndScore();
  SmithWaterman.__computeTracebacks();
  SmithWaterman.__createAlignments();
}


SmithWaterman.__initializeMatrix <- function() {
  SmithWaterman.__matrix <<- matrix(0, SmithWaterman.__matrixHeight, SmithWaterman.__matrixWidth);
}


SmithWaterman.__computeMatrixAndScore <- function() {
  maxValue <- 0;
  
  # going over each matrix cell
  for (i in 2:SmithWaterman.__matrixHeight) {
    aChar <- substr(SmithWaterman.__sequenceA, i-1, i-1);
    
    for (j in 2:SmithWaterman.__matrixWidth) {
      bChar <- substr(SmithWaterman.__sequenceB, j-1, j-1);
      
      SmithWaterman.__matrix[i, j] <<- SmithWaterman.__recursionFunction(aChar, bChar, i, j);
      
      if (maxValue < SmithWaterman.__matrix[i, j]) {
        maxValue <- SmithWaterman.__matrix[i, j];
      }
    }
  }
  
  # score is stored in the right bottom cell
  SmithWaterman.__score <<- maxValue;
}


SmithWaterman.__recursionFunction <- function(aChar, bChar, i, j) {
  if (aChar == bChar) { 
    matchOrMismatch <- SmithWaterman.__match; 
  } else {
    matchOrMismatch <- SmithWaterman.__mismatch;
  }
  
  diagonalValue <- SmithWaterman.__matrix[i - 1, j - 1] + matchOrMismatch;
  upValue <- SmithWaterman.__matrix[i - 1, j] + SmithWaterman.__gap;
  leftValue <- SmithWaterman.__matrix[i, j - 1] + SmithWaterman.__gap;
  
  return(max(diagonalValue, upValue, leftValue, 0));
}


SmithWaterman.__computeTracebacks <- function() {
  # computing all traceback start-positions
  backtraceStarts = SmithWaterman.__getAllMaxPositions();

  Traceback.init();
  for (i in 1:length(backtraceStarts)) {
    Traceback.computeLocalTracebackPaths(list(backtraceStarts[[i]]), SmithWaterman.__matrix, SmithWaterman.__neighbourFunction);
  }
  
  SmithWaterman.__tracebacks <<- Traceback.getTracebacks();
}


SmithWaterman.__getAllMaxPositions <- function() {
  maxPositions <- list();
  
  if (SmithWaterman.__score > 0) {  # only positions bigger 0 can be start positions (local alignments never 0 or lower)
    for (i in 1:SmithWaterman.__matrixHeight) {
      for (j in 1:SmithWaterman.__matrixWidth) {
        if (SmithWaterman.__matrix[i, j] == SmithWaterman.__score) {  # if position is matrix maximum value, then store
          maxPositions <- rlist::list.append(maxPositions, c(i = i, j = j));
        }
      }
    }
  }
  
  return(maxPositions);
}


SmithWaterman.__neighbourFunction <- function(currentCell) {
  neighboured <- list();
  
  left <- currentCell[["j"]] - 1;
  up <- currentCell[["i"]] - 1;
  
  # retrieve values
  if(left >= 1) { bChar <- substr(SmithWaterman.__sequenceB, left, left); } else { bChar <- Symbols.EMPTY; }
  if(up >= 1) { aChar <- substr(SmithWaterman.__sequenceA, up, up); } else { aChar <- Symbols.EMPTY; }
  
  currentValue <- SmithWaterman.__matrix[currentCell[["i"]], currentCell[["j"]]];
  
  if(aChar == bChar) { matchOrMismatch <- SmithWaterman.__match; } else { matchOrMismatch <- SmithWaterman.__mismatch; }
  
  if(left >= 1 && up >= 1) { diagonalValue <- SmithWaterman.__matrix[up, left]; } else { diagonalValue <- Symbols.INF; }
  if(up >= 1) { upValue <- SmithWaterman.__matrix[up, currentCell[["j"]]]; } else { upValue <- Symbols.INF; }
  if(left >= 1) { leftValue <- SmithWaterman.__matrix[currentCell[["i"]], left]; } else { leftValue <- Symbols.INF; }
  
  # check
  isMatchMismatch <- currentValue == diagonalValue + matchOrMismatch;
  isDeletion <- currentValue == upValue + SmithWaterman.__gap;
  isInsertion <- currentValue == leftValue + SmithWaterman.__gap;
  
  isMatchMismatch <- isMatchMismatch || (currentValue == 0 && up >= 1 && left >= 1);
  isDeletion <- isDeletion || currentValue == 0 && up >= 1;  # lower 0 -> cut away
  isInsertion <- isInsertion || currentValue == 0 && left >= 1;  # lower 0 -> cut away
  
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


SmithWaterman.__createAlignments <- function() {
  SmithWaterman.__alignments <<- Alignment.createAlignments(SmithWaterman.__tracebacks, 
                                                            SmithWaterman.__sequenceA, 
                                                            SmithWaterman.__sequenceB);
}
