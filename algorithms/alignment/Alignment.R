compiler::enableJIT(3);  # compile functions (closures), loops and more

ALIGNMENT_IMPORTED = TRUE;  # to see if this file is imported

#########################################################
# algorithm implementation
#########################################################
Alignment.createAlignments <- function(tracebacks, sequenceA, sequenceB) {
  alignments <- list();
  numTracebacks <- length(tracebacks);
  
  for (i in 1:numTracebacks) {
    alignment <- Alignment.__createAlignment(tracebacks[[i]], sequenceA, sequenceB);
    alignments <- rlist::list.append(alignments, alignment);
  }
  
  return(alignments);
}


Alignment.__createAlignment <- function(path, sequenceA, sequenceB) {
  path <- rev(path);  # reverse: allows more intuitive calculations from start (left) to finish (right)
  
  alignedSequenceA <- Symbols.EMPTY;
  matchOrMismatchString <- Symbols.EMPTY;
  alignedSequenceB <- Symbols.EMPTY;
  
  lastPosition <- path[[1]];
  currentPositionA <- lastPosition[["i"]];
  currentPositionB <- lastPosition[["j"]];
  
  # going through each element of the path and look on the differences between vectors
  # to find out the type of difference vector (arrow)
  for (k in 2:length(path)) {
    currentPosition = path[[k]];
    
    verticalDifference = currentPosition[["i"]] - lastPosition[["i"]];
    horizontalDifference = currentPosition[["j"]] - lastPosition[["j"]];
    
    if (verticalDifference == 1 && horizontalDifference == 1) {  # diagonal case
      aChar <- substr(sequenceA, currentPositionA, currentPositionA);
      bChar <- substr(sequenceB, currentPositionB, currentPositionB);
      
      alignedSequenceA <- paste(alignedSequenceA, aChar, sep=Symbols.EMPTY);
      if (bChar == aChar) { matchOrMismatchString <- paste(matchOrMismatchString, Symbols.MATCH, sep=Symbols.EMPTY); } 
      else { matchOrMismatchString <- paste(matchOrMismatchString, Symbols.MISMATCH, sep=Symbols.EMPTY); }
      alignedSequenceB <- paste(alignedSequenceB, bChar, sep=Symbols.EMPTY);
      
      currentPositionB <- currentPositionB + 1;
      currentPositionA <- currentPositionA + 1;
    } else if (horizontalDifference > 0) {  # horizontal case
      bChar <- substr(sequenceB, currentPositionB, currentPositionB);
      
      alignedSequenceA <- paste(alignedSequenceA, Symbols.GAP, sep=Symbols.EMPTY);
      matchOrMismatchString <- paste(matchOrMismatchString, Symbols.SPACE, sep=Symbols.EMPTY);
      alignedSequenceB <- paste(alignedSequenceB, bChar, sep=Symbols.EMPTY);
      
      currentPositionB <- currentPositionB + 1;
    } else if (verticalDifference > 0) {  # vertical case
      aChar <- substr(sequenceA, currentPositionA, currentPositionA);
      
      alignedSequenceA <- paste(alignedSequenceA, aChar, sep=Symbols.EMPTY);
      matchOrMismatchString <- paste(matchOrMismatchString, Symbols.SPACE, sep=Symbols.EMPTY);
      alignedSequenceB <- paste(alignedSequenceB, Symbols.GAP, sep=Symbols.EMPTY);
      
      currentPositionA <- currentPositionA + 1;
    }
    
    lastPosition = currentPosition;
  }
  
  return(list(alignedSequenceA = alignedSequenceA, 
              matchOrMismatchString = matchOrMismatchString, 
              alignedSequenceB = alignedSequenceB));
}