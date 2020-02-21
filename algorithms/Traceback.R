compiler::enableJIT(3);  # compile functions (closures), loops and more

TRACEBACK_IMPORTED = TRUE;  # to see if this file is imported

#########################################################
# algorithm implementation
#########################################################
Traceback.__tracebacks <- NULL;

Traceback.init <- function() {
  Traceback.__tracebacks <<- list();
} 


Traceback.computeGlobalTracebackPaths <- function(path, neighbourFunction) {
  currentCell <- path[[length(path)]];
  neighbouredCells <- neighbourFunction(currentCell);
  
  for(cell in neighbouredCells) {
    if (cell[["i"]] == 1 && cell[["j"]] == 1) {
      path <- rlist::list.append(path, cell);
      Traceback.__tracebacks <<- rlist::list.append(Traceback.__tracebacks, path);  # always: deep copy in R
      path[length(path)] <- NULL;
    } else {
      path <- rlist::list.append(path, cell);
      Traceback.computeGlobalTracebackPaths(path, neighbourFunction);
      path[length(path)] <- NULL;
    }
  }
}


Traceback.computeLocalTracebackPaths <- function(path, matrix, neighbourFunction) {
  currentCell <- path[[length(path)]];
  neighbouredCells <- neighbourFunction(currentCell);
  
  for(cell in neighbouredCells) {
    if (matrix[cell[["i"]], cell[["j"]]] == 0) {
      path <- rlist::list.append(path, cell);
      Traceback.__tracebacks <<- rlist::list.append(Traceback.__tracebacks, path);  # always: deep copy in R
      path[length(path)] <- NULL;
    } else {
      path <- rlist::list.append(path, cell);
      Traceback.computeLocalTracebackPaths(path, matrix, neighbourFunction);
      path[length(path)] <- NULL;
    }
  }
}


Traceback.getTracebacks <- function() {
  return(Traceback.__tracebacks);
}