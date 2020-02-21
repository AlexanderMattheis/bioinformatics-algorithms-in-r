library(testthat)

if(!exists("NEEDLEMAN_WUNSCH_IMPORTED")) source("algorithms/alignment/NeedlemanWunsch.R");
if(!exists("SMITH_WATERMAN_IMPORTED")) source("algorithms/alignment/SmithWaterman.R");

test_results <- test_dir("tests", reporter="summary")