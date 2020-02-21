# hint: under "Files" (File Explorer) set this files-folder as working directory
if(!exists("NEEDLEMAN_WUNSCH_IMPORTED")) source("algorithms/alignment/NeedlemanWunsch.R");
if(!exists("SMITH_WATERMAN_IMPORTED")) source("algorithms/alignment/SmithWaterman.R");

#########################################################
NeedlemanWunsch.setInput("AATCG", "ACG", -2, 1, -1);
NeedlemanWunsch.start();

SmithWaterman.setInput("AATCG", "AACG", -2, 1, -1);
SmithWaterman.start();
