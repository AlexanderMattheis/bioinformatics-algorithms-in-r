
test_that("Test1", {
  NeedlemanWunsch.setInput("AATCG", "AACG", -2, 1, -1);
  NeedlemanWunsch.start();
  
  testthat::expect_equal(2, NeedlemanWunsch.__score);
  testthat::expect_equal("AATCG", NeedlemanWunsch.__alignments[[1]][[1]]);
  testthat::expect_equal("AA_CG", NeedlemanWunsch.__alignments[[1]][[3]]);
});


test_that("Test2", {
  NeedlemanWunsch.setInput("ATC", "AGTC", -2, 1, -1);
  NeedlemanWunsch.start();
  
  testthat::expect_equal(1, NeedlemanWunsch.__score);
  testthat::expect_equal("A_TC", NeedlemanWunsch.__alignments[[1]][[1]]);
  testthat::expect_equal("AGTC", NeedlemanWunsch.__alignments[[1]][[3]]);
});


test_that("Test3", {
  NeedlemanWunsch.setInput("CCCCGCGACTCGGGTTCAAGGG", "GGGTGAGACCCCAGTTCAACCC", -2, 4, -1);
  NeedlemanWunsch.start();
  
  testthat::expect_equal(33, NeedlemanWunsch.__score);
  testthat::expect_equal("CCCCGCGACTCGGGTTCAAGGG", NeedlemanWunsch.__alignments[[1]][[1]]);
  testthat::expect_equal("GGGTGAGACCCCAGTTCAACCC", NeedlemanWunsch.__alignments[[1]][[3]]);
});


test_that("Test4", {
  NeedlemanWunsch.setInput("TCCGA", "TACGCGC", -1, 1, 0);
  NeedlemanWunsch.start();
  
  testthat::expect_equal(2, NeedlemanWunsch.__score);
  testthat::expect_equal("T_C_CGA", NeedlemanWunsch.__alignments[[1]][[1]]);
  testthat::expect_equal("TACGCGC", NeedlemanWunsch.__alignments[[1]][[3]]);
});


test_that("Test5", {
  NeedlemanWunsch.setInput("AATCG", "ACG", -2, 1, -1);
  NeedlemanWunsch.start();
  
  testthat::expect_equal(-1, NeedlemanWunsch.__score);
  testthat::expect_equal("AATCG", NeedlemanWunsch.__alignments[[1]][[1]]);
  testthat::expect_equal("_A_CG", NeedlemanWunsch.__alignments[[1]][[3]]);
  
  testthat::expect_equal("AATCG", NeedlemanWunsch.__alignments[[2]][[1]]);
  testthat::expect_equal("A__CG", NeedlemanWunsch.__alignments[[2]][[3]]);
});