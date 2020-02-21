
test_that("Test1", {
  SmithWaterman.setInput("AATCG", "AACG", -2, 1, -1);
  SmithWaterman.start();
  
  testthat::expect_equal(2, SmithWaterman.__score);
  testthat::expect_equal("AA", SmithWaterman.__alignments[[1]][[1]]);
  testthat::expect_equal("AA", SmithWaterman.__alignments[[1]][[3]]);
  
  testthat::expect_equal("CG", SmithWaterman.__alignments[[2]][[1]]);
  testthat::expect_equal("CG", SmithWaterman.__alignments[[2]][[3]]);
});


test_that("Test2", {
  SmithWaterman.setInput("TCCGA", "TACGCAGA", -1, 1, 0);
  SmithWaterman.start();
  
  testthat::expect_equal(3, SmithWaterman.__score);
  testthat::expect_equal("TCCG", SmithWaterman.__alignments[[1]][[1]]);
  testthat::expect_equal("TACG", SmithWaterman.__alignments[[1]][[3]]);
  
  testthat::expect_equal("TCCGA", SmithWaterman.__alignments[[2]][[1]]);
  testthat::expect_equal("TACGC", SmithWaterman.__alignments[[2]][[3]]);
  
  testthat::expect_equal("TCCG_A", SmithWaterman.__alignments[[3]][[1]]);
  testthat::expect_equal("TACGCA", SmithWaterman.__alignments[[3]][[3]]);
  
  testthat::expect_equal("CCGA", SmithWaterman.__alignments[[4]][[1]]);
  testthat::expect_equal("CAGA", SmithWaterman.__alignments[[4]][[3]]);
});


test_that("Test3", {
  SmithWaterman.setInput("CCCCGCGACTCGGGTTCAAGGG", "GGGTGAGACCCCAGTTCAACCC", -2, 4, -1);
  SmithWaterman.start();
  
  testthat::expect_equal(40, SmithWaterman.__score);
  testthat::expect_equal("GCGACTCGGGTTCAA", SmithWaterman.__alignments[[1]][[1]]);
  testthat::expect_equal("GAGACCCCAGTTCAA", SmithWaterman.__alignments[[1]][[3]]);
});