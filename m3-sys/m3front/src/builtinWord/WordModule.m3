MODULE WordModule =
  GenModule (Int,
             WordPlus, WordTimes, WordMinus, WordDivide, WordMod,
             WordLT, WordLE, WordGT, WordGE,
             WordAnd, WordOr, WordXor, WordNot,
             WordShift, WordRotate, WordExtract, WordInsert)
END WordModule.
