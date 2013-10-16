{-# LANGUAGE CPP #-}

#if 0
genTypesVals :: Int -> IO ()
genTypesVals n0 =
  putStrLn $ "#define TypesVals(macro) \\\n" ++ concatMap doit [1..n0]
  where doit n = "  macro(" ++ icl " -> " (as n) ++ ", \\\n" ++
                 "        " ++ icl " " (as n) ++ ")" ++
                 (if n < n0 then "; \\\n" else "\n")
        as n = map (\i -> "a" ++ show i) [1..n]
        icl _ [] = []
        icl s (h:t) = h ++ concatMap (\a -> s ++ a) t
#endif

#define TypesVals(macro) \
  macro(a1, \
        a1); \
  macro(a1 -> a2, \
        a1 a2); \
  macro(a1 -> a2 -> a3, \
        a1 a2 a3); \
  macro(a1 -> a2 -> a3 -> a4, \
        a1 a2 a3 a4); \
  macro(a1 -> a2 -> a3 -> a4 -> a5, \
        a1 a2 a3 a4 a5); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6, \
        a1 a2 a3 a4 a5 a6); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7, \
        a1 a2 a3 a4 a5 a6 a7); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8, \
        a1 a2 a3 a4 a5 a6 a7 a8); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9, \
        a1 a2 a3 a4 a5 a6 a7 a8 a9); \
  macro(a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10, \
        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)

