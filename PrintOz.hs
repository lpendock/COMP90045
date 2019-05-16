module PrintOz (printOzCode) where

import OzTree

printOzCode :: OzCode -> String
printOzCode (OzCode num lines) = concat (map printOzLine lines)

printOzLine :: OzLine -> String
printOzLine line =
  case line of
    LabelLine label -> show label ++ "\n"
    ProcLine ident -> "proc_" ++ ident ++ "\n"
    InstrLine instr -> "    " ++ (show instr) ++ "\n"