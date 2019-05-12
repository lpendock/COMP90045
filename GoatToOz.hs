data Reg
    = Reg Int
    deriving (Eq)
  instance Show Reg where
    show (Reg x) = 'r':(show x)
  
  data Slot
    = Slot Int
    deriving (Show, Eq)
  
  data Label
    = Label Int
    deriving (Show, Eq)
  
  data Instr
    = Load Reg Slot             --  rI = x
    | Load_Address Reg Slot      --  rI = &x
    | Load_Indirect Reg Reg      --  rI = *rJ
    | Store Slot Reg            --  x = rI
    | Store_Indirect Reg Reg     --  *rI = rJ
  
    | Int_Const Reg Int            
    | Real_Const Reg Double       
    | Str_Const Reg String       
  
    | Add_Int Reg Reg Reg        --
    | Add_Real Reg Reg Reg
    | Add_Offset Reg Reg Reg
    | Sub_Int Reg Reg Reg        --
    | Sub_Real Reg Reg Reg
    | Sub_Offset Reg Reg Reg
    | Mul_Int Reg Reg Reg
    | Mul_Real Reg Reg Reg
    | Div_Int Reg Reg Reg
    | Div_Real Reg Reg Reg
    | Neg_Int Reg Reg
    | Reg_Real Reg Reg
  
    | Cmp_Eq_Int Reg Reg Reg
    | Cmp_Ne_Int Reg Reg Reg
    | Cmp_Gt_Int Reg Reg Reg
    | Cmp_Ge_Int Reg Reg Reg
    | Cmp_Lt_Int Reg Reg Reg
    | Cmp_Le_Int Reg Reg Reg
    | Cmp_Eq_Real Reg Reg Reg
    | Cmp_Ne_Real Reg Reg Reg
    | Cmp_Gt_Real Reg Reg Reg
    | Cmp_Ge_Real Reg Reg Reg
    | Cmp_Lt_Real Reg Reg Reg
    | Cmp_Le_Real Reg Reg Reg
  
   -- | And Reg Reg Reg
   -- | Or Reg Reg Reg
   -- | Not Reg Reg
  
    | Int_To_Real Reg Reg
    | Move Reg Reg
  
    | Branch_On_True Reg Label
    | Branch_On_False Reg Label
    | Branch_Uncond Label
    deriving (Show, Eq)
  