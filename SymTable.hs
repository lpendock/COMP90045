module SymTable where

import GoatAST
import Data.Maybe

data Lookup = Lookup Ident ParMode GoatType
data LookupTable = LookupTable [Lookup]
data ProcTable = ProcTable [Procedure]

lookup_from_arg_spec :: FormalArgSpec -> Lookup
lookup_from_arg_spec (FormalArgSpec _ parmode basetype ident) 
  = Lookup ident parmode (Base basetype)

lookup_from_decl :: Decl -> Lookup
lookup_from_decl (Decl _ ident goattype) =  Lookup ident Val goattype

build_lookup_table :: Procedure -> LookupTable
build_lookup_table (Procedure _ _ formalargspec decl _)
  = LookupTable (map lookup_from_arg_spec formalargspec ++ 
  (map lookup_from_decl decl))

search_lookup_table :: Ident -> LookupTable -> Maybe Lookup
search_lookup_table ident (LookupTable ((Lookup i p g):ls))
  = if ident == i 
    then Just (Lookup i p g) 
    else search_lookup_table ident (LookupTable ls)
search_lookup_table _ (LookupTable []) = Nothing

proc_from_ident :: Ident -> ProcTable -> Maybe Procedure
proc_from_ident ident (ProcTable ((Procedure pp pident pargs pdecls pstmts):xs)) 
  = if ident == pident 
    then Just (Procedure pp pident pargs pdecls pstmts)
    else proc_from_ident ident (ProcTable xs)
proc_from_ident ident (ProcTable []) = Nothing