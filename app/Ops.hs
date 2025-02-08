module Ops where

import Data.Int (Int16, Int8)
import Data.List (intercalate)

type BcompAsm = [Op]

type Label = String

data Addr
  = AddrAbs Label -- Direct absolute addressing
  | AddrRel Label -- Direct relative addressing
  | AddrInd Label -- Indirect relative addressing
  | AddrIndI Label -- Indirect autoincremental addressing
  | AddrIndD Label -- Indirect autodecremental addressing
  | AddrStk Int8 -- Addressing with offset relative to SP
  | AddrFwd Int8 -- Addressing with direct operand loading
  deriving (Eq)

instance Show Addr where
  show :: Addr -> String
  show (AddrAbs l) = "$" ++ l
  show (AddrRel l) = show l
  show (AddrInd l) = "(" ++ l ++ ")"
  show (AddrIndI l) = "(" ++ l ++ ")+"
  show (AddrIndD l) = "-(" ++ l ++ ")"
  show (AddrStk i) = "&" ++ show i
  show (AddrFwd i) = "#" ++ show i

data CWord
  = CWord Int16
  | CWordUnd
  | CWordArr [Int16]
  | CWordDup Int Int16
  | CWordDupUnd Int
  deriving (Eq)

instance Show CWord where
  show :: CWord -> String
  show (CWord i) = "WORD " ++ show i
  show CWordUnd = "WORD ?"
  show (CWordArr arr) = "WORD " ++ intercalate "," (map show arr)
  show (CWordDup c i) = "WORD " ++ show c ++ " DUP " ++ show i
  show (CWordDupUnd c) = "WORD " ++ show c ++ " DUP ?"

data Op
  = OP_ORG Int
  | OP_LABEL Label -- LABEl: ...
  | OP_WORD CWord
  | OP_NOP
  | OP_HLT
  | OP_CLA
  | OP_NOT
  | OP_CLC
  | OP_ROL
  | OP_ROR
  | OP_ASL
  | OP_ASR
  | OP_SXTB
  | OP_SWAB
  | OP_INC
  | OP_DEC
  | OP_NEG
  | OP_POP
  | OP_POPF
  | OP_RET
  | OP_IRET
  | OP_PUSH
  | OP_PUSHF
  | OP_SWAP
  | OP_DI
  | OP_EI
  | OP_IN String
  | OP_OUT String
  | OP_INT Int
  | OP_AND Addr
  | OP_OR Addr
  | OP_ADD Addr
  | OP_ADC Addr
  | OP_SUB Addr
  | OP_CMP Addr
  | OP_LOOP Addr
  | OP_LD Addr
  | OP_SWAM Addr
  | OP_JUMP Addr
  | OP_CALL Addr
  | OP_ST Addr
  | OP_BEQ Label
  | OP_BNE Label
  | OP_BMI Label
  | OP_BPL Label
  | OP_BHIS Label
  | OP_BLO Label
  | OP_BVS Label
  | OP_BVC Label
  | OP_BLT Label
  | OP_BGE Label
  deriving (Eq)

instance Show Op where
  show :: Op -> String
  show (OP_ORG i) = "ORG " ++ show i
  show (OP_LABEL s) = s ++ ":"
  show (OP_WORD w) = show w
  show OP_NOP = "NOP"
  show OP_HLT = "HLT"
  show OP_CLA = "CLA"
  show OP_NOT = "NOT"
  show OP_CLC = "CLC"
  show OP_ROL = "ROL"
  show OP_ROR = "ROR"
  show OP_ASL = "ASL"
  show OP_ASR = "ASR"
  show OP_SXTB = "SXTB"
  show OP_SWAB = "SWAB"
  show OP_INC = "INC"
  show OP_DEC = "DEC"
  show OP_NEG = "NEG"
  show OP_POP = "POP"
  show OP_POPF = "POPF"
  show OP_RET = "RET"
  show OP_IRET = "IRET"
  show OP_PUSH = "PUSH"
  show OP_PUSHF = "PUSHF"
  show OP_SWAP = "SWAP"
  show OP_DI = "DI"
  show OP_EI = "EI"
  show (OP_IN s) = "IN " ++ show s
  show (OP_OUT s) = "OUT " ++ show s
  show (OP_INT i) = "INT " ++ show i
  show (OP_AND a) = "AND " ++ show a
  show (OP_OR a) = "OR " ++ show a
  show (OP_ADD a) = "ADD " ++ show a
  show (OP_ADC a) = "ADC " ++ show a
  show (OP_SUB a) = "SUB " ++ show a
  show (OP_CMP a) = "CMP " ++ show a
  show (OP_LOOP a) = "LOOP " ++ show a
  show (OP_LD a) = "LD " ++ show a
  show (OP_SWAM a) = "SWAM " ++ show a
  show (OP_JUMP a) = "JUMP " ++ show a
  show (OP_CALL a) = "CALL " ++ show a
  show (OP_ST a) = "ST " ++ show a
  show (OP_BEQ l) = "BEQ " ++ l
  show (OP_BNE l) = "BNE " ++ l
  show (OP_BMI l) = "BMI " ++ l
  show (OP_BPL l) = "BPL " ++ l
  show (OP_BHIS l) = "BHIS " ++ l
  show (OP_BLO l) = "BLO " ++ l
  show (OP_BVS l) = "BVS " ++ l
  show (OP_BVC l) = "BVC " ++ l
  show (OP_BGE l) = "BGE " ++ l
  show (OP_BLT l) = "BLT " ++ l

showAsm :: BcompAsm -> String
showAsm = foldr f ""
  where
    f (OP_LABEL l) s = (l ++ ":\n") ++ s
    f op s = ("    " ++ show op ++ "\n") ++ s