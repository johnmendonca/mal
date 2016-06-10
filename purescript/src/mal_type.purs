module MalType where

data MalType = MalList    (Array MalType)
             | MalSymbol  String
             | MalInt     Int
             | MalString  String
             | MalNil

