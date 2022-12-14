module Hw1 where
nonTerms = ["assign", "comp", "comp'", "cond", "expr", "expr'", "expr''", "factor", "prog", "stmt", "stmt_list", "stmt_list'", "term", "term'", "term''"]
terms = ["$", "'!'", "'('", "')'", "'*'", "'+'", "'-'", "'/'", "';'", "'<='", "'=='", "'>='", "'if'", "'{'", "'}'", "ID", "INT"]
table = [["","","","","","","","","","","","e","","",""], -- $
  ["","'!' factor comp'","","","term expr''","","","'!' factor","","","","","factor term''","",""],
  ["","'(' expr ')' comp'","","","term expr''","","","'(' expr ')'","","","","","factor term''","",""],
  ["","","","","","","e","","","","","","","","e"], -- )
  ["","","","","","","","","","","","","","'*' factor","term' term''"], -- *
  ["","","","","","'+' term","expr' expr''","","","","","","","","e"], -- +
  ["","","","","","'-' term","expr' expr''","","","","","","","","e"], -- -
  ["","","","","","","","","","","","","","'/' factor","term' term''"],
  ["","","","","","","e","","","","","","","","e"], -- ;
  ["","","'<=' factor","","","","","","","","","","","",""],
  ["","","'==' factor","","","","","","","","","","","",""],
  ["","","'>=' factor","","","","","","","","","","","",""],
  ["","","","'if' comp '{' stmt_list '}' 'else' '{' stmt_list '}'","","","","","stmt_list","cond","stmt stmt_list'","stmt stmt_list'","","",""],
  ["","","","","","","","","","","","","","",""], -- {
  ["","","","","","","","","","","","e","","",""],
  ["ID '=' expr ';'","ID comp'","","","term expr''","","","ID","stmt_list","assign","stmt stmt_list'","ID '=' expr ';' stmt_list'","factor term''","",""],
  ["","INT comp'","","","term expr''","","","INT","","","","","factor term''","",""]]

nonTermToPos :: String -> Int -> [String] -> Int
nonTermToPos n 15 _ = -1
nonTermToPos n m (x:nonTerminals)
  | n == x = m
  | otherwise = nonTermToPos n (m+1) nonTerminals
doNTermPos :: String -> Int
doNTermPos n = nonTermToPos n 0 nonTerms

termToPos :: String -> Int -> [String] -> Int
termToPos n 17 _ = -1
termToPos n m (x:terms)
  | n == x = m
  | otherwise = termToPos n (m+1) terms
doTermPos :: String -> Int
doTermPos n = termToPos n 0 terms

getRule:: String -> String -> String
getRule acc i
 | ((doNTermPos acc /= -1) && (doTermPos i /= -1)) = (table !! (doTermPos i)) !! (doNTermPos acc)
 | otherwise ="\1"
getListedRule :: String -> String -> [String]
getListedRule acc i =  words (getRule acc i)

accum = "prog":["$"]

analyze :: String -> String
analyze input = analyze' ((words (input))++["$"]) accum

analyze' :: [String] -> [String] -> String
analyze' ["$"] ["$"] =  "TRUE"
analyze' _ ["$"] = "FALSE"
analyze' (i:input) (a:accum)
  | i == a =  analyze' input accum
  | a == "\1" =  "FALSE"
  | a =="e" =  analyze' (i:input) accum
  | otherwise =   analyze' (i:input) ((getListedRule a i)++accum)

anal1 = analyze "ID '=' ID ';'"
anal2 = analyze "ID '=' INT ';'"
