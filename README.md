README

How to run the code :
in the terminal type 'sudo sml a.sml' ; this commands takes in input from the file "prog.txt" prompts user for input from terminal (if any), runs the code

INPUT OUPUT SETUP : 
The input is taken from prog.txt and read input is taken from the terminal and the output is stored

Instructions for the programmer /Summary :
The language is statically typed with standard rules. Raises sufficient relevant errors. Comments using "(*" and its closing equivalent
all comments close by one closing comment bracket
The errors are not very descriptive, so try writing good errors.
Also use sufficiently many brackets.
output in the form of rational normal (well suited for fast I/O)


Design decision :
1. No UNARY plus
2. Output in Rational normal form 
3. MOD for negative numbers has definition mentioned : a/b has the magnitude of |a|/|b| where '/' is integer division and the remainder is calculated by euclid divison lemma

Errors : Some errors which are raised are Type Mismatch in case of some operators,Undeclared variable exception , Cannot print procedure , cannot call regular variable , Identifier not found in case of print or LHS use of an identifier before assigning any value. 

Acknowledgements :
1. ml lex and ml yacc documentation
2. I have taken some code and Ideas from Chinmay Mittal's Github (Assignment 3).The AST part of my assignment in inspired by "https://github.com/ChinmayMittal/COL226/tree/main/Assignment%203"
3. The lex part of my assignment is inspired by the example PASCAL language ml-lex (source: downloaded along with the sml nj program)The Evaluations part is original. 


REGULAR EXPRESSION FOR TOKENS : 
Tokens.
Reserved words. rational, integer, boolean, tt, ff, var, if, then, else, fi, while, do, od procedure, print,
read, call.
For convenience some other operations/functions which use letters are also regarded as reserved. See
below.
Rational Operators.
Unary. ˜, +, inverse (reserved)
Binary. .+., .-., .*., ./. (rational division)
Unary Conversions. make rat, rat, showRat, showDecimal, fromDecimal, toDecimal are all also
reserved words.
Integer Operators.
Unary. ˜, +
Binary. +, -, *, / (div) , % (mod)
Boolean Operators.
Unary. ! (boolean negation)
Binary. && (andalso) , ∥ (orelse)
Relational Operators. =, <>, <, <=, >, >=
Assignment. :=
Brackets. (, ), {, }
Punctuation. ;, ,
Identifiers. (A-Za-z)(A-Za-z0-9)*
Comment. Any string of printable ASCII characters enclosed in (*, *)

GRAMMAR : 
Program ::= Block .
Block ::= DeclarationSeq CommandSeq .
DeclarationSeq ::= [V arDecls] [P rocDecls] .
VarDecls ::= [RatV arDecls] [IntV arDecls] [BoolV arDecls] .
RatV arDecls ::= rational Ident {, Ident}; .
IntV arDecls ::= integer Ident {, Ident}; .
BoolV arDecls ::= boolean Ident {, Ident}; .
ProcDecls ::= ProcDef ; ProcDecls .
ProcDef ::= procedure Ident Block .
CommandSeq ::= {{Command;}} .
Command ::= AssignmentCmd | CallCmd | ReadCmd | PrintCmd |
ConditionalCmd | W hileCmd .
AssignmentCmd ::= Ident := Expression .
CallCmd ::= call Ident .
ReadCmd ::= read( Ident ) .
PrintCmd ::= print( Expression ) .
Expression ::= RatExpression | IntExpression | BoolExpression .
ConditionalCmd ::= if BoolExpression then CommandSeq else CommandSeq fi .
WhileCmd ::= while BoolExpression do CommandSeq od .


The regular expression has standard symbols |, * etc.
And in the grammar {} means kleene closure of a token and [] refers optional of token ness