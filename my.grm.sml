functor MyLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : My_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes ; 
fun lmaooo(x) = 1


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\052\000\002\000\051\000\003\000\050\000\006\000\049\000\
\\034\000\048\000\035\000\047\000\038\000\046\000\039\000\045\000\
\\041\000\044\000\049\000\043\000\050\000\042\000\000\000\
\\001\000\002\000\016\000\000\000\
\\001\000\002\000\021\000\000\000\
\\001\000\002\000\040\000\000\000\
\\001\000\002\000\059\000\000\000\
\\001\000\004\000\014\000\000\000\
\\001\000\005\000\037\000\000\000\
\\001\000\006\000\038\000\000\000\
\\001\000\006\000\039\000\000\000\
\\001\000\006\000\079\000\000\000\
\\001\000\007\000\086\000\021\000\076\000\022\000\075\000\023\000\074\000\
\\024\000\073\000\025\000\072\000\026\000\071\000\027\000\070\000\
\\028\000\069\000\029\000\068\000\030\000\067\000\031\000\066\000\
\\036\000\065\000\037\000\064\000\045\000\063\000\046\000\062\000\
\\047\000\061\000\048\000\060\000\000\000\
\\001\000\007\000\087\000\000\000\
\\001\000\007\000\107\000\021\000\076\000\022\000\075\000\023\000\074\000\
\\024\000\073\000\025\000\072\000\026\000\071\000\027\000\070\000\
\\028\000\069\000\029\000\068\000\030\000\067\000\031\000\066\000\
\\036\000\065\000\037\000\064\000\045\000\063\000\046\000\062\000\
\\047\000\061\000\048\000\060\000\000\000\
\\001\000\007\000\114\000\021\000\076\000\022\000\075\000\023\000\074\000\
\\024\000\073\000\025\000\072\000\026\000\071\000\027\000\070\000\
\\028\000\069\000\029\000\068\000\030\000\067\000\031\000\066\000\
\\036\000\065\000\037\000\064\000\045\000\063\000\046\000\062\000\
\\047\000\061\000\048\000\060\000\000\000\
\\001\000\008\000\000\000\000\000\
\\001\000\010\000\111\000\000\000\
\\001\000\011\000\084\000\021\000\076\000\022\000\075\000\023\000\074\000\
\\024\000\073\000\025\000\072\000\026\000\071\000\027\000\070\000\
\\028\000\069\000\029\000\068\000\030\000\067\000\031\000\066\000\
\\036\000\065\000\037\000\064\000\045\000\063\000\046\000\062\000\
\\047\000\061\000\048\000\060\000\000\000\
\\001\000\012\000\115\000\000\000\
\\001\000\014\000\077\000\021\000\076\000\022\000\075\000\023\000\074\000\
\\024\000\073\000\025\000\072\000\026\000\071\000\027\000\070\000\
\\028\000\069\000\029\000\068\000\030\000\067\000\031\000\066\000\
\\036\000\065\000\037\000\064\000\045\000\063\000\046\000\062\000\
\\047\000\061\000\048\000\060\000\000\000\
\\001\000\015\000\109\000\000\000\
\\001\000\016\000\020\000\000\000\
\\001\000\016\000\030\000\000\000\
\\001\000\016\000\033\000\000\000\
\\001\000\016\000\036\000\000\000\
\\001\000\016\000\056\000\000\000\
\\001\000\017\000\110\000\021\000\076\000\022\000\075\000\023\000\074\000\
\\024\000\073\000\025\000\072\000\026\000\071\000\027\000\070\000\
\\028\000\069\000\029\000\068\000\030\000\067\000\031\000\066\000\
\\036\000\065\000\037\000\064\000\045\000\063\000\046\000\062\000\
\\047\000\061\000\048\000\060\000\000\000\
\\001\000\042\000\054\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\032\000\012\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\018\000\009\000\000\000\
\\126\000\000\000\
\\127\000\019\000\018\000\000\000\
\\128\000\000\000\
\\129\000\020\000\007\000\000\000\
\\130\000\000\000\
\\131\000\002\000\029\000\009\000\028\000\013\000\027\000\033\000\026\000\
\\043\000\025\000\044\000\024\000\000\000\
\\132\000\000\000\
\\133\000\017\000\031\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\021\000\076\000\022\000\075\000\023\000\074\000\024\000\073\000\
\\025\000\072\000\026\000\071\000\027\000\070\000\028\000\069\000\
\\029\000\068\000\030\000\067\000\031\000\066\000\036\000\065\000\
\\037\000\064\000\045\000\063\000\046\000\062\000\047\000\061\000\
\\048\000\060\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\027\000\070\000\028\000\069\000\029\000\068\000\030\000\067\000\
\\031\000\066\000\045\000\063\000\046\000\062\000\047\000\061\000\
\\048\000\060\000\000\000\
\\142\000\027\000\070\000\028\000\069\000\029\000\068\000\030\000\067\000\
\\031\000\066\000\045\000\063\000\046\000\062\000\047\000\061\000\
\\048\000\060\000\000\000\
\\143\000\021\000\076\000\022\000\075\000\025\000\072\000\026\000\071\000\
\\027\000\070\000\028\000\069\000\029\000\068\000\030\000\067\000\
\\031\000\066\000\045\000\063\000\046\000\062\000\047\000\061\000\
\\048\000\060\000\000\000\
\\144\000\021\000\076\000\022\000\075\000\025\000\072\000\026\000\071\000\
\\027\000\070\000\028\000\069\000\029\000\068\000\030\000\067\000\
\\031\000\066\000\045\000\063\000\046\000\062\000\047\000\061\000\
\\048\000\060\000\000\000\
\\145\000\027\000\070\000\028\000\069\000\029\000\068\000\030\000\067\000\
\\031\000\066\000\045\000\063\000\046\000\062\000\047\000\061\000\
\\048\000\060\000\000\000\
\\146\000\027\000\070\000\028\000\069\000\029\000\068\000\030\000\067\000\
\\031\000\066\000\045\000\063\000\046\000\062\000\047\000\061\000\
\\048\000\060\000\000\000\
\\147\000\029\000\068\000\030\000\067\000\031\000\066\000\045\000\063\000\
\\046\000\062\000\047\000\061\000\048\000\060\000\000\000\
\\148\000\029\000\068\000\030\000\067\000\031\000\066\000\045\000\063\000\
\\046\000\062\000\047\000\061\000\048\000\060\000\000\000\
\\149\000\045\000\063\000\046\000\062\000\047\000\061\000\048\000\060\000\000\000\
\\150\000\045\000\063\000\046\000\062\000\047\000\061\000\048\000\060\000\000\000\
\\151\000\045\000\063\000\046\000\062\000\047\000\061\000\048\000\060\000\000\000\
\\152\000\046\000\062\000\048\000\060\000\000\000\
\\153\000\046\000\062\000\048\000\060\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\021\000\076\000\022\000\075\000\023\000\074\000\024\000\073\000\
\\025\000\072\000\026\000\071\000\027\000\070\000\028\000\069\000\
\\029\000\068\000\030\000\067\000\031\000\066\000\045\000\063\000\
\\046\000\062\000\047\000\061\000\048\000\060\000\000\000\
\\157\000\021\000\076\000\022\000\075\000\023\000\074\000\024\000\073\000\
\\025\000\072\000\026\000\071\000\027\000\070\000\028\000\069\000\
\\029\000\068\000\030\000\067\000\031\000\066\000\036\000\065\000\
\\045\000\063\000\046\000\062\000\047\000\061\000\048\000\060\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\"
val actionRowNumbers =
"\039\000\035\000\030\000\005\000\
\\027\000\001\000\037\000\001\000\
\\020\000\029\000\002\000\028\000\
\\041\000\021\000\043\000\033\000\
\\001\000\022\000\030\000\039\000\
\\023\000\006\000\007\000\008\000\
\\003\000\000\000\000\000\026\000\
\\038\000\001\000\024\000\034\000\
\\031\000\032\000\041\000\040\000\
\\000\000\004\000\050\000\018\000\
\\000\000\009\000\000\000\074\000\
\\073\000\000\000\000\000\000\000\
\\076\000\077\000\075\000\016\000\
\\000\000\044\000\036\000\042\000\
\\010\000\011\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\005\000\
\\071\000\000\000\072\000\078\000\
\\068\000\012\000\005\000\047\000\
\\046\000\045\000\064\000\062\000\
\\065\000\063\000\067\000\066\000\
\\061\000\060\000\059\000\058\000\
\\057\000\056\000\055\000\053\000\
\\054\000\051\000\052\000\019\000\
\\025\000\069\000\015\000\049\000\
\\000\000\005\000\013\000\017\000\
\\070\000\048\000\014\000"
val gotoT =
"\
\\001\000\114\000\002\000\004\000\005\000\003\000\006\000\002\000\
\\010\000\001\000\000\000\
\\007\000\006\000\000\000\
\\003\000\009\000\004\000\008\000\000\000\
\\008\000\011\000\000\000\
\\000\000\
\\014\000\013\000\000\000\
\\009\000\015\000\000\000\
\\014\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\021\000\013\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\030\000\000\000\
\\000\000\
\\003\000\032\000\004\000\008\000\000\000\
\\002\000\033\000\005\000\003\000\006\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\039\000\000\000\
\\015\000\051\000\000\000\
\\000\000\
\\000\000\
\\014\000\053\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\055\000\013\000\020\000\000\000\
\\000\000\
\\015\000\056\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\076\000\000\000\
\\000\000\
\\015\000\078\000\000\000\
\\000\000\
\\000\000\
\\015\000\079\000\000\000\
\\015\000\080\000\000\000\
\\015\000\081\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\083\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\086\000\000\000\
\\015\000\087\000\000\000\
\\015\000\088\000\000\000\
\\015\000\089\000\000\000\
\\015\000\090\000\000\000\
\\015\000\091\000\000\000\
\\015\000\092\000\000\000\
\\015\000\093\000\000\000\
\\015\000\094\000\000\000\
\\015\000\095\000\000\000\
\\015\000\096\000\000\000\
\\015\000\097\000\000\000\
\\015\000\098\000\000\000\
\\015\000\099\000\000\000\
\\015\000\100\000\000\000\
\\015\000\101\000\000\000\
\\015\000\102\000\000\000\
\\008\000\103\000\000\000\
\\000\000\
\\015\000\104\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\106\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\110\000\000\000\
\\008\000\111\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 115
val numrules = 52
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | RATNUM of unit ->  (string) | IDENTIFIER of unit ->  (string)
 | POSNUMERAL of unit ->  (string)
 | expression of unit ->  (string*Exp)
 | varlist of unit ->  (string list) | command of unit ->  (CMD)
 | dec of unit ->  (DEC) | commands of unit ->  (CMD list)
 | ratvardecls of unit ->  (RATDEC)
 | boolvardecls of unit ->  (BOOLDEC) | comseq of unit ->  (CMD list)
 | intvardecls of unit ->  (INTDEC) | vardecls of unit ->  (VARDEC)
 | decseq of unit ->  (DECSEQ) | procdef of unit ->  (PROCDEF)
 | procdecls of unit ->  (PROCDEF list) | blk of unit ->  (BLK)
 | start of unit ->  (AST)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 7) => true | _ => false
val showTerminal =
fn (T 0) => "POSNUMERAL"
  | (T 1) => "IDENTIFIER"
  | (T 2) => "RATNUM"
  | (T 3) => "LBRACE"
  | (T 4) => "RBRACE"
  | (T 5) => "LPAREN"
  | (T 6) => "RPAREN"
  | (T 7) => "EOF"
  | (T 8) => "IF"
  | (T 9) => "ELSE"
  | (T 10) => "THEN"
  | (T 11) => "ENDIF"
  | (T 12) => "WHILE"
  | (T 13) => "DO"
  | (T 14) => "ENDWH"
  | (T 15) => "SEMI"
  | (T 16) => "COMMA"
  | (T 17) => "INTE"
  | (T 18) => "BOOLE"
  | (T 19) => "RATE"
  | (T 20) => "LT"
  | (T 21) => "LEQ"
  | (T 22) => "NEQ"
  | (T 23) => "EQ"
  | (T 24) => "GT"
  | (T 25) => "GEQ"
  | (T 26) => "PLUS"
  | (T 27) => "MINUS"
  | (T 28) => "TIMES"
  | (T 29) => "DIV"
  | (T 30) => "MOD"
  | (T 31) => "PROCEDURE"
  | (T 32) => "CALL"
  | (T 33) => "INVERSE"
  | (T 34) => "NOT"
  | (T 35) => "AND"
  | (T 36) => "OR"
  | (T 37) => "TRUE"
  | (T 38) => "FALSE"
  | (T 39) => "VAR"
  | (T 40) => "NEG"
  | (T 41) => "ASSIGN"
  | (T 42) => "READ"
  | (T 43) => "PRINT"
  | (T 44) => "RATMINUS"
  | (T 45) => "RATDIV"
  | (T 46) => "RATPLUS"
  | (T 47) => "RATTIMES"
  | (T 48) => "MAKERAT"
  | (T 49) => "RAT"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 49) $$ (T 48) $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43)
 $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36)
 $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29)
 $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22)
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15)
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.blk blk1, blk1left, blk1right)) :: rest671)
) => let val  result = MlyValue.start (fn _ => let val  (blk as blk1)
 = blk1 ()
 in (  PROG( blk ))
end)
 in ( LrTable.NT 0, ( result, blk1left, blk1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.comseq comseq1, _, comseq1right)) :: ( _, ( 
MlyValue.decseq decseq1, decseq1left, _)) :: rest671)) => let val  
result = MlyValue.blk (fn _ => let val  (decseq as decseq1) = decseq1
 ()
 val  (comseq as comseq1) = comseq1 ()
 in (BLK(decseq , comseq ))
end)
 in ( LrTable.NT 1, ( result, decseq1left, comseq1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.procdecls procdecls1, _, procdecls1right))
 :: ( _, ( MlyValue.vardecls vardecls1, vardecls1left, _)) :: rest671)
) => let val  result = MlyValue.decseq (fn _ => let val  (vardecls as 
vardecls1) = vardecls1 ()
 val  (procdecls as procdecls1) = procdecls1 ()
 in (DEXSEQ(vardecls,procdecls))
end)
 in ( LrTable.NT 4, ( result, vardecls1left, procdecls1right), rest671
)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.procdecls (fn _ => (
[]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.procdecls procdecls1, _, procdecls1right))
 :: _ :: ( _, ( MlyValue.procdef procdef1, procdef1left, _)) :: 
rest671)) => let val  result = MlyValue.procdecls (fn _ => let val  (
procdef as procdef1) = procdef1 ()
 val  (procdecls as procdecls1) = procdecls1 ()
 in (procdef :: procdecls )
end)
 in ( LrTable.NT 2, ( result, procdef1left, procdecls1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.blk blk1, _, blk1right)) :: ( _, ( 
MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, PROCEDURE1left, _
)) :: rest671)) => let val  result = MlyValue.procdef (fn _ => let
 val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (blk as blk1) = blk1 ()
 in (PROCDEX(IDENTIFIER,blk))
end)
 in ( LrTable.NT 3, ( result, PROCEDURE1left, blk1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.boolvardecls boolvardecls1, _, 
boolvardecls1right)) :: ( _, ( MlyValue.intvardecls intvardecls1, _, _
)) :: ( _, ( MlyValue.ratvardecls ratvardecls1, ratvardecls1left, _))
 :: rest671)) => let val  result = MlyValue.vardecls (fn _ => let val 
 (ratvardecls as ratvardecls1) = ratvardecls1 ()
 val  (intvardecls as intvardecls1) = intvardecls1 ()
 val  (boolvardecls as boolvardecls1) = boolvardecls1 ()
 in (VARDEX(intvardecls,boolvardecls,ratvardecls))
end)
 in ( LrTable.NT 5, ( result, ratvardecls1left, boolvardecls1right), 
rest671)
end
|  ( 7, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.varlist 
varlist1, _, _)) :: ( _, ( _, INTE1left, _)) :: rest671)) => let val  
result = MlyValue.intvardecls (fn _ => let val  (varlist as varlist1)
 = varlist1 ()
 in ( INTDEX(varlist , "bigint"))
end)
 in ( LrTable.NT 6, ( result, INTE1left, SEMI1right), rest671)
end
|  ( 8, ( rest671)) => let val  result = MlyValue.intvardecls (fn _ =>
 (INTDEX([],"bigint")))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.varlist 
varlist1, _, _)) :: ( _, ( _, BOOLE1left, _)) :: rest671)) => let val 
 result = MlyValue.boolvardecls (fn _ => let val  (varlist as varlist1
) = varlist1 ()
 in (BOOLDEX(varlist , "bool"))
end)
 in ( LrTable.NT 8, ( result, BOOLE1left, SEMI1right), rest671)
end
|  ( 10, ( rest671)) => let val  result = MlyValue.boolvardecls (fn _
 => (BOOLDEX([],"bool")))
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 11, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.varlist 
varlist1, _, _)) :: ( _, ( _, RATE1left, _)) :: rest671)) => let val  
result = MlyValue.ratvardecls (fn _ => let val  (varlist as varlist1)
 = varlist1 ()
 in ( RATDEX(varlist , "rational"))
end)
 in ( LrTable.NT 9, ( result, RATE1left, SEMI1right), rest671)
end
|  ( 12, ( rest671)) => let val  result = MlyValue.ratvardecls (fn _
 => (RATDEX([],"rational")))
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 13, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.commands 
commands1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.comseq (fn _ => let val  (commands as 
commands1) = commands1 ()
 in ((commands))
end)
 in ( LrTable.NT 7, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 14, ( rest671)) => let val  result = MlyValue.commands (fn _ => (
([])))
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 15, ( ( _, ( MlyValue.commands commands1, _, commands1right)) ::
 _ :: ( _, ( MlyValue.command command1, command1left, _)) :: rest671))
 => let val  result = MlyValue.commands (fn _ => let val  (command as 
command1) = command1 ()
 val  (commands as commands1) = commands1 ()
 in ((command::commands))
end)
 in ( LrTable.NT 10, ( result, command1left, commands1right), rest671)

end
|  ( 16, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.varlist
 (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in (([IDENTIFIER]))
end)
 in ( LrTable.NT 13, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 17, ( ( _, ( MlyValue.varlist varlist1, _, varlist1right)) :: _
 :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)) :: 
rest671)) => let val  result = MlyValue.varlist (fn _ => let val  (
IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (varlist as varlist1) = varlist1 ()
 in ((IDENTIFIER::varlist))
end)
 in ( LrTable.NT 13, ( result, IDENTIFIER1left, varlist1right), 
rest671)
end
|  ( 18, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.IDENTIFIER 
IDENTIFIER1, _, _)) :: _ :: ( _, ( _, READ1left, _)) :: rest671)) =>
 let val  result = MlyValue.command (fn _ => let val  (IDENTIFIER as 
IDENTIFIER1) = IDENTIFIER1 ()
 in ((Read(IDENTIFIER)))
end)
 in ( LrTable.NT 12, ( result, READ1left, RPAREN1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) =>
 let val  result = MlyValue.command (fn _ => let val  (expression as 
expression1) = expression1 ()
 in ((Print(expression)))
end)
 in ( LrTable.NT 12, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)
) :: rest671)) => let val  result = MlyValue.command (fn _ => let val 
 (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (expression as expression1) = expression1 ()
 in ((  SET(IDENTIFIER ,#2 expression )))
end)
 in ( LrTable.NT 12, ( result, IDENTIFIER1left, expression1right), 
rest671)
end
|  ( 21, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.comseq 
comseq2, _, _)) :: _ :: ( _, ( MlyValue.comseq comseq1, _, _)) :: _ ::
 ( _, ( MlyValue.expression expression1, _, _)) :: ( _, ( _, IF1left,
 _)) :: rest671)) => let val  result = MlyValue.command (fn _ => let
 val  (expression as expression1) = expression1 ()
 val  comseq1 = comseq1 ()
 val  comseq2 = comseq2 ()
 in ( ( ITE( #2 expression , comseq1  , comseq2)))
end)
 in ( LrTable.NT 12, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 22, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.comseq 
comseq1, _, _)) :: _ :: ( _, ( MlyValue.expression expression1, _, _))
 :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (expression as expression1) = 
expression1 ()
 val  (comseq as comseq1) = comseq1 ()
 in ( (  WH( #2 expression , comseq )))
end)
 in ( LrTable.NT 12, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, IDENTIFIER1right
)) :: ( _, ( _, CALL1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = 
IDENTIFIER1 ()
 in ((Call(IDENTIFIER)))
end)
 in ( LrTable.NT 12, ( result, CALL1left, IDENTIFIER1right), rest671)

end
|  ( 24, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ( (  ("bool" , LEQ(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("bool" , LT(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("bool" , EQ(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 27, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("bool" , NEQ(#2 expression1 , #2 expression2)) ))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("bool" , GT(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("bool" , GEQ(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("bigint" , PLUS(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 31, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("bigint" , MINUS(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("bigint" , TIMES(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 33, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("bigint" , DIV(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 34, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("bigint" , MOD(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("rational" , RATPLUS(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 36, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("rational" , RATMINUS(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 37, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("rational" , RATTIMES(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 38, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("rational" , RATDIV(#2 expression1 , #2 expression2))))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 39, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( ("bool" , AND(#2 expression1 , #2 expression2))  ))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 40, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((("bool" , OR(#2 expression1 , #2 expression2)) ))
end)
 in ( LrTable.NT 14, ( result, expression1left, expression2right), 
rest671)
end
|  ( 41, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, INVERSE1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in (("rational", INVERSE(#2 expression)))
end)
 in ( LrTable.NT 14, ( result, INVERSE1left, expression1right), 
rest671)
end
|  ( 42, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => let val  (expression as 
expression1) = expression1 ()
 in (( expression))
end)
 in ( LrTable.NT 14, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 43, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression2, _, _)) :: _ :: ( _, ( MlyValue.expression expression1, _,
 _)) :: _ :: ( _, ( _, MAKERAT1left, _)) :: rest671)) => let val  
result = MlyValue.expression (fn _ => let val  expression1 = 
expression1 ()
 val  expression2 = expression2 ()
 in (("rational", MAKERAT(#2 expression1, #2 expression2)))
end)
 in ( LrTable.NT 14, ( result, MAKERAT1left, RPAREN1right), rest671)

end
|  ( 44, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, RAT1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in (("rational", RAT(#2 expression)))
end)
 in ( LrTable.NT 14, ( result, RAT1left, expression1right), rest671)

end
|  ( 45, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, NEG1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in ((("bigint" , NEG( #2 expression))))
end)
 in ( LrTable.NT 14, ( result, NEG1left, expression1right), rest671)

end
|  ( 46, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.expression (fn _ => (("bool" , TT )))
 in ( LrTable.NT 14, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 47, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => (("bool" , FF )))
 in ( LrTable.NT 14, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.POSNUMERAL POSNUMERAL1, POSNUMERAL1left, 
POSNUMERAL1right)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (POSNUMERAL as POSNUMERAL1) = 
POSNUMERAL1 ()
 in (( "bigint" , Posnumeral(POSNUMERAL) ))
end)
 in ( LrTable.NT 14, ( result, POSNUMERAL1left, POSNUMERAL1right), 
rest671)
end
|  ( 49, ( ( _, ( MlyValue.RATNUM RATNUM1, RATNUM1left, RATNUM1right))
 :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  (RATNUM as RATNUM1) = RATNUM1 ()
 in (("rational", Ratnum(RATNUM)))
end)
 in ( LrTable.NT 14, ( result, RATNUM1left, RATNUM1right), rest671)

end
|  ( 50, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = 
IDENTIFIER1 ()
 in (( "identifier", Identifier(IDENTIFIER) ))
end)
 in ( LrTable.NT 14, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 51, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in ( ("bool" , NOT(#2 expression)) )
end)
 in ( LrTable.NT 14, ( result, NOT1left, expression1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : My_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun POSNUMERAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.POSNUMERAL (fn () => i),p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.IDENTIFIER (fn () => i),p1,p2))
fun RATNUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.RATNUM (fn () => i),p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun INTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun RATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun PROCEDURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun INVERSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun RATMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun RATDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun RATPLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun RATTIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun MAKERAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
end
end
