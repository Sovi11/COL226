structure DataTypes = 
struct
datatype AST  = PROG of BLK 
and BLK = BLK of (DECSEQ) * ( CMD list ) 
and DECSEQ = DEXSEQ of VARDEC *(PROCDEF list)
and VARDEC = VARDEX of INTDEC * BOOLDEC * RATDEC
and PROCDEF = PROCDEX of string * BLK
and INTDEC = INTDEX of string list * string
and BOOLDEC = BOOLDEX of string list * string
and RATDEC = RATDEX of string list * string
and DEC = DEC of ( (string list) * string )
and CMD = SET of string*Exp | WH of Exp* (CMD list )| ITE of Exp * ( CMD list ) * (CMD list ) | Read of string | Print of string * Exp | Call of string
and Exp = NOT of Exp
          |  NEG of Exp
          | AND of Exp*Exp
          | OR of Exp*Exp
          | PLUS of Exp*Exp
          | MINUS of Exp*Exp
          | TIMES of Exp*Exp
          | DIV of Exp * Exp 
          | RATPLUS of Exp*Exp
          | RATMINUS of Exp*Exp
          | RATTIMES of Exp*Exp
          | RATDIV of Exp * Exp 
          | MOD of Exp * Exp 
          | LT of Exp*Exp
          | LEQ of Exp*Exp
          | NEQ of Exp*Exp
          | EQ of Exp*Exp
          | GT of Exp*Exp
          | GEQ of Exp * Exp 
          | INVERSE of Exp
          | MAKERAT of Exp * Exp 
          | RAT of Exp
          | TT
          | FF 
          | Posnumeral of string 
          | Identifier of string 
          | Ratnum of string
open TextIO;


exception VariableRedeclarationException of string ; 
exception TypeMisMatchException ; 
exception UnDeclaredVariableException ; 
exception AssignmentAndDeclarationDontMatch ;
exception CannotPrintProcedure ;
exception CannotCallNonProcedure ;
exception CannotPrintUnassignedVariable
exception BoolnahiHaimereBhai ;
exception IntnahiHaimereBhai ;
exception RatnahiHaimereBhai ;
exception WrongOperator ;
exception WrongInput ;


(* Open the file *)
val outstream = TextIO.openOut "output.txt"

fun printFile(write) = TextIO.output(outstream,write)
(* Close the file *)



fun hashinggg(a : string * int) =  (HashString.hashString(#1 a) + Word.fromInt(#2 a))
val curr = ref "main"
val counter = ref 0
val scope = ref ~1 
val mex = ref 0
val runningprocedure = ref "None"
fun maxI(a,b) = if (a>b) then a else b 

val liststackScopes : (string,int list ref) HashTable.hash_table = 
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "hehe3 identifier not found") ;
val parentTable : (string,string) HashTable.hash_table = 
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "hehe1 identifier not found") ; (* second element is the parent of first *)

val dynamicparentTable : ((string * int),(string * int)) HashTable.hash_table = 
    HashTable.mkTable (hashinggg, op=) (42, Fail "hehe2 identifier not found") ;


fun traverseList(kk: PROCDEF list) =
if null(kk) then () else 
let val PROCDEX(aa,bb) = hd kk in
(HashTable.insert parentTable (aa, !curr) ; (curr := (aa)) ; procedureTree((bb)) ; (curr := (HashTable.lookup parentTable (!curr))) ;traverseList(tl kk))
end
and procedureTree(b : BLK) =
let val BLK(x,y) = b 
val DEXSEQ(c,d) =  x
in
traverseList(d)
end



fun clean_list(l : char list,temp) = 
if (null(l)) then temp 
else if (((hd l)<> (#"\n")) andalso (hd(l)<> (#" "))) then clean_list(tl l, hd(l)::temp)
else clean_list(tl l , temp)

fun clean(s : string) = 
let val r = String.explode(s) in 
String.implode(rev(clean_list(r,[])))
end



val typeTable1 : (int, (string, string) HashTable.hash_table) HashTable.hash_table=
    HashTable.mkTable (Word.fromInt, op=) (42, Fail "1. identifier not found") ; 

val valTableInt : (int, (string, BigInt.bigint) HashTable.hash_table) HashTable.hash_table = 
    HashTable.mkTable (Word.fromInt, op=) (42, Fail "2. identifier not found") ; 

val valTableRat : (int, (string, Rational.rational) HashTable.hash_table) HashTable.hash_table = 
    HashTable.mkTable (Word.fromInt, op=) (42, Fail "3. identifier not found") ; 

val valTableBool : (int, (string, bool) HashTable.hash_table) HashTable.hash_table = 
    HashTable.mkTable (Word.fromInt, op=) (42, Fail "4. identifier not found") ; 
val procedureTable : (string, BLK) HashTable.hash_table = 
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "hehe identifier not found") ; 

fun findUntilfoundtemp(x : string,r : string) = 
if isSome(HashTable.find (HashTable.lookup typeTable1 (hd (!(HashTable.lookup liststackScopes r)))) (x)) then (hd (!(HashTable.lookup liststackScopes r)))
else if (r = "main") then raise UnDeclaredVariableException
else findUntilfoundtemp(x,(HashTable.lookup parentTable r))

fun findUntilfound(x : string) = 
findUntilfoundtemp(x, ! runningprocedure)

fun set_things(ss : string) = 
let 
val newtypeTable : (string, string) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "type identifier not found") ; 
val newvalTableInt : (string, BigInt.bigint) HashTable.hash_table = 
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "int identifier not found") ; 

val newvalTableRat : (string, Rational.rational) HashTable.hash_table = 
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "rational identifier not found") ; 

val newvalTableBool : (string, bool) HashTable.hash_table = 
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "boolean identifier not found") ; 
val _ = HashTable.insert dynamicparentTable ((ss, !mex), (!runningprocedure,!scope))
val _ = (scope := !(mex))
val b = ref []
val _ =  if not (isSome(HashTable.find liststackScopes ss)) then (HashTable.insert liststackScopes (ss,b) ) else ()
val t = (HashTable.lookup liststackScopes ss)
val _ = (t := !scope :: (! t))
val _ = ((HashTable.remove liststackScopes ss)  ; (HashTable.insert liststackScopes (ss,t)))
val _ = (mex := !(mex) + 1)
val _ = (runningprocedure := ss)
val _ = HashTable.insert typeTable1 (! scope,newtypeTable)
val _ = HashTable.insert valTableBool (! scope,newvalTableBool)
val _ = HashTable.insert valTableInt (! scope,newvalTableInt)
val _ = HashTable.insert valTableRat (! scope,newvalTableRat)
in 
()
end

fun end_things() = 
let 
val ss = (! runningprocedure)
val rr = HashTable.lookup dynamicparentTable (ss,!scope)
val t = (HashTable.lookup liststackScopes ss)
val _ = (t := tl (! t))
val _ = ((HashTable.remove liststackScopes ss)  ; (HashTable.insert liststackScopes (ss,t)))

val _ = (scope := #2 rr)
val _ = (runningprocedure := #1 rr)
in
()
end



fun getType ( id : string ) = 
         if( isSome (HashTable.find (HashTable.lookup typeTable1 (! scope))  id)) 
         then 
                HashTable.lookup (HashTable.lookup typeTable1 (! scope)) id 
         else 
                raise UnDeclaredVariableException ; 

fun getTyperandom ( id : string , r : int ) = 
         if( isSome (HashTable.find (HashTable.lookup typeTable1 (r))  id)) 
         then 
                HashTable.lookup (HashTable.lookup typeTable1 (r)) id 
         else 
                raise UnDeclaredVariableException ; 

fun getTypeExp(e : string * Exp) = 
if ((#1 e) <> "identifier") then (#1 e)
else
case (#2 e) of Identifier(x) => let val kis_scope_me_mila = findUntilfound(x)  
val jj = getTyperandom(x, kis_scope_me_mila)
in jj 
end 


fun insertInTable1( idList : string list , idType : string) = 
    if ( null idList ) then () else ( if isSome (HashTable.find (HashTable.lookup typeTable1 (! scope)) (hd idList)) then raise VariableRedeclarationException( hd idList) else 
            HashTable.insert (HashTable.lookup typeTable1 (! scope)) ( hd idList , idType ) ; insertInTable1( tl idList , idType) 
            ) ; 
fun insertInTableInt( id : string  , value : BigInt.bigint ) = 
if (not (isSome (HashTable.find (HashTable.lookup typeTable1 (! scope)) (id)))) then raise UnDeclaredVariableException else 
(if (getType(id)<>"bigint") then raise AssignmentAndDeclarationDontMatch 
else if (isSome (HashTable.find (HashTable.lookup valTableInt (! scope)) (id))) then  (HashTable.remove (HashTable.lookup valTableInt (! scope)) (id) ; HashTable.insert (HashTable.lookup valTableInt (! scope)) (id,value))
else  HashTable.insert (HashTable.lookup valTableInt (! scope)) ( id , value ))

fun insertInTableRat( id : string  , value : Rational.rational ) = 
if (not (isSome (HashTable.find (HashTable.lookup typeTable1 (! scope)) (id)))) then raise UnDeclaredVariableException else 
(if (getType(id)<>"rational") then raise AssignmentAndDeclarationDontMatch 
else if (isSome (HashTable.find (HashTable.lookup valTableRat (! scope)) (id))) then  (HashTable.remove (HashTable.lookup valTableRat (! scope)) id ; HashTable.insert (HashTable.lookup valTableRat (! scope)) ( id , value ) ) 
else  HashTable.insert (HashTable.lookup valTableRat (! scope)) ( id , value )) 

fun insertInTableBool( id : string  , value : bool ) = 
if (not (isSome (HashTable.find (HashTable.lookup typeTable1 (! scope)) (id)))) then raise UnDeclaredVariableException else 
(if (getType(id)<>"bool") then raise AssignmentAndDeclarationDontMatch 
else if (isSome (HashTable.find (HashTable.lookup valTableBool (! scope)) (id))) then  (HashTable.remove (HashTable.lookup valTableBool (! scope)) id ; HashTable.insert (HashTable.lookup valTableBool (! scope)) ( id , value ) ) 
else  HashTable.insert (HashTable.lookup valTableBool (! scope)) ( id , value ))

fun insertInTablePROC( id : string  ,  value : BLK ) = 
( HashTable.insert procedureTable ( id , value ) ) 

fun getvalueInt(id : string) = 
if (not (isSome (HashTable.find (HashTable.lookup typeTable1 (! scope)) (id)))) then raise UnDeclaredVariableException else 
(if (getType(id)<>"bigint") then raise TypeMisMatchException else (HashTable.lookup (HashTable.lookup valTableInt (! scope)) id))

fun getvalueRat(id : string) = 
if (not (isSome (HashTable.find (HashTable.lookup typeTable1 (! scope)) (id)))) then raise UnDeclaredVariableException else 
(if (getType(id)<>"rational") then raise TypeMisMatchException 
else if (not (isSome(HashTable.find (HashTable.lookup valTableRat (! scope)) id))) then raise CannotPrintUnassignedVariable else ( HashTable.lookup (HashTable.lookup valTableRat (! scope)) id  ))

fun getvalueBool(id : string) = 
if (not (isSome (HashTable.find (HashTable.lookup typeTable1 (! scope)) (id)))) then raise UnDeclaredVariableException else 
(if (getType(id)<>"bool") then raise TypeMisMatchException else (HashTable.lookup (HashTable.lookup valTableBool (! scope)) id))

fun getvalueProc(id : string) = 
if (not (isSome (HashTable.find (procedureTable) (id)))) then (raise VariableRedeclarationException(id)) else 
( (HashTable.lookup procedureTable id))


fun getTypeofExp(e : Exp) = 
case e of Posnumeral (s) => "bigint"
| Ratnum (s) => "rational"
| TT => "bool"
| FF => "bool"
| NOT( a1) => "bool"
| NEG( a1) =>getTypeofExp(a1)
| AND( a1,b1) => "bool"
| OR( a1,b1) => "bool"
| PLUS( a1,b1) => "bigint"
| MINUS( a1,b1) => "bigint"
| TIMES( a1,b1) => "bigint"
| DIV( a1,b1) => "bigint"
| RATPLUS( a1,b1) => "rational"
| RATMINUS( a1,b1) => "rational"
| RATTIMES( a1,b1) => "rational"
| RATDIV( a1,b1 )  => "rational"
| MOD( a1,b1) => "bigint"
| LT( a1,b1) => "bool"
| LEQ( a1,b1) => "bool"
| NEQ( a1,b1) => "bool"
| EQ( a1,b1) => "bool"
| GT( a1,b1) => "bool"
| GEQ( a1,b1) => "bool"
| INVERSE( a1) => "rational"
| MAKERAT( a1,b1) => "rational"
| RAT( a1) => "rational"
| Identifier(a1) => getTypeExp("identifier",Identifier(a1))

fun valuebataTameezseBool(x) = 
let val kis_scope_me_mila = findUntilfound(x)  
val jj = getTyperandom(x, kis_scope_me_mila)
in 
if (jj = "bool") then (HashTable.lookup (HashTable.lookup valTableBool kis_scope_me_mila) x) 
else (raise BoolnahiHaimereBhai)
end 

fun valuebataTameezseint(x) = 
let val kis_scope_me_mila = findUntilfound(x)  
val jj = getTyperandom(x, kis_scope_me_mila)

in 
if (jj = "bigint") then (HashTable.lookup (HashTable.lookup valTableInt kis_scope_me_mila) x )
else (raise  IntnahiHaimereBhai)
end 

fun valuebataTameezseRat(x) = 
let val kis_scope_me_mila = findUntilfound(x)  
val jj = getTyperandom(x, kis_scope_me_mila)
in 
if (jj = "rational") then (HashTable.lookup (HashTable.lookup valTableRat kis_scope_me_mila) x) 
else (raise RatnahiHaimereBhai)
end 

fun subtract_int(a : Exp,b : Exp) =
BigInt.sub_big_F(solveEXPRESSIONINT(a),solveEXPRESSIONINT(b))

and div_int(a : Exp,b : Exp) = 
valOf(BigInt.div_big_F(solveEXPRESSIONINT(a),solveEXPRESSIONINT(b)))

and mult_int(a : Exp,b : Exp) =
BigInt.mul_big_F(solveEXPRESSIONINT(a),solveEXPRESSIONINT(b))


and plus_int(a : Exp,b : Exp) =
BigInt.add_big_F(solveEXPRESSIONINT(a),solveEXPRESSIONINT(b))

and mod_int(a : Exp,b : Exp) =
valOf(BigInt.rem_big_F(solveEXPRESSIONINT(a),solveEXPRESSIONINT(b)))

and solveEXPRESSIONINT(e : Exp ) = 
case e of MINUS x => subtract_int(#1 x, #2 x) 
| PLUS x => plus_int(#1 x, #2 x)
| TIMES x => mult_int(#1 x, #2 x)
| DIV x => div_int(#1 x, #2 x)
| MOD x => mod_int(#1 x, #2 x)
| Posnumeral x => BigInt.read_big_F(x)
| NEG x => subtract_int(Posnumeral "0",x)
| Identifier x => (valuebataTameezseint x)

and solveEXPRESSIONBOOL(e : Exp) = 
case e of NOT x => not (solveEXPRESSIONBOOL(x))
| OR x => ((solveEXPRESSIONBOOL (#1 x)) orelse ((solveEXPRESSIONBOOL (#2 x))))
| AND x => ((solveEXPRESSIONBOOL (#1 x)) andalso ((solveEXPRESSIONBOOL (#2 x))))
| TT => true 
| FF => false
| LEQ (x1,x2) => (if getTypeofExp(x1) = "bool" orelse getTypeofExp(x2) = "bool" then raise WrongOperator
        else if getTypeofExp(x1) = "rational" andalso getTypeofExp(x2) <> "rational" then raise TypeMisMatchException
        else if getTypeofExp(x1) = "int" andalso getTypeofExp(x2) <> "int" then raise TypeMisMatchException
        else if getTypeofExp(x1) = "rational" andalso getTypeofExp(x2) = "rational" then (Rational.less(solveEXPRESSIONRAT(x1),solveEXPRESSIONRAT(x2)) orelse Rational.equal(solveEXPRESSIONRAT(x1),solveEXPRESSIONRAT(x2)))
        else BigInt.lte_big_F(((solveEXPRESSIONINT(x1)),(solveEXPRESSIONINT(x2)))))
| LT (x1,x2) => (if getTypeofExp(x1) = "bool" orelse getTypeofExp(x2) = "bool" then raise WrongOperator
        else if getTypeofExp(x1) = "rational" andalso getTypeofExp(x2) <> "rational" then raise TypeMisMatchException
        else if getTypeofExp(x1) = "int" andalso getTypeofExp(x2) <> "int" then raise TypeMisMatchException
        else if getTypeofExp(x1) = "rational" andalso getTypeofExp(x2) = "rational" then (Rational.less(solveEXPRESSIONRAT(x1),solveEXPRESSIONRAT(x2)))
        else BigInt.lt_big_F(((solveEXPRESSIONINT(x1)),(solveEXPRESSIONINT(x2)))))
| GT (x1,x2) => not( solveEXPRESSIONBOOL(LEQ(x1,x2)))
| GEQ (x1,x2) => not( solveEXPRESSIONBOOL(LT(x1,x2)))
| EQ (x1,x2) => (if getTypeofExp(x1) = "bool" andalso getTypeofExp(x2) <> "bool" then raise TypeMisMatchException
        else if getTypeofExp(x1) = "rational" andalso getTypeofExp(x2) <> "rational" then raise TypeMisMatchException
        else if getTypeofExp(x1) = "int" andalso getTypeofExp(x2) <> "int" then raise TypeMisMatchException
        else if getTypeofExp(x1) = "bool" andalso getTypeofExp(x2) = "bool" then (solveEXPRESSIONBOOL(x1) = solveEXPRESSIONBOOL(x2)) 
        else if getTypeofExp(x1) = "rational" andalso getTypeofExp(x2) = "rational" then Rational.equal(solveEXPRESSIONRAT(x1),solveEXPRESSIONRAT(x2))
        else BigInt.eq_big_F(((solveEXPRESSIONINT(x1)),(solveEXPRESSIONINT(x2)))))
| NEQ (x1,x2) => not (solveEXPRESSIONBOOL(EQ(x1,x2)))
| Identifier x => (valuebataTameezseBool x)

and solveEXPRESSIONRAT(e : Exp) = 
case e of Ratnum x => Rational.fromDecimal(x) 
| RATPLUS x => Rational.add(solveEXPRESSIONRAT(#1 x),solveEXPRESSIONRAT(#2 x))
| RATMINUS x => Rational.subtract(solveEXPRESSIONRAT(#1 x),solveEXPRESSIONRAT(#2 x))
| RATTIMES x => Rational.multiply(solveEXPRESSIONRAT(#1 x),solveEXPRESSIONRAT(#2 x))
| RATDIV x => valOf(Rational.divide(solveEXPRESSIONRAT(#1 x),solveEXPRESSIONRAT(#2 x)))
| NEG x => Rational.neg(solveEXPRESSIONRAT(x))
| MAKERAT x => valOf(Rational.make_rat(solveEXPRESSIONINT(#1 x),solveEXPRESSIONINT(#2 x)))
| RAT x => valOf( Rational.rat(solveEXPRESSIONINT(x)))
| INVERSE x => valOf(Rational.inverse(solveEXPRESSIONRAT(x)))
| Identifier x => valuebataTameezseRat x


fun SETkartameezse(x,y) =
let
val kis_scope_me_mila = findUntilfound(x)  
val jj = getTyperandom(x, kis_scope_me_mila) 
in 
if ((! scope) =  kis_scope_me_mila) then (
if (getType(x) = "bigint") then  insertInTableInt(x, solveEXPRESSIONINT (y)) else (if (getType( x) = "bool") then  insertInTableBool( x, solveEXPRESSIONBOOL (y)) else  insertInTableRat(x, solveEXPRESSIONRAT (y)))
)
else (
if (jj = "bigint") then (let val parth1= solveEXPRESSIONINT(y)  in  (if isSome(HashTable.find (HashTable.lookup valTableInt kis_scope_me_mila) x) then ((HashTable.remove (HashTable.lookup valTableInt kis_scope_me_mila) x ); (HashTable.insert (HashTable.lookup valTableInt kis_scope_me_mila) (x,parth1))) else (HashTable.insert (HashTable.lookup valTableInt kis_scope_me_mila) (x,parth1)) )end )
else if (jj = "bool") then (let val parth1= solveEXPRESSIONBOOL(y)  in (if isSome(HashTable.find (HashTable.lookup valTableBool kis_scope_me_mila) x) then ((HashTable.remove (HashTable.lookup valTableBool kis_scope_me_mila) x ); (HashTable.insert (HashTable.lookup valTableBool kis_scope_me_mila) (x,parth1))) else (HashTable.insert (HashTable.lookup valTableBool kis_scope_me_mila) (x,parth1)) )end )
else if (jj = "rational") then (let val parth1= solveEXPRESSIONRAT(y) in (if isSome(HashTable.find (HashTable.lookup valTableRat kis_scope_me_mila) x) then ((HashTable.remove (HashTable.lookup valTableRat kis_scope_me_mila) x ); (HashTable.insert (HashTable.lookup valTableRat kis_scope_me_mila) (x,parth1))) else (HashTable.insert (HashTable.lookup valTableRat kis_scope_me_mila) (x,parth1)) )end )
else ())
end

fun solveCMD_temp(c : CMD) = 
case c of SET (x,y) => SETkartameezse(x,y)
| ITE (a1,a2,a3) => if (solveEXPRESSIONBOOL(a1)) then solveCMD(a2) else solveCMD(a3)  (* Yahan bhi kuchch bhi likha hai *)
| WH(a1,a2) => if (solveEXPRESSIONBOOL(a1)) then (solveCMD(a2) ; solveCMD_temp(c)) else ()    (* yahan then vaali branch me do something with a2 and call while firse hona chahiye*)
(* | Print(x) => (if (getType(x) = "bigint") then print("value of "^ x^ " is "^BigInt.show_big(getvalueInt(x))^"\n") else (if (getType(x) = "rational") then print("value of "^ x^ " is "^Rational.toDecimal(getvalueRat(x))^"\n") else (if getvalueBool(x) then print("value of "^ x^ " is true\n") else print("value of "^ x^ " is false\n")))) *)
| Print(x) => if (getTypeExp(x) = "bigint") then printFile(BigInt.show_big(solveEXPRESSIONINT(#2 x))^"\n") 
              else if (getTypeExp(x) = "rational") then printFile(Rational.showRat(solveEXPRESSIONRAT(#2 x))^"\n") (* Here the printFile(Rational.showRat(solveEXPRESSIONRAT(#2 x))^"\n") can be replaced with printFile(Rational.showDecimal(solveEXPRESSIONRAT(#2 x))^"\n") to produce outputs in decimal normal form *)
              else if (getTypeExp(x) = "bool") then (if solveEXPRESSIONBOOL(#2 x) then printFile("true\n") else printFile("false\n"))
              else raise CannotPrintProcedure
| Call(x) => if ( not (isSome (HashTable.find (procedureTable) x))) then raise UnDeclaredVariableException else (set_things(x) ; breakBLK(getvalueProc x) ; end_things()) 
| Read(x) => (print("Enter the value of "^x^" : ") ;(let val input = TextIO.inputLine TextIO.stdIn ;
val line = case input of SOME s => clean(s) | NONE   => "" in 
if (getType (x) = "bigint") then insertInTableInt(x,BigInt.read_big_F((line))) 
else if  (getType (x) = "rational") then insertInTableRat(x,Rational.fromDecimal(line)) 
else if (getType (x) = "bool") then (if line = "tt" then insertInTableBool(x,true) else if line = "ff" then insertInTableBool(x,false) else raise WrongInput) else raise TypeMisMatchException end  ))

and solveCMD(cc : CMD list )= 
if null(cc) then () else (solveCMD_temp(hd cc) ; solveCMD(tl cc))

and ProcedureSmjh(a : PROCDEF) = 
case a of PROCDEX(t) =>  (insertInTablePROC(#1 t, #2 t))

and ProcedureSmjhlist(a: PROCDEF list ) = 
if null(a) then () else (ProcedureSmjh(hd a) ; ProcedureSmjhlist(tl a))

and NewIdentifiersInt(a : INTDEC)=
case a of INTDEX(b) => insertInTable1(#1 b, #2 b)

and NewIdentifiersBool(a : BOOLDEC)=
case a of BOOLDEX(b) => insertInTable1(#1 b, #2 b)

and NewIdentifiersRat(a : RATDEC)=
case a of RATDEX(b) => insertInTable1(#1 b, #2 b)

and NewIdentifiers3(a : INTDEC, b : BOOLDEC , c : RATDEC) =
(NewIdentifiersInt(a);NewIdentifiersBool(b);NewIdentifiersRat(c))

and NewIdentifiers2(d : VARDEC) = 
case d of VARDEX(a) => NewIdentifiers3(#1 a, #2 a, #3 a)

and NewIdentifiers1(a : DECSEQ) = 
case a of DEXSEQ(x) => ( NewIdentifiers2(#1 x)  ;  ProcedureSmjhlist(#2 x))

and breakBLK(b : BLK) = 
let 
val BLK (x1,x2) = b ; 
in 
(NewIdentifiers1(x1) ; solveCMD(x2))
end 

and breakAST(a : AST ) = 
let 
val PROG x = a ;
val _ =  procedureTree(x)
val _ = set_things("main")
val _ = breakBLK(x)
val _ = end_things()
val _ = TextIO.closeOut outstream
in 
()
end 
end ; 

