fun int_to_char(a : int ) = 
    if a=0 then #"0" 
    else if a=1 then #"1"
    else if a=2 then #"2"
    else if a=3 then #"3"
    else if a=4 then #"4"
    else if a=5 then #"5"
    else if a=6 then #"6"
    else if a=7 then #"7"
    else if a=8 then #"8"
    else #"9"
 
fun char_to_int(a : char) = 
    if a= #"0" then 0 
    else if a= #"1" then 1 
    else if a= #"2" then 2 
    else if a= #"3" then 3 
    else if a= #"4" then 4 
    else if a= #"5" then 5 
    else if a= #"6" then 6 
    else if a= #"7" then 7 
    else if a= #"8" then 8 
    else 9


signature BIGINT = 
sig 
type bigint = bool * int list
val zero : bigint
val show_big : bigint -> string 
val gt_big_F : bigint * bigint -> bool
val eq_big_F : bigint *bigint -> bool 
val lt_big_F : bigint* bigint -> bool 
val lte_big_F : bigint * bigint -> bool 
val gte_big_F : bigint * bigint -> bool
val neg_big_F : bigint -> bigint
val add_big_F : bigint * bigint -> bigint
val sub_big_F : bigint * bigint -> bigint
val mul_big_F : bigint * bigint -> bigint
val div_big_F : bigint * bigint -> bigint option
val rem_big_F : bigint * bigint -> bigint option
val gcd_big_F : bigint * bigint -> bigint
val value_big_F : bigint -> int
val read_big_F : string -> bigint

end

structure BigInt : BIGINT = 
(* ulti list hogi with a boolean value*)
struct
type bigint = bool * int list 

val zero= (false , [0])  

fun rem_lead_0_ff(l : int list)=
if null l then (l) 
else if ((hd(l) = 0) andalso (null (tl(l)))) then (l)
else if hd(l) <> 0 then (l) 
else 
rem_lead_0_ff(tl(l))

fun rem_lead_0((b: bool,l : int list) : bigint) = 
if null(l) then  (false, [0])
else (b,rev(rem_lead_0_ff(rev(l))))

fun int_list_to_char_list(l: int list) = 
if null(l) then [] 
else (int_to_char(hd(l))::int_list_to_char_list(tl(l)))

fun rev_int_list_to_char_list(l:int list) = 
rev(int_list_to_char_list(l))

fun show_big((a:bool, b : int list) : bigint) = 
if a then 
implode(#"~" ::(rev_int_list_to_char_list(b)))
else 
implode((rev_int_list_to_char_list(b)))


fun greater_than((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint, preffered : bool)=
if b1 andalso (not b2) then false 
else if (not b1) andalso b2 then true 
else if (null(l1)) andalso null(l2) then preffered
else if b1 then 
    if null(l1) then true 
    else if null(l2) then false 
    else if hd(l1) > hd(l2) then greater_than((b1, tl(l1)) , (b2,tl(l2)),false) 
    else if hd(l1) < hd(l2) then greater_than((b1, tl(l1)) , (b2,tl(l2)),true) 
    else greater_than((b1, tl(l1)) , (b2,tl(l2)),preffered) 
else 
    if null(l2) then true 
    else if null(l1) then false 
    else if hd(l1) < hd(l2) then greater_than((b1, tl(l1)) , (b2,tl(l2)),false) 
    else if hd(l1) > hd(l2) then greater_than((b1, tl(l1)) , (b2,tl(l2)),true) 
    else greater_than((b1, tl(l1)) , (b2,tl(l2)),preffered)

fun gt_big((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint) = 
if hd(l1) = 0 andalso null(tl(l1)) andalso hd(l2)=0 andalso null(tl(l2)) then false 
else greater_than((b1,l1),(b2,l2),false)

fun gt_big_F((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint)=
gt_big(rem_lead_0((b1,l1)),rem_lead_0((b2,l2)))

fun equal_big((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint) =
if b1 <> b2 then false 
else if null(l1) andalso null(l2) then true 
else if null(l1) then false 
else if null(l2) then false 
else if hd(l1)<>hd(l2) then false 
else equal_big((b1,tl(l1)),(b2,tl(l2)))

fun eq_big((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint)= 
if hd(l1) = 0 andalso null(tl(l1)) andalso hd(l2)=0 andalso null(tl(l2)) then true 
else equal_big((b1,l1),(b2,l2))

fun eq_big_F((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint) = 
eq_big(rem_lead_0(b1,l1),rem_lead_0(b2,l2))

fun gte_big_F((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint)=
eq_big_F((b1,l1),(b2,l2)) orelse gt_big_F((b1,l1),(b2,l2))

fun lt_big_F((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint)=
(not (gte_big_F((b1,l1),(b2,l2))))

fun lte_big_F((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint)=
(not (gt_big_F((b1,l1),(b2,l2))))

fun neg_big_F((b1 : bool, l1 : int list))=
(not b1, l1)



fun list_addition(l1 : int list , l2 : int list , carry : int) =
    if null l1 andalso null l2 then carry::[]
    else 
    if null l1 andalso carry = 0 then l2 
    else 
    if null l2 andalso carry = 0 then l1 
    else 
    if null l1 then list_addition(l2 , [carry ] , 0 ) 
    else 
    if null l2 then list_addition(l1, [carry] , 0 )
    else 
    let val rr1 = (hd(l1))
    val rr2 = (hd(l2))
    val rr3 = (carry)
    val temp = rr1 + rr2 + rr3 
    val carry2 =(temp div 10) 
    val temp2 = (temp mod 10)
    in
    temp2::list_addition(tl(l1),tl(l2),carry2)
    end  


fun subtract_list( l1 : int list , l2 : int list , sub : int ) =
    if null l1 then sub::[]
    else 
    if null l2 andalso sub = 0 then l1
    else
    if null l2 then subtract_list(l1, [sub] , 0 )
    else
    let val tt1 = (hd(l1))
    val tt2 = (hd(l2)) 
    val tt3 = (sub)
    in
    if (tt1 >= tt2 + tt3) 
    then 
    let val temp1 = ((tt1 - tt2) -tt3)
    in 
    temp1 :: subtract_list(tl(l1),tl(l2), 0)
    end
    else
    let val temp2 = ((((tt1 + 10) - tt2) - tt3))
    in 
    temp2 :: subtract_list(tl(l1),tl(l2),1)
    end 
    end

fun add_big((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint) = 
    if (b1 andalso b2) orelse (not b1 andalso not b2) then (b1,list_addition(l1,l2,0))
    else if eq_big_F((true,l1),(true,l2)) then (false, [0])
    else if gt_big_F((false,l1),(false,l2)) then (b1,subtract_list(l1,l2,0))
    else (b2,subtract_list(l2,l1,0))

fun sub_big((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint) = 
    if (b1 andalso not b2) orelse (not b1 andalso b2) then (b1,list_addition(l1,l2,0))
    else if b1 then 
    if eq_big_F((true,l1),(true,l2)) then (false, [0])
    else if gt_big_F((false,l1),(false,l2)) then (true,subtract_list(l1,l2,0))
    else (false,subtract_list(l2,l1,0))
    else 
    if eq_big_F((true,l1),(true,l2)) then (false, [0])
    else if gt_big_F((false,l1),(false,l2)) then (false,subtract_list(l1,l2,0))
    else (true,subtract_list(l2,l1,0))

fun add_big_F((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint)=
    rem_lead_0(add_big((b1,l1),(b2,l2)))

fun sub_big_F((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint) = 
    rem_lead_0(sub_big((b1,l1),(b2,l2)))
fun digit_multiplication(l1 : int, l2 : int list ) =
    if l1 = 0 then [0] 
    else 
    list_addition(l2, digit_multiplication(l1-1, l2),0)  
fun list_multiply(l1 : int list, l2 : int list) = 
    if null(l1) then []
    else list_addition(digit_multiplication(hd(l1),l2),0::list_multiply(tl(l1),l2),0)

fun mul_big((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint)=
    ((b1 andalso not b2) orelse (not b1 andalso  b2),list_multiply(l1,l2))

fun mul_big_F((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint) =
    rem_lead_0(mul_big((b1,l1),(b2,l2)))

fun just_lower_or_equal(l1 : int list,l2 : int list,x : int list, y : int) =
if gt_big_F((false,x),(false,l1)) then y-1
else just_lower_or_equal(l1,l2,list_addition(x,l2,0),y+1)

fun jloe(l1,l2) =
just_lower_or_equal(l1,l2,[0],0)


fun size( l) = 
if null l then 0 
else 
1 + size(tl(l))

fun ind_ex(l1,i)=
if i=0 then hd(l1)
else ind_ex(tl(l1),i-1) 

fun division_list(l1 : int list, l2 : int list,rem : int list,i : int,quo : int list) = 
(*perform l1/l2*)
let
val rr = size(l1)
val rem1 = ind_ex(l1,i)::rem
val temp = jloe(rem1,l2)
val quo1 = temp :: quo
val rem2  = subtract_list(rem1,digit_multiplication(temp,l2),0)
val i1 = i+1 
in 
if (i1=rr) then quo1 
else division_list(l1,l2,rem2,i1,quo1)
end 

fun divi_list(l1 : int list , l2 : int list) = 
division_list(rev(l1),l2,[],0,[])

fun div_big_F((b1 : bool, l1 : int list) : bigint , (b2: bool, l2 : int list): bigint) =
    if eq_big_F((false, [0]),(b2,l2)) then NONE 
    else 
    SOME (rem_lead_0((b1 andalso not b2) orelse (not b1 andalso  b2),divi_list(l1,l2)))

fun rem_big_F(a1: bigint , a2: bigint)=
if eq_big_F((false, [0]),a2) then NONE 
else SOME (sub_big_F(a1,mul_big_F(valOf(div_big_F(a1,a2)),a2)))

fun gcd_big(a1: bigint , a2: bigint)=
if eq_big_F((false, [0]),a1) andalso eq_big_F((false, [0]),a2) then (false,[1])
else if eq_big_F((false, [0]),a1) then a2 
else if eq_big_F((false, [0]),a2) then a1 
else if gt_big_F(a1,a2) then gcd_big(valOf(rem_big_F(a1,a2)),a2)
else gcd_big(valOf(rem_big_F(a2,a1)),a1)

fun gcd_big_F(a1: bigint , a2: bigint)=
rem_lead_0(gcd_big(a1,a2))

fun value_big( l1 : int list) =
if null(l1) then 0
else 10*value_big(tl(l1))+hd(l1)

fun value_big_F((b1 : bool, l1: int list): bigint) =
if b1 then ~1 * value_big(l1)
else value_big(l1)

fun char_to_int_list_a(l : char list,temp : int list) = 
if null(l) then temp 
else char_to_int_list_a(tl(l),(char_to_int(hd(l))::temp))

fun char_to_int_list(l : char list) = 
char_to_int_list_a(l,[]) 

fun read_big_F(s : string) = 
let val l = String.explode(s)
in
if hd(l) = #"~" then (true , char_to_int_list(tl(l)) )
else (false, char_to_int_list(l) )
end 
end 

signature RATIONALL =
sig
type rational 
type bigint = BigInt.bigint
exception rat_error 
val make_rat: bigint * bigint -> rational option
val rat: bigint -> rational option
val reci: bigint -> rational option
val neg: rational -> rational
val inverse : rational -> rational option
val equal : rational * rational -> bool (* equality *)
val less : rational * rational -> bool (* less than *)
val add : rational * rational -> rational (* addition *)
val subtract : rational * rational -> rational (* subtraction *)
val multiply : rational * rational -> rational (* multiplication *)
val divide : rational * rational -> rational option (* division *)
val showRat : rational -> string
val fromDecimal : string -> rational
val toDecimal : rational -> string 
val showDecimal : rational -> string
end

functor RATIONAL(BigInt : BIGINT) : RATIONALL =
struct 
type bigint = BigInt.bigint
type rational = bigint * bigint
exception rat_error


fun make_rat_dummy(x,y) = 
    let val t = BigInt.gcd_big_F(x,y)
    in 
        (valOf(BigInt.div_big_F(x,t)) ,valOf(BigInt.div_big_F(y,t)))
    end 
fun make_rat((b1 : bool, l1 : int list) : BigInt.bigint , (b2: bool, l2 : int list): BigInt.bigint) = 
    if BigInt.eq_big_F((b2,l2),BigInt.zero) then NONE 
    else if (b1 andalso b2) orelse (not (b1) andalso not (b2)) then SOME (make_rat_dummy((false,l1),(false, l2)))
    else SOME ((true,#2(#1 (make_rat_dummy((false,l1),(false, l2))))),(false,#2(#2 (make_rat_dummy((false,l1),(false, l2))))))

fun rat(x) = make_rat(x,(false,[1]))

fun reci(x) = make_rat((false,[1]),x)

fun neg((b1 : bool, l1 : int list) : BigInt.bigint , (b2: bool, l2 : int list): BigInt.bigint) = 
valOf(make_rat((not b1,l1),(b2,l2)))

fun inverse((b1 : bool, l1 : int list) : BigInt.bigint , (b2: bool, l2 : int list): BigInt.bigint) = 
make_rat((b2,l2),(b1,l1))


fun equal(((b1 : bool, l1 : int list) : BigInt.bigint , (b2: bool, l2 : int list): BigInt.bigint) : rational ,((b3 : bool, l3 : int list) : BigInt.bigint , (b4: bool, l4 : int list): BigInt.bigint) : rational ) = 
BigInt.eq_big_F(BigInt.mul_big_F(((b1,l1),(b4,l4))),BigInt.mul_big_F((b2,l2),(b3,l3)))


fun less(((b1 : bool, l1 : int list) : BigInt.bigint , (b2: bool, l2 : int list): BigInt.bigint) : rational ,((b3 : bool, l3 : int list) : BigInt.bigint , (b4: bool, l4 : int list): BigInt.bigint) : rational) =
BigInt.lt_big_F(BigInt.mul_big_F(((b1,l1),(b4,l4))),BigInt.mul_big_F((b2,l2),(b3,l3)))


fun add(((b1 : bool, l1 : int list) : BigInt.bigint , (b2: bool, l2 : int list): BigInt.bigint) : rational ,((b3 : bool, l3 : int list) : BigInt.bigint , (b4: bool, l4 : int list): BigInt.bigint) : rational) = 
valOf(make_rat(BigInt.add_big_F(BigInt.mul_big_F(((b1,l1),(b4,l4))), BigInt.mul_big_F((b2,l2),(b3,l3))) ,BigInt.mul_big_F((b2,l2),(b4,l4))))


fun subtract(((b1 : bool, l1 : int list) : BigInt.bigint , (b2: bool, l2 : int list): BigInt.bigint) : rational ,((b3 : bool, l3 : int list) : BigInt.bigint , (b4: bool, l4 : int list): BigInt.bigint) : rational) =
valOf(make_rat(BigInt.sub_big_F(BigInt.mul_big_F(((b1,l1),(b4,l4))), BigInt.mul_big_F((b2,l2),(b3,l3))) ,BigInt.mul_big_F((b2,l2),(b4,l4))))

fun multiply((((b1 : bool, l1 : int list) : BigInt.bigint , (b2: bool, l2 : int list): BigInt.bigint) : rational ,((b3 : bool, l3 : int list) : BigInt.bigint , (b4: bool, l4 : int list): BigInt.bigint) : rational)) =
 valOf(make_rat((BigInt.mul_big_F(((b1,l1),(b3,l3)))),BigInt.mul_big_F((b2,l2),(b4,l4))))

fun divide((((b1 : bool, l1 : int list) : BigInt.bigint , (b2: bool, l2 : int list): BigInt.bigint) : rational ,((b3 : bool, l3 : int list) : BigInt.bigint , (b4: bool, l4 : int list): BigInt.bigint) : rational)) =
(make_rat((BigInt.mul_big_F(((b1,l1),(b4,l4)))),BigInt.mul_big_F((b2,l2),(b3,l3))))

fun showRat(((b1 : bool, l1 : int list) : BigInt.bigint , (b2: bool, l2 : int list): BigInt.bigint) : rational) = 
BigInt.show_big((b1,l1)) ^ "/" ^ BigInt.show_big((b2,l2))

fun ind_ex(l1,i)=
if i=0 then hd(l1)
else ind_ex(tl(l1),i-1) 

fun size( l) = 
if null l then 0 
else 
1 + size(tl(l))

fun pattern_checking1(k : char list ,lparen1 : bool , decipoint : bool, rparen1 : bool,i) = 
if null(k) andalso (not decipoint orelse not lparen1 orelse not rparen1) then false
else if null(k) then true
else if hd(k) = #"+" andalso i<>0 then raise rat_error
else if hd(k) = #"~" andalso i<>0 then raise rat_error
else if hd(k) = #")" andalso (rparen1=true orelse lparen1=false) then raise rat_error
else if hd(k) = #"(" andalso (lparen1=true orelse rparen1= true) then raise rat_error
else if hd(k) = #"." andalso (lparen1=true orelse rparen1 = true orelse decipoint=true) then raise rat_error
else if hd(k) = #"+" then pattern_checking1(tl(k)  ,lparen1 , decipoint, rparen1,i+1)
else if hd(k) = #"~" then pattern_checking1(tl(k)  ,lparen1 , decipoint, rparen1,i+1)
else if hd(k) = #")" then pattern_checking1(tl(k)  ,lparen1 , decipoint, true,i+1)
else if hd(k) = #"(" then pattern_checking1(tl(k)  ,true , decipoint, rparen1,i+1)
else if hd(k) = #"." then pattern_checking1(tl(k)  ,lparen1 , true, rparen1,i+1)
else if not ((ord(hd(k)) -48) < 10  andalso ((ord(hd(k)) -48) >= 0)) then raise rat_error
else pattern_checking1(tl(k)  ,lparen1 , decipoint, rparen1,i+1)

fun fromDecimal_aux(k : char list ,positiv : bool,lparen1 : bool , decipoint : bool, inti : int list, NR : int list, R : int list ) =
if null(k) then (positiv : bool ,inti : int list , NR : int list ,R : int list )
else if hd(k) = #"+" then fromDecimal_aux(tl(k),true, lparen1,decipoint,inti,NR,R) 
else if hd(k)= #"~" then fromDecimal_aux(tl(k),false, lparen1,decipoint,inti,NR,R) 
else if hd(k) = #"." then fromDecimal_aux(tl(k),positiv, lparen1,true,inti,NR,R) 
else if decipoint = false then fromDecimal_aux(tl(k),positiv,lparen1,decipoint,char_to_int(hd(k))::inti, NR, R)
else if hd(k) = #"(" then fromDecimal_aux(tl(k),positiv,true,decipoint,inti, NR, R)
else if lparen1 = false then fromDecimal_aux(tl(k),positiv,lparen1,decipoint,inti, char_to_int(hd(k))::NR, R)
else if hd(k) = #")" then (positiv,inti, NR,R)
else fromDecimal_aux(tl(k),positiv,lparen1,decipoint,inti, NR, char_to_int(hd(k))::R)

fun fromDecimal_aux1(k : char list ) =
if not(pattern_checking1(k ,false,false,false,0)) then raise rat_error
else fromDecimal_aux(k,true,false,false,[],[],[])

fun fromDecimal_auxi(s : string) = 
let 
val k = explode(s) 
in 
fromDecimal_aux1(k)
end 

fun repeated_mult(x: BigInt.bigint, y : BigInt.bigint, i :int) =
if i=0 then x 
else repeated_mult(BigInt.mul_big_F(x,y),y,i-1)

fun fromDecimal(s : string) = 
let 
val (A,B,C,D) = fromDecimal_auxi(s)
val s1 = size(B)
val s2 = size(C)
val s3 = size(D)
val bi1 = (false,B) : BigInt.bigint 
val bi2 = (false,C) : BigInt.bigint 
val bi3 = (false,D) : BigInt.bigint
val bi11 = repeated_mult(bi1,(false,[0,1]),s2)
val bi12 = BigInt.add_big_F(bi11,(false,C))
val bi21 = repeated_mult(bi12,(false,[0,1]),s3)
val bi22 = BigInt.add_big_F(bi21,(false,D))
val qq1 = repeated_mult((false,[1]),(false,[0,1]),s3+s2)
val qq2 = repeated_mult((false,[1]),(false,[0,1]),s2)
val q= BigInt.sub_big_F(qq1,qq2)
in 
if A then valOf(make_rat(BigInt.sub_big_F(bi22,bi12),q))
else valOf(make_rat(BigInt.mul_big_F(BigInt.sub_big_F(bi22,bi12),(true,[1])),q))
end 

fun create_list(l1, sz, vali) =
if sz = 0 then l1 
else create_list(vali::l1,sz-1,vali) 

fun change_list(l1,i,new_val,l2) = 
if null(l1) then l2 
else if i=0 then change_list(tl(l1),i-1,new_val,new_val::l2)
else  change_list(tl(l1),i-1,new_val,hd(l1)::l2)

fun change_list_F(l1,i,new_val) = 
rev(change_list(l1,i,new_val,[]))


fun marking_remainders_aux(p: int, q: int,r : int list,i : int) = 
if ind_ex(r,p) = ~1 then marking_remainders_aux((10*p) mod q, q,change_list_F(r,p,i), i+1)
else (ind_ex(r,p),i)

fun marking_remainders_aux1(p: int, q: int)=
marking_remainders_aux(p,q,create_list([],q,~1),0)

fun marking_remainders((p : BigInt.bigint, q:BigInt.bigint): rational)=
let 
val (a,b) = p 
val (c,d) = q 
val t1 = BigInt.value_big_F((false,b))
val t2 = BigInt.value_big_F((false,d))
in 
marking_remainders_aux1(t1,t2)
end 

fun toDecimal_aux1(p: int, q: int, N : int list, R : int list,i: int, a:int, b: int)=
if i = b then (N,R)
else if i<a then toDecimal_aux1(10*p mod q, q ,  (10*p div q) ::N, R,i+1,a,b)
else toDecimal_aux1(10*p mod q, q ,  N, (10*p div q) :: R,i+1,a,b) 


fun toDecimal_aux2(p: int, q: int) =
let 
val (a,b) = marking_remainders_aux1(p,q)
in
toDecimal_aux1(p,q,[],[],0,a,b)
end 

fun toDecimal_aux3((p:BigInt.bigint, q:BigInt.bigint) : rational)=
let 
val t1 = BigInt.value_big_F(p)
val t2 = BigInt.value_big_F(q)
in 
toDecimal_aux2(t1,t2)
end 

fun toDecimal((p:BigInt.bigint, q:BigInt.bigint) : rational) =
let 
val (a,b) = p 
val (c,d) = q 
val tt1 = (false,b) : BigInt.bigint
val tt2 = (false,d) : BigInt.bigint
val k1= valOf(BigInt.div_big_F(tt1,tt2))
val s1 = BigInt.show_big(k1)
val k2 = valOf(BigInt.rem_big_F(tt1,tt2))
val (e,f) = toDecimal_aux3(k2,q)
val s2 = BigInt.show_big((false,e))
val s3 = BigInt.show_big((false,f))
in 
if (a andalso c) orelse (not a andalso not c) then (s1 ^ "." ^ s2^ "(" ^ s3 ^ ")")
else ("~" ^ s1 ^ "." ^ s2^ "(" ^ s3 ^ ")")

end


fun showDecimal((p:BigInt.bigint,q:BigInt.bigint) : rational)= 
toDecimal(p,q)


end 
structure Rational = RATIONAL(BigInt)

(* Error dena hai 
sabko make rat banado
+ vagerah lagana hai ya nahi 
rat error 
accessor methods 
*)

(* Test cases *)
(* Rational.equal(((false,[0]),(false,[1])),((true,[0]),(false,[1]))) ; *)
