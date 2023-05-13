structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)

%%
%header (functor MyLexFun(structure Tokens: My_TOKENS));
%state C;
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
<INITIAL>\n       => ( pos:= (!pos) + 1; lex()); 
<INITIAL>{ws}+    => (lex());
<INITIAL> "fromDecimal" => (lex()) ;
<INITIAL>"var" => (Tokens.VAR(!pos , !pos )) ; 
<INITIAL>"tt" => ( Tokens.TRUE(!pos , !pos ) ) ;
<INITIAL>"ff" => (Tokens.FALSE(!pos , !pos )) ;  
<INITIAL>"("      => (Tokens.LPAREN(!pos , !pos ));
<INITIAL>")"      => (Tokens.RPAREN(!pos , !pos )) ; 
<INITIAL>"}"      => (Tokens.RBRACE(!pos , !pos )) ; 
<INITIAL>"{"      => (Tokens.LBRACE(!pos , !pos )) ; 
<INITIAL>"!"      => (Tokens.NOT(!pos , !pos )) ; 
<INITIAL>"&&"       => ( Tokens.AND(!pos , !pos )) ; 
<INITIAL>"||"       => (Tokens.OR(!pos , !pos )) ;
<INITIAL>";"       => (Tokens.SEMI(!pos , !pos )) ;
<INITIAL>","       => (Tokens.COMMA(!pos , !pos )) ;
<INITIAL>":="       => (Tokens.ASSIGN(!pos , !pos )) ;
<INITIAL>".+."       => (Tokens.RATPLUS(!pos , !pos )) ;
<INITIAL>".-."       => (Tokens.RATMINUS(!pos , !pos )) ;
<INITIAL>".*."       => (Tokens.RATTIMES(!pos , !pos )) ;
<INITIAL>"./."       => (Tokens.RATDIV(!pos , !pos )) ;
<INITIAL>"+"       => (Tokens.PLUS(!pos , !pos )) ;
<INITIAL>"-"       => (Tokens.MINUS(!pos , !pos )) ;
<INITIAL>"*"       => (Tokens.TIMES(!pos , !pos )) ;
<INITIAL>"/"       => (Tokens.DIV(!pos , !pos )) ;
<INITIAL>"%"       => (Tokens.MOD(!pos , !pos )) ;
<INITIAL>"read"       => (Tokens.READ(!pos , !pos )) ;
<INITIAL>"print"       => (Tokens.PRINT(!pos , !pos )) ;
<INITIAL>"procedure"       => (Tokens.PROCEDURE(!pos , !pos )) ;
<INITIAL>"call"       => (Tokens.CALL(!pos , !pos )) ;
<INITIAL>"if"       => (Tokens.IF(!pos , !pos )) ;
<INITIAL>"else" => (Tokens.ELSE(!pos , !pos )) ; 
<INITIAL>"fi"       => (Tokens.ENDIF(!pos , !pos )) ;
<INITIAL>"while"       => (Tokens.WHILE(!pos , !pos )) ;
<INITIAL>"od"       => (Tokens.ENDWH(!pos , !pos )) ;
<INITIAL>"then"       => (Tokens.THEN(!pos , !pos )) ;
<INITIAL>"do"       => (Tokens.DO(!pos , !pos )) ;
<INITIAL>"integer"       => (Tokens.INTE(!pos , !pos )) ;
<INITIAL>"boolean"       => (Tokens.BOOLE(!pos , !pos )) ;
<INITIAL>"rational"       => (Tokens.RATE(!pos , !pos )) ;
<INITIAL>"inverse" => (Tokens.INVERSE(!pos , !pos )) ;
<INITIAL>"make_rat" => (Tokens.MAKERAT(!pos , !pos )) ;
<INITIAL>"rat" => (Tokens.RAT(!pos , !pos )) ;
<INITIAL>"<="       => (Tokens.LEQ(!pos , !pos )) ;
<INITIAL>">="       => (Tokens.GEQ(!pos , !pos )) ;
<INITIAL>"<>"       => (Tokens.NEQ(!pos , !pos )) ;
<INITIAL>">"       => (Tokens.GT(!pos , !pos )) ;
<INITIAL>"="       => (Tokens.EQ(!pos , !pos )) ;
<INITIAL>"<"       => (Tokens.LT(!pos , !pos )) ;
<INITIAL>[~]?{digit}*"."{digit}*"("{digit}+")" => (Tokens.RATNUM ((yytext), !pos, !pos));
<INITIAL>([~]?)({digit}){digit}* => ( Tokens.POSNUMERAL( yytext , !pos , !pos ) ) ; 
<INITIAL>{alpha}({alpha} | {digit} | "_")* => ( Tokens.IDENTIFIER( yytext ,!pos , !pos )) ; 
<INITIAL>"~" => (Tokens.NEG(!pos , !pos)) ; 
<INITIAL>"(*" => (YYBEGIN C; lex());
<C>\n+		=> (pos := (!pos) + (String.size yytext); lex());
<C>[^()*\n]+	=> (lex());
<C>"(*"		=> (lex());
<C>"*)"		=> (YYBEGIN INITIAL; lex());
<C>[*()]	=> (lex());






