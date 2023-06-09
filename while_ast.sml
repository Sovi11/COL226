(* while_ast.sml *)


structure My=
    struct
    exception MyError;
    fun compile (fileName) =
    let
        val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn
        n => if TextIO.endOfStream inStream
            then ""
            else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
        (msg,line,col) =>
        print (fileName^"["^Int.toString line^":"
        ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = MyParser.parse
        (15,
        (MyParser.makeLexer grab ),
        printError,
        ()) 
        handle MyParser.ParseError => raise MyError ; 
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;

    in 
        tree 
    end

    fun compile2(filename)=
        let
            open DataTypes;
            val xi = compile(filename)
            val tt = breakAST(xi) ;
        in
            tt
        end
end


