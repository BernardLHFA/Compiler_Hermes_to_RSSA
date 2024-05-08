(* Compiler *)

structure Compiler :> Compiler =
struct

  exception Error of string*(int*int)

  fun printVar [] = "]\n"
    | printVar ((t, a, n, b) :: rest) =
        a^(Int.toString n)^", "^printVar rest

  (* store new variable name into variable list *)
  fun store_Variable x t var b =
        var@[(t, x, 0, b)]

  (* Increment the id of a variable *)
  fun incr_Variable [] x = []
    | incr_Variable ((t, a, n, b) :: res) x =
        if x = a
        then [(t, a, (n+1), b)]@res
        else [(t, a, n, b)]@(incr_Variable res x)
  
  fun incr_Arguments [] = []
    | incr_Arguments ((t, a, n, b) :: res) =
        [(t, a, (n+1), b)]@(incr_Arguments res)

  (* Get a variable from the list or store a new one if not present *)
  fun get_Variable [] x t ovar isArray = 
        let
          val var2 = (store_Variable x t ovar isArray)
        in
          (x^"0", var2)
        end
    | get_Variable ((tr, a, n, b) :: rest) x t ovar isArray =
        if x = a
        then (a^(Int.toString n), ovar)
        else get_Variable rest x t ovar isArray
  
  fun is_Variable_In x [] = false
    | is_Variable_In x ((t, a, n, b) :: res) =
        if x = a
        then true
        else is_Variable_In x res

  fun get_Arguments [] p unused = []
    | get_Arguments ((t, a, n, b) :: res) p unused =
        case a of
          "T" => (get_Arguments res p unused)
        | "I" => (get_Arguments res p unused)
        | _ =>
            if is_Variable_In a unused
            then (get_Arguments res p unused)
            else
              if b 
              then [Data.ArgS((Data.TypeS(Data.Public, Data.u64)), Data.VarS(a^(Int.toString n), p))]@(get_Arguments res p unused)
              else [Data.ArgS(t, Data.VarS(a^(Int.toString n), p))]@(get_Arguments res p unused)
  
  fun get_Arguments_Variable [] unused = []
    | get_Arguments_Variable ((t, a, n, b) :: res) unused =
        if is_Variable_In a unused
        then (get_Arguments_Variable res unused)
        else [(t, a, n, b)]@(get_Arguments_Variable res unused)
  
  (* Get the type associated with a variable *)
  fun get_Type x [] p = raise Error ("Variable " ^ x ^ " not found", p)
    | get_Type x ((t, a, n, b) :: res) p =
        if x = a
        then t
        else get_Type x res p

  fun get_unused var1 [] = []
    | get_unused var1 ((t, a, n, b) :: res) =
        if is_Variable_In a var1
        then get_unused var1 res
        else [(t, a, n, b)]@(get_unused var1 res)
  
  (* Compile a variable either as a constant or a variable *)
  fun comp_Var x p =
        case String.sub(x, 0) of
          #"0" => Data.CstS(x, p)
        | #"1" => Data.CstS(x, p)
        | #"2" => Data.CstS(x, p)
        | #"3" => Data.CstS(x, p)
        | #"4" => Data.CstS(x, p)
        | #"5" => Data.CstS(x, p)
        | #"6" => Data.CstS(x, p)
        | #"7" => Data.CstS(x, p)
        | #"8" => Data.CstS(x, p)
        | #"9" => Data.CstS(x, p)
        | _ => Data.VarS(x, p)

  (* Compile type *)
  fun comp_Type (v, i) =
        case (v, i) of
          (Hermes.Secret, Hermes.U8) => Data.TypeS(Data.Secret, Data.u8)
        | (Hermes.Secret, Hermes.U16) => Data.TypeS(Data.Secret, Data.u16)
        | (Hermes.Secret, Hermes.U32) => Data.TypeS(Data.Secret, Data.u32)
        | (Hermes.Secret, Hermes.U64) => Data.TypeS(Data.Secret, Data.u64)
        | (Hermes.Public, Hermes.U8) => Data.TypeS(Data.Public, Data.u8)
        | (Hermes.Public, Hermes.U16) => Data.TypeS(Data.Public, Data.u16)
        | (Hermes.Public, Hermes.U32) => Data.TypeS(Data.Public, Data.u32)
        | (Hermes.Public, Hermes.U64) => Data.TypeS(Data.Public, Data.u64)
  
  (* Get the size of a type *)
  fun get_Size (Data.TypeS(_, s)) =
        case s of
          (Data.u8) => 8
        | (Data.u16) => 16
        | (Data.u32) => 32
        | (Data.u64) => 64
  
  (* Compile Update Operations *)
  fun comp_Upop Hermes.Add = "+"
    | comp_Upop Hermes.Sub = "-"
    | comp_Upop Hermes.XorWith = "^"
    | comp_Upop Hermes.RoL = "<<"
    | comp_Upop Hermes.RoR = ">>"

  (* Compile Binary Operations *)
  fun comp_BinOp Hermes.Plus = "+"
    | comp_BinOp Hermes.Minus = "-"
    | comp_BinOp Hermes.Times = "*"
    | comp_BinOp Hermes.Divide = "div"
    | comp_BinOp Hermes.Modulo = "mod"
    | comp_BinOp Hermes.Xor = "^"
    | comp_BinOp Hermes.BAnd= "&"
    | comp_BinOp Hermes.BOr = "|"
    | comp_BinOp Hermes.ShiftL = "<<"
    | comp_BinOp Hermes.ShiftR = ">>"
    | comp_BinOp Hermes.Equal = "=="
    | comp_BinOp Hermes.Less = "<"
    | comp_BinOp Hermes.Greater = ">"
    | comp_BinOp Hermes.Neq = "!="
    | comp_BinOp Hermes.Leq = "<="
    | comp_BinOp Hermes.Geq = ">="
  
  fun check_Comp Hermes.Equal = true
    | check_Comp Hermes.Less = true
    | check_Comp Hermes.Greater = true
    | check_Comp Hermes.Neq = true
    | check_Comp Hermes.Leq = true
    | check_Comp Hermes.Geq = true
    | check_Comp bop = false

  fun comp_Decl [] var = ([], var)
    | comp_Decl (Hermes.ConstDecl(y, i, p) :: res) var =
        let
          val (y0, var2) = get_Variable var y (Data.TypeS(Data.Public, Data.u64)) var false
          val start = [Data.Assign2S((Data.TypeS(Data.Public, Data.u64), Data.VarS(y0, p), Data.CstS(i, p), p))]
          val (finish, var3) = comp_Decl res var2
        in
          (start@finish, var3)
        end
    | comp_Decl (Hermes.VarDecl(y, t, p) :: res) var =
        let
          val t0 = comp_Type t
          val (y0, var2) = get_Variable var y t0 var false
          val start = [Data.Assign2S((t0, Data.VarS(y0, p), Data.CstS("0", p), p))]
          val (finish, var3) = comp_Decl res var2
        in
          (start@finish, var3)
        end
  
  fun comp_Undecl [] var = ([], var)
    | comp_Undecl (Hermes.ConstDecl(y, i, p) :: res) var =
        let
          val (y0, var2) = get_Variable var y (Data.TypeS(Data.Public, Data.u64)) var false
          val finish = [Data.Assign2S((Data.TypeS(Data.Public, Data.u64), Data.CstS(i, p), Data.VarS(y0, p), p))]
          val (start, var3) = comp_Undecl res var2
        in
          (start@finish, var3)
        end
    | comp_Undecl (Hermes.VarDecl(y, t, p) :: res) var =
        let
          val t0 = comp_Type t
          val (y0, var2) = get_Variable var y t0 var false
          val finish = [Data.Assign2S((t0, Data.CstS("0", p), Data.VarS(y0, p), p))]
          val (start, var3) = comp_Undecl res var2
        in
          (start@finish, var3)
        end


  (* Compile L-Values when used in udpates *)
  fun use_LVal (Hermes.Var(y, p)) var =
        let
          val t = get_Type y var p
          val (y0, var2) = get_Variable var y t var false
        in
          ([], [], (Data.VarS(y0, p)), var2)
        end
    | use_LVal (Hermes.Array(y, e, p)) var =
        let
          val t = get_Type (y^"T") var p
          val size = (get_Size t) div 8
          val var2 = incr_Variable var "T"
          val (T1, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (indexCreate, var4) = comp_E (Hermes.Bin(Hermes.Times, e, Hermes.Const(Int.toString(size), p), p)) "+" (Data.TypeS(Data.Public, Data.u64)) T1 "0" p p var3 true
          val (indexEmpty, var5) = comp_E (Hermes.Bin(Hermes.Times, e, Hermes.Const(Int.toString(size), p), p)) "-" (Data.TypeS(Data.Public, Data.u64)) "0" T1 p p var4 true
          val var6 = incr_Variable var5 "I"
          val (I, var7) = get_Variable var6 "I" (Data.TypeS(Data.Public, Data.u64)) var6 false
          val var8 = incr_Variable var7 "T"
          val (T2, var9) = get_Variable var8 "T" (Data.TypeS(Data.Public, Data.u64)) var8 false
          val (yT, var10) = get_Variable var9 (y^"T") t var9 true
          val start = [Data.AssignS(Data.TypeS(Data.Public, Data.u64), Data.VarS(I, p), Data.UpdOp2S("+", Data.CstS("0", p), Data.Op2S("+", Data.VarS(yT, p), Data.VarS(T1, p), p), p), p),
                      Data.SwapS(Data.VarS(T2, p), Data.MemoryS(t, Data.VarS(I, p), p), Data.CstS("0", p), p)]
          val finish = [Data.SwapS(Data.CstS("0", p), Data.MemoryS(t, Data.VarS(I, p), p), Data.VarS(T2, p), p),
                        Data.AssignS(Data.TypeS(Data.Public, Data.u64), Data.CstS("0", p), Data.UpdOp2S("-", Data.VarS(I, p), Data.Op2S("+", Data.VarS(yT, p), Data.VarS(T1, p), p), p), p)]
        in
          (indexCreate@start, finish@indexEmpty, Data.VarS(T2, p), var10)
        end
  (* Compile L-Values when being updated *)
  and comp_LVal (Hermes.Var(x, p)) var =
        let
          val t = get_Type x var p
          val (x0, var2) = get_Variable var x t var false
          val var3 = incr_Variable var2 x
          val (x1, var4) = get_Variable var3 x t var3 false
        in
          ([], [], t, x1, x0, p, var4, true)
        end
    | comp_LVal (Hermes.Array(x, e, p)) var =
        let
          val t = get_Type (x^"T") var p
          val size = (get_Size t) div 8
          val var2 = incr_Variable var "T"
          val (T1, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (indexCreate, var4) = comp_E (Hermes.Bin(Hermes.Times, e, Hermes.Const(Int.toString(size), p), p)) "+" (Data.TypeS(Data.Public, Data.u64)) T1 "0" p p var3 true
          val (indexEmpty, var5) = comp_E (Hermes.Bin(Hermes.Times, e, Hermes.Const(Int.toString(size), p), p)) "-" (Data.TypeS(Data.Public, Data.u64)) "0" T1 p p var4 true
          val var6 = incr_Variable var5 "I"
          val (I, var7) = get_Variable var6 "I" (Data.TypeS(Data.Public, Data.u64)) var6 false
          val (xT, var8) = get_Variable var7 (x^"T") t var7 true
          val start = [Data.AssignS(Data.TypeS(Data.Public, Data.u64), Data.VarS(I, p), Data.UpdOp2S("+", Data.CstS("0", p), Data.Op2S("+", Data.VarS(xT, p), Data.VarS(T1, p), p), p), p)]
          val finish = [Data.AssignS(Data.TypeS(Data.Public, Data.u64), Data.CstS("0", p), Data.UpdOp2S("-", Data.VarS(I, p), Data.Op2S("+", Data.VarS(xT, p), Data.VarS(T1, p), p), p), p)]
        in
          (indexCreate@start, finish@indexEmpty, t, I, "", p, var8, false)
        end
  (* Compile Expressions *)
  and comp_E (Hermes.Const(i, p)) upop t x x0 p2 p3 var bool =
        if bool
        then
          let
            val xout = comp_Var x0 p2
            val xin = comp_Var x p2
          in
            ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.SimOp2S(Data.CstS(i, p)), p2), p3)], var)
          end
        else
          let
            val xin = comp_Var x p2
          in
            ([Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.SimOp2S(Data.CstS(i, p)), p2)], var)
          end
    | comp_E (Hermes.Rval(lval)) upop t x x0 p2 p3 var bool =
        let
          val (S, F, y, var2) = use_LVal lval var
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (S@[Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.SimOp2S(y), p2), p3)]@F, var2)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (S@[Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.SimOp2S(y), p2)]@F, var2)
            end
        end
    | comp_E (Hermes.Size(y, p)) upop t x x0 p2 p3 var bool =
        let
          val (yS, var2) = get_Variable var (y^"S") (Data.TypeS(Data.Public, Data.u64)) var true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.SimOp2S(Data.VarS(yS, p)), p2), p3)], var2)
            end
          else
            let
              val xin = comp_Var x p2
            in
              ([Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.SimOp2S(Data.VarS(yS, p)), p2)], var2)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Const(i2, ep2), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.CstS(i1, ep1), Data.CstS(i2, ep2), p), p2), p3)], var)
            end
          else
            let
              val xin = comp_Var x p2
            in
              ([Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.CstS(i1, ep1), Data.CstS(i2, ep2), p), p2)], var)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Rval(lval), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val (S, F, y, var2) = use_LVal lval var
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (S@[Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.CstS(i1, ep1), y, p), p2), p3)]@F, var2)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (S@[Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.CstS(i1, ep1), y, p), p2)]@F, var2)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Size(y, ep2), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val (yS, var2) = get_Variable var (y^"S") (Data.TypeS(Data.Public, Data.u64)) var true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.CstS(i1, ep1), Data.VarS(yS, ep2), p), p2), p3)], var2)
            end
          else
            let
              val xin = comp_Var x p2
            in
              ([Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.CstS(i1, ep1), Data.VarS(yS, ep2), p), p2)], var2)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Rval(lval), Hermes.Const(i2, ep2), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val (S, F, y, var2) = use_LVal lval var
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (S@[Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, y, Data.CstS(i2, ep2), p), p2), p3)]@F, var2)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (S@[Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, y, Data.CstS(i2, ep2), p), p2)]@F, var2)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Rval(lval1), Hermes.Rval(lval2), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val (S1, F1, y1, var2) = use_LVal lval1 var
          val (S2, F2, y2, var3) = use_LVal lval2 var2
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (S1@S2@[Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, y1, y2, p), p2), p3)]@F2@F1, var3)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (S1@S2@[Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, y1, y2, p), p2)]@F2@F1, var3)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Rval(lval1), Hermes.Size(y2, ep2), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val (S1, F1, y1, var2) = use_LVal lval1 var
          val (yS, var3) = get_Variable var2 (y2^"S") (Data.TypeS(Data.Public, Data.u64)) var2 true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (S1@[Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, y1, Data.VarS(yS, ep2), p), p2), p3)]@F1, var3)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (S1@[Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, y1, Data.VarS(yS, ep2), p), p2)]@F1, var3)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Size(y, ep1), Hermes.Const(i2, ep2), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val (yS, var2) = get_Variable var (y^"S") (Data.TypeS(Data.Public, Data.u64)) var true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(yS, ep1), Data.CstS(i2, ep2),  p), p2), p3)], var2)
            end
          else
            let
              val xin = comp_Var x p2
            in
              ([Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.VarS(yS, ep1), Data.CstS(i2, ep2), p), p2)], var2)
            end
        end
    | comp_E (Hermes.Bin(bop,  Hermes.Size(y1, ep1), Hermes.Rval(lval2), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val (S1, F1, y2, var2) = use_LVal lval2 var
          val (yS, var3) = get_Variable var2 (y1^"S") (Data.TypeS(Data.Public, Data.u64)) var2 true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (S1@[Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(yS, ep1), y2, p), p2), p3)]@F1, var3)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (S1@[Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.VarS(yS, ep1), y2, p), p2)]@F1, var3)
            end
        end
    | comp_E (Hermes.Bin(bop,  Hermes.Size(y1, ep1), Hermes.Size(y2, ep2), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val (yS1, var2) = get_Variable var (y1^"S") (Data.TypeS(Data.Public, Data.u64)) var true
          val (yS2, var3) = get_Variable var2 (y2^"S") (Data.TypeS(Data.Public, Data.u64)) var2 true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(yS1, ep1), Data.VarS(yS2, ep2), p), p2), p3)], var3)
            end
          else
            let
              val xin = comp_Var x p2
            in
              ([Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.VarS(yS1, ep1), Data.VarS(yS2, ep2), p), p2)], var3)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Const(i2, ep2), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (exprD, exprF, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (exprD@
              [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), Data.CstS(i2, ep2), p), p2), p3)]
              @exprF, var4)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (exprD@
              [Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.VarS(T, p), Data.CstS(i2, ep2), p), p2)]
              @exprF, var4)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Rval(lval), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (exprD, exprF, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 true
          val (S, F, y, var5) = use_LVal lval var4
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (exprD@S@
              [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), y, p), p2), p3)]
              @F@exprF, var5)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (exprD@S@
              [Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.VarS(T, p), y, p), p3)]
              @F@exprF, var5)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Size(y2, ep2), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (exprD, exprF, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 true
          val (yS, var5) = get_Variable var4 (y2^"S") (Data.TypeS(Data.Public, Data.u64)) var4 true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (exprD@
              [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), Data.VarS(yS, ep2), p), p2), p3)]
              @exprF, var5)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (exprD@
              [Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.VarS(T, p), Data.VarS(yS, ep2), p), p2)]
              @exprF, var5)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Bin(e), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (exprD, exprF, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (exprD@
              [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.CstS(i1, ep1), Data.VarS(T, p), p), p2), p3)]
              @exprF, var4)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (exprD@
              [Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.CstS(i1, ep1), Data.VarS(T, p), p), p2)]
              @exprF, var4)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Rval(lval), Hermes.Bin(e), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (exprD, exprF, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 true
          val (S, F, y, var5) = use_LVal lval var4
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (exprD@S@
              [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, y, Data.VarS(T, p), p), p2), p3)]
              @F@exprF, var5)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (exprD@S@
              [Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, y, Data.VarS(T, p), p), p2)]
              @F@exprF, var5)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Size(y1, ep1), Hermes.Bin(e), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (exprD, exprF, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 true
          val (yS, var5) = get_Variable var4 (y1^"S") (Data.TypeS(Data.Public, Data.u64)) var4 true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (exprD@
              [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(yS, ep1), Data.VarS(T, p), p), p2), p3)]
              @exprF, var4)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (exprD@
              [Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.VarS(yS, ep1), Data.VarS(T, p), p), p2)]
              @exprF, var4)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Bin(e1), Hermes.Bin(e2), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T1, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val var4 = incr_Variable var3 "T"
          val (T2, var5) = get_Variable var4 "T" (Data.TypeS(Data.Public, Data.u64)) var4 false
          val (exprD1, exprF1, var6) = comp_E_contD (Hermes.Bin(e1)) "^" (Data.TypeS(Data.Public, Data.u64)) T1 "0" p2 p3 var5 true
          val (exprD2, exprF2, var7) = comp_E_contD (Hermes.Bin(e2)) "^" (Data.TypeS(Data.Public, Data.u64)) T2 "0" p2 p3 var6 true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (exprD1@exprD2@
              [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T1, p), Data.VarS(T2, p), p), p2), p3)]
              @exprF2@exprF1, var7)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (exprD1@exprD2@
              [Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.VarS(T1, p), Data.VarS(T2, p), p), p2)]
              @exprF2@exprF1, var7)
            end
        end
  (* Compile Expressions once a complex operation is found *)
  and comp_E_contD (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Const(i2, ep2), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (exprD, exprF, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 bool
        in
          (exprD@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), Data.CstS(i2, ep2), p), p2), p3)], 
          [Data.AssignS(t, xout, Data.UpdOp2S(upop, xin, Data.Op2S(b, Data.VarS(T, p), Data.CstS(i2, ep2), p), p2), p3)]
          @exprF, var4)
        end
    | comp_E_contD (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Rval(lval), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (exprD, exprF, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 bool
          val (S, F, y, var5) = use_LVal lval var4
        in
          (exprD@S@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), y, p), p2), p3)], 
          [Data.AssignS(t, xout, Data.UpdOp2S(upop, xin, Data.Op2S(b, Data.VarS(T, p), y, p), p2), p3)]
          @F@exprF, var5)
        end
    | comp_E_contD (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Size(y2, ep2), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (exprD, exprF, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 bool
          val (yS, var5) = get_Variable var4 (y2^"S") (Data.TypeS(Data.Public, Data.u64)) var4 true
        in
          (exprD@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), Data.VarS(yS, ep2), p), p2), p3)], 
          [Data.AssignS(t, xout, Data.UpdOp2S(upop, xin, Data.Op2S(b, Data.VarS(T, p), Data.VarS(yS, ep2), p), p2), p3)]
          @exprF, var5)
        end
    | comp_E_contD (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Bin(e), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (exprD, exprF, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 bool
        in
          (exprD@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.CstS(i1, ep1), Data.VarS(T, p), p), p2), p3)], 
          [Data.AssignS(t, xout, Data.UpdOp2S(upop, xin, Data.Op2S(b, Data.CstS(i1, ep1), Data.VarS(T, p), p), p2), p3)]
          @exprF, var4)
        end
    | comp_E_contD (Hermes.Bin(bop, Hermes.Rval(lval), Hermes.Bin(e), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (exprD, exprF, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 bool
          val (S, F, y, var5) = use_LVal lval var4
        in
          (exprD@S@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, y, Data.VarS(T, p), p), p2), p3)],
          [Data.AssignS(t, xout, Data.UpdOp2S(upop, xin, Data.Op2S(b, y, Data.VarS(T, p), p), p2), p3)]
          @F@exprF, var5)
        end
    | comp_E_contD (Hermes.Bin(bop, Hermes.Size(y1, ep1), Hermes.Bin(e), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (exprD, exprF, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 bool
          val (yS, var5) = get_Variable var4 (y1^"S") (Data.TypeS(Data.Public, Data.u64)) var4 true
        in
          (exprD@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(yS, ep1), Data.VarS(T, p),  p), p2), p3)], 
          [Data.AssignS(t, xout, Data.UpdOp2S(upop, xin, Data.Op2S(b, Data.VarS(yS, ep1), Data.VarS(T, p),  p), p2), p3)]
          @exprF, var5)
        end
    | comp_E_contD (Hermes.Bin(bop, Hermes.Bin(e1), Hermes.Bin(e2), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T1, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 false
          val var4 = incr_Variable var3 "T"
          val (T2, var5) = get_Variable var4 "T" (Data.TypeS(Data.Public, Data.u64)) var4 false
          val (exprD1, exprF1, var6) = comp_E_contD (Hermes.Bin(e1)) "^" (Data.TypeS(Data.Public, Data.u64)) T1 "0" p2 p3 var5 bool
          val (exprD2, exprF2, var7) = comp_E_contD (Hermes.Bin(e2)) "^" (Data.TypeS(Data.Public, Data.u64)) T2 "0" p2 p3 var6 bool
        in
          (exprD1@exprD2@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T1, p), Data.VarS(T2, p), p), p2), p3)], 
          [Data.AssignS(t, xout, Data.UpdOp2S(upop, xin, Data.Op2S(b, Data.VarS(T1, p), Data.VarS(T2, p), p), p2), p3)]
          @exprF2@exprF1,var7)
        end
    | comp_E_contD e upop t x x0 p2 p3 var bool = 
        let
          val (expr1, var2) = comp_E e upop t x x0 p2 p3 var bool
          val (expr2, var3) = comp_E e upop t x0 x p2 p3 var2 bool
        in
          (expr1, expr2, var3)
        end
  
  fun comp_E_condD (Hermes.Rval(lval)) upop t x x0 p2 p3 var bool t0 i0 = 
        (case lval of
          (Hermes.Var(_)) =>
            let
              val (expr, var2) = comp_E (Hermes.Rval(lval)) upop t x x0 p2 p3 var bool
            in
              ([], expr, var2)
            end
        | (Hermes.Array(y, e, p)) =>
            let
              val ta = get_Type (y^"T") var p
              val size = (get_Size t) div 8
              val var2 = incr_Variable var t0
              val (T1, var3) = get_Variable var2 t0 (Data.TypeS(Data.Public, Data.u64)) var2 false
              val (indexCreate, var4) = comp_E (Hermes.Bin(Hermes.Times, e, Hermes.Const(Int.toString(size), p), p)) "+" (Data.TypeS(Data.Public, Data.u64)) T1 "0" p p var3 true
              val var5 = incr_Variable var4 i0
              val (I, var6) = get_Variable var5 i0 (Data.TypeS(Data.Public, Data.u64)) var5 false
              val var7 = incr_Variable var6 t0
              val (T2, var8) = get_Variable var7 t0 (Data.TypeS(Data.Public, Data.u64)) var7 false
              val (yT, var9) = get_Variable var8 (y^"T") t var8 true
              val start = [Data.AssignS(Data.TypeS(Data.Public, Data.u64), Data.VarS(I, p), Data.UpdOp2S("+", Data.CstS("0", p), Data.Op2S("+", Data.VarS(yT, p), Data.VarS(T1, p), p), p), p),
                          Data.SwapS(Data.VarS(T2, p), Data.MemoryS(ta, Data.VarS(I, p), p), Data.CstS("0", p), p)]
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (indexCreate@start, [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.SimOp2S(Data.VarS(T2, p)), p2), p3)], var9)
            end)
    | comp_E_condD (Hermes.Bin(bop, e1, e2, p)) upop t x x0 p2 p3 var bool t0 i0 =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var t0
          val (T1, var3) = get_Variable var2 t0 (Data.TypeS(Data.Public, Data.u64)) var2 false
          val var4 = incr_Variable var3 "T"
          val (T2, var5) = get_Variable var4 "T" (Data.TypeS(Data.Public, Data.u64)) var4 false
          val (exprD, exprF, var6) = comp_E_contD (Hermes.Bin(bop, e1, e2, p)) "^" (Data.TypeS(Data.Public, Data.u64)) T2 "0" p2 p3 var5 bool
          val start = [Data.AssignS(Data.TypeS(Data.Public, Data.u64), Data.VarS(T1, p), Data.UpdOp2S("^", Data.CstS("0", p), Data.SimOp2S(Data.VarS(T2, p)), p2), p3)]
        in
          (exprD@start@exprF, [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.SimOp2S(Data.VarS(T1, p)), p2), p3)], var6)
        end
    | comp_E_condD e upop t x x0 p2 p3 var bool t0 i0 =
        let
          val (expr, var2) = comp_E e upop t x x0 p2 p3 var bool
        in
          ([], expr, var2)
        end
    
  fun comp_E_condF (Hermes.Rval(lval)) upop t x x0 p2 p3 var bool t0 i0 = 
        (case lval of
          (Hermes.Var(_)) =>
            let
              val (expr, var2) = comp_E (Hermes.Rval(lval)) upop t x x0 p2 p3 var bool
            in
              (expr, [], var2)
            end
        | (Hermes.Array(y, e, p)) =>
            let
              val ta = get_Type (y^"T") var p
              val size = (get_Size t) div 8
              val (I, var2) = get_Variable var i0 (Data.TypeS(Data.Public, Data.u64)) var false
              val (T1, var3) = get_Variable var2 t0 (Data.TypeS(Data.Public, Data.u64)) var2 false
              val swap = [Data.SwapS(Data.CstS("0", p), Data.MemoryS(ta, Data.VarS(I, p), p), Data.VarS(T1, p), p)]
              val var4 = incr_Variable var3 t0
              val (T2, var5) = get_Variable var4 t0 (Data.TypeS(Data.Public, Data.u64)) var4 false
              val (indexCreate, var6) = comp_E (Hermes.Bin(Hermes.Times, e, Hermes.Const(Int.toString(size), p), p)) "+" (Data.TypeS(Data.Public, Data.u64)) T2 "0" p p var5 true
              val (yT, var7) = get_Variable var6 (y^"T") t var6 true
              val finish = [Data.AssignS(Data.TypeS(Data.Public, Data.u64), Data.CstS("0", p), Data.UpdOp2S("-", Data.VarS(I, p), Data.Op2S("+", Data.VarS(yT, p), Data.VarS(T2, p), p), p), p)]
              val (indexEmpty, var8) = comp_E (Hermes.Bin(Hermes.Times, e, Hermes.Const(Int.toString(size), p), p)) "-" (Data.TypeS(Data.Public, Data.u64)) "0" T2 p p var7 true
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.SimOp2S(Data.VarS(T1, p)), p2), p3)], 
              swap@indexCreate@finish@indexEmpty,var8)
            end)
    | comp_E_condF (Hermes.Bin(bop, e1, e2, p)) upop t x x0 p2 p3 var bool t0 i0 =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val (T1, var2) = get_Variable var t0 (Data.TypeS(Data.Public, Data.u64)) var false
          val var3 = incr_Variable var2 "T"
          val (T2, var4) = get_Variable var3 "T" (Data.TypeS(Data.Public, Data.u64)) var3 false
          val (exprD, exprF, var5) = comp_E_contD (Hermes.Bin(bop, e1, e2, p)) "^" (Data.TypeS(Data.Public, Data.u64)) T2 "0" p2 p3 var4 bool
          val finish = [Data.AssignS(Data.TypeS(Data.Public, Data.u64), Data.CstS("0", p), Data.UpdOp2S("^", Data.VarS(T1, p), Data.SimOp2S(Data.VarS(T2, p)), p2), p3)]
        in
          ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.SimOp2S(Data.VarS(T1, p)), p2), p3)], exprD@finish@exprF, var5)
        end
    | comp_E_condF e upop t x x0 p2 p3 var bool t0 i0 =
        let
          val (expr, var2) = comp_E e upop t x x0 p2 p3 var bool
        in
          (expr, [], var2)
        end

  fun checkD (Hermes.Bin(bop, e1, e2, p)) var p2 label incr =
        if check_Comp bop
        then 
          let
            val (start1, expr1, var2, T1, _, _) = checkD e1 var p2 label incr
            val (start2, expr2, var3, T2, _, _) = checkD e2 var2 p2 (label^"bis") incr
          in
            (start1@start2, expr1@expr2, var3, T1, T2, true)
          end
        else
          let
            val var2 = incr_Variable var (label^(Int.toString incr)^"TT")
            val (T0, var3) = get_Variable var2 (label^(Int.toString incr)^"TT") (Data.TypeS(Data.Public, Data.u64)) var2 false
            val (start, expr, var4) = comp_E_condD (Hermes.Bin(bop, e1, e2, p)) "^" (Data.TypeS(Data.Public, Data.u64)) T0 "0" p2 p2 var3 true (label^(Int.toString incr)^"T") (label^(Int.toString incr)^"I")
          in
            (start, expr, var4, T0, "", false)
          end
    | checkD (Hermes.Rval(lval)) var p2 label incr =
        (case lval of
          (Hermes.Var(y, p)) =>
            ([], [], var, "", "", false)
        | (Hermes.Array(y, e, p)) =>
            let
            val var2 = incr_Variable var (label^(Int.toString incr)^"TT")
            val (T0, var3) = get_Variable var2 (label^(Int.toString incr)^"TT") (Data.TypeS(Data.Public, Data.u64)) var2 false
            val (start, expr, var4) = comp_E_condD (Hermes.Rval(lval)) "^" (Data.TypeS(Data.Public, Data.u64)) T0 "0" p2 p2 var3 true (label^(Int.toString incr)^"T") (label^(Int.toString incr)^"I")
          in
            (start, expr, var4, T0, "", false)
          end)
    | checkD e var p2 label incr =
        ([], [], var, "", "", false)
  
  fun checkF (Hermes.Bin(bop, e1, e2, p)) var p2 label incr =
        if check_Comp bop
        then 
          let
            val (expr1, finish1, var2, T1, _, _) = checkF e1 var p2 label incr
            val (expr2, finish2, var3, T2, _, _) = checkF e2 var2 p2 (label^"bis") incr
          in
            (expr2@expr1, finish2@finish1, var3, T1, T2, true)
          end
        else
          let
            val (T0, var2) = get_Variable var (label^(Int.toString incr)^"TT") (Data.TypeS(Data.Public, Data.u64)) var false
            val (expr, finish, var3) = comp_E_condF (Hermes.Bin(bop, e1, e2, p)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T0 p2 p2 var2 true (label^(Int.toString incr)^"T") (label^(Int.toString incr)^"I")
          in
            (expr, finish, var3, T0, "", false)
          end
    | checkF (Hermes.Rval(lval)) var p2 label incr =
        (case lval of
          (Hermes.Var(y, p)) =>
            ([], [], var, "", "", false)
        | (Hermes.Array(y, e, p)) =>
            let
            val (T0, var2) = get_Variable var (label^(Int.toString incr)^"TT") (Data.TypeS(Data.Public, Data.u64)) var false
            val (start, expr, var3) = comp_E_condF (Hermes.Rval(lval)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T0 p2 p2 var2 true (label^(Int.toString incr)^"T") (label^(Int.toString incr)^"I")
          in
            (start, expr, var3, T0, "", false)
          end)
    | checkF e var p2 label incr =
        ([], [], var, "", "", false)
  
  fun get_Condition (Hermes.Const(i, p)) var p2 t0 t1 isComp =
        Data.BoolOp2S("!=", Data.CstS(i, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Rval(lval)) var p2 t0 t1 isComp =
        (case lval of
          (Hermes.Var(y, py)) =>
            let
              val t = get_Type y var py
              val (y1, _) = get_Variable var y t var false
            in
              Data.BoolOp2S("!=", Data.VarS(y1, py), Data.CstS("0", py), p2)
            end
        | (Hermes.Array(y, e, py)) =>
            Data.BoolOp2S("!=", Data.VarS(t0, py), Data.CstS("0", py), p2))
    | get_Condition (Hermes.Size(y, p)) var p2 t0 t1 isComp =
        let
          val (yS, _) = get_Variable var (y^"S") (Data.TypeS(Data.Public, Data.u64)) var true
        in
          Data.BoolOp2S("!=", Data.VarS(yS, p), Data.CstS("0", p), p2)
        end
    | get_Condition (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Const(i2, ep2), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
          in
            Data.BoolOp2S(b, Data.CstS(i1, ep1), Data.CstS(i2, ep2), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Rval(lval1), Hermes.Const(i2, ep2), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
          in
            case lval1 of
              (Hermes.Var(y, py)) =>
                let
                  val t = get_Type y var py
                  val (y1, _) = get_Variable var y t var false
                in
                  Data.BoolOp2S(b, Data.VarS(y1, py), Data.CstS(i2, ep2), p2)
                end
            | (Hermes.Array(y, e, py)) =>
                Data.BoolOp2S(b, Data.VarS(t0, py), Data.CstS(i2, ep2), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Size(y1, ep1), Hermes.Const(i2, ep2), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
            val (yS, _) = get_Variable var (y1^"S") (Data.TypeS(Data.Public, Data.u64)) var true
          in
            Data.BoolOp2S(b, Data.VarS(yS, ep1), Data.CstS(i2, ep2), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Rval(lval2), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
          in
            case lval2 of
              (Hermes.Var(y, py)) =>
                let
                  val t = get_Type y var py
                  val (y2, _) = get_Variable var y t var false
                in
                  Data.BoolOp2S(b, Data.CstS(i1, ep1), Data.VarS(y2, py), p2)
                end
            | (Hermes.Array(y, e, py)) =>
                Data.BoolOp2S(b, Data.CstS(i1, ep1), Data.VarS(t1, py), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Rval(lval1), Hermes.Rval(lval2), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
          in
            case lval1 of
              (Hermes.Var(y1, ep1)) =>
                let
                  val t = get_Type y1 var ep1
                  val (yv1, _) = get_Variable var y1 t var false
                in
                  case lval2 of
                    (Hermes.Var(y2, ep2)) =>
                      let
                        val t2 = get_Type y2 var ep2
                        val (yv2, _) = get_Variable var y2 t2 var false
                      in
                        Data.BoolOp2S(b, Data.VarS(yv1, ep1), Data.VarS(yv2, ep2), p2)
                      end
                  | (Hermes.Array(y2, e2, ep2)) =>
                      Data.BoolOp2S(b, Data.VarS(yv1, ep1), Data.VarS(t1, ep2), p2)
                end
            | (Hermes.Array(y1, e1, ep1)) =>
                case lval2 of
                  (Hermes.Var(y2, ep2)) =>
                    let
                      val t2 = get_Type y2 var ep2
                      val (yv2, _) = get_Variable var y2 t2 var false
                    in
                      Data.BoolOp2S(b, Data.VarS(t0, ep1), Data.VarS(yv2, ep2), p2)
                    end
                | (Hermes.Array(y2, e2, ep2)) =>
                    Data.BoolOp2S(b, Data.VarS(t0, ep1), Data.VarS(t1, ep2), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Size(y1, ep1), Hermes.Rval(lval2), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
            val (yS, _) = get_Variable var (y1^"S") (Data.TypeS(Data.Public, Data.u64)) var true
          in
            case lval2 of
              (Hermes.Var(y, py)) =>
                let
                  val t = get_Type y var py
                  val (y2, _) = get_Variable var y t var false
                in
                  Data.BoolOp2S(b, Data.VarS(yS, ep1), Data.VarS(y2, py), p2)
                end
            | (Hermes.Array(y, e, py)) =>
                Data.BoolOp2S(b, Data.VarS(yS, ep1), Data.VarS(t1, py), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Size(y2, ep2), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
            val (yS, _) = get_Variable var (y2^"S") (Data.TypeS(Data.Public, Data.u64)) var true
          in
            Data.BoolOp2S(b, Data.CstS(i1, ep1), Data.VarS(yS, ep2), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Rval(lval1), Hermes.Size(y2, ep2), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
            val (yS, _) = get_Variable var (y2^"S") (Data.TypeS(Data.Public, Data.u64)) var true
          in
            case lval1 of
              (Hermes.Var(y, py)) =>
                let
                  val t = get_Type y var py
                  val (y1, _) = get_Variable var y t var false
                in
                  Data.BoolOp2S(b, Data.VarS(y1, py), Data.VarS(yS, ep2), p2)
                end
            | (Hermes.Array(y, e, py)) =>
                Data.BoolOp2S(b, Data.VarS(t0, py), Data.VarS(yS, ep2), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Size(y1, ep1), Hermes.Size(y2, ep2), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
            val (yS1, _) = get_Variable var (y1^"S") (Data.TypeS(Data.Public, Data.u64)) var true
            val (yS2, _) = get_Variable var (y2^"S") (Data.TypeS(Data.Public, Data.u64)) var true
          in
            Data.BoolOp2S(b, Data.VarS(yS1, ep1), Data.VarS(yS2, ep2), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Const(i2, ep2), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
          in
            Data.BoolOp2S(b, Data.VarS(t0, p), Data.CstS(i2, ep2), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Rval(lval2), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
          in
            case lval2 of
              (Hermes.Var(y, py)) =>
                let
                  val t = get_Type y var py
                  val (y2, _) = get_Variable var y t var false
                in
                  Data.BoolOp2S(b, Data.VarS(t0, p), Data.VarS(y2, py), p2)
                end
            | (Hermes.Array(y, e1, py)) =>
                Data.BoolOp2S(b, Data.VarS(t0, p), Data.VarS(t1, py), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Size(y2, ep2), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
            val (yS, _) = get_Variable var (y2^"S") (Data.TypeS(Data.Public, Data.u64)) var true
          in
            Data.BoolOp2S(b, Data.VarS(t0, p), Data.VarS(yS, ep2), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Bin(e), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
          in
            Data.BoolOp2S(b, Data.CstS(i1, ep1), Data.VarS(t1, p), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Rval(lval1), Hermes.Bin(e), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
          in
            case lval1 of
              (Hermes.Var(y, py)) =>
                let
                  val t = get_Type y var py
                  val (y1, _) = get_Variable var y t var false
                in
                  Data.BoolOp2S(b, Data.VarS(y1, py), Data.VarS(t1, p), p2)
                end
            | (Hermes.Array(y, e1, py)) =>
                Data.BoolOp2S(b, Data.VarS(t0, py), Data.VarS(t1, p), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Size(y1, ep1), Hermes.Bin(e), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
            val (yS, _) = get_Variable var (y1^"S") (Data.TypeS(Data.Public, Data.u64)) var true
          in
            Data.BoolOp2S(b, Data.VarS(yS, ep1), Data.VarS(t1, p), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)
    | get_Condition (Hermes.Bin(bop, Hermes.Bin(e1), Hermes.Bin(e2), p)) var p2 t0 t1 isComp =
        if isComp
        then
          let
            val b = comp_BinOp bop
          in
            Data.BoolOp2S(b, Data.VarS(t0, p), Data.VarS(t1, p), p2)
          end
        else
          Data.BoolOp2S("!=", Data.VarS(t0, p), Data.CstS("0", p), p2)

  fun get_Condition_Equal a (Hermes.Const(i, p)) var p2 t0 = 
        Data.BoolOp2S("==", Data.VarS(a, p2), Data.CstS(i, p), p2)
    | get_Condition_Equal a (Hermes.Rval(lval)) var p2 t0 =
        (case lval of
          (Hermes.Var(y, p)) =>
            let
              val (_, _, l, var2) = use_LVal lval var
            in
              Data.BoolOp2S("==", Data.VarS(a, p2), l, p2)
            end
        | (Hermes.Array(y, e, p)) =>
            let
              val (T, var2) = get_Variable var t0 (Data.TypeS(Data.Public, Data.u64)) var false
            in
              Data.BoolOp2S("==", Data.VarS(a, p2), Data.VarS(T, p), p2)
            end)
    | get_Condition_Equal a (Hermes.Size(x, p)) var p2 t0 =
        let
          val (xS, _) = get_Variable var (x^"S") (Data.TypeS(Data.Public, Data.u64)) var true
        in
          Data.BoolOp2S("==", Data.VarS(a, p2), Data.VarS(xS, p), p2)
        end
    | get_Condition_Equal a (Hermes.Bin(b, e1, e2, p)) var p2 t0 =
        let
          val (T, var2) = get_Variable var t0 (Data.TypeS(Data.Public, Data.u64)) var false
        in
          Data.BoolOp2S("==", Data.VarS(a, p2), Data.VarS(T, p), p2)
        end

  (* Compiler Arguments *)
  fun comp_Args [] var res = (res, var)
    | comp_Args (Hermes.VarArg(x, t, p) :: pargs) var res =
        let
          val ty = comp_Type t
          val (atom, var2) = get_Variable var x ty var false
        in
          comp_Args pargs var2 (res@[Data.ArgS(ty, Data.VarS(atom, p))])
        end
    | comp_Args (Hermes.ArrayArg(x, t, p) :: pargs) var res =
        let
          val ty = comp_Type t
          val (atomBegin, var2) = get_Variable var (x^"T") ty var true
          val (atomSize, var3) = get_Variable var2 (x^"S") (Data.TypeS(Data.Public, Data.u64)) var2 true
        in
          comp_Args pargs var3 (res@[Data.ArgS(Data.TypeS(Data.Public, Data.u64), Data.VarS(atomBegin, p)),
                                    Data.ArgS(Data.TypeS(Data.Public, Data.u64), Data.VarS(atomSize, p))])
        end
  
  (* Compile Statements *)
  fun comp_S block res (Hermes.Skip) entry var label incr = (block, entry, res, var) (* Skip *)
    | comp_S block res (Hermes.Update(upop, lv, e, p)) entry var label incr = (* Updates *)
        let
          val (start, finish, t, x, x0, p2, var2, bool) = comp_LVal lv var
          val u = comp_Upop upop
          val (expr, var3) = comp_E e u t x x0 p2 p var2 bool
        in
          (block, entry, res@start@expr@finish, var3)
        end
    | comp_S block res (Hermes.Swap(lval1, lval2, p)) entry var label incr = (* Swaps *)
        let
          val (start1, finish1, t1, x1, x0, p1, var2, bool1) = comp_LVal lval1 var
          val (start2, finish2, t2, y1, y0, p2, var3, bool2) = comp_LVal lval2 var2
        in
          case (lval1, lval2) of
            (Hermes.Var(_), Hermes.Var(_)) =>
              (block, entry, res@[Data.DAssignS(t1, Data.VarS(x1, p1), t2, Data.VarS(y1, p2), Data.VarS(y0, p2), Data.VarS(x0, p1), p)], var3)
          | (Hermes.Array(_), Hermes.Array(_)) =>
              (block, entry, res@start1@start2@
              [Data.MemSwapS(Data.MemoryS(t1, Data.VarS(x1, p1), p), Data.MemoryS(t2, Data.VarS(y1, p2), p), p)]
              @finish2@finish1, var3)
          | (Hermes.Var(_), Hermes.Array(_)) =>
              (block, entry, res@start2@
              [Data.SwapS(Data.VarS(x1, p1), Data.MemoryS(t2, Data.VarS(y1, p2), p), Data.VarS(x0, p1), p)]
              @finish2, var3)
          | (Hermes.Array(_), Hermes.Var(_)) =>
              (block, entry, res@start1@
              [Data.SwapS(Data.VarS(y1, p2), Data.MemoryS(t1, Data.VarS(x1, p1), p), Data.VarS(y0, p2), p)]
              @finish1, var3)
        end
    | comp_S block res (Hermes.If(e, s1, s2, p)) entry var label incr =
        let
          val (start, expr1, var2, t01, t02, isComp1) = checkD e var p label incr
          val cond = get_Condition e var2 p t01 t02 isComp1
          val args = get_Arguments var2 p []
          val exit1 = Data.CondExitS(cond, (label^(Int.toString incr)^"L1", p), (label^(Int.toString incr)^"L2", p), args, p)
          val newblock = Data.BlockS(entry, res@start@expr1, exit1, p)
          val var3 = incr_Arguments var2
          val args2 = get_Arguments var3 p []
          val entry2 = Data.UncondEntryS((label^(Int.toString incr)^"L1", p), args2, p)
          val (block2, entry2bis, stats1, var4) = comp_S [newblock] [] s1 entry2 var3 (label^"if") 0
          val args3 = get_Arguments var4 p []
          val exit2 = Data.UncondExitS((label^(Int.toString incr)^"L3", p), args3, p)
          val newblock2 = Data.BlockS(entry2bis, stats1, exit2, p)
          val var5 = incr_Arguments var4
          val args4 = get_Arguments var5 p []
          val entry3 = Data.UncondEntryS((label^(Int.toString incr)^"L2", p), args4, p)
          val (block3, entry3bis, stats2, var6) = comp_S (block2@[newblock2]) [] s2 entry3 var5 (label^"else") 0
          val args5 = get_Arguments var6 p []
          val exit3 = Data.UncondExitS((label^(Int.toString incr)^"L4", p), args5, p)
          val newblock3 = Data.BlockS(entry3bis, stats2, exit3, p)
          val var7 = incr_Arguments var6
          val args6 = get_Arguments var7 p []
          val (expr2, finish, var8, t11, t12, isComp2) = checkF e var7 p label incr
          val cond2 = get_Condition e var7 p t11 t12 isComp2
          val entry4 = Data.CondEntryS(cond2, (label^(Int.toString incr)^"L3", p), (label^(Int.toString incr)^"L4", p), args6, p)
        in
          (block@block3@[newblock3], entry4, expr2@finish, var7)
        end
    | comp_S block res (Hermes.For(i, e1, e2, ss, p)) entry var label incr = 
        let
          val var2 = incr_Variable var i
          val (i0, var3) = get_Variable var2 i (Data.TypeS(Data.Public, Data.u64)) var2 false
          val (start1, expr1, var4) = comp_E_condD e1 "^" (Data.TypeS(Data.Public, Data.u64)) i0 "0" p p var3 true (label^(Int.toString incr)^"T1") (label^(Int.toString incr)^"I1")
          val (start2, _, var5) = comp_E_condD e2 "^" (Data.TypeS(Data.Public, Data.u64)) i0 "0" p p var4 true (label^(Int.toString incr)^"T2") (label^(Int.toString incr)^"I2")
          val args = get_Arguments var5 p []
          val exit1 = Data.UncondExitS((label^(Int.toString incr)^"L1", p), args, p)
          val newblock = Data.BlockS(entry, res@start1@start2@expr1, exit1, p)
          val var6 = incr_Arguments var5
          val args2 = get_Arguments var6 p []
          val (i1, var7) = get_Variable var6 i (Data.TypeS(Data.Public, Data.u64)) var6 false
          val cond1 = get_Condition_Equal i1 e1 var7 p (label^(Int.toString incr)^"T1")
          val entry2 = Data.CondEntryS(cond1, (label^(Int.toString incr)^"L1", p), (label^(Int.toString incr)^"L2", p), args2, p)
          val (block2, entry3, stats, var8) = comp_S [newblock] [] ss entry2 var7 (label^"for") 0
          val unused = get_unused var7 var8
          val args3 = get_Arguments var8 p unused
          val (i2, var9) = get_Variable var8 i (Data.TypeS(Data.Public, Data.u64)) var8 false
          val cond2 = get_Condition_Equal i2 e2 var9 p (label^(Int.toString incr)^"T2")
          val exit2 = Data.CondExitS(cond2, (label^(Int.toString incr)^"L3", p), (label^(Int.toString incr)^"L2", p), args3, p)
          val newblock2 = Data.BlockS(entry3, stats, exit2, p)
          val var10 = incr_Arguments var9
          val args4 = get_Arguments var10 p unused
          val entry4 = Data.UncondEntryS((label^(Int.toString incr)^"L3", p), args4, p)
          val (i3, var11) = get_Variable var10 i (Data.TypeS(Data.Public, Data.u64)) var10 false
          val (expr2, finish2, var12) = comp_E_condF e2 "^" (Data.TypeS(Data.Public, Data.u64)) "0" i3 p p var11 true (label^(Int.toString incr)^"T2") (label^(Int.toString incr)^"I2")
          val (_, finish1, var13) = comp_E_condF e1 "^" (Data.TypeS(Data.Public, Data.u64)) "0" i3 p p var12 true (label^(Int.toString incr)^"T1") (label^(Int.toString incr)^"I1")
        in
          (block@block2@[newblock2], entry4, expr2@finish2@finish1, var11)
        end
    | comp_S block res (Hermes.Block(d, ss, p)) entry var label incr = (* Blocks *)
        let
          val (start, var2) = comp_Decl d var
          val (block2, entry2, stats, var3) = comp_Block block (res@start) ss entry var2 label incr
          val (finish, var4) = comp_Undecl d var3
        in
          (block2, entry2, stats@finish, var4)
        end
  (* Compile Blocks *)
  and comp_Block block res [] entry var label incr = (block, entry, res, var)
    | comp_Block block res (s :: ss) entry var label incr =
        let
          val (block2, entry2, stats, var2) = comp_S block res s entry var label incr
        in
          case s of
            (Hermes.For(_)) =>
              let
                val unused = get_unused var var2
                val newvar = get_Arguments_Variable var2 unused

              in
                comp_Block block2 stats ss entry2 newvar label (incr+1)
              end
          | (Hermes.If(_)) =>
              let
                val unused = get_unused var var2
                val newvar = get_Arguments_Variable var2 unused
              in
                comp_Block block2 stats ss entry2 newvar label (incr+1)
              end
          | _ =>
            comp_Block block2 stats ss entry2 var2 label incr
        end

  (* Compile Begin entry *)
  fun comp_Entry f pargs i p var =
        let
          val (args, var2) = comp_Args pargs var []
        in
          if i = 0
          then (Data.BeginS(("main", p), args, p), "main", var2)
          else (Data.BeginS((f, p), args, p), f, var2)
        end
  
  (* Compile End exit *)
  fun comp_Exit f pargs i p var =
        let
          val (args, var2) = comp_Args pargs var []
        in
          if i = 0
          then (Data.EndS(("main", p), args, p), var2)
          else (Data.EndS((f, p), args, p), var2)
        end

  (* Compile Program *)
  fun comp_P [] i = []
    | comp_P ((f, pars, s, p) :: ps) i =
        let
          val (entry, label, var) = comp_Entry f pars i p []
          val _ = TextIO.print("[" ^ printVar var)
          val (block, entry2, stats, var2) = comp_S [] [] s entry var label 0
          val (exit, var3) = comp_Exit f pars i p var2
          val _ = TextIO.print("[" ^ printVar var3)
        in
          block@[Data.BlockS(entry2, stats, exit, p)]@(comp_P ps (i+1))
        end

  fun compile pgm =
        let
          val blocks = comp_P pgm 0
        in
          Data.ProgramS(blocks)
        end

end
