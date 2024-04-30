(* Compiler *)

structure Compiler :> Compiler =
struct

  exception Error of string*(int*int)

  fun printVar [] = "]\n"
    | printVar ((t, a, n) :: rest) =
        a^(Int.toString n)^", "^printVar rest

  fun store_Variable x t var =
        var@[(t, x, 0)]

  fun incr_Variable [] x = []
    | incr_Variable ((t, a, n) :: res) x =
        if x = a
        then [(t, a, (n+1))]@res
        else [(t, a, n)]@(incr_Variable res x)

  fun get_Variable [] x t ovar= 
        let
          val var2 = (store_Variable x t ovar)
        in
          (x^"0", var2)
        end
    | get_Variable ((tr, a, n) :: rest) x t ovar =
        if x = a
        then (a^(Int.toString n), ovar)
        else get_Variable rest x t ovar
  
  fun get_Type x [] p = raise Error ("Variable " ^ x ^ " not found", p)
    | get_Type x ((t, a, n) :: res) p =
        if x = a
        then t
        else get_Type x res p
  
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
  
  fun get_Size (Data.TypeS(_, s)) =
        case s of
          (Data.u8) => 8
        | (Data.u16) => 16
        | (Data.u32) => 32
        | (Data.u64) => 64
  
  fun comp_Upop Hermes.Add = "+"
    | comp_Upop Hermes.Sub = "-"
    | comp_Upop Hermes.XorWith = "^"
    | comp_Upop Hermes.RoL = "<<"
    | comp_Upop Hermes.RoR = ">>"

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
  
  fun use_LVal (Hermes.Var(y, p)) var =
        let
          val t = get_Type y var p
          val (y0, var2) = get_Variable var y t var
        in
          ([], [], (Data.VarS(y0, p)), var2)
        end
    | use_LVal (Hermes.Array(y, e, p)) var =
        let
          val t = get_Type (y^"T") var p
          val size = (get_Size t) div 8
          val var2 = incr_Variable var "T"
          val (T1, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2 
          val (indexCreate, var4) = comp_E (Hermes.Bin(Hermes.Times, e, Hermes.Const(Int.toString(size), p), p)) "+" (Data.TypeS(Data.Public, Data.u64)) T1 "0" p p var3 true
          val (indexEmpty, var5) = comp_E (Hermes.Bin(Hermes.Times, e, Hermes.Const(Int.toString(size), p), p)) "-" (Data.TypeS(Data.Public, Data.u64)) "0" T1 p p var4 true
          val var6 = incr_Variable var5 "I"
          val (I, var7) = get_Variable var6 "I" (Data.TypeS(Data.Public, Data.u64)) var6
          val var8 = incr_Variable var7 "T"
          val (T2, var9) = get_Variable var8 "T" (Data.TypeS(Data.Public, Data.u64)) var8
          val (yT, var10) = get_Variable var9 (y^"T") t var9
          val start = [Data.AssignS(Data.TypeS(Data.Public, Data.u64), Data.VarS(I, p), Data.UpdOp2S("+", Data.CstS("0", p), Data.Op2S("+", Data.VarS(yT, p), Data.VarS(T1, p), p), p), p),
                      Data.SwapS(Data.VarS(T2, p), Data.MemoryS(t, Data.VarS(I, p), p), Data.CstS("0", p), p)]
          val finish = [Data.SwapS(Data.CstS("0", p), Data.MemoryS(t, Data.VarS(I, p), p), Data.VarS(T2, p), p),
                        Data.AssignS(Data.TypeS(Data.Public, Data.u64), Data.CstS("0", p), Data.UpdOp2S("-", Data.VarS(I, p), Data.Op2S("+", Data.VarS(yT, p), Data.VarS(T1, p), p), p), p)]
        in
          (indexCreate@start, finish@indexEmpty, Data.VarS(T2, p), var10)
        end
  and comp_LVal (Hermes.Var(x, p)) var =
        let
          val t = get_Type x var p
          val (x0, var2) = get_Variable var x t var
          val var3 = incr_Variable var2 x
          val (x1, var4) = get_Variable var3 x t var3
        in
          ([], [], t, x1, x0, p, var4, true)
        end
    | comp_LVal (Hermes.Array(x, e, p)) var =
        let
          val t = get_Type (x^"T") var p
          val size = (get_Size t) div 8
          val var2 = incr_Variable var "T"
          val (T1, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val (indexCreate, var4) = comp_E (Hermes.Bin(Hermes.Times, e, Hermes.Const(Int.toString(size), p), p)) "+" (Data.TypeS(Data.Public, Data.u64)) T1 "0" p p var3 true
          val (indexEmpty, var5) = comp_E (Hermes.Bin(Hermes.Times, e, Hermes.Const(Int.toString(size), p), p)) "-" (Data.TypeS(Data.Public, Data.u64)) "0" T1 p p var4 true
          val var6 = incr_Variable var5 "I"
          val (I, var7) = get_Variable var6 "I" (Data.TypeS(Data.Public, Data.u64)) var6
          val (xT, var8) = get_Variable var7 (x^"T") t var7
          val start = [Data.AssignS(Data.TypeS(Data.Public, Data.u64), Data.VarS(I, p), Data.UpdOp2S("+", Data.CstS("0", p), Data.Op2S("+", Data.VarS(xT, p), Data.VarS(T1, p), p), p), p)]
          val finish = [Data.AssignS(Data.TypeS(Data.Public, Data.u64), Data.CstS("0", p), Data.UpdOp2S("-", Data.VarS(I, p), Data.Op2S("+", Data.VarS(xT, p), Data.VarS(T1, p), p), p), p)]
        in
          (indexCreate@start, finish@indexEmpty, t, I, "", p, var8, false)
        end
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
          val (yS, var2) = get_Variable var (y^"S") (Data.TypeS(Data.Public, Data.u64)) var
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
    | comp_E (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Const(i2, ep2), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val (exprD, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 true
          val (exprF, var5) = comp_E_contF (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T p2 p3 var4 true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (exprD@
              [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), Data.CstS(i2, ep2), p), p2), p3)]
              @exprF, var5)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (exprD@
              [Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.VarS(T, p), Data.CstS(i2, ep2), p), p2)]
              @exprF, var5)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Rval(lval), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val (exprD, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 true
          val (exprF, var5) = comp_E_contF (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T p2 p3 var4 true
          val (S, F, y, var6) = use_LVal lval var5
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (exprD@S@
              [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), y, p), p2), p3)]
              @F@exprF, var6)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (exprD@S@
              [Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.VarS(T, p), y, p), p3)]
              @F@exprF, var6)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Bin(e), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val (exprD, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 true
          val (exprF, var5) = comp_E_contF (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T p2 p3 var4 true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (exprD@
              [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.CstS(i1, ep1), Data.VarS(T, p), p), p2), p3)]
              @exprF, var5)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (exprD@
              [Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.CstS(i1, ep1), Data.VarS(T, p), p), p2)]
              @exprF, var5)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Rval(lval), Hermes.Bin(e), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val (exprD, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 true
          val (exprF, var5) = comp_E_contF (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T p2 p3 var4 true
          val (S, F, y, var6) = use_LVal lval var5
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (exprD@S@
              [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, y, Data.VarS(T, p), p), p2), p3)]
              @F@exprF, var6)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (exprD@S@
              [Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, y, Data.VarS(T, p), p), p2)]
              @F@exprF, var6)
            end
        end
    | comp_E (Hermes.Bin(bop, Hermes.Bin(e1), Hermes.Bin(e2), p)) upop t x x0 p2 p3 var bool =
        let
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T1, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val var4 = incr_Variable var3 "T"
          val (T2, var5) = get_Variable var4 "T" (Data.TypeS(Data.Public, Data.u64)) var4
          val (exprD1, var6) = comp_E_contD (Hermes.Bin(e1)) "^" (Data.TypeS(Data.Public, Data.u64)) T1 "0" p2 p3 var5 true
          val (exprD2, var7) = comp_E_contD (Hermes.Bin(e2)) "^" (Data.TypeS(Data.Public, Data.u64)) T2 "0" p2 p3 var6 true
          val (exprF1, var8) = comp_E_contF (Hermes.Bin(e1)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T1 p2 p3 var7 true
          val (exprF2, var9) = comp_E_contF (Hermes.Bin(e2)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T2 p2 p3 var8 true
        in
          if bool
          then
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
            in
              (exprD1@exprD2@
              [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T1, p), Data.VarS(T2, p), p), p2), p3)]
              @exprF2@exprF1, var9)
            end
          else
            let
              val xin = comp_Var x p2
            in
              (exprD1@exprD2@
              [Data.MemOp2S((upop^"="), Data.MemoryS(t, xin, p2), Data.Op2S(b, Data.VarS(T1, p), Data.VarS(T2, p), p), p2)]
              @exprF2@exprF1, var9)
            end
        end
  and comp_E_contD (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Const(i2, ep2), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val (exprD, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 bool
        in
          (exprD@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), Data.CstS(i2, ep2), p), p2), p3)], var4)
        end
    | comp_E_contD (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Rval(lval), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val (exprD, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 bool
          val (S, F, y, var5) = use_LVal lval var4
        in
          (exprD@S@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), y, p), p2), p3)], var5)
        end
    | comp_E_contD (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Bin(e), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val (exprD, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 bool
        in
          (exprD@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.CstS(i1, ep1), Data.VarS(T, p), p), p2), p3)], var4)
        end
    | comp_E_contD (Hermes.Bin(bop,  Hermes.Rval(lval), Hermes.Bin(e), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val (exprD, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3 bool
          val (S, F, y, var5) = use_LVal lval var4
        in
          (exprD@S@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, y, Data.VarS(T, p), p), p2), p3)], var5)
        end
    | comp_E_contD (Hermes.Bin(bop, Hermes.Bin(e1), Hermes.Bin(e2), p)) upop t x x0 p2 p3 var bool =
      let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T1, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val var4 = incr_Variable var3 "T"
          val (T2, var5) = get_Variable var4 "T" (Data.TypeS(Data.Public, Data.u64)) var4
          val (exprD1, var6) = comp_E_contD (Hermes.Bin(e1)) "^" (Data.TypeS(Data.Public, Data.u64)) T1 "0" p2 p3 var5 bool
          val (exprD2, var7) = comp_E_contD (Hermes.Bin(e2)) "^" (Data.TypeS(Data.Public, Data.u64)) T2 "0" p2 p3 var6 bool
        in
          (exprD1@exprD2@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T1, p), Data.VarS(T2, p), p), p2), p3)], var7)
        end
    | comp_E_contD e upop t x x0 p2 p3 var bool = comp_E e upop t x x0 p2 p3 var bool
  and comp_E_contF (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Const(i2, ep2), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val (T, var2) = get_Variable var "T" (Data.TypeS(Data.Public, Data.u64)) var
          val (exprF, var3) = comp_E_contF (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T p2 p3 var2 bool
        in
          ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), Data.CstS(i2, ep2), p), p2), p3)]
          @exprF, var3)
        end
    | comp_E_contF (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Rval(lval), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val (T, var2) = get_Variable var "T" (Data.TypeS(Data.Public, Data.u64)) var
          val (exprF, var3) = comp_E_contF (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T p2 p3 var2 bool
          val (S, F, y, var4) = use_LVal lval var3
        in
          ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), y, p), p2), p3)]
          @F@exprF, var4)
        end
    | comp_E_contF (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Bin(e), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val (T, var2) = get_Variable var "T" (Data.TypeS(Data.Public, Data.u64)) var
          val (exprF, var3) = comp_E_contF (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T p2 p3 var2 bool
        in
          ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.CstS(i1, ep1), Data.VarS(T, p), p), p2), p3)]
          @exprF, var3)
        end
    | comp_E_contF (Hermes.Bin(bop, Hermes.Rval(lval), Hermes.Bin(e), p)) upop t x x0 p2 p3 var bool =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val (T, var2) = get_Variable var "T" (Data.TypeS(Data.Public, Data.u64)) var
          val (exprF, var3) = comp_E_contF (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T p2 p3 var2 bool
          val (S, F, y, var4) = use_LVal lval var3
        in
          ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, y, Data.VarS(T, p), p), p2), p3)]
          @F@exprF, var4)
        end
    | comp_E_contF (Hermes.Bin(bop, Hermes.Bin(e1), Hermes.Bin(e2), p)) upop t x x0 p2 p3 var bool =
      let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val (T1, var2) = get_Variable var "T" (Data.TypeS(Data.Public, Data.u64)) var
          val (T2, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val (exprF1, var4) = comp_E_contF (Hermes.Bin(e1)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T1 p2 p3 var3 bool
          val (exprF2, var5) = comp_E_contF (Hermes.Bin(e2)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T2 p2 p3 var4 bool
        in
          ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T1, p), Data.VarS(T2, p), p), p2), p3)]
          @exprF2@exprF1, var5)
        end
    | comp_E_contF e upop t x x0 p2 p3 var bool = comp_E e upop t x x0 p2 p3 var bool

  fun comp_Args [] var res = (res, var)
    | comp_Args (Hermes.VarArg(x, t, p) :: pargs) var res =
        let
          val ty = comp_Type t
          val (atom, var2) = get_Variable var x ty var
        in
          comp_Args pargs var2 (res@[Data.ArgS(ty, Data.VarS(atom, p))])
        end
    | comp_Args (Hermes.ArrayArg(x, t, p) :: pargs) var res =
        let
          val ty = comp_Type t
          val (atomBegin, var2) = get_Variable var (x^"T") ty var
          val (atomSize, var3) = get_Variable var2 (x^"S") (Data.TypeS(Data.Public, Data.u64)) var2
        in
          comp_Args pargs var3 (res@[Data.ArgS(Data.TypeS(Data.Public, Data.u64), Data.VarS(atomBegin, p)),
                                    Data.ArgS(Data.TypeS(Data.Public, Data.u64), Data.VarS(atomSize, p))])
        end
  
  fun comp_S res (Hermes.Skip) var = (res, var)
    | comp_S res (Hermes.Update(upop, lv, e, p)) var =
        let
          val (start, finish, t, x, x0, p2, var2, bool) = comp_LVal lv var
          val u = comp_Upop upop
          val (expr, var3) = comp_E e u t x x0 p2 p var2 bool
        in
          (res@start@expr@finish, var3)
        end
    | comp_S res (Hermes.Swap(lval1, lval2, p)) var =
        let
          val (start1, finish1, t1, x1, x0, p1, var2, bool1) = comp_LVal lval1 var
          val (start2, finish2, t2, y1, y0, p2, var3, bool2) = comp_LVal lval2 var2
        in
          case (lval1, lval2) of
            (Hermes.Var(_), Hermes.Var(_)) =>
              (res@[Data.DAssignS(t1, Data.VarS(x1, p1), t2, Data.VarS(y1, p2), Data.VarS(y0, p2), Data.VarS(x0, p1), p)], var3)
        end
    | comp_S res (Hermes.Block(d, ss, p)) var =
        let
          val (stats, var2) = comp_Block res ss var
        in
          (stats, var2)
        end
  and comp_Block res [] var = (res, var)
    | comp_Block res (s :: ss) var =
        let
          val (stats, var2) = comp_S res s var
        in
          comp_Block stats ss var2
        end


  fun comp_Entry f pargs i p var =
        let
          val (args, var2) = comp_Args pargs var []
        in
          if i = 0
          then (Data.BeginS(("main", p), args, p), var2)
          else (Data.BeginS((f, p), args, p), var2)
        end
  
  fun comp_Exit f pargs i p var =
        let
          val (args, var2) = comp_Args pargs var []
        in
          if i = 0
          then (Data.EndS(("main", p), args, p), var2)
          else (Data.EndS((f, p), args, p), var2)
        end

  fun comp_P [] i = []
    | comp_P ((f, pars, s, p) :: ps) i =
        let
          val (entry, var) = comp_Entry f pars i p []
          val _ = TextIO.print("[" ^ printVar var)
          val (stats, var2) = comp_S [] s var
          val (exit, var3) = comp_Exit f pars i p var2
          val _ = TextIO.print("[" ^ printVar var3)
        in
          [Data.BlockS(entry, stats, exit, p)]@(comp_P ps (i+1))
        end

  fun compile pgm =
        let
          val blocks = comp_P pgm 0
        in
          Data.ProgramS(blocks)
        end

end
