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
        then 
          let
            val _ = TextIO.print("hello\n")
          in
            (a^(Int.toString n), ovar)
          end
        else get_Variable rest x t ovar
  
  fun get_Type x [] p = raise Error ("Variable " ^ x ^ " not found", p)
    | get_Type x ((t, a, n) :: res) p =
        if x = a
        then t
        else get_Type x res p
  
  fun comp_Var x p =
        case String.sub(x, 0) of
          #"0" => 
            let
              val _ =TextIO.print("hi\n");
            in
              Data.CstS(x, p)
            end
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
  
  fun comp_LVal (Hermes.Var(x, p)) var =
        let
          val t = get_Type x var p
          val (x0, var2) = get_Variable var x t var
          val var3 = incr_Variable var x
          val (x1, var4) = get_Variable var3 x t var3
        in
          (t, x1, x0, p, var4)
        end
  and comp_E (Hermes.Const(i, p)) upop t x x0 p2 p3 var =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
        in
          ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.SimOp2S(Data.CstS(i, p)), p2), p3)], var)
        end
    | comp_E (Hermes.Rval(lval)) upop t x x0 p2 p3 var =
        (case lval of
          (Hermes.Var(y, p)) =>
            let
              val xout = comp_Var x0 p2
              val xin = comp_Var x p2
              val t2 = get_Type y var p
              val (y0, var2) = get_Variable var y t2 var
            in
              ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.SimOp2S(Data.VarS(y0, p)), p2), p3)], var2)
            end)
    | comp_E (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Const(i2, ep2), p)) upop t x x0 p2 p3 var =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
        in
          ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.CstS(i1, ep1), Data.CstS(i2, ep2), p), p2), p3)], var)
        end
    | comp_E (Hermes.Bin(bop, Hermes.Const(i1, ep1), Hermes.Rval(Hermes.Var(y, ep2)), p)) upop t x x0 p2 p3 var =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val t2 = get_Type y var ep2
          val (y0, var2) = get_Variable var y t2 var
        in
          ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.CstS(i1, ep1), Data.VarS(y0, ep2), p), p2), p3)], var2)
        end
    | comp_E (Hermes.Bin(bop, Hermes.Rval(Hermes.Var(y, ep1)), Hermes.Const(i2, ep2), p)) upop t x x0 p2 p3 var =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val t2 = get_Type y var ep1
          val (y0, var2) = get_Variable var y t2 var
        in
          ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(y0, ep1), Data.CstS(i2, ep2), p), p2), p3)], var2)
        end
    | comp_E (Hermes.Bin(bop, Hermes.Rval(Hermes.Var(y1, ep1)), Hermes.Rval(Hermes.Var(y2, ep2)), p)) upop t x x0 p2 p3 var =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val t12 = get_Type y1 var ep1
          val (y10, var2) = get_Variable var y1 t12 var
          val t22 = get_Type y2 var ep2
          val (y20, var3) = get_Variable var y2 t22 var2
        in
          ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(y10, ep1), Data.VarS(y20, ep2), p), p2), p3)], var3)
        end
    | comp_E (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Const(i2, ep2), p)) upop t x x0 p2 p3 var =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val (exprD, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3
          val (exprF, var5) = comp_E_contF (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T p2 p3 var4
        in
          (exprD@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), Data.CstS(i2, ep2), p), p2), p3)]
          @exprF, var4)
        end
  and comp_E_contD (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Const(i2, ep2), p)) upop t x x0 p2 p3 var =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val var2 = incr_Variable var "T"
          val (T, var3) = get_Variable var2 "T" (Data.TypeS(Data.Public, Data.u64)) var2
          val (exprD, var4) = comp_E_contD (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) T "0" p2 p3 var3
        in
          (exprD@
          [Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), Data.CstS(i2, ep2), p), p2), p3)], var4)
        end
    | comp_E_contD e upop t x x0 p2 p3 var = comp_E e upop t x x0 p2 p3 var
  and comp_E_contF (Hermes.Bin(bop, Hermes.Bin(e), Hermes.Const(i2, ep2), p)) upop t x x0 p2 p3 var =
        let
          val xout = comp_Var x0 p2
          val xin = comp_Var x p2
          val b = comp_BinOp bop
          val (T, var2) = get_Variable var "T" (Data.TypeS(Data.Public, Data.u64)) var
          val (exprF, var3) = comp_E_contF (Hermes.Bin(e)) "^" (Data.TypeS(Data.Public, Data.u64)) "0" T p2 p3 var2
        in
          ([Data.AssignS(t, xin, Data.UpdOp2S(upop, xout, Data.Op2S(b, Data.VarS(T, p), Data.CstS(i2, ep2), p), p2), p3)]
          @exprF, var3)
        end
    | comp_E_contF e upop t x x0 p2 p3 var = comp_E e upop t x x0 p2 p3 var

  fun comp_Args [] var res = (res, var)
    | comp_Args (Hermes.VarArg(x, t, p) :: pargs) var res =
        let
          val ty = comp_Type t
          val (atom, var2) = get_Variable var x ty var
        in
          comp_Args pargs var2 (res@[Data.ArgS(ty, Data.VarS(atom, p))])
        end
  
  fun comp_S res (Hermes.Skip) var = (res, var)
    | comp_S res (Hermes.Update(upop, lv, e, p)) var =
        let
          val (t, x, x0, p2, var2) = comp_LVal lv var
          val u = comp_Upop upop
          val (expr, var3) = comp_E e u t x x0 p2 p var2
        in
          (res@expr, var3)
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
