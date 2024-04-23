(* Compiler *)

structure Compiler :> Compiler =
struct

  exception Error of string*(int*int)

  fun printVar [] = "]\n"
    | printVar ((a,n) :: rest) =
        a^(Int.toString n)^", "^printVar rest

  fun store_Variable x var =
        var@[(x, 0)]

  fun get_Variable [] x ovar= 
        let
          val var2 = (store_Variable x ovar)
        in
          (x^"0", var2)
        end
    | get_Variable ((a, n) :: rest) x ovar =
        if x = a
        then 
          let
            val _ = TextIO.print("hello\n")
          in
            (a^(Int.toString n), ovar)
          end
        else get_Variable rest x ovar

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

  fun comp_Args [] var res = (res, var)
    | comp_Args (Hermes.VarArg(x, t, p) :: pargs) var res =
        let
          val ty = comp_Type t
          val (atom, var2) = get_Variable var x var
        in
          comp_Args pargs var2 (res@[Data.ArgS(ty, Data.VarS(atom, p))])
        end
  
  fun comp_S s var = ([], var)

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
          val (stats, var2) = comp_S s var
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
