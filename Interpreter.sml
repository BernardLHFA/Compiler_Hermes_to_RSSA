structure Interpreter :> Interpreter =
struct
  open BigInt;;
  
  exception Error of string*(int*int)

  val R = Data.R

  val s8 = []

  datatype location = Variable of hex ref
                    (*| Array of int * hex array
                    | ArrayElement of int * hex array
                    | NullLocation*)
  
  (* lookup in environment *)

  fun lookup x [] pos =
        raise Error ("undeclared identifier" ^ x, pos)
    | lookup x ((y,v) :: env) pos =
        if x = y then v else lookup x env pos

  fun lookup2 x [] =
        raise Error ("undeclared identifier" ^ x, (0, 0))
    | lookup2 x ((y,v) :: env) =
        if x = y then v else lookup2 x env

  fun rem_rho x [] = []
    | rem_rho x ((y, v) :: rho) =
        if x = y then rem_rho x rho else (y, v)::rem_rho x rho

  (* get position of variable *)
  fun get_Pos (s, p) = p

  (* get name of variable *)
  fun get_Var (s, p) = s

  fun BinOp bop z v1 v2 p =
        case bop of
          "+" => limitZ z (hAdd64 v1 v2)
        | "-" => limitZ z (hSub64 v1 v2)
        | "^" => limitZ z (hXor64 v1 v2)
        | "<<" => limitZ z (hShiftL64 v1 v2)
        | ">>" => limitZ z (hShiftR64 v1 v2)
        | "*" => limitZ z (hTimes64 v1 v2)
        | "div" => limitZ z (hDiv64 v1 v2 p)
        | "mod" => limitZ z (hMod64 v1 v2 p)
        | "&" => limitZ z (hAnd64 v1 v2)
        | "|" => limitZ z (hOr64 v1 v2)
        | _ => raise Error ("Simbol not allowed", p)
  
  fun UpBinOp bop z v1 v2 p =
        case bop of
          "+" => limitZ z (hAdd64 v1 v2)
        | "-" => limitZ z (hSub64 v1 v2)
        | "^" => limitZ z (hXor64 v1 v2)
        | "<<" => limitZ z (hShiftL64 v1 v2)
        | ">>" => limitZ z (hShiftR64 v1 v2)
        | _ => raise Error ("Simbol not allowed", p)

  fun do_O rho (Data.SimOp2S(a)) =
        (case a of
          (Data.VarS(v)) =>
            let
              val (_, loc) = lookup (get_Var v) rho (get_Pos v) 
            in
              loc
            end
        | (Data.CstS(c, _)) =>
            (Variable (ref (limitZ 64 (string2h c (0, 0))))))
    | do_O rho (Data.Op2S(bop, a1, a2, p)) =
        case (a1, a2) of
          (Data.VarS(v1), Data.VarS(v2)) =>
            let
              val (z1, Variable loc1) = lookup (get_Var v1) rho p
              val (z2, Variable loc2) = lookup (get_Var v2) rho p
              val lambda = BinOp bop (h2int z1) (!loc1) (!loc2) p
            in
              Variable (ref (lambda))
            end
        | (Data.VarS(v), Data.CstS(c, _)) =>
            let
              val (z, Variable loc) = lookup (get_Var v) rho p
              val lambda = BinOp bop (h2int z) (!loc) (limitZ 64 (string2h c (0, 0))) p
            in
              Variable (ref (lambda))
            end
        | (Data.CstS(c, _), Data.VarS(v)) =>
            let
              val (z, Variable loc) = lookup (get_Var v) rho p
              val lambda = BinOp bop (h2int z) (limitZ 64 (string2h c (0, 0))) (!loc) p
            in
              Variable (ref (lambda))
            end
        | (Data.CstS(c1, _), Data.CstS(c2, _)) =>
            let
              val lambda = BinOp bop 64 (limitZ 64 (string2h c1 (0, 0))) (limitZ 64 (string2h c2 (0, 0))) p
            in
              Variable (ref (lambda))
            end

  fun do_U rho (Data.UpdOp2S(bop, a, e, p)) =
        let
          val (Variable loc) = do_O rho e
        in
          case a of
            (Data.VarS(v)) =>
              let
                val (z0, Variable loc2) = lookup (get_Var v) rho p
                val rho2 = rem_rho (get_Var v) rho
                val lambda = UpBinOp bop (h2int z0) (!loc2) (!loc) p
              in
                (rho2, z0, Variable (ref (lambda)))
              end
          | (Data.CstS(c, _)) =>
              let
                val lambda = UpBinOp bop 64 (!loc) (limitZ 64 (string2h c (0, 0))) p
              in
                (rho, (int2h 64), Variable (ref (lambda)))
              end
        end

  fun do_S gamma rho [] = rho
    | do_S gamma rho (Data.AssignS(t, a, u, p) :: s) =
        let
          val (rho2, z0, Variable loc) = do_U rho u
        in
          case a of
            (Data.VarS(v)) =>
              do_S gamma (rho2@[((get_Var v), (z0, Variable loc))]) s
          | (Data.CstS(c, _)) =>
              if (limitZ 64 (string2h c (0, 0))) = !loc
              then do_S gamma rho2 s
              else raise Error ("Operation does not equal the desired constant", p)
        end
    | do_S gamma rho (Data.Assign2S(t, a1, a2, p) :: s) =
        (case a2 of
          (Data.VarS(v)) =>
            let
              val (z0, Variable loc) = lookup (get_Var v) rho p
              val rho2 = rem_rho (get_Var v) rho
            in
              (case a1 of
                (Data.VarS(v2)) =>
                  do_S gamma (rho2@[((get_Var v2), (z0, Variable loc))]) s
              | (Data.CstS(c, _)) =>
                  if (limitZ 64 (string2h c (0, 0))) = !loc
                  then do_S gamma rho2 s
                  else raise Error ("Variable should be equal to constant", p))
            end
        | (Data.CstS(c, _)) =>
            (case a1 of
              (Data.VarS(v2)) =>
                do_S gamma (rho@[((get_Var v2), ((int2h 64), Variable (ref (limitZ 64 (string2h c (0, 0))))))]) s
            | (Data.CstS(c2, _)) =>
                if c = c2
                then do_S gamma rho s
                else raise Error ("Constants are not equal", p)))
    | do_S gamma rho (Data.DAssignS(t1, a1, t2, a2, a3, a4, p) :: s) = do_S gamma rho s
    | do_S gamma rho (Data.MemOp2S(st, m, e, p) :: s) = do_S gamma rho s
    | do_S gamma rho (Data.MemSwapS(m1, m2, p) :: s) = do_S gamma rho s
    | do_S gamma rho (Data.SwapS(a1, m, a2, p) :: s) = do_S gamma rho s
    | do_S gamma rho (Data.AssignArgS(a, c, p) :: s) = do_S gamma rho s
    | do_S gamma rho (Data.TAssignS(t, a, tc, p) :: s) = do_S gamma rho s

  fun get_Args [] = []
    | get_Args (Data.ArgS(t, a) :: args) =
        case a of
          (Data.VarS(v)) => (t, (get_Var v))::get_Args args
        | (Data.CstS(_, _)) => get_Args args

  fun rho_Args [] = []
    | rho_Args (Data.ArgS(Data.TypeS(_, t), a) :: args) = 
        case a of
          (Data.VarS(x)) =>
            let
              val z = case t of
                        Data.u8 => 8
                      | Data.u16 => 16
                      | Data.u32 => 32
                      | Data.u64 => 64
              val str = valOf (TextIO.inputLine TextIO.stdIn)
              val v = limitZ z (string2h str (0,0))
              val loc = (int2h z, Variable (ref v))
            in
              ((get_Var x), loc)::rho_Args args
            end
        | (Data.CstS(s, p)) => rho_Args args

  fun start_rho_exit (Data.EndS(f, args, p)) = rho_Args args
    | start_rho_exit (Data.UncondExitS(f, args, p)) = raise Error ("Starting exit must be an end", p)
    | start_rho_exit (Data.CondExitS(c, f1, f2, args, p)) = raise Error ("Starting exit must be an end", p)
  
  fun start_rho_entry (Data.BeginS(f, args, p)) = rho_Args args
    | start_rho_entry (Data.UncondEntryS(f, args, p)) = raise Error ("Starting entry must be an begin", p)
    | start_rho_entry (Data.CondEntryS(c, f1, f2, args, p)) = raise Error ("Starting entry must be an begin", p)

  fun do_B (Data.BlockS(en, ss, ex, p)) backwards gamma =
        if backwards
        then
          let
            val rho = start_rho_exit ex
          in
            do_S gamma rho ss
          end
        else
          let
            val rho = start_rho_entry en
          in
            do_S gamma rho ss
          end

  fun do_Exit (Data.EndS(f, args, p)) ss = [(false, f, (args, ss))]
    | do_Exit (Data.UncondExitS(f, args, p)) ss = [(false, f, (args, ss))]
    | do_Exit (Data.CondExitS(c, f1, f2, args, p)) ss = [(false, f1, (args, ss)), (false, f2, (args, ss))]

  fun do_Entry (Data.BeginS(f, args, p)) ss = [(true, f, (args, ss))]
    | do_Entry (Data.UncondEntryS(f, args, p)) ss = [(true, f, (args, ss))]
    | do_Entry (Data.CondEntryS(c, f1, f2, args, p)) ss = [(true, f1, (args, ss)), (true, f2, (args, ss))]

  fun do_G [] = []
    | do_G (Data.BlockS(en, ss, ex, p) :: ps) =
        let
          val gamma1 = do_Entry en ss
          val gamma2 = do_Exit ex ss
          val gamma = gamma1@gamma2
        in
          gamma :: do_G ps
        end
  
  fun do_P_backwards [] backwards gamma = raise Error ("Main function doesn't have correct end statement", (0, 0))
    | do_P_backwards (Data.BlockS(en, ss, ex, p) :: ps) backwards gamma =
        case ex of
          (Data.EndS(f, args, p2)) =>
            if (get_Var f) = "main"
            then do_B (Data.BlockS(en, (List.rev (List.map R ss)), ex, p)) backwards gamma
            else do_P_backwards ps backwards gamma
        | _ => do_P_backwards ps backwards gamma

  fun get_Args_final_end [] = raise Error ("Main function doesn't have correct end statement", (0, 0))
    | get_Args_final_end (Data.BlockS(en, ss, ex, p) :: ps) = 
        case ex of
          (Data.EndS(f, args, p2)) =>
            if (get_Var f) = "main"
            then get_Args args
            else get_Args_final_end ps
        | _ => get_Args_final_end ps

  fun do_P [] backwards gamma = raise Error ("No Main function", (0, 0))
    | do_P (Data.BlockS(en, ss, ex, p) :: ps) backwards gamma =
        if backwards
        then
          case en of
            (Data.BeginS(f, args, p2)) =>
              if (get_Var f) = "main"
              then ((do_P_backwards (Data.BlockS(en, ss, ex, p) :: ps) backwards gamma), (get_Args args))
              else do_P ps backwards gamma
          | _ => do_P ps backwards gamma
        else
          case en of
            (Data.BeginS(f, args, p2)) =>
              if (get_Var f) = "main"
              then ((do_B (Data.BlockS(en, ss, ex, p)) backwards gamma), (get_Args_final_end (Data.BlockS(en, ss, ex, p) :: ps)))
              else do_P ps backwards gamma
          | _ => do_P ps backwards gamma
  
  (* print parameter *)
  fun printPar rho (t, x) =
        let
          val (z, Variable l) = lookup2 x rho (* nonexhaustive *)
        in
          TextIO.print (h2hString (!l) ^ "\n")
        end

  fun run (Data.ProgramS(prg)) backwards = 
    let
      val gamma = do_G prg
      val (rho, args) = do_P prg backwards gamma
    in
      List.app (printPar rho) args
      (*do_P prg backwards gamma*)
      (*List.app (do_P prg backwards) gamma*)
    end

end