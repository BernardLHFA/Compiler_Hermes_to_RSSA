structure Interpreter :> Interpreter =
struct
  open BigInt;;
  
  exception Error of string*(int*int)

  val R = Data.R

  (* get name of variable *)
  fun get_Var (s, p) = s

  fun do_S gamma rho ss = ()

  fun rho_Args [] = []
    | rho_Args (Data.ArgS(t, a) :: args) = 
        case a of
          (Data.VarS(v)) => (t, (get_Var v))::rho_Args args
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

  fun do_P [] backwards gamma = raise Error ("No Main function", (0, 0))
    | do_P (Data.BlockS(en, ss, ex, p) :: ps) backwards gamma =
        if backwards
        then
          case ex of
            (Data.EndS(f, args, p2)) =>
              if (get_Var f) = "main"
              then do_B (Data.BlockS(en, (List.rev (List.map R ss)), ex, p)) backwards gamma
              else do_P ps backwards gamma
          | _ => do_P ps backwards gamma
        else
          case en of
            (Data.BeginS(f, args, p2)) =>
              if (get_Var f) = "main"
              then do_B (Data.BlockS(en, ss, ex, p)) backwards gamma
              else do_P ps backwards gamma
          | _ => do_P ps backwards gamma

  fun run (Data.ProgramS(prg)) backwards = 
    let
      val gamma = do_G prg
    in
      List.app (do_P prg backwards) gamma
    end

end