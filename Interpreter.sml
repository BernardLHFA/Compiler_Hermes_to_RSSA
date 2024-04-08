structure Interpreter :> Interpreter =
struct
  open BigInt;;
  
  exception Error of string*(int*int)

  (* get name of variable *)
  fun get_Var (s, p) = s

  fun run [] backwards = ()
    | run (Data.ProgramS(Data.BlockS(en, ss, ex, p)) :: ps) backwards =
        if backwards
        then
          case ex of
            (Data.EndS(f, args, p2)) =>
              if (get_Var f) = "main"
              then do_B Data.BlockS(en, (List.rev (List.map R ss)), ex, p)
              else run ps backwards
          | _ => run ps backwards
        else
          case en of
            (Data.BeginS(f, args, p2)) =>
              if (get_Var f) = "main"
              then do_B Data.BlockS(en, ss, ex, p)
              else run ps backwards
          | _ => run ps backwards

end