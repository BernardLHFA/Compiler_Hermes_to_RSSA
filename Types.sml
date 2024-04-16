structure Types :> Types =
struct

  exception Error of string*(int*int)

  (* lookup in environment *)
  fun lookup x [] =
        false
    | lookup x ((t, a) :: env) =
        if x = a then true else lookup x env
  
  (* lookup in gamma*)
  fun lookup_G x [] =
        false
    | lookup_G x ((b, f, arg) :: gamma) =
        if x = f then true else lookup_G x gamma


  (* check if value is in list *)
  fun isIn x xs = List.exists (fn y => x = y) xs

  (* remove a variable from the environment*)
  fun remove_rho a [] p = raise Error ("Variable " ^ a ^ " is not defined in the environment", p)
    | remove_rho a ((t, b) :: rho) p =
        if a = b
        then rho
        else [(t, b)]@(remove_rho a rho p)

  (* get name of variable *)
  fun get_Var (s, p) = s

  (* get position of variable *)
  fun get_Pos (s, p) = p

  (* get type of memory access *)
  fun get_Type_M (Data.MemoryS(t, a, p)) = t

  (* check if type is secret *)
  fun isSecret (Data.TypeS(Data.Secret, i)) = true
    | isSecret (Data.TypeS(Data.Public, i)) = false

  (* check if type if public *)
  fun isPublic (Data.TypeS(Data.Public, i)) = true
    | isPublic (Data.TypeS(Data.Secret, i)) = false

  (* transforms string in argument list into proper argument *)
  fun string_to_arg [] p = []
    | string_to_arg ((t, a) :: arg) p = [Data.ArgS(t, Data.VarS(a, p))]@(string_to_arg arg p)

  (* get list of arguments *)
  fun get_Args x b p [] = raise Error ("Function not defined", p)
    | get_Args x b1 p ((b2, f, arg) :: gamma) =
        if x = f
        then
          if b1 = b2
          then string_to_arg arg p
          else get_Args x b1 p gamma
        else get_Args x b1 p gamma

  (* remove argument from environment *)
  fun remove_Args [] rho = rho
    | remove_Args ((Data.ArgS(t, a) :: args)) rho =
        case a of
          (Data.VarS(v)) => remove_Args args (remove_rho (get_Var v) rho (get_Pos v))
        | (Data.CstS(_, _)) => remove_Args args rho
  
  (* get list of arguments, used in entry and exit blocks *)
  fun get_List [] = []
    | get_List (Data.ArgS(t, a) :: args) =
        case a of
          (Data.VarS(v)) => [(t, get_Var(v))]@(get_List args)
        | (Data.CstS(_, _)) => get_List args

  (* get the call or uncall function *)
  fun get_Call_function (Data.CallS(f, arg, p)) = 
        ((get_Var f), false)
    | get_Call_function (Data.UncallS(f, arg, p)) = 
        ((get_Var f), true)

  (* get type of variable *)
  fun get_Type (Data.CstS(s, p)) rho = Data.TypeS(Data.Public, Data.u64)
    | get_Type (Data.VarS(v)) [] = raise Error ("Variable " ^ (get_Var v) ^ " not defined", (get_Pos v))
    | get_Type (Data.VarS(v)) ((t, b) :: rho) =
        if (get_Var v) = b
        then t
        else get_Type (Data.VarS(v)) rho

  (* check if atom can be used withouth modifying the environment *)
  fun check_Use rho (Data.CstS(s, p)) = true
    | check_Use rho (Data.VarS(v)) =
        lookup (get_Var v) rho

  (* check if atom can be used with modification to the environment *)
  fun check_A rho t (Data.CstS(s, p)) = rho
    | check_A rho t (Data.VarS(v)) =
        if lookup (get_Var v) rho
        then raise Error ("Variable " ^ (get_Var v) ^ " already defined", (get_Pos v))
        else rho@[(t, (get_Var v))]
  
  (* check if atom can be removed from environment *)
  fun check_A_rem rho (Data.CstS(s, p)) = rho
    | check_A_rem rho (Data.VarS(v)) =
        if lookup (get_Var v) rho
        then remove_rho (get_Var v) rho (get_Pos v)
        else raise Error ("Variable " ^ (get_Var v) ^ " not defined" , (get_Pos v))

  (* check if condition is valid *)
  fun check_Cond rho (Data.BoolOp2S(s, a1, a2, p)) =
        if check_Use rho a1 andalso check_Use rho a2
        then 
          let
            val t1 = get_Type a1 rho
            val t2 = get_Type a2 rho
          in
            if isPublic t1
            then
              if isPublic t2
              then true
              else false
            else false
          end
        else false

  (* check if argument can be used, modifying the environment *)
  fun check_Arg rho [] = rho
    | check_Arg rho (Data.ArgS(t, a) :: arg) =
        case a of
          Data.VarS(v) =>
            if lookup (get_Var v) rho
            then raise Error ("Argument " ^ (get_Var v) ^ " not defined" , (get_Pos v))
            else check_Arg (rho@[(t, (get_Var v))]) arg
        | Data.CstS(_, _) =>
            check_Arg rho arg
  
  (* check if two argument lists have the same types *)
  fun check_Type_Args [] [] = true
    | check_Type_Args args [] = false
    | check_Type_Args [] args = false
    | check_Type_Args (Data.ArgS(t1, a1) :: args1) (Data.ArgS(t2, a2) :: args2) = 
        if t1 = t2
        then check_Type_Args args1 args2
        else false
  
  (* check if memory is valid*)
  fun check_M rho (Data.MemoryS(t, a, p)) =
        let
          val t2 = get_Type a rho
        in
          if t2 = (Data.TypeS(Data.Public, Data.u64))
          then
            (case a of
              Data.VarS(v) =>
                if lookup (get_Var v) rho
                then true
                else false
            | Data.CstS(_, p) => true)
          else raise Error ("Type of index not u64", p)
        end
  
  (* check if reveal and hide functions are valid *)
  fun check_TChange rho (Data.RevealS(t, a, p)) =
        if t = get_Type a rho
        then
          let
            val rho2 = check_A_rem rho a
          in
            (t, rho2, true)
          end
        else raise Error ("Mismatched types", p)
    | check_TChange rho (Data.HideS(t, a, p)) =
        if t = get_Type a rho
        then
          let
            val rho2 = check_A_rem rho a
          in
            (t, rho2, false)
          end
        else raise Error ("Mismatched types", p)

  (* check if call and uncall functions are valid*)
  fun check_C gamma rho (Data.CallS(a, args, p)) =
        if lookup_G (get_Var a) gamma
        then 
          let
            val args2 = get_Args (get_Var a) true  p gamma 
          in
            if (length args) = (length args2)
            then
              if check_Type_Args args args2
              then remove_Args args rho
              else raise Error ("Arguments don't respect the types", p)
            else raise Error ("Length of arguments is not respected", p)
          end
        else raise Error ("Function " ^ (get_Var a) ^ " not defined", p)
    | check_C gamma rho (Data.UncallS(a, args, p)) =
        if lookup_G (get_Var a) gamma
        then 
          let
            val args2 = get_Args (get_Var a) false  p gamma 
          in
            if (length args) = (length args2)
            then
              if check_Type_Args args args2
              then remove_Args args rho
              else raise Error ("Arguments don't respect the types", p)
            else raise Error ("Length of arguments is not respected", p)
          end
        else raise Error ("Function " ^ (get_Var a) ^ " not defined", p)
  
  (* check if operations are valid *)
  fun check_O rho (Data.SimOp2S(a)) =
        let
          val t = get_Type a rho
        in
          if isPublic t
          then
            if check_Use rho a
            then ("public", rho)
            else 
              (case a of
                Data.VarS(v) => raise Error ("Variable " ^ (get_Var v) ^ " not defined" , (get_Pos v))
              | Data.CstS(_, p) => raise Error ("How did you get here? check_O simgle atom", p))
          else
            if check_Use rho a
            then ("secret", rho)
            else 
              (case a of
                Data.VarS(v) => raise Error ("Variable " ^ (get_Var v) ^ " not defined" , (get_Pos v))
              | Data.CstS(_, p) => raise Error ("How did you get here? check_O simgle atom", p))
        end
    | check_O rho (Data.Op2S(s, a1, a2, p)) =
        let
          val t1 = get_Type a1 rho
          val t2 = get_Type a2 rho
        in
          if isPublic t1
          then
            if isPublic t2
            then
              if check_Use rho a1
              then
                if check_Use rho a2
                then ("public", rho)
                else
                  (case a2 of
                    Data.VarS(v) => raise Error ("Variable " ^ (get_Var v) ^ " not defined" , (get_Pos v))
                  | Data.CstS(_, p) => raise Error ("How did you get here? check_O multiple atoms", p))
              else
                (case a1 of
                  Data.VarS(v) => raise Error ("Variable " ^ (get_Var v) ^ " not defined" , (get_Pos v))
                | Data.CstS(_, p) => raise Error ("How did you get here? check_O multiple atoms", p))
            else
              if check_Use rho a1
              then
                if check_Use rho a2
                then ("secret", rho)
                else
                  (case a2 of
                    Data.VarS(v) => raise Error ("Variable " ^ (get_Var v) ^ " not defined" , (get_Pos v))
                  | Data.CstS(_, p) => raise Error ("How did you get here? check_O multiple atoms", p))
              else
                (case a1 of
                  Data.VarS(v) => raise Error ("Variable " ^ (get_Var v) ^ " not defined" , (get_Pos v))
                | Data.CstS(_, p) => raise Error ("How did you get here? check_O multiple atoms", p))
          else
            if check_Use rho a1
            then
              if check_Use rho a2
              then ("secret", rho)
              else
                (case a2 of
                  Data.VarS(v) => raise Error ("Variable " ^ (get_Var v) ^ " not defined" , (get_Pos v))
                | Data.CstS(_, p) => raise Error ("How did you get here? check_O multiple atoms", p))
            else
              (case a1 of
                Data.VarS(v) => raise Error ("Variable " ^ (get_Var v) ^ " not defined" , (get_Pos v))
              | Data.CstS(_, p) => raise Error ("How did you get here? check_O multiple atoms", p))
        end

  (* check if updates are valid *)
  fun check_U rho (Data.UpdOp2S(s, a, e, p)) =
        let
          val t = get_Type a rho
          val rho2 = check_A_rem rho a
          val (ptype, rho3) = check_O rho2 e
        in
          if ptype = "public"
          then (t, rho3)
          else
            if isSecret t
            then (t, rho3)
            else raise Error ("Type must be secret", p)
        end

  (* Check statements *)
  fun check_S gamma rho [] = rho
  (* Simple Assignment of atom *)
    | check_S gamma rho (Data.AssignS(t, a, u, p) :: s) =
        let
          val (t2, rho2) = check_U rho u
          val rho3 = check_A rho2 t a
        in
          if t = t2
          then check_S gamma rho3 s
          else raise Error ("Atoms must have the same type", p)
        end
    (* Assignment with single atom *)
    | check_S gamma rho (Data.Assign2S(t, a1, a2, p) :: s) =
        let
          val t2 = get_Type a2 rho
          val rho2 = check_A_rem rho a2
          val rho3 = check_A rho2 t a1
        in
          if t = t2
          then
            (case (a1, a2) of
              (Data.VarS(v1), Data.VarS(v2)) =>
                if (get_Var v1) = (get_Var v2)
                then raise Error ("Same variable", p)
                else check_S gamma rho3 s
            | _ => check_S gamma rho3 s)
          else raise Error ("Atoms must have the same type", p)
        end
    (* Double Assignment *)
    | check_S gamma rho (Data.DAssignS(t1, a1, t2, a2, a3, a4, p) :: s) = 
        let
          val t3 = get_Type a3 rho
          val t4 = get_Type a4 rho
          val rho2 = check_A_rem rho a3
          val rho3 = check_A_rem rho2 a4
          val rho4 = check_A rho3 t1 a1
          val rho5 = check_A rho4 t2 a2
        in
          if t1 = t3
          then
            if t2 = t4
            then check_S gamma rho5 s
            else raise Error ("Atoms must have the same type", p)
          else raise Error ("Atoms must have the same type", p)
        end
    (* Memory Update *)
    | check_S gamma rho (Data.MemOp2S(st, m, e, p) :: s) =
        let
          val (ptype, rho2) = check_O rho e
          val tm = get_Type_M m
        in
          if isSecret tm
          then
            if check_M rho2 m
            then check_S gamma rho2 s
            else raise Error ("Memory not well handled", p)
          else
            if ptype = "public"
            then
              if check_M rho2 m
              then check_S gamma rho2 s
              else raise Error ("Memory not well handled", p)
            else raise Error ("Type of memory can't be public", p)
        end
    (* Memory Swap *)
    | check_S gamma rho (Data.MemSwapS(m1, m2, p) :: s) =
        let
          val t1 = get_Type_M m1
          val t2 = get_Type_M m2
        in
          if t1 = t2
          then
            if check_M rho m1
            then
              if check_M rho m2
              then check_S gamma rho s
              else raise Error ("Memory not well handled", p)
            else raise Error ("Memory not well handled", p)
          else raise Error ("Memory types do not match", p)
        end
    (* Variable-Memory Swap*)
    | check_S gamma rho (Data.SwapS(a1, m, a2, p) :: s) =
        let
          val t = get_Type a2 rho
          val rho2 = check_A_rem rho a2
          val rho3 = check_A rho2 t a1
        in
          if t = (get_Type_M m)
          then
            if check_M rho m
            then check_S gamma rho3 s
            else raise Error ("Memory not well handled", p)
          else raise Error ("Types of atoms do not correspond to memory", p)
        end
    (* Call and uncall functions on arguments *)
    | check_S gamma rho (Data.AssignArgS(args, c, p) :: s) =
        let
          val (f, b) = get_Call_function c
          val rho2 = check_C gamma rho c
          val args2 = get_Args f b p gamma
          val list = get_List args
        in
          if (length args) = (length args2)
          then
            if check_Type_Args args args2
            then check_S gamma (rho2@list) s
            else raise Error ("Arguments don't respect the types", p)
          else raise Error ("Length of arguments is not respected", p)
        end
    (* Reveal and Hide *)
    | check_S gamma rho (Data.TAssignS(t, a, tc, p) :: s) =
        let
          val (t2, rho2, b) = check_TChange rho tc
          val rho3 = check_A rho2 t a
        in
          (case b of
            true =>
              (case t2 of
                (Data.TypeS(Data.Secret, Data.u8)) =>
                  if t = Data.TypeS(Data.Public, Data.u8)
                  then check_S gamma rho3 s
                  else raise Error ("Types mismatched for reveal", p)
              | (Data.TypeS(Data.Secret, Data.u16)) =>
                  if t = Data.TypeS(Data.Public, Data.u16)
                  then check_S gamma rho3 s
                  else raise Error ("Types mismatched for reveal", p)
              | (Data.TypeS(Data.Secret, Data.u32)) =>
                  if t = Data.TypeS(Data.Public, Data.u32)
                  then check_S gamma rho3 s
                  else raise Error ("Types mismatched for reveal", p)
              | (Data.TypeS(Data.Secret, Data.u64)) =>
                  if t = Data.TypeS(Data.Public, Data.u64)
                  then check_S gamma rho3 s
                  else raise Error ("Types mismatched for reveal", p)
              | _ => raise Error ("Reveal can't be done on a public variable", p))
          | false =>
              case t2 of
                Data.TypeS(Data.Public, Data.u8) =>
                  if t = Data.TypeS(Data.Secret, Data.u8)
                  then check_S gamma rho3 s
                  else raise Error ("Types mismatched for hide", p)
              | (Data.TypeS(Data.Public, Data.u16)) =>
                  if t = Data.TypeS(Data.Secret, Data.u16)
                  then check_S gamma rho3 s
                  else raise Error ("Types mismatched for hide", p)
              | (Data.TypeS(Data.Public, Data.u32)) =>
                  if t = Data.TypeS(Data.Secret, Data.u32)
                  then check_S gamma rho3 s
                  else raise Error ("Types mismatched for hide", p)
              | (Data.TypeS(Data.Public, Data.u64)) =>
                  if t = Data.TypeS(Data.Secret, Data.u64)
                  then check_S gamma rho3 s
                  else raise Error ("Types mismatched for hide", p)
              | _ => raise Error ("Hide can't be done on a secret variable", p))
        end

  (* check if exit jumps are valid *)
  fun check_Exit rho (Data.EndS(f, a, p)) =
      let
        val args = check_Arg rho a
        val var = get_Var f
      in
        [(var, args)]
      end
    | check_Exit rho (Data.UncondExitS(f, a, p)) =
      let
        val args = check_Arg rho a
        val var = get_Var f
      in
        [(var, args)]
      end
    | check_Exit rho (Data.CondExitS(c, f1, f2, a, p)) =
      let
        val args = check_Arg rho a
        val var1 = get_Var f1
        val var2 = get_Var f2
      in
        [(var1, args)]@[(var2, args)]
      end

  (* check if entry jumps are valid *)
  fun check_Entry rho (Data.BeginS(f, a, p)) =
      let
        val args = check_Arg rho a
        val var = get_Var f
      in
        [(var, args)]
      end
    | check_Entry rho (Data.UncondEntryS(f, a, p)) =
      let
        val args = check_Arg rho a
        val var = get_Var f
      in
        [(var, args)]
      end
    | check_Entry rho (Data.CondEntryS(c, f1, f2, a, p)) =
      let
        val args = check_Arg rho a
        val var1 = get_Var f1
        val var2 = get_Var f2
      in
        if check_Cond args c
        then [(var1, args)]@[(var2, args)]
        else raise Error ("Entry Condition invalid", p)
      end

  (* beginning check for entry jumps *)
  fun check_BEn gamma [] = gamma
    | check_BEn gamma (Data.BlockS(en, s, ex, p) :: b) =
      let
        val rho = check_Entry [] en
      in
        check_BEn (gamma@(List.map (fn (f, a) => (true, f, a)) rho)) b
      end
  
  (* beginning check for exit jumps *)
  fun check_BEx gamma [] = gamma
    | check_BEx gamma (Data.BlockS(en, s, ex, p) :: b) =
      let
        val rho = check_Exit [] ex
      in
        check_BEx (gamma@(List.map (fn (f, a) => (false, f, a)) rho)) b
      end

  (* check if arguments in exit jump are in the environment *)
  fun check_Exit_final [] rho = ()
    | check_Exit_final (Data.ArgS(t, a) :: args) rho =
        case a of
          (Data.VarS(v)) => 
            if lookup (get_Var v) rho
            then
              if t = get_Type a rho
              then check_Exit_final args rho
              else raise Error ("Exit variable " ^ (get_Var v) ^ " does not have the correct type", (get_Pos v))
            else raise Error ("Exit variable " ^ (get_Var v) ^ " does not correspond to environment", (get_Pos v))
        | (Data.CstS(_, _)) => check_Exit_final args rho

  (* check full block *)
  fun check'B gamma (Data.BlockS(en, s, ex, p)) = 
    case (en, ex) of
      (Data.BeginS(f1, a1, p1), Data.EndS(f2, a2, p2)) =>
        let
          val rho = check_S gamma (check_Arg [] a1) s
        in
          check_Exit_final a2 rho
        end
    | (Data.BeginS(f1, a1, p1), Data.UncondExitS(f2, a2, p2)) =>
        let
          val rho = check_S gamma (check_Arg [] a1) s
        in
          check_Exit_final a2 rho
        end
    | (Data.BeginS(f1, a1, p1), Data.CondExitS(c, f2, f3, a2, p2)) =>
        let
          val rho = check_S gamma (check_Arg [] a1) s
        in
          if check_Cond rho c
          then check_Exit_final a2 rho
          else raise Error ("Exit Condition invalid", p2)
        end
    | (Data.UncondEntryS(f1, a1, p1), Data.EndS(f2, a2, p2)) =>
        let
          val rho = check_S gamma (check_Arg [] a1) s
        in
          check_Exit_final a2 rho
        end
    | (Data.UncondEntryS(f1, a1, p1), Data.UncondExitS(f2, a2, p2)) =>
        let
          val rho = check_S gamma (check_Arg [] a1) s
        in
          check_Exit_final a2 rho
        end
    | (Data.UncondEntryS(f1, a1, p1), Data.CondExitS(c, f2, f3, a2, p2)) =>
        let
          val rho = check_S gamma (check_Arg [] a1) s
        in
          if check_Cond rho c
          then check_Exit_final a2 rho
          else raise Error ("Exit Condition invalid", p2)
        end
    | (Data.CondEntryS(c, f1, f2, a1, p1), Data.EndS(f3, a2, p2)) =>
        let
          val rho = check_S gamma (check_Arg [] a1) s
        in
          check_Exit_final a2 rho
        end
    | (Data.CondEntryS(c, f1, f2, a1, p1), Data.UncondExitS(f3, a2, p2)) =>
        let
          val rho = check_S gamma (check_Arg [] a1) s
        in
          check_Exit_final a2 rho
        end
    | (Data.CondEntryS(c1, f1, f2, a1, p1), Data.CondExitS(c2, f3, f4, a2, p2)) =>
        let
          val rho = check_S gamma (check_Arg [] a1) s
        in
          if check_Cond rho c2
          then check_Exit_final a2 rho
          else raise Error ("Exit Condition invalid", p2)
        end

  fun check (Data.ProgramS(p)) = (* (Program) *)
    let
      val gamma1 = check_BEn [] p
      val gamma2 = check_BEx [] p
      val gamma = gamma1@gamma2
      val pNames = List.map (#2) gamma
    in
      (*notIn pNames (List.map (fn n => [n]) pNames) " is declared twice " (0,0);*)
      List.app (check'B gamma) p
    end

end