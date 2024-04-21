(*Interpreter*)
structure Interpreter :> Interpreter =
struct
  open BigInt;;
  
  exception Error of string*(int*int)

  val R = Data.R

  (* Creation of arrays for the memory access *)
  val secret =
    let
      val _ = TextIO.print("Initialize Secret Memory: ")
      val str = valOf (TextIO.inputLine TextIO.stdIn)
      val strs = String.tokens Char.isSpace str
      val ve = List.map (fn st => limitZ 8 (string2h st (0,0))) strs
      val arr = Array.fromList(ve)
      val new = Array.array(80, limitZ 64 (int2h 0))
      val _ = Array.copy{di = 0, dst = new, src = arr}
    in
      new
    end

  val public =
    let
      val _ = TextIO.print("Initialize Public Memory: ")
      val str = valOf (TextIO.inputLine TextIO.stdIn)
      val strs = String.tokens Char.isSpace str
      val ve = List.map (fn st => limitZ 8 (string2h st (0,0))) strs
      val arr = Array.fromList(ve)
      val new = Array.array(80, limitZ 64 (int2h 0))
      val _ = Array.copy{di = 0, dst = new, src = arr}
    in
      new
    end
  
  (* Loading elements inside of memories *)
  fun loadMem(mem, address, size) =
    if size = 0 then []
    else Array.sub(mem, address) :: loadMem(mem, address+1, size-1)
  
  (* Storing element inside of memories *)
  fun storeMem(mem, address, []) = ()
    | storeMem(mem, address, (x :: byte)) =
        let
          val _ = Array.update(mem, address, x);
        in
          storeMem(mem, address+1, byte)
        end
  
  fun addByte [] = limitZ 8 (int2h 0)
    | addByte (x :: rm) =
        limitZ 64 (hAdd64 x (addByte rm))
  
  fun turnByte(z, h, p) =
        case z of
          64 => 
            if (h2int h) <= (h2int (limitZ 32 hMax64))
            then turnByte((z div 2), (int2h 0), p)@turnByte((z div 2), h, p)
            else turnByte((z div 2), (limitZ 64 ((hDiv64 h (hAdd64 (limitZ 32 hMax64) (int2h 1)) p))), p)@turnByte((z div 2), (limitZ 64 (hMod64 h (hAdd64 (limitZ 32 hMax64) (int2h 1)) p)), p)
        | 32 =>
            if (h2int h) <= (h2int (limitZ 16 hMax64))
            then turnByte((z div 2), (int2h 0), p)@turnByte((z div 2), h, p)
            else turnByte((z div 2), (limitZ 64 ((hDiv64 h (hAdd64 (limitZ 16 hMax64) (int2h 1)) p))), p)@turnByte((z div 2), (limitZ 64 (hMod64 h (hAdd64 (limitZ 16 hMax64) (int2h 1)) p)), p)
        | 16 => 
            if (h2int h) <= (h2int (limitZ 8 hMax64))
            then [(limitZ 8 (int2h 0))]@turnByte((z div 2), h, p)
            else [(hDiv64 h (hAdd64 (limitZ 8 hMax64) (int2h 1)) p)]@turnByte((z div 2), (limitZ 64 (hMod64 h (hAdd64 (limitZ 8 hMax64) (int2h 1)) p)), p)
        | 8 =>
            [(hMod64 h (limitZ 64 (hAdd64 (limitZ 8 hMax64) (int2h 1))) p)]
        | _ => raise Error ("Size not suited", p)


  datatype location = Variable of hex ref
  
  (* get position of variable *)
  fun get_Pos (s, p) = p

  (* get name of variable *)
  fun get_Var (s, p) = s
  
  (* lookup in environment *)
  fun lookup x [] pos =
        raise Error ("undeclared identifier" ^ x, pos)
    | lookup x ((y,v) :: env) pos =
        if x = y then v else lookup x env pos
  
  (* lookup in gamma *)
  fun lookupG x [] pos b =
        raise Error ("undeclared function " ^ x, pos)
    | lookupG x ((v, (y, _), res) :: gamma) pos b = 
        if v = b
        then
          if x = y then res else lookupG x gamma pos b
        else lookupG x gamma pos b

  (* Alternative lookup in environment *)
  fun lookup2 x [] =
        raise Error ("undeclared identifier" ^ x, (0, 0))
    | lookup2 x ((y,v) :: env) =
        if x = y then v else lookup2 x env

  (* Remove atom from environment *)
  fun rem_rho x [] = []
    | rem_rho x ((y, v) :: rho) =
        if x = y then rem_rho x rho else (y, v)::rem_rho x rho
  
  (* Remove arguments from environment *)
  fun rem_rho_Arg [] rho = []
    | rem_rho_Arg (Data.ArgS(t, a):: arg) rho =
        case a of
          (Data.VarS(v)) => rem_rho_Arg arg (rem_rho (get_Var v) rho)
        | (Data.CstS(_, _)) => rem_rho_Arg arg rho

  (* Calculations for binary operations *)
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
  
  (* Calculations for binary operations in updates *)
  fun UpBinOp bop z v1 v2 p =
        case bop of
          "+" => limitZ z (hAdd64 v1 v2)
        | "-" => limitZ z (hSub64 v1 v2)
        | "^" => limitZ z (hXor64 v1 v2)
        | "<<" => limitZ z (hShiftL64 v1 v2)
        | ">>" => limitZ z (hShiftR64 v1 v2)
        | _ => raise Error ("Simbol not allowed", p)
  
  (* Calculations for binary operations used for memories *)
  fun MemBinOp bop z v1 v2 p =
        case bop of
          "+=" => limitZ z (hAdd64 v1 v2)
        | "-=" => limitZ z (hSub64 v1 v2)
        | "^=" => limitZ z (hXor64 v1 v2)
        | "<<=" => limitZ z (hShiftL64 v1 v2)
        | ">>=" => limitZ z (hShiftR64 v1 v2)
        | _ => raise Error ("Simbol not allowed", p)
  
  (* Testing of boolean binary operations *)
  fun BoolBinOp bop v1 v2 p =
        case bop of
          "==" => if hEqual64 v1 v2 then hMax64 else []
        | "!=" => if hEqual64 v1 v2 then [] else hMax64
        | "<" => if hLess64 v1 v2 then hMax64 else []
        | "<=" => if hLeq64 v1 v2 then hMax64 else []
        | ">" => if hGreater64 v1 v2 then hMax64 else []
        | "=>" => if hGeq64 v1 v2 then hMax64 else []
        | _ => raise Error ("Simbol not allowed", p)
  
  (* Creates a new environment when accessing a new block *)
  fun new_rho [] [] rho p = []
    | new_rho arg [] rho p = raise Error ("Arguments with different sizes", p)
    | new_rho [] arg rho p = raise Error ("Arguments with different sizes", p)
    | new_rho (Data.ArgS(Data.TypeS(_, t1), a1) :: argms) (Data.ArgS(Data.TypeS(_, t2), a2) :: args) rho p =
        case a1 of
          (Data.VarS(x)) =>
            let
              val z = case t1 of
                        Data.u8 => 8
                      | Data.u16 => 16
                      | Data.u32 => 32
                      | Data.u64 => 64
            in
              case a2 of
                (Data.VarS(v)) =>
                  let
                    val loc = lookup (get_Var v) rho (get_Pos v)
                  in
                    ((get_Var x), loc)::new_rho argms args rho p
                  end
              | (Data.CstS(c, _)) =>
                  ((get_Var x), ((int2h 64), Variable (ref (limitZ 64 (string2h c (0, 0))))))::new_rho argms args rho p
            end
        | (Data.CstS(c, _)) =>
            case a2 of
              (Data.CstS(c2, _)) =>
                if c = c2
                then new_rho argms args rho p
                else raise Error ("Constants must be equal", p)
            | (Data.VarS(v)) => raise Error ("Variable must be constant", p)

  (* Adds arguments to environment, used for calls *)
  fun add_Args [] [] rho pos = rho
    | add_Args [] nrho rho pos = raise Error ("Number of arguments is not correct", pos)
    | add_Args arg [] rho pos = raise Error ("Number of arguments is not correct", pos)
    | add_Args ((Data.ArgS(t, a)) :: arg) ((_, (z0, Variable loc)) :: nrho) rho pos =
        case a of
          (Data.VarS(v)) =>
            add_Args arg nrho (rho@[((get_Var v), (z0, Variable loc))]) pos
        | (Data.CstS(c, _)) =>
            if (limitZ 64 (string2h c (0, 0))) = !loc
            then add_Args arg nrho rho pos
            else raise Error ("Constant must be equal to variable", pos)

(* Creates a new environment from arguments *)
fun rho_return [] rho p = []
    | rho_return ((Data.ArgS(t, a)) :: arg) rho p =
        case a of
          (Data.VarS(v)) =>
            let
              val loc = lookup (get_Var v) rho p
            in
              [((get_Var v), loc)]@(rho_return arg rho p)
            end
        | (Data.CstS(_, _)) => rho_return arg rho p
  
  (* Get variables and types of arguments *)
  fun get_Args [] = []
    | get_Args (Data.ArgS(t, a) :: args) =
        case a of
          (Data.VarS(v)) => (t, (get_Var v))::get_Args args
        | (Data.CstS(_, _)) => get_Args args

  (* Creates a new environment from predetermined arguments *)
  fun rho_new_Args [] [] p = []
    | rho_new_Args [] rho p = raise Error ("Number of arguments inconsistent", p)
    | rho_new_Args args [] p = raise Error ("Number of arguments inconsistent", p)
    | rho_new_Args (Data.ArgS(Data.TypeS(_, t), a) :: args) ((y,(z0, Variable loc)) :: rho) p = 
        case a of
          (Data.VarS(x)) =>
            let
              val z = case t of
                        Data.u8 => 8
                      | Data.u16 => 16
                      | Data.u32 => 32
                      | Data.u64 => 64
            in
              if (int2h z) = z0
              then ((get_Var x), (z0, Variable loc))::rho_new_Args args rho p
              else raise Error ("Variables between jumps of different sizes", (get_Pos x))
            end
        | (Data.CstS(s, p)) => rho_new_Args args rho p

  (* Creates the environment from user inputs, used at the start of the program *)
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
  
  (* Creates environment from exit jumps *)
  fun rho_exit (Data.EndS(f, args, p)) rho = rho_new_Args args rho p
    | rho_exit (Data.UncondExitS(f, args, p)) rho = rho_new_Args args rho p
    | rho_exit (Data.CondExitS(c, f1, f2, args, p)) rho = rho_new_Args args rho p

  (* Creates environment from end jumps *)
  fun start_rho_exit (Data.EndS(f, args, p)) = rho_Args args
    | start_rho_exit (Data.UncondExitS(f, args, p)) = raise Error ("Starting exit must be an end", p)
    | start_rho_exit (Data.CondExitS(c, f1, f2, args, p)) = raise Error ("Starting exit must be an end", p)
  
  (* Creates environment from entry jumps *)
  fun rho_entry (Data.BeginS(f, args, p)) rho = rho_new_Args args rho p
    | rho_entry (Data.UncondEntryS(f, args, p)) rho = rho_new_Args args rho p
    | rho_entry (Data.CondEntryS(c, f1, f2, args, p)) rho = rho_new_Args args rho p

  (* Creates environment from begin jumps *)
  fun start_rho_entry (Data.BeginS(f, args, p)) = rho_Args args
    | start_rho_entry (Data.UncondEntryS(f, args, p)) = raise Error ("Starting entry must be an begin", p)
    | start_rho_entry (Data.CondEntryS(c, f1, f2, args, p)) = raise Error ("Starting entry must be an begin", p)

  (* Get the block associated with the function name *)
  fun get_Block_EnEx f [] b p = raise Error("Variable is not the name of a function", p)
    | get_Block_EnEx f (Data.BlockS(en, ss, ex, p) :: prg) b p =
        if b
        then
          case en of
            (Data.BeginS(f1, arg, pos)) =>
              if (get_Var f) = (get_Var f1)
              then Data.BlockS(en, ss, ex, p)
              else get_Block_EnEx f prg b p
          | _ => get_Block_EnEx f prg b p
        else
          case ex of
            (Data.EndS(f1, arg, pos)) =>
              if (get_Var f) = (get_Var f1)
              then Data.BlockS(en, ss, ex, p)
              else get_Block_EnEx f prg b p
          | _ => get_Block_EnEx f prg b p

  (* Get the block from a jump from the jump name *)
  fun get_Block f [] b p = raise Error("Jump not found in program", p)
    | get_Block f (Data.BlockS(en, ss, ex, p) :: prg) b p =
        if b
        then
          case en of
            (Data.BeginS(s, arg, pos)) => get_Block f prg b p
          | (Data.UncondEntryS(s, arg, pos)) =>
              if (get_Var f) = (get_Var s)
              then Data.BlockS(en, ss, ex, p)
              else get_Block f prg b p
          | (Data.CondEntryS(c, f1, f2, arg, p)) =>
              if (get_Var f) = (get_Var f1) orelse (get_Var f) = (get_Var f2)
              then Data.BlockS(en, ss, ex, p)
              else get_Block f prg b p
        else
          case ex of
            (Data.EndS(s, arg, pos)) => get_Block f prg b p
          | (Data.UncondExitS(s, arg, pos)) =>
              if (get_Var f) = (get_Var s)
              then Data.BlockS(en, ss, ex, p)
              else get_Block f prg b p
          | (Data.CondExitS(c, f1, f2, arg, p)) =>
              if (get_Var f) = (get_Var f1) orelse (get_Var f) = (get_Var f2)
              then Data.BlockS(en, ss, ex, p)
              else get_Block f prg b p
  
  (* Evaluation Boolean Conditions *)
  fun eval_Cond (Data.BoolOp2S(opr, a1, a2, p)) rho =
        case (a1, a2) of
          (Data.VarS(v1), Data.VarS(v2)) =>
            let
              val (z1, Variable loc1) = lookup (get_Var v1) rho p
              val (z2, Variable loc2) = lookup (get_Var v2) rho p
              val lambda = BoolBinOp opr (!loc1) (!loc2) p
            in
              lambda = hMax64
            end
        | (Data.VarS(v), Data.CstS(c, _)) =>
            let
              val (z0, Variable loc) = lookup (get_Var v) rho p
              val lambda = BoolBinOp opr (!loc) (limitZ 64 (string2h c (0, 0))) p
            in
              lambda = hMax64
            end
        | (Data.CstS(c, _), Data.VarS(v)) =>
            let
              val (z0, Variable loc) = lookup (get_Var v) rho p
              val lambda = BoolBinOp opr (limitZ 64 (string2h c (0, 0))) (!loc) p
            in
              lambda = hMax64
            end
        | (Data.CstS(c1, _), Data.CstS(c2, _)) =>
            let
              val lambda = BoolBinOp opr (limitZ 64 (string2h c1 (0, 0))) (limitZ 64 (string2h c2 (0, 0))) p
            in
              lambda = hMax64
            end
  
  (* Evaluates reveal and hide uses *)
  fun do_TChange rho (Data.RevealS(t, a, p)) =
        (case a of
          (Data.VarS(v)) =>
            let
              val (z0, Variable loc) = lookup (get_Var v) rho p
              val rho2 = rem_rho (get_Var v) rho
            in
              (rho2, z0, Variable loc)
            end
        | _ => raise Error ("Revealed variable can't be a constant", p))
    | do_TChange rho (Data.HideS(t, a, p)) =
        case a of
          (Data.VarS(v)) =>
            let
              val (z0, Variable loc) = lookup (get_Var v) rho p
              val rho2 = rem_rho (get_Var v) rho
            in
              (rho2, z0, Variable loc)
            end
        | _ => raise Error ("Hidden variable can't be a constant", p)
  
  (* Evaluates Memory uses *)
  fun do_M rho (Data.MemoryS(t, a, p)) =
        case a of
          (Data.VarS(v)) =>
            let
              val (_, Variable loc) = lookup (get_Var v) rho p
            in
              case t of
                (Data.TypeS(Data.Secret, Data.u8)) => (secret, h2int (!loc), 8)
              | (Data.TypeS(Data.Secret, Data.u16)) => (secret, h2int (!loc), 16)
              | (Data.TypeS(Data.Secret, Data.u32)) => (secret, h2int (!loc), 32)
              | (Data.TypeS(Data.Secret, Data.u64)) => (secret, h2int (!loc), 64)
              | (Data.TypeS(Data.Public, Data.u8)) => (public, h2int (!loc), 8)
              | (Data.TypeS(Data.Public, Data.u16)) => (public, h2int (!loc), 16)
              | (Data.TypeS(Data.Public, Data.u32)) => (public, h2int (!loc), 32)
              | (Data.TypeS(Data.Public, Data.u64)) => (public, h2int (!loc), 64)
            end
        | (Data.CstS(c, _)) =>
            case t of
              (Data.TypeS(Data.Secret, Data.u8)) => (secret, valOf (Int.fromString c), 8)
            | (Data.TypeS(Data.Secret, Data.u16)) => (secret, valOf (Int.fromString c), 16)
            | (Data.TypeS(Data.Secret, Data.u32)) => (secret, valOf (Int.fromString c), 32)
            | (Data.TypeS(Data.Secret, Data.u64)) => (secret, valOf (Int.fromString c), 64)
            | (Data.TypeS(Data.Public, Data.u8)) => (public, valOf (Int.fromString c), 8)
            | (Data.TypeS(Data.Public, Data.u16)) => (public, valOf (Int.fromString c), 16)
            | (Data.TypeS(Data.Public, Data.u32)) => (public, valOf (Int.fromString c), 32)
            | (Data.TypeS(Data.Public, Data.u64)) => (public, valOf (Int.fromString c), 64)

  (* Makes operations with no inpact on the environment *)
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

  (* Makes update operations, changing the environement *)
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

  (* Evaluate statements *)
  fun do_S gamma rho [] oprg = rho
    | do_S gamma rho (Data.AssignS(t, a, u, p) :: s) oprg = (* Update Assignment *)
        let
          val (rho2, z0, Variable loc) = do_U rho u
        in
          case a of
            (Data.VarS(v)) =>
              do_S gamma (rho2@[((get_Var v), (z0, Variable loc))]) s oprg
          | (Data.CstS(c, _)) =>
              if (limitZ 64 (string2h c (0, 0))) = !loc
              then do_S gamma rho2 s oprg
              else raise Error ("Operation does not equal the desired constant", p)
        end
    | do_S gamma rho (Data.Assign2S(t, a1, a2, p) :: s) oprg = (* Simple Assignement *)
        (case a2 of
          (Data.VarS(v)) =>
            let
              val (z0, Variable loc) = lookup (get_Var v) rho p
              val rho2 = rem_rho (get_Var v) rho
            in
              (case a1 of
                (Data.VarS(v2)) =>
                  do_S gamma (rho2@[((get_Var v2), (z0, Variable loc))]) s oprg
              | (Data.CstS(c, _)) =>
                  if (limitZ 64 (string2h c (0, 0))) = !loc
                  then do_S gamma rho2 s oprg
                  else raise Error ("Variable should be equal to constant", p))
            end
        | (Data.CstS(c, _)) =>
            (case a1 of
              (Data.VarS(v2)) =>
                do_S gamma (rho@[((get_Var v2), ((int2h 64), Variable (ref (limitZ 64 (string2h c (0, 0))))))]) s oprg
            | (Data.CstS(c2, _)) =>
                if c = c2
                then do_S gamma rho s oprg
                else raise Error ("Constants are not equal", p)))
    | do_S gamma rho (Data.DAssignS(t1, a1, t2, a2, a3, a4, p) :: s) oprg = (* Double Assignement *)
        (case (a3, a4) of
          (Data.VarS(v1), Data.VarS(v2)) =>
            let
              val (z1, Variable loc1) = lookup (get_Var v1) rho p
              val (z2, Variable loc2) = lookup (get_Var v2) rho p
              val rho2 = rem_rho (get_Var v1) rho
              val rho3 = rem_rho (get_Var v2) rho2
            in
              case (a1, a2) of
                (Data.VarS(v3), Data.VarS(v4)) =>
                  do_S gamma (rho3@[((get_Var v3), (z1, Variable loc1)), ((get_Var v4), (z2, Variable loc2))]) s oprg
              | _ => raise Error ("Constants are not allowed", p)
            end
        | _ => raise Error ("Constants are not allowed", p))
    | do_S gamma rho (Data.MemOp2S(st, m, e, p) :: s) oprg = (* Memory Update*)
        let
          val (Variable loc) = do_O rho e
          val (mem, i, z0) = do_M rho m
          val rm = loadMem(mem, i, (z0 div 8))
          val x = MemBinOp st z0 (addByte rm) (!loc) p
          val byte = turnByte(z0, (limitZ z0 x), p)
        in
          storeMem(mem, i, byte) ;
          do_S gamma rho s oprg
        end
    | do_S gamma rho (Data.MemSwapS(m1, m2, p) :: s) oprg = (* Memory Swap *)
        let
          val (mem1, i1, z1) = do_M rho m1
          val (mem2, i2, z2) = do_M rho m2
          val x1 = loadMem(mem1, i1, (z1 div 8))
          val x2 = loadMem(mem2, i2, (z2 div 8))
        in
          storeMem(mem1, i1, x2) ;
          storeMem(mem2, i2, x1) ;
          do_S gamma rho s oprg
        end
    | do_S gamma rho (Data.SwapS(a1, m, a2, p) :: s) oprg = (* Variable Swap, using memory *)
        let
          val (mem, i, z1) = do_M rho m
          val rn = loadMem(mem, i, (z1 div 8))
          val x = addByte rn
        in
          case a2 of
            (Data.VarS(v2)) =>
              let
                val (z0, Variable loc) = lookup (get_Var v2) rho p
                val rho2 = rem_rho (get_Var v2) rho
                val rn2 = turnByte((h2int z0), !loc, p)
              in
                storeMem(mem, i, rn2);
                case a1 of
                  (Data.VarS(v1)) =>
                    do_S gamma (rho2@[((get_Var v1), (int2h z1, Variable (ref  x)))]) s oprg
                | (Data.CstS(c, _)) =>
                    if (limitZ 64 (string2h c (0, 0))) = x
                    then do_S gamma rho2 s oprg
                    else raise Error ("Constant isn't equal to memory", p)
              end
          | (Data.CstS(c, _)) =>
              (storeMem(mem, i, turnByte(64, (limitZ z1 (string2h c (0, 0))), p)) ;
              case a1 of
                (Data.VarS(v1)) =>
                  do_S gamma (rho@[((get_Var v1), (int2h z1, Variable (ref x)))]) s oprg
              | (Data.CstS(c2, _)) =>
                  if (limitZ 64 (string2h c2 (0, 0))) = x
                  then do_S gamma rho s oprg
                  else raise Error ("Constant isn't equal to memory", p))
        end
    | do_S gamma rho (Data.AssignArgS(arg, c, p) :: s) oprg = (* Assignment of arguments, with calls *)
        let
          val (nrho, rho2) = do_Call gamma rho c oprg
          val rho3 = add_Args arg nrho rho2 p
        in
          do_S gamma rho3 s oprg
        end
    | do_S gamma rho (Data.TAssignS(t, a, tc, p) :: s) oprg = (* Change types, reveal and hide *)
        let
          val (rho2, z0, Variable loc) = do_TChange rho tc
        in
          case a of
            (Data.VarS(v)) =>
              do_S gamma (rho2@[((get_Var v), (z0, Variable loc))]) s oprg
          | _ => raise Error ("Variable can't be constant during a type change", p)
        end
  
  (* Evaluates calls, using other functions *)
  and do_Call gamma rho (Data.CallS(f, arg, p)) oprg =
        let
          val (argm, ss) = lookupG (get_Var f) gamma p true
          val (argf, _) = lookupG (get_Var f) gamma p false
          val block = get_Block_EnEx f oprg true p
          val nrho = new_rho argm arg rho p
          val nrho2 = do_main_extra block false gamma oprg nrho
          val nrho3 = rho_return argf nrho2 p
          val rho2 = rem_rho_Arg arg rho
        in
          (nrho3, rho2)
        end
    | do_Call gamma rho (Data.UncallS(f, arg, p)) oprg =
        let
          val (argm, ss) = lookupG (get_Var f) gamma p false
          val (argf, _) = lookupG (get_Var f) gamma p true
          val block = get_Block_EnEx f oprg false p
          val nrho = new_rho argm arg rho p
          val nrho2 = do_main_backwards_extra block true gamma oprg nrho
          val nrho3 = rho_return argf nrho2 p
          val rho2 = rem_rho_Arg arg rho
        in
          (nrho3, rho2)
        end

  (* Evaluate extra block (other than main) *)
  and do_B_extra (Data.BlockS(en, ss, ex, p)) backwards gamma rho oprg =
        if backwards
        then
          let
            val rho2 = rho_exit ex rho
          in
            do_S gamma rho2 ss oprg
          end
        else
          let
            val rho2 = rho_entry en rho
          in
            do_S gamma rho2 ss oprg
          end

  (* Evaluate block of main function *)
  and do_B (Data.BlockS(en, ss, ex, p)) backwards gamma oprg =
        if backwards
        then
          let
            val rho = start_rho_exit ex
          in
            do_S gamma rho ss oprg
          end
        else
          let
            val rho = start_rho_entry en
          in
            do_S gamma rho ss oprg
          end
  
  (* Start reverse evaluation for other block (outside of main) *)
  and do_main_backwards_extra (Data.BlockS(en, ss, ex, p)) backwards gamma oprg rho =
        let
          val rho2 = do_B_extra (Data.BlockS(en, (List.rev (List.map R ss)), ex, p)) backwards gamma rho oprg
        in
          case en of
            (Data.BeginS(f, arg, p2)) => rho2
          | (Data.UncondEntryS(f, arg, p2)) =>
              let
                val block = get_Block f oprg false p2
                val nrho = rho_return arg rho2 p2
              in
                do_main_backwards_extra block backwards gamma oprg nrho
              end
          | (Data.CondEntryS(c, f1, f2, arg, p2)) =>
              if eval_Cond c rho2
              then
                let
                  val block = get_Block f1 oprg false p2
                  val nrho = rho_return arg rho2 p2
                in
                  do_main_backwards_extra block backwards gamma oprg nrho
                end
              else 
                let
                  val block = get_Block f2 oprg false p2
                  val nrho = rho_return arg rho2 p2
                in
                  do_main_backwards_extra block backwards gamma oprg nrho
                end
        end

  (* Start reverse evaluation for main block *)
  and do_main_backwards (Data.BlockS(en, ss, ex, p)) backwards gamma oprg =
        let
          val rho = do_B (Data.BlockS(en, (List.rev (List.map R ss)), ex, p)) backwards gamma oprg
        in
          case en of
            (Data.BeginS(f, arg, p2)) => rho
          | (Data.UncondEntryS(f, arg, p2)) =>
              let
                val block = get_Block f oprg false p2
                val nrho = rho_return arg rho p2
              in
                do_main_backwards_extra block backwards gamma oprg nrho
              end
          | (Data.CondEntryS(c, f1, f2, arg, p2)) =>
              if eval_Cond c rho
              then
                let
                  val block = get_Block f1 oprg false p2
                  val nrho = rho_return arg rho p2
                in
                  do_main_backwards_extra block backwards gamma oprg nrho 
                end
              else 
                let
                  val block = get_Block f2 oprg false p2
                  val nrho = rho_return arg rho p2
                in
                  do_main_backwards_extra block backwards gamma oprg nrho
                end
        end
  
  (* Start evaluation for other blocks, (other than main) *)
  and do_main_extra (Data.BlockS(en, ss, ex, p)) backwards gamma oprg rho =
        let
          val rho2 = do_B_extra (Data.BlockS(en, ss, ex, p)) backwards gamma rho oprg
        in
          case ex of
            (Data.EndS(f, arg, p2)) => rho2
          | (Data.UncondExitS(f, arg, p2)) =>
              let
                val block = get_Block f oprg true p2
                val nrho = rho_return arg rho2 p2
              in
                do_main_extra block backwards gamma oprg nrho
              end
          | (Data.CondExitS(c, f1, f2, arg, p2)) =>
              if eval_Cond c rho2
              then
                let
                  val block = get_Block f1 oprg true p2
                  val nrho = rho_return arg rho2 p2
                in
                  do_main_extra block backwards gamma oprg nrho
                end
              else 
                let
                  val block = get_Block f2 oprg true p2
                  val nrho = rho_return arg rho2 p2
                in
                  do_main_extra block backwards gamma oprg nrho
                end
        end

  (* Start evaluation for main block *)
  and do_main (Data.BlockS(en, ss, ex, p)) backwards gamma oprg =
        let
          val rho = do_B (Data.BlockS(en, ss, ex, p)) backwards gamma oprg
        in
          case ex of
            (Data.EndS(f, arg, p2)) => rho
          | (Data.UncondExitS(f, arg, p2)) =>
              let
                val block = get_Block f oprg true p2
                val nrho = rho_return arg rho p2
              in
                do_main_extra block backwards gamma oprg nrho
              end
          | (Data.CondExitS(c, f1, f2, arg, p2)) =>
              if eval_Cond c rho
              then
                let
                  val block = get_Block f1 oprg true p2
                  val nrho = rho_return arg rho p2
                in
                  do_main_extra block backwards gamma oprg nrho 
                end
              else 
                let
                  val block = get_Block f2 oprg true p2
                  val nrho = rho_return arg rho p2
                in
                  do_main_extra block backwards gamma oprg nrho
                end
        end

  (* Return gamma from exit *)
  fun do_Exit (Data.EndS(f, args, p)) ss = [(false, f, (args, ss))]
    | do_Exit (Data.UncondExitS(f, args, p)) ss = [(false, f, (args, ss))]
    | do_Exit (Data.CondExitS(c, f1, f2, args, p)) ss = [(false, f1, (args, ss)), (false, f2, (args, ss))]

  (* Return gamma from begin *)
  fun do_Entry (Data.BeginS(f, args, p)) ss = [(true, f, (args, ss))]
    | do_Entry (Data.UncondEntryS(f, args, p)) ss = [(true, f, (args, ss))]
    | do_Entry (Data.CondEntryS(c, f1, f2, args, p)) ss = [(true, f1, (args, ss)), (true, f2, (args, ss))]

  (* Create gamma *)
  fun do_G [] = []
    | do_G (Data.BlockS(en, ss, ex, p) :: ps) =
        let
          val gamma1 = do_Entry en ss
          val gamma2 = do_Exit ex ss
          val gamma = gamma1@gamma2
        in
          gamma@do_G ps
        end
  
  (* Evaluate Program for reverse evaluation *)
  fun do_P_backwards [] backwards gamma oprg = raise Error ("Main function doesn't have correct end statement", (0, 0))
    | do_P_backwards (Data.BlockS(en, ss, ex, p) :: ps) backwards gamma oprg =
        case ex of
          (Data.EndS(f, args, p2)) =>
            if (get_Var f) = "main"
            then do_main_backwards (Data.BlockS(en, ss, ex, p)) backwards gamma oprg
            else do_P_backwards ps backwards gamma oprg
        | _ => do_P_backwards ps backwards gamma oprg

  (* Get final arguments *)
  fun get_Args_final_end [] = raise Error ("Main function doesn't have correct end statement", (0, 0))
    | get_Args_final_end (Data.BlockS(en, ss, ex, p) :: ps) = 
        case ex of
          (Data.EndS(f, args, p2)) =>
            if (get_Var f) = "main"
            then get_Args args
            else get_Args_final_end ps
        | _ => get_Args_final_end ps

  (* Evaluate Program *)
  fun do_P [] backwards gamma oprg = raise Error ("No Main function", (0, 0))
    | do_P (Data.BlockS(en, ss, ex, p) :: ps) backwards gamma oprg =
        if backwards
        then
          case en of
            (Data.BeginS(f, args, p2)) =>
              if (get_Var f) = "main"
              then ((do_P_backwards (Data.BlockS(en, ss, ex, p) :: ps) backwards gamma oprg), (get_Args args))
              else do_P ps backwards gamma oprg
          | _ => do_P ps backwards gamma oprg
        else
          case en of
            (Data.BeginS(f, args, p2)) =>
              if (get_Var f) = "main"
              then ((do_main (Data.BlockS(en, ss, ex, p)) backwards gamma oprg), (get_Args_final_end (Data.BlockS(en, ss, ex, p) :: ps)))
              else do_P ps backwards gamma oprg
          | _ => do_P ps backwards gamma oprg
  
  fun printArray memory i =
        if i < Array.length(memory) - 1
        then (h2hString (Array.sub(memory, i))) ^ ", " ^ (printArray memory (i+1))
        else if i < Array.length(memory)
        then (h2hString (Array.sub(memory, i))) ^ (printArray memory (i+1))
        else ""

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
      val (rho, args) = do_P prg backwards gamma prg
    in
      TextIO.print ("Secret Array: [" ^ printArray secret 0 ^ "]\n");
      TextIO.print ("Public Array: [" ^ printArray public 0 ^ "]\n");
      List.app (printPar rho) args
      (*do_P prg backwards gamma*)
      (*List.app (do_P prg backwards) gamma*)
    end

end