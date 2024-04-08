(* Data.sml  -- the abstract syntax datatype *)
structure Data =
struct
  type pos = int * int

  type Variable = string * pos

  datatype Atom =
      VarS of Variable
    | CstS of string * pos

  datatype valType = Secret | Public

  datatype intType = u8 | u16 | u32 | u64

  datatype Type =
      TypeS of valType * intType

  datatype Condition =
      BoolOp2S of string * Atom * Atom * pos

  datatype Arguments =
      ArgS of Type * Atom

  datatype Memory =
      MemoryS of Type * Atom * pos

  datatype TypeChange =
      RevealS of Type * Atom * pos
    | HideS of Type * Atom * pos

  datatype Calling =
      CallS of Atom * Arguments list * pos
    | UncallS of Atom * Arguments list * pos

  datatype Operation =
      SimOp2S of Atom
    | Op2S of string * Atom * Atom * pos

  datatype Update =
      UpdOp2S of string * Atom * Operation * pos

  datatype Stat =
      AssignS of Type * Atom * Update * pos
    | Assign2S of Type * Atom * Atom * pos
    | DAssignS of Type * Atom * Type * Atom * Atom * Atom * pos
    | MemOp2S of string * Memory * Operation * pos
    | MemSwapS of Memory * Memory * pos
    | SwapS of Atom * Memory * Atom * pos
    | AssignArgS of Arguments list * Calling * pos
    | TAssignS of Type * Atom * TypeChange * pos

  datatype Exit =
      EndS of Variable * Arguments list * pos
    | UncondExitS of Variable * Arguments list * pos
    | CondExitS of Condition * Variable * Variable * Arguments list * pos

  datatype Entry =
      BeginS of Variable * Arguments list * pos
    | UncondEntryS of Variable * Arguments list * pos
    | CondEntryS of Condition * Variable * Variable * Arguments list * pos

  datatype Block =
      BlockS of Entry * Stat list * Exit * pos

  datatype Program = 
      ProgramS of Block list

  fun invertUpOp upOp =
    case upOp of
      "+" => "-"
    | "-" => "+"
    | "^" => "^"
    | ">>" => "<<"
    | "<<" => ">>"
    | "+=" => "-="
    | "-=" => "+="
    | "^=" => "^="
    | ">>=" => "<<="
    | "<<=" => ">>="
    | upOp => upOp

  (* reverse statement *)
  fun R s =
    case s of
      AssignS(t, a1, UpdOp2S(s, a2, e, p2), p1) => AssignS(t, a2, UpdOp2S((invertUpOp s), a1, e, p2), p1)
    | Assign2S(t, a1, a2, p) => Assign2S(t, a2, a1, p)
    | DAssignS(t1, a1, t2, a2, a3, a4, p) => DAssignS(t1, a3, t2, a4, a1, a2, p)
    | MemOp2S(s, m, e, p) => MemOp2S(invertUpOp s, m, e, p)
    | SwapS(a1, m, a2, p) => SwapS(a2, m, a1, p)
    | AssignArgS(args1, CallS(a, args2, p2), p1) => AssignArgS(args2, UncallS(a, args1, p2), p1)
    | AssignArgS(args1, UncallS(a, args2, p2), p1) => AssignArgS(args2, CallS(a, args1, p2), p1)
    | TAssignS(t1, a1, RevealS(t2, a2, p2), p1) => TAssignS(t2, a2, HideS(t1, a1, p2), p1)
    | TAssignS(t1, a1, HideS(t2, a2, p2), p1) => TAssignS(t2, a2, RevealS(t1, a1, p2), p1)
    | s => s

(* printing syntax *)

  fun showVar (s, p) = s

  fun showAtom (VarS(v)) = showVar v
    | showAtom (CstS(s, p)) = s

  fun showIntType (u8) = "8"
    | showIntType (u16) = "16"
    | showIntType (u32) = "32"
    | showIntType (u64) = "64"
  
  fun showValType (Secret) = "s"
    | showValType (Public) = "p"

  fun showType (TypeS(v, i)) = 
          showValType v ^ showIntType i

  fun showCond (BoolOp2S(s, a1, a2, p)) =
          showAtom a1 ^ " " ^ s ^ " " ^ showAtom a2

  fun showArg (ArgS(t, a)) =
          showType t ^ " " ^ showAtom a
  
  fun showMem (MemoryS(t, a, p)) =
          showType t ^ "[" ^ showAtom a ^ "]"

  fun showTypeChan (RevealS(t, a, p)) =
          "reveal " ^ showType t ^ " " ^ showAtom a
    | showTypeChan (HideS(t, a, p)) =
          "hide " ^ showType t ^ " " ^ showAtom a

  fun showCall (CallS(a, aas, p)) =
          "call " ^ showAtom a ^ "(" ^
          let
            val args = String.concat (List.map (fn a => showArg a ^ ", ") aas)
          in
            if args = "" then ""
            else String.substring (args, 0, String.size args - 2)
          end ^ ")"
    | showCall (UncallS(a, aas, p)) =
          "uncall " ^ showAtom a ^ "(" ^
          let
            val args = String.concat (List.map (fn a => showArg a ^ ", ") aas)
          in
            if args = "" then ""
            else String.substring (args, 0, String.size args - 2)
          end ^ ")"

  fun showOp (SimOp2S(a)) =
          showAtom a
    | showOp (Op2S(s, a1, a2, p)) =
          "(" ^ showAtom a1 ^ " " ^ s ^ " " ^ showAtom a2 ^ ")"

  fun showUpdate (UpdOp2S(s, a, e, p)) =
          showAtom a ^ " " ^ s ^ " " ^ showOp e

  fun showStat (AssignS(t, a, u, p)) = 
          showType t ^ " " ^ showAtom a ^ " := " ^ showUpdate u
    | showStat (Assign2S(t, a1, a2, p)) = 
          showType t ^ " " ^ showAtom a1 ^ " := " ^ showAtom a2
    | showStat (DAssignS(t1, a1, t2, a2, a3, a4, p)) = 
          showType t1 ^ " " ^ showAtom a1 ^ ", " ^ showType t2 ^ " " ^ showAtom a2 ^ " := " ^ showAtom a3 ^ ", " ^ showAtom a4
    | showStat (MemOp2S(s, m, e, p)) =
          showMem m ^ " " ^ s ^ " " ^ showOp e 
    | showStat (MemSwapS(m1, m2, p)) =
          showMem m1 ^ " <-> " ^ showMem m2
    | showStat (SwapS(a1, m, a2, p)) =
          showAtom a1 ^ " := " ^ showMem m ^ " := " ^ showAtom a2
    | showStat (AssignArgS(aas, c, p)) =
          "(" ^
          let
            val args = String.concat (List.map (fn a => showArg a ^ ", ") aas)
          in
            if args = "" then ""
            else String.substring (args, 0, String.size args - 2)
          end ^ ") := " ^ showCall c
    | showStat (TAssignS(t, a, tc, p)) =
          showType t ^ " " ^ showAtom a ^ " := " ^ showTypeChan tc

  fun showExit (EndS(v, aas, p)) =
          "end " ^ showVar v ^ "(" ^
          let
            val args = String.concat (List.map (fn a => showArg a ^ ", ") aas)
          in
            if args = "" then ""
            else String.substring (args, 0, String.size args - 2)
          end ^ ")"
    | showExit (UncondExitS(v, aas, p)) =
          "-> " ^ showVar v ^ "(" ^
          let
            val args = String.concat (List.map (fn a => showArg a ^ ", ") aas)
          in
            if args = "" then ""
            else String.substring (args, 0, String.size args - 2)
          end ^ ")"
    | showExit (CondExitS(c, v1, v2, aas, p)) =
          showCond c ^ " -> " ^ showVar v1 ^ "(" ^
          let
            val args = String.concat (List.map (fn a => showArg a ^ ", ") aas)
          in
            if args = "" then ""
            else String.substring (args, 0, String.size args - 2)
          end ^ ")" ^ showVar v2

  fun showEntry (BeginS(v, aas, p)) =
          "begin " ^ showVar v ^ "(" ^
          let
            val args = String.concat (List.map (fn a => showArg a ^ ", ") aas)
          in
            if args = "" then ""
            else String.substring (args, 0, String.size args - 2)
          end ^ ")"
    | showEntry (UncondEntryS(v, aas, p)) =
          showVar v ^ "(" ^
          let
            val args = String.concat (List.map (fn a => showArg a ^ ", ") aas)
          in
            if args = "" then ""
            else String.substring (args, 0, String.size args - 2)
          end ^ ")" ^ " <-"
    | showEntry (CondEntryS(c, v1, v2, aas, p)) =
          showVar v1 ^ "(" ^
          let
            val args = String.concat (List.map (fn a => showArg a ^ ", ") aas)
          in
            if args = "" then ""
            else String.substring (args, 0, String.size args - 2)
          end ^ ")" ^ showVar v2 ^ " <- " ^ showCond c

  fun showBlock (BlockS(en, ss, ex, p)) =
          showEntry en ^ "\n" ^ 
          let 
            val stats = String.concat (List.map (fn s => "  " ^ showStat s ^ "\n") ss)
          in
            if stats = "" then ""
            else String.substring (stats, 0, String.size stats - 1)
          end ^ "\n" ^ showExit ex ^ "\n"

  fun showProgram (ProgramS(ps)) =
         String.concat (List.map showBlock ps)
end