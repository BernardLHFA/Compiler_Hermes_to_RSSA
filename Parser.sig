local
type t__1__ = (int*int)
type t__2__ = (int*int)
type t__3__ = (int*int)
type t__4__ = (int*int)
type t__5__ = (int*int)
type t__6__ = (int*int)
type t__7__ = (int*int)
type t__8__ = (int*int)
type t__9__ = (int*int)
type t__10__ = (int*int)
type t__11__ = (int*int)
type t__12__ = (int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = string*(int*int)
type t__16__ = (int*int)
type t__17__ = (int*int)
type t__18__ = (int*int)
type t__19__ = (int*int)
type t__20__ = (int*int)
type t__21__ = (int*int)
type t__22__ = (int*int)
type t__23__ = (int*int)
type t__24__ = (int*int)
type t__25__ = (int*int)
type t__26__ = (int*int)
type t__27__ = string*(int*int)
type t__28__ = (int*int)
type t__29__ = (int*int)
type t__30__ = (int*int)
type t__31__ = (int*int)
type t__32__ = (int*int)
type t__33__ = (int*int)
type t__34__ = (int*int)
type t__35__ = (int*int)
type t__36__ = (int*int)
type t__37__ = (int*int)
type t__38__ = (int*int)
type t__39__ = (int*int)
type t__40__ = (int*int)
type t__41__ = (int*int)
type t__42__ = (int*int)
type t__43__ = (int*int)
type t__44__ = (int*int)
type t__45__ = (int*int)
type t__46__ = (int*int)
type t__47__ = (int*int)
type t__48__ = (int*int)
type t__49__ = (int*int)
in
datatype token =
    AMPERSAND of t__1__
  | ASSIGN of t__2__
  | BAR of t__3__
  | BEGIN of t__4__
  | CALL of t__5__
  | COLON of t__6__
  | COMM of t__7__
  | DIV of t__8__
  | END of t__9__
  | EOF of t__10__
  | EQ of t__11__
  | GE of t__12__
  | GT of t__13__
  | HIDE of t__14__
  | INT of t__15__
  | JUNK of t__16__
  | LARR of t__17__
  | LBRACKET of t__18__
  | LE of t__19__
  | LPAR of t__20__
  | LSHIFT of t__21__
  | LSHIFTEQ of t__22__
  | LT of t__23__
  | MINUS of t__24__
  | MINUSEQ of t__25__
  | MOD of t__26__
  | NAME of t__27__
  | NE of t__28__
  | P16 of t__29__
  | P32 of t__30__
  | P64 of t__31__
  | P8 of t__32__
  | PLUS of t__33__
  | PLUSEQ of t__34__
  | RARR of t__35__
  | RBRACKET of t__36__
  | REVEAL of t__37__
  | RPAR of t__38__
  | RSHIFT of t__39__
  | RSHIFTEQ of t__40__
  | S16 of t__41__
  | S32 of t__42__
  | S64 of t__43__
  | S8 of t__44__
  | SWAP of t__45__
  | TIMES of t__46__
  | UNCALL of t__47__
  | XOR of t__48__
  | XOREQ of t__49__
end;

val Program :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Data.Program;
