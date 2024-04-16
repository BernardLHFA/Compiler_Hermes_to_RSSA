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

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

open Data;
(* Line 7, file Parser.sml *)
val yytransl = #[
  257 (* AMPERSAND *),
  258 (* ASSIGN *),
  259 (* BAR *),
  260 (* BEGIN *),
  261 (* CALL *),
  262 (* COLON *),
  263 (* COMM *),
  264 (* DIV *),
  265 (* END *),
  266 (* EOF *),
  267 (* EQ *),
  268 (* GE *),
  269 (* GT *),
  270 (* HIDE *),
  271 (* INT *),
  272 (* JUNK *),
  273 (* LARR *),
  274 (* LBRACKET *),
  275 (* LE *),
  276 (* LPAR *),
  277 (* LSHIFT *),
  278 (* LSHIFTEQ *),
  279 (* LT *),
  280 (* MINUS *),
  281 (* MINUSEQ *),
  282 (* MOD *),
  283 (* NAME *),
  284 (* NE *),
  285 (* P16 *),
  286 (* P32 *),
  287 (* P64 *),
  288 (* P8 *),
  289 (* PLUS *),
  290 (* PLUSEQ *),
  291 (* RARR *),
  292 (* RBRACKET *),
  293 (* REVEAL *),
  294 (* RPAR *),
  295 (* RSHIFT *),
  296 (* RSHIFTEQ *),
  297 (* S16 *),
  298 (* S32 *),
  299 (* S64 *),
  300 (* S8 *),
  301 (* SWAP *),
  302 (* TIMES *),
  303 (* UNCALL *),
  304 (* XOR *),
  305 (* XOREQ *),
    0];

val yylhs = "\255\255\
\\001\000\002\000\002\000\003\000\004\000\004\000\004\000\005\000\
\\005\000\005\000\006\000\006\000\007\000\007\000\015\000\015\000\
\\015\000\015\000\016\000\016\000\016\000\016\000\008\000\008\000\
\\009\000\009\000\009\000\009\000\009\000\009\000\010\000\011\000\
\\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\\011\000\011\000\011\000\011\000\011\000\012\000\012\000\013\000\
\\013\000\013\000\013\000\013\000\014\000\014\000\014\000\014\000\
\\014\000\014\000\014\000\014\000\014\000\014\000\014\000\000\000";

val yylen = "\002\000\
\\002\000\001\000\002\000\005\000\005\000\005\000\007\000\005\000\
\\007\000\005\000\004\000\002\000\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\\003\000\003\000\003\000\003\000\003\000\003\000\004\000\004\000\
\\004\000\009\000\003\000\003\000\003\000\003\000\003\000\003\000\
\\005\000\009\000\009\000\007\000\007\000\001\000\002\000\003\000\
\\003\000\003\000\003\000\003\000\001\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\005\000\005\000\005\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\000\000\000\000\064\000\000\000\000\000\000\000\
\\000\000\000\000\001\000\003\000\000\000\000\000\016\000\017\000\
\\018\000\015\000\020\000\021\000\022\000\019\000\000\000\000\000\
\\014\000\013\000\024\000\000\000\023\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\047\000\000\000\000\000\000\000\005\000\006\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\053\000\038\000\036\000\035\000\039\000\040\000\037\000\
\\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\\011\000\000\000\031\000\000\000\000\000\033\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\007\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\041\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\025\000\030\000\029\000\028\000\027\000\026\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\051\000\049\000\048\000\
\\052\000\050\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\062\000\063\000\060\000\057\000\
\\055\000\061\000\054\000\058\000\059\000\056\000\008\000\010\000\
\\000\000\044\000\045\000\000\000\000\000\000\000\000\000\042\000\
\\043\000\034\000\009\000";

val yydgoto = "\002\000\
\\005\000\006\000\007\000\008\000\075\000\023\000\024\000\066\000\
\\077\000\032\000\033\000\034\000\086\000\067\000\025\000\026\000";

val yysindex = "\012\000\
\\005\255\000\000\245\254\006\255\000\000\020\255\005\255\028\255\
\\031\255\159\255\000\000\000\000\151\255\159\255\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\015\255\042\255\
\\000\000\000\000\000\000\159\255\000\000\002\255\054\255\249\254\
\\151\255\052\255\053\255\070\255\024\255\241\254\068\255\043\255\
\\042\255\073\255\080\255\159\255\021\255\021\255\021\255\021\255\
\\159\255\021\255\000\000\010\255\078\255\093\255\000\000\000\000\
\\104\255\159\255\123\255\087\255\159\255\042\255\077\255\124\255\
\\042\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\100\255\101\255\000\000\060\255\095\255\129\255\130\255\042\255\
\\000\000\000\255\000\000\106\255\022\255\000\000\042\255\121\255\
\\114\255\115\255\042\255\042\255\042\255\042\255\042\255\042\255\
\\109\255\102\255\132\255\000\000\111\255\116\255\142\255\021\255\
\\021\255\021\255\021\255\021\255\000\000\042\255\042\255\042\255\
\\042\255\042\255\042\255\042\255\042\255\042\255\042\255\159\255\
\\159\255\000\000\000\000\000\000\000\000\000\000\000\000\128\255\
\\056\255\072\255\131\255\133\255\125\255\000\000\000\000\000\000\
\\000\000\000\000\112\255\117\255\118\255\119\255\126\255\127\255\
\\138\255\139\255\141\255\158\255\160\255\161\255\159\255\134\255\
\\135\255\159\255\159\255\152\255\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\166\255\000\000\000\000\167\255\168\255\170\255\180\255\000\000\
\\000\000\000\000\000\000";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\198\255\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\162\255\000\000\059\255\069\255\000\000\000\000\171\255\000\000\
\\000\000\208\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\143\255\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000";

val yygindex = "\000\000\
\\000\000\204\000\000\000\000\000\000\000\242\255\247\255\254\255\
\\132\000\005\000\000\000\180\000\000\000\216\255\244\255\246\255";

val YYTABLESIZE = 213;
val yytable = "\037\000\
\\035\000\056\000\036\000\030\000\101\000\068\000\069\000\070\000\
\\003\000\072\000\031\000\057\000\001\000\040\000\045\000\009\000\
\\027\000\046\000\073\000\041\000\035\000\039\000\036\000\030\000\
\\027\000\010\000\047\000\043\000\042\000\011\000\031\000\004\000\
\\048\000\013\000\063\000\027\000\029\000\049\000\060\000\063\000\
\\065\000\050\000\104\000\081\000\074\000\105\000\102\000\029\000\
\\064\000\076\000\014\000\084\000\038\000\071\000\106\000\044\000\
\\027\000\052\000\053\000\085\000\107\000\055\000\088\000\134\000\
\\135\000\136\000\137\000\138\000\029\000\108\000\091\000\092\000\
\\093\000\014\000\058\000\054\000\014\000\076\000\094\000\061\000\
\\059\000\062\000\095\000\013\000\109\000\014\000\013\000\096\000\
\\122\000\123\000\124\000\125\000\126\000\127\000\041\000\013\000\
\\019\000\020\000\021\000\022\000\015\000\016\000\017\000\018\000\
\\078\000\149\000\150\000\139\000\140\000\141\000\142\000\143\000\
\\144\000\145\000\146\000\147\000\148\000\153\000\152\000\079\000\
\\080\000\110\000\083\000\111\000\082\000\087\000\089\000\090\000\
\\112\000\097\000\098\000\099\000\103\000\120\000\121\000\128\000\
\\169\000\131\000\129\000\172\000\173\000\113\000\132\000\133\000\
\\114\000\130\000\115\000\151\000\032\000\157\000\154\000\156\000\
\\155\000\116\000\158\000\159\000\160\000\032\000\174\000\117\000\
\\170\000\171\000\032\000\161\000\162\000\027\000\118\000\046\000\
\\119\000\032\000\028\000\032\000\032\000\032\000\032\000\163\000\
\\164\000\029\000\165\000\015\000\016\000\017\000\018\000\032\000\
\\032\000\032\000\032\000\015\000\016\000\017\000\018\000\019\000\
\\020\000\021\000\022\000\166\000\178\000\167\000\168\000\019\000\
\\020\000\021\000\022\000\175\000\176\000\177\000\179\000\002\000\
\\012\000\023\000\012\000\100\000\051\000";

val yycheck = "\014\000\
\\013\000\017\001\013\000\013\000\005\001\046\000\047\000\048\000\
\\004\001\050\000\013\000\027\001\001\000\028\000\022\001\027\001\
\\015\001\025\001\009\001\018\001\033\000\024\000\033\000\033\000\
\\015\001\020\001\034\001\030\000\027\001\010\001\033\000\027\001\
\\040\001\006\001\044\000\015\001\027\001\045\001\041\000\049\000\
\\020\001\049\001\021\001\058\000\035\001\024\001\047\001\027\001\
\\044\000\052\000\020\001\061\000\038\001\049\000\033\001\002\001\
\\015\001\006\001\006\001\062\000\039\001\038\001\065\000\104\000\
\\105\000\106\000\107\000\108\000\027\001\048\001\011\001\012\001\
\\013\001\015\001\007\001\006\001\018\001\080\000\019\001\007\001\
\\038\001\002\001\023\001\015\001\087\000\027\001\018\001\028\001\
\\091\000\092\000\093\000\094\000\095\000\096\000\018\001\027\001\
\\041\001\042\001\043\001\044\001\029\001\030\001\031\001\032\001\
\\027\001\120\000\121\000\110\000\111\000\112\000\113\000\114\000\
\\115\000\116\000\117\000\118\000\119\000\130\000\129\000\027\001\
\\017\001\001\001\036\001\003\001\002\001\002\001\027\001\027\001\
\\008\001\035\001\002\001\002\001\027\001\020\001\020\001\027\001\
\\151\000\027\001\037\001\154\000\155\000\021\001\027\001\002\001\
\\024\001\014\001\026\001\020\001\006\001\038\001\020\001\027\001\
\\020\001\033\001\038\001\038\001\038\001\015\001\007\001\039\001\
\\027\001\027\001\020\001\038\001\038\001\015\001\046\001\006\001\
\\048\001\027\001\020\001\029\001\030\001\031\001\032\001\038\001\
\\038\001\027\001\038\001\029\001\030\001\031\001\032\001\041\001\
\\042\001\043\001\044\001\029\001\030\001\031\001\032\001\041\001\
\\042\001\043\001\044\001\038\001\027\001\038\001\038\001\041\001\
\\042\001\043\001\044\001\038\001\038\001\038\001\027\001\010\001\
\\038\001\002\001\007\000\080\000\033\000";

val yyact = vector_ 65 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file Parser.grm, line 38 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : Data.Block list
val d__2__ = peekVal 0 : (int*int)
in
( ProgramS((d__1__)) ) end : Data.Program))
;
(* Rule 2, file Parser.grm, line 41 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 0 : Data.Block
in
( [ (d__1__) ] ) end : Data.Block list))
;
(* Rule 3, file Parser.grm, line 42 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 1 : Data.Block
val d__2__ = peekVal 0 : Data.Block list
in
( (d__1__) :: (d__2__) ) end : Data.Block list))
;
(* Rule 4, file Parser.grm, line 45 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 4 : Data.Entry
val d__2__ = peekVal 3 : (int*int)
val d__3__ = peekVal 2 : Data.Stat list
val d__4__ = peekVal 1 : (int*int)
val d__5__ = peekVal 0 : Data.Exit
in
( BlockS((d__1__), (d__3__), (d__5__), (d__2__)) ) end : Data.Block))
;
(* Rule 5, file Parser.grm, line 48 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : string*(int*int)
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : Data.Arguments list
val d__5__ = peekVal 0 : (int*int)
in
( BeginS((d__2__), (d__4__), (d__1__)) ) end : Data.Entry))
;
(* Rule 6, file Parser.grm, line 49 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 4 : string*(int*int)
val d__2__ = peekVal 3 : (int*int)
val d__3__ = peekVal 2 : Data.Arguments list
val d__4__ = peekVal 1 : (int*int)
val d__5__ = peekVal 0 : (int*int)
in
( UncondEntryS((d__1__), (d__3__), (d__5__)) ) end : Data.Entry))
;
(* Rule 7, file Parser.grm, line 50 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 6 : string*(int*int)
val d__2__ = peekVal 5 : (int*int)
val d__3__ = peekVal 4 : Data.Arguments list
val d__4__ = peekVal 3 : (int*int)
val d__5__ = peekVal 2 : string*(int*int)
val d__6__ = peekVal 1 : (int*int)
val d__7__ = peekVal 0 : Data.Condition
in
( CondEntryS((d__7__), (d__1__), (d__5__), (d__3__), (d__6__)) ) end : Data.Entry))
;
(* Rule 8, file Parser.grm, line 53 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : string*(int*int)
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : Data.Arguments list
val d__5__ = peekVal 0 : (int*int)
in
( EndS((d__2__), (d__4__), (d__1__)) ) end : Data.Exit))
;
(* Rule 9, file Parser.grm, line 54 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 6 : Data.Condition
val d__2__ = peekVal 5 : (int*int)
val d__3__ = peekVal 4 : string*(int*int)
val d__4__ = peekVal 3 : (int*int)
val d__5__ = peekVal 2 : Data.Arguments list
val d__6__ = peekVal 1 : (int*int)
val d__7__ = peekVal 0 : string*(int*int)
in
( CondExitS((d__1__), (d__3__), (d__7__), (d__5__), (d__2__)) ) end : Data.Exit))
;
(* Rule 10, file Parser.grm, line 55 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : string*(int*int)
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : Data.Arguments list
val d__5__ = peekVal 0 : (int*int)
in
( UncondExitS((d__2__), (d__4__), (d__1__)) ) end : Data.Exit))
;
(* Rule 11, file Parser.grm, line 58 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 3 : Data.Type
val d__2__ = peekVal 2 : Data.Atom
val d__3__ = peekVal 1 : (int*int)
val d__4__ = peekVal 0 : Data.Arguments list
in
( ArgS((d__1__), (d__2__)) :: (d__4__) ) end : Data.Arguments list))
;
(* Rule 12, file Parser.grm, line 59 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 1 : Data.Type
val d__2__ = peekVal 0 : Data.Atom
in
( [ ArgS((d__1__), (d__2__)) ] ) end : Data.Arguments list))
;
(* Rule 13, file Parser.grm, line 62 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 0 : Data.intType
in
( TypeS(Secret, (d__1__)) ) end : Data.Type))
;
(* Rule 14, file Parser.grm, line 63 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 0 : Data.intType
in
( TypeS(Public, (d__1__)) ) end : Data.Type))
;
(* Rule 15, file Parser.grm, line 66 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( u8 ) end : Data.intType))
;
(* Rule 16, file Parser.grm, line 67 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( u16 ) end : Data.intType))
;
(* Rule 17, file Parser.grm, line 68 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( u32 ) end : Data.intType))
;
(* Rule 18, file Parser.grm, line 69 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( u64 ) end : Data.intType))
;
(* Rule 19, file Parser.grm, line 72 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( u8 ) end : Data.intType))
;
(* Rule 20, file Parser.grm, line 73 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( u16 ) end : Data.intType))
;
(* Rule 21, file Parser.grm, line 74 *)
val _ = update_ yyact 21
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( u32 ) end : Data.intType))
;
(* Rule 22, file Parser.grm, line 75 *)
val _ = update_ yyact 22
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( u64 ) end : Data.intType))
;
(* Rule 23, file Parser.grm, line 78 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( VarS (d__1__) ) end : Data.Atom))
;
(* Rule 24, file Parser.grm, line 79 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( CstS (d__1__) ) end : Data.Atom))
;
(* Rule 25, file Parser.grm, line 82 *)
val _ = update_ yyact 25
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Atom
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Atom
in
( BoolOp2S("==", (d__1__), (d__3__), (d__2__)) ) end : Data.Condition))
;
(* Rule 26, file Parser.grm, line 83 *)
val _ = update_ yyact 26
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Atom
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Atom
in
( BoolOp2S("!=", (d__1__), (d__3__), (d__2__)) ) end : Data.Condition))
;
(* Rule 27, file Parser.grm, line 84 *)
val _ = update_ yyact 27
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Atom
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Atom
in
( BoolOp2S("<", (d__1__), (d__3__), (d__2__)) ) end : Data.Condition))
;
(* Rule 28, file Parser.grm, line 85 *)
val _ = update_ yyact 28
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Atom
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Atom
in
( BoolOp2S("<=", (d__1__), (d__3__), (d__2__)) ) end : Data.Condition))
;
(* Rule 29, file Parser.grm, line 86 *)
val _ = update_ yyact 29
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Atom
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Atom
in
( BoolOp2S(">", (d__1__), (d__3__), (d__2__)) ) end : Data.Condition))
;
(* Rule 30, file Parser.grm, line 87 *)
val _ = update_ yyact 30
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Atom
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Atom
in
( BoolOp2S(">=", (d__1__), (d__3__), (d__2__)) ) end : Data.Condition))
;
(* Rule 31, file Parser.grm, line 90 *)
val _ = update_ yyact 31
(fn () => repr(let
val d__1__ = peekVal 3 : Data.Type
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Data.Atom
val d__4__ = peekVal 0 : (int*int)
in
( MemoryS((d__1__), (d__3__), (d__2__)) ) end : Data.Memory))
;
(* Rule 32, file Parser.grm, line 93 *)
val _ = update_ yyact 32
(fn () => repr(let
val d__1__ = peekVal 3 : Data.Type
val d__2__ = peekVal 2 : Data.Atom
val d__3__ = peekVal 1 : (int*int)
val d__4__ = peekVal 0 : Data.Atom
in
( Assign2S((d__1__), (d__2__), (d__4__), (d__3__)) ) end : Data.Stat))
;
(* Rule 33, file Parser.grm, line 94 *)
val _ = update_ yyact 33
(fn () => repr(let
val d__1__ = peekVal 3 : Data.Type
val d__2__ = peekVal 2 : Data.Atom
val d__3__ = peekVal 1 : (int*int)
val d__4__ = peekVal 0 : Data.Update
in
( AssignS((d__1__), (d__2__), (d__4__), (d__3__)) ) end : Data.Stat))
;
(* Rule 34, file Parser.grm, line 95 *)
val _ = update_ yyact 34
(fn () => repr(let
val d__1__ = peekVal 8 : Data.Type
val d__2__ = peekVal 7 : string*(int*int)
val d__3__ = peekVal 6 : (int*int)
val d__4__ = peekVal 5 : Data.Type
val d__5__ = peekVal 4 : string*(int*int)
val d__6__ = peekVal 3 : (int*int)
val d__7__ = peekVal 2 : string*(int*int)
val d__8__ = peekVal 1 : (int*int)
val d__9__ = peekVal 0 : string*(int*int)
in
( DAssignS((d__1__), VarS (d__2__), (d__4__), VarS (d__5__), VarS (d__7__), VarS (d__9__), (d__6__)) ) end : Data.Stat))
;
(* Rule 35, file Parser.grm, line 96 *)
val _ = update_ yyact 35
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Memory
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Operation
in
( MemOp2S("+=", (d__1__), (d__3__), (d__2__)) ) end : Data.Stat))
;
(* Rule 36, file Parser.grm, line 97 *)
val _ = update_ yyact 36
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Memory
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Operation
in
( MemOp2S("-=", (d__1__), (d__3__), (d__2__)) ) end : Data.Stat))
;
(* Rule 37, file Parser.grm, line 98 *)
val _ = update_ yyact 37
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Memory
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Operation
in
( MemOp2S("^=", (d__1__), (d__3__), (d__2__)) ) end : Data.Stat))
;
(* Rule 38, file Parser.grm, line 99 *)
val _ = update_ yyact 38
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Memory
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Operation
in
( MemOp2S("<<=", (d__1__), (d__3__), (d__2__)) ) end : Data.Stat))
;
(* Rule 39, file Parser.grm, line 100 *)
val _ = update_ yyact 39
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Memory
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Operation
in
( MemOp2S(">>=", (d__1__), (d__3__), (d__2__)) ) end : Data.Stat))
;
(* Rule 40, file Parser.grm, line 101 *)
val _ = update_ yyact 40
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Memory
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Memory
in
( MemSwapS((d__1__), (d__3__), (d__2__)) ) end : Data.Stat))
;
(* Rule 41, file Parser.grm, line 102 *)
val _ = update_ yyact 41
(fn () => repr(let
val d__1__ = peekVal 4 : Data.Atom
val d__2__ = peekVal 3 : (int*int)
val d__3__ = peekVal 2 : Data.Memory
val d__4__ = peekVal 1 : (int*int)
val d__5__ = peekVal 0 : Data.Atom
in
( SwapS((d__1__), (d__3__), (d__5__), (d__2__)) ) end : Data.Stat))
;
(* Rule 42, file Parser.grm, line 103 *)
val _ = update_ yyact 42
(fn () => repr(let
val d__1__ = peekVal 8 : (int*int)
val d__2__ = peekVal 7 : Data.Arguments list
val d__3__ = peekVal 6 : (int*int)
val d__4__ = peekVal 5 : (int*int)
val d__5__ = peekVal 4 : (int*int)
val d__6__ = peekVal 3 : string*(int*int)
val d__7__ = peekVal 2 : (int*int)
val d__8__ = peekVal 1 : Data.Arguments list
val d__9__ = peekVal 0 : (int*int)
in
( AssignArgS((d__2__), CallS((d__6__), (d__8__), (d__5__)), (d__4__)) ) end : Data.Stat))
;
(* Rule 43, file Parser.grm, line 104 *)
val _ = update_ yyact 43
(fn () => repr(let
val d__1__ = peekVal 8 : (int*int)
val d__2__ = peekVal 7 : Data.Arguments list
val d__3__ = peekVal 6 : (int*int)
val d__4__ = peekVal 5 : (int*int)
val d__5__ = peekVal 4 : (int*int)
val d__6__ = peekVal 3 : string*(int*int)
val d__7__ = peekVal 2 : (int*int)
val d__8__ = peekVal 1 : Data.Arguments list
val d__9__ = peekVal 0 : (int*int)
in
( AssignArgS((d__2__), UncallS((d__6__), (d__8__), (d__5__)), (d__4__)) ) end : Data.Stat))
;
(* Rule 44, file Parser.grm, line 105 *)
val _ = update_ yyact 44
(fn () => repr(let
val d__1__ = peekVal 6 : Data.intType
val d__2__ = peekVal 5 : (int*int)
val d__3__ = peekVal 4 : string*(int*int)
val d__4__ = peekVal 3 : (int*int)
val d__5__ = peekVal 2 : (int*int)
val d__6__ = peekVal 1 : Data.intType
val d__7__ = peekVal 0 : string*(int*int)
in
( TAssignS(TypeS(Public, (d__1__)), VarS (d__3__), RevealS(TypeS (Secret, (d__6__)), VarS (d__7__), (d__5__)), (d__4__)) ) end : Data.Stat))
;
(* Rule 45, file Parser.grm, line 106 *)
val _ = update_ yyact 45
(fn () => repr(let
val d__1__ = peekVal 6 : Data.intType
val d__2__ = peekVal 5 : (int*int)
val d__3__ = peekVal 4 : string*(int*int)
val d__4__ = peekVal 3 : (int*int)
val d__5__ = peekVal 2 : (int*int)
val d__6__ = peekVal 1 : Data.intType
val d__7__ = peekVal 0 : string*(int*int)
in
( TAssignS(TypeS(Secret, (d__1__)), VarS (d__3__), HideS(TypeS (Public, (d__6__)), VarS (d__7__), (d__5__)), (d__4__)) ) end : Data.Stat))
;
(* Rule 46, file Parser.grm, line 109 *)
val _ = update_ yyact 46
(fn () => repr(let
val d__1__ = peekVal 0 : Data.Stat
in
( [ (d__1__) ] ) end : Data.Stat list))
;
(* Rule 47, file Parser.grm, line 110 *)
val _ = update_ yyact 47
(fn () => repr(let
val d__1__ = peekVal 1 : Data.Stat
val d__2__ = peekVal 0 : Data.Stat list
in
( (d__1__) :: (d__2__) ) end : Data.Stat list))
;
(* Rule 48, file Parser.grm, line 113 *)
val _ = update_ yyact 48
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Atom
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Operation
in
( UpdOp2S("+", (d__1__), (d__3__), (d__2__)) ) end : Data.Update))
;
(* Rule 49, file Parser.grm, line 114 *)
val _ = update_ yyact 49
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Atom
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Operation
in
( UpdOp2S("-", (d__1__), (d__3__), (d__2__)) ) end : Data.Update))
;
(* Rule 50, file Parser.grm, line 115 *)
val _ = update_ yyact 50
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Atom
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Operation
in
( UpdOp2S("^", (d__1__), (d__3__), (d__2__)) ) end : Data.Update))
;
(* Rule 51, file Parser.grm, line 116 *)
val _ = update_ yyact 51
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Atom
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Operation
in
( UpdOp2S("<<", (d__1__), (d__3__), (d__2__)) ) end : Data.Update))
;
(* Rule 52, file Parser.grm, line 117 *)
val _ = update_ yyact 52
(fn () => repr(let
val d__1__ = peekVal 2 : Data.Atom
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Data.Operation
in
( UpdOp2S(">>", (d__1__), (d__3__), (d__2__)) ) end : Data.Update))
;
(* Rule 53, file Parser.grm, line 120 *)
val _ = update_ yyact 53
(fn () => repr(let
val d__1__ = peekVal 0 : Data.Atom
in
( SimOp2S((d__1__)) ) end : Data.Operation))
;
(* Rule 54, file Parser.grm, line 121 *)
val _ = update_ yyact 54
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : Data.Atom
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : Data.Atom
val d__5__ = peekVal 0 : (int*int)
in
( Op2S("+", (d__2__), (d__4__), (d__3__)) ) end : Data.Operation))
;
(* Rule 55, file Parser.grm, line 122 *)
val _ = update_ yyact 55
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : Data.Atom
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : Data.Atom
val d__5__ = peekVal 0 : (int*int)
in
( Op2S("-", (d__2__), (d__4__), (d__3__)) ) end : Data.Operation))
;
(* Rule 56, file Parser.grm, line 123 *)
val _ = update_ yyact 56
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : Data.Atom
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : Data.Atom
val d__5__ = peekVal 0 : (int*int)
in
( Op2S("^", (d__2__), (d__4__), (d__3__)) ) end : Data.Operation))
;
(* Rule 57, file Parser.grm, line 124 *)
val _ = update_ yyact 57
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : Data.Atom
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : Data.Atom
val d__5__ = peekVal 0 : (int*int)
in
( Op2S("<<", (d__2__), (d__4__), (d__3__)) ) end : Data.Operation))
;
(* Rule 58, file Parser.grm, line 125 *)
val _ = update_ yyact 58
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : Data.Atom
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : Data.Atom
val d__5__ = peekVal 0 : (int*int)
in
( Op2S(">>", (d__2__), (d__4__), (d__3__)) ) end : Data.Operation))
;
(* Rule 59, file Parser.grm, line 126 *)
val _ = update_ yyact 59
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : Data.Atom
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : Data.Atom
val d__5__ = peekVal 0 : (int*int)
in
( Op2S("*", (d__2__), (d__4__), (d__3__)) ) end : Data.Operation))
;
(* Rule 60, file Parser.grm, line 127 *)
val _ = update_ yyact 60
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : Data.Atom
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : Data.Atom
val d__5__ = peekVal 0 : (int*int)
in
( Op2S("div", (d__2__), (d__4__), (d__3__)) ) end : Data.Operation))
;
(* Rule 61, file Parser.grm, line 128 *)
val _ = update_ yyact 61
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : Data.Atom
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : Data.Atom
val d__5__ = peekVal 0 : (int*int)
in
( Op2S("mod", (d__2__), (d__4__), (d__3__)) ) end : Data.Operation))
;
(* Rule 62, file Parser.grm, line 129 *)
val _ = update_ yyact 62
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : Data.Atom
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : Data.Atom
val d__5__ = peekVal 0 : (int*int)
in
( Op2S("&", (d__2__), (d__4__), (d__3__)) ) end : Data.Operation))
;
(* Rule 63, file Parser.grm, line 130 *)
val _ = update_ yyact 63
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : Data.Atom
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : Data.Atom
val d__5__ = peekVal 0 : (int*int)
in
( Op2S("|", (d__2__), (d__4__), (d__3__)) ) end : Data.Operation))
;
(* Entry Program *)
val _ = update_ yyact 64 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun Program lexer lexbuf = yyparse yytables 1 lexer lexbuf;
