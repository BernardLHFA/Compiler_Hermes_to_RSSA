signature Interpreter =
sig

  exception Error of string*(int*int)

  val run : Data.Program -> bool -> unit
  val R: Data.Stat -> Data.Stat

end
