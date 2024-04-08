signature Types =
sig

  exception Error of string*(int*int)

  val check : Data.Program -> unit
end
