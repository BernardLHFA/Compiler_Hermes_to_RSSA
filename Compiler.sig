signature Compiler =
sig

  exception Error of string*(int*int)

  val compile : Hermes.program -> Data.Program
end
