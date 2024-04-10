begin f(p64 x0):
	p64 x1 := x0 + 2
	p64 y := 0
	:
end f(p64 x1)

begin main(p64 x0):
	(p64 x1) := call f(p64 x0)
	:
end main(p64 x1)
