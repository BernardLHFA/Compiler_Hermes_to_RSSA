begin main(p64 x0):
	p64 x1 := x0
	:
	x1 > 0 -> L1(p64 x1)L2
	L1(p64 x2)L3 <- x2 > 0:
	p64 x3 := x2 + 1
	:
	x3 > 0 -> L2(p64 x3)L3
	L2(p64 x4) <-:
	p64 x5 := x4
	:
end main(p64 x5)
