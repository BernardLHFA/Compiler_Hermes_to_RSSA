begin main(p64 x0):
	p64 x1 := x0 + (1 + 0)
	:
	x1 > 5 -> L1(p64 x1)L2
	L1(p64 x2) <-:
	p64 x3 := x2 + 1
	:
	-> L3(p64 x3)
	L3(p64 x4)L2 <- x4 > 6:
	p64 x5 := x4 + 1
	:
end main(p64 x5)
