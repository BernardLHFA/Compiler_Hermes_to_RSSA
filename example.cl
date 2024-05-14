begin main(p64 ctT0, p64 ctS0, p64 ST0, p64 SS0):
  s32 A0 := 0
  s32 B0 := 0
  s32 C0 := 0
  s32 D0 := 0
  s32 E0 := 0
  s32 F0 := 0
  p64 T0 := 0 + (0 * 4)
  p64 I0 := 0 + (ctT0 + T0)
  A1 := s32[I0] := A0
  p64 0 := I0 - (ctT0 + T0)
  p64 0 := T0 - (0 * 4)
  p64 T1 := 0 + (1 * 4)
  p64 I1 := 0 + (ctT0 + T1)
  B1 := s32[I1] := B0
  p64 0 := I1 - (ctT0 + T1)
  p64 0 := T1 - (1 * 4)
  p64 T2 := 0 + (0 * 4)
  p64 I2 := 0 + (ST0 + T2)
  T3 := s32[I2] := 0
  s32 A2 := A1 + T3
  0 := s32[I2] := T3
  p64 0 := I2 - (ST0 + T2)
  p64 0 := T2 - (0 * 4)
  p64 T4 := 0 + (1 * 4)
  p64 I3 := 0 + (ST0 + T4)
  T5 := s32[I3] := 0
  s32 B2 := B1 + T5
  0 := s32[I3] := T5
  p64 0 := I3 - (ST0 + T4)
  p64 0 := T4 - (1 * 4)
  p64 i0 := 0 ^ 2
  :
-> main0L1(p64 ctT0, p64 ctS0, p64 ST0, p64 SS0, s32 A2, s32 B2, s32 C0, s32 D0, s32 E0, s32 F0, p64 i0)
main0L1(p64 ctT1, p64 ctS1, p64 ST1, p64 SS1, s32 A3, s32 B3, s32 C1, s32 D1, s32 E1, s32 F1, p64 i1)main0L2 <- i1 == 2:
  s32 A4 := A3 ^ B3
  s32 A5 := A4 << B3
  p64 T7 := 0 + (i1 * 4)
  p64 I5 := 0 + (ST1 + T7)
  T8 := s32[I5] := 0
  s32 A6 := A5 + T8
  0 := s32[I5] := T8
  p64 0 := I5 - (ST1 + T7)
  p64 0 := T7 - (i1 * 4)
  s32 B4, s32 A7 := A6, B3
  p64 i2 := i1 + 1
  :
i2 == SS1 -> main0L3(p64 ctT1, p64 ctS1, p64 ST1, p64 SS1, s32 A7, s32 B4, s32 C1, s32 D1, s32 E1, s32 F1, p64 i2)main0L2
main0L3(p64 ctT2, p64 ctS2, p64 ST2, p64 SS2, s32 A8, s32 B5, s32 C2, s32 D2, s32 E2, s32 F2, p64 i3) <-:
  p64 0 := i3 ^ SS2
  p64 T10 := 0 + (1 * 4)
  p64 I7 := 0 + (ctT2 + T10)
  B6 := s32[I7] := B5
  p64 0 := I7 - (ctT2 + T10)
  p64 0 := T10 - (1 * 4)
  p64 T11 := 0 + (0 * 4)
  p64 I8 := 0 + (ctT2 + T11)
  A9 := s32[I8] := A8
  p64 0 := I8 - (ctT2 + T11)
  p64 0 := T11 - (0 * 4)
  s32 0 := F2
  s32 0 := E2
  s32 0 := D2
  s32 0 := C2
  s32 0 := B6
  s32 0 := A9
  :
end main(p64 ctT2, p64 ctS2, p64 ST2, p64 SS2)
