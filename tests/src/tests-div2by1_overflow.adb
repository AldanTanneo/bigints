procedure Tests.Div2By1_Overflow is
   R : constant Recip := Create_Recip (U64'Last - 1);
   D : constant Tuple := Div2By1 (U64'Last - 2, U64'Last - 63, R);
begin
   Assert_Eq (D.Fst, U64'Last, "D.Fst != U64'Last");
   Assert_Eq (D.Snd, U64'Last - 65, "D.Snd != U64'Last - 65");
end Tests.Div2By1_Overflow;
