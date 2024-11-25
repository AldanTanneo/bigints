procedure Tests.Bit_Vartime is
   use U256s;

   X : constant U256 := [2#10001001011#, 2#1010#, 2#0101#, 0];
begin
   Assert (Bit_Vartime (X, 0) = True, "Bit(0) != 1");
   Assert (Bit_Vartime (X, 1) = True, "Bit(1) != 1");
   Assert (Bit_Vartime (X, 2) = False, "Bit(2) != 0");
   Assert (Bit_Vartime (X, 3) = True, "Bit(3) != 1");
   Assert (Bit_Vartime (X, 9) = False, "Bit(9) != 0");
   Assert (Bit_Vartime (X, 10) = True, "Bit(10) != 1");

   Assert (Bit_Vartime (X, 64) = False, "Bit(64) != 0");
   Assert (Bit_Vartime (X, 65) = True, "Bit(65) != 1");

   Assert (Bit_Vartime (X, 130) = True, "Bit(130) != 1");
   Assert (Bit_Vartime (X, 131) = False, "Bit(131) != 0");
end Tests.Bit_Vartime;
