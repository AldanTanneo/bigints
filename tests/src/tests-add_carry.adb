procedure Tests.Add_Carry is
   use U256s;

   A : constant U256 := [others => 0];
   B : constant U256 := [1, 0, 0, 0];
   C : constant U256 := [others => U64'Last];
   D : constant U256 := [U64'Last, U64'Last, 0, 0];
   E : constant U256 := [U64'Last - 1, U64'Last, 0, 0];
   F : constant U256 := [1 => U64'Last - 1, others => U64'Last];

   Tuple : Uint_Carry;
   Res   : U256 renames Tuple.Res;
   Carry : U64 renames Tuple.Carry;
begin
   Tuple := Add_Carry (A, A, 0);
   Assert_Eq (Res, A, "Res != 0");
   Assert_Eq (Carry, 0, "Carry != 0");

   Tuple := Add_Carry (A, A, 1);
   Assert_Eq (Res, B, "Res != 0");
   Assert_Eq (Carry, 0, "Carry != 0");

   Tuple := Add_Carry (A, D, 0);
   Assert_Eq (Res, D, "Res != 2**128 - 1");
   Assert_Eq (Carry, 0, "Carry != 0");

   Tuple := Add_Carry (C, D, 0);
   Assert_Eq (Res, E, "Res != 2**128 - 2");
   Assert_Eq (Carry, 1, "Carry != 1");

   Tuple := Add_Carry (C, B, 0);
   Assert_Eq (Res, A, "Res != 0");
   Assert_Eq (Carry, 1, "Carry != 1");

   Tuple := Add_Carry (C, C, 0);
   Assert_Eq (Res, F, "Res != 2**256 - 2");
   Assert_Eq (Carry, 1, "Carry != 1");

   Tuple := Add_Carry (C, C, 1);
   Assert_Eq (Res, C, "Res != 2**256 - 1");
   Assert_Eq (Carry, 1, "Carry != 1");
end Tests.Add_Carry;
