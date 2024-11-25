procedure Tests.Sub_Borrow is
   use U256s;

   A : constant U256 := [others => 0];
   B : constant U256 := [1, 0, 0, 0];
   C : constant U256 := [others => U64'Last];
   D : constant U256 := [U64'Last, U64'Last, 0, 0];

   Tuple  : Uint_Carry;
   Res    : U256 renames Tuple.Res;
   Borrow : U64 renames Tuple.Carry;
begin
   Tuple := Sub_Borrow (B, B, 0);
   Assert_Eq (Res, A, "Res != 0");
   Assert_Eq (Borrow, 0, "Borrow != 0");

   Tuple := Sub_Borrow (B, A, U64'Last);
   Assert_Eq (Res, A, "Res != 0");
   Assert_Eq (Borrow, 0, "Borrow != 0");

   Tuple := Sub_Borrow (A, B, 0);
   Assert_Eq (Res, C, "Res != -1");
   Assert_Eq (Borrow, U64'Last, "Borrow != -1");

   Tuple := Sub_Borrow (A, A, U64'Last);
   Assert_Eq (Res, C, "Res != -1");
   Assert_Eq (Borrow, U64'Last, "Borrow != -1");

   Tuple := Sub_Borrow (C, D, 0);
   Assert_Eq
     (Res, [3 .. 4 => U64'Last, others => 0], "Res != 2**256 - 2**128");
end Tests.Sub_Borrow;
