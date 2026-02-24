procedure Tests.Mul_Wide is
   use U256s;

   A : constant U256 := [others => 0];
   B : constant U256 := [1, 0, 0, 0];
   C : constant U256 := [others => U64'Last];
   D : constant U256 := [U64'Last, U64'Last, 0, 0];

   X, Y : U256;
   Res  : Wide_Uint;

   N : constant Natural := (if Test_Is_Github_CI then 10 ** 6 else 10 ** 5);
begin
   Res := Mul_Wide (A, B);
   Assert_Eq (Res, [others => 0], "0 * 1 != 0");

   Res := Mul_Wide (B, C);
   Assert_Eq (Res, [1 .. 4 => U64'Last, others => 0], "1 * (2**256-1) != 2**256-1");

   Res := Mul_Wide (B + B, C);
   Assert_Eq
     (Res,
      [1 => 2 * U64'Last, 2 .. 4 => U64'Last, 5 => 1, others => 0],
      "2 * (2**256-1) != 2**257 - 2");

   Res := Mul_Wide (C, D);
   Assert_Eq
     (Res,
      [1 => 1, 2 => 0, 3 .. 4 => U64'Last, 5 => U64'Last - 1, 6 => U64'Last, 7 .. 8 => 0],
      "(2**128-1) * (2**256-1) != 2**384 - 2**256 - 2**128 - 1");

   Res := Mul_Wide (D, D);
   Assert_Eq
     (Res,
      [1 => 1, 3 => 2 * U64'Last, 4 => U64'Last, others => 0],
      "(2**128-1)**2 != 2**256 - 2**129 + 1");

   Res := Mul_Wide (C, C);
   Assert_Eq
     (Res,
      [1 => 1, 2 .. 4 => 0, 5 => U64'Last - 1, 6 .. 8 => U64'Last],
      "(2**256-1)**2 != 2**512 - 2**257 + 1");

   for I in 1 .. N loop
      X := Random_U256;
      Y := Random_U256;
      Res := Mul_Wide (X, Y);

      declare
         Xb : constant Big_Natural := To_Big_Number (X);
         Yb : constant Big_Natural := To_Big_Number (Y);
         Mb : constant Big_Natural := Xb * Yb;
      begin
         Assert_Eq (Res, To_Wide_Uint (Mb), "invalid wide mul");
      end;
   end loop;
end Tests.Mul_Wide;
