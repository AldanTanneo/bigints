procedure Tests.Square is
   use U256s;

   A  : constant U256 := From_U64 (16#ffff_ffff_ffff_ffff#);
   A2 : constant U256 := [1, 16#ffff_ffff_ffff_fffe#, 0, 0];

   B  : constant U256 := U256s.MAX;
   B2 : constant U256 := U256s.MAX - ONE;

   Res : Wide_Uint;

   X : U256;
begin
   Assert_Eq (Square (A), A2);

   Res := Square_Wide (B);
   Assert_Eq (Truncate (Res), ONE);
   Assert_Eq (Truncate_Upper (Res), B2);

   for I in 1 .. 100000 loop
      X := Random_U256;
      Assert (Square_Wide (X) = Mul_Wide (X, X));
   end loop;
end Tests.Square;
