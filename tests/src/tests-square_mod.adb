procedure Tests.Square_Mod is
   use F25519;

   X : Fp;

   N : constant Natural := (if Test_Is_Github_CI then 10 ** 6 else 10 ** 5);
begin
   for I in 1 .. N loop
      X := Create (Random_U256);
      Assert_Eq (Square (X), X * X);
   end loop;
end Tests.Square_Mod;
