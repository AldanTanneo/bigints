procedure Tests.Square_Mod is
   use F25519;

   X : Fp;
begin
   for I in 1 .. 100000 loop
      X := Create (Random_U256);
      Assert_Eq (Square (X), X * X);
   end loop;
end Tests.Square_Mod;