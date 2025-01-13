procedure Tests.Mul_Mod is
   use F25519;

   X, Y, Res : Fp;
begin
   for I in 1 .. 100000 loop
      X   := Create (Random_U256);
      Y   := Create (Random_U256);
      Res := X * Y;

      declare
         Xb : constant Big_Natural := To_Big_Number (Retrieve (X));
         Yb : constant Big_Natural := To_Big_Number (Retrieve (Y));
         Mb : constant Big_Natural := (Xb * Yb) mod To_Big_Number (MODULUS);
      begin
         Assert_Eq (Retrieve (Res), To_Uint (Mb), "invalid modular mul");
      end;
   end loop;
end Tests.Mul_Mod;
