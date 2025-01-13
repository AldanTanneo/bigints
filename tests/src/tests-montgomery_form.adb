with Bigints.F25519;
procedure Tests.Montgomery_Form is
   use U256s;

   A : U256;
   B : Fp;
begin
   Assert_Eq (F25519.Retrieve (F25519.ONE), ONE);

   for I in 1 .. 100000 loop
      A := Random_U256;
      B := F25519.Create (A);

      Assert_Eq
        (A mod F25519.MODULUS, F25519.Retrieve (B), "invalid montgomery form");
   end loop;
end Tests.Montgomery_Form;
