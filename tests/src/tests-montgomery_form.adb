with Bigints.F25519;

procedure Tests.Montgomery_Form is
   use U256s;

   A : U256;
   B : Fp;

   N : constant Natural := (if Test_Is_Github_CI then 10 ** 6 else 10 ** 5);
begin
   Assert_Eq (F25519.Retrieve (F25519.ONE), ONE);

   for I in 1 .. N loop
      A := Random_U256;
      B := F25519.Create (A);

      Assert_Eq (A mod F25519.MODULUS, F25519.Retrieve (B), "invalid montgomery form");
   end loop;
end Tests.Montgomery_Form;
