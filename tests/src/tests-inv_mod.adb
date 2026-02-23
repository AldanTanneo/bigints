procedure Tests.Inv_Mod is
   use F25519;

   A, B : Fp;

   N : constant Natural := (if Test_Is_Github_CI then 10 ** 6 else 10 ** 4);
begin
   for I in 1 .. N loop
      A := Create (Random_U256);
      if A /= ZERO then
         B := Inv (A);
         Assert_Eq (Retrieve (A * B), U256s.ONE);

         B := Inv_Vartime (A);
         Assert_Eq (Retrieve (A * B), U256s.ONE);
      end if;
   end loop;
end Tests.Inv_Mod;
