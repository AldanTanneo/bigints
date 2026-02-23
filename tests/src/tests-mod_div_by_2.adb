procedure Tests.Mod_Div_By_2 is
   use F25519;

   A     : Fp;
   Inv_2 : constant Fp := Inv_Vartime (Create (2));

   N : constant Natural := (if Test_Is_Github_CI then 10 ** 6 else 10 ** 5);
begin
   for I in 1 .. N loop
      A := Create (Random_U256);

      Assert_Eq (Retrieve (Div_By_2 (A)), Retrieve (A * Inv_2));
   end loop;
end Tests.Mod_Div_By_2;
