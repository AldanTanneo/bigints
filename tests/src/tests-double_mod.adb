procedure Tests.Double_Mod is
   A : constant U256 :=
     U256s.From_Hex ("44acf6b7e36c1342c2c5897204fe09504e1e2efb1a900377dbc4e7a6a133ec56");
   N : constant U256 :=
     U256s.From_Hex ("ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551");
begin
   Assert_Eq (U256_Modulo.Add_Mod (A, A, N), U256_Modulo.Double_Mod (A, N), "A + A /= 2 * A");
end Tests.Double_Mod;
