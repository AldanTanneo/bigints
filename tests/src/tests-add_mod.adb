procedure Tests.Add_Mod is
   A : constant U256 :=
     U256s.From_Hex
       ("44acf6b7e36c1342c2c5897204fe09504e1e2efb1a900377dbc4e7a6a133ec56");
   B : constant U256 :=
     U256s.From_Hex
       ("d5777c45019673125ad240f83094d4252d829516fac8601ed01979ec1ec1a251");
   N : constant U256 :=
     U256s.From_Hex
       ("ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551");

   Actual : constant U256 := U256_Modulo.Add_Mod (A, B, N);

   Expected : constant U256 :=
     U256s.From_Hex
       ("1a2472fde50286541d97ca6a3592dd75beb9c9646e40c511b82496cfc3926956");
begin
   Assert_Eq (Expected, Actual, "invalid (A + B) % N");
end Tests.Add_Mod;
