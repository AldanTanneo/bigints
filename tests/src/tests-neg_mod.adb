procedure Tests.Neg_Mod is
   use U256s;

   X : constant U256 :=
     U256s.From_Hex ("8d16e171674b4e6d8529edba4593802bf30b8cb161dd30aa8e550d41380007c2");
   P : constant U256 :=
     U256s.From_Hex ("928334a4e4be0843ec225a4c9c61df34bdc7a81513e4b6f76f2bfa3148e2e1b5");

   Actual : constant U256 := U256_Modulo.Neg_Mod (X, P);

   Expected : constant U256 :=
     U256s.From_Hex ("056c53337d72b9d666f86c9256ce5f08cabc1b63b207864ce0d6ecf010e2d9f3");
begin
   Assert_Eq (Expected, Actual, "invalid -X % P");

   Assert_Eq (U256_Modulo.Neg_Mod (ZERO, P), ZERO, "-0 % P != 0");
end Tests.Neg_Mod;
