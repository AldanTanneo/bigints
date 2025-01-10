with Bigints.Modular;

procedure Tests.Modular is
   P7 : constant U256 := U256s.From_U64 (7);
   package GF7 is new Bigints.Modular (U256s, P7);

   subtype Fp is GF7.Fp;

   use GF7;

   X  : constant Fp   := Create (3);
   Y  : constant Fp   := Create (4);

   IX : constant Fp := Inv (X);
begin
   Assert_Eq (Retrieve (X * Y), U256s.From_U64 ((3 * 4) mod 7));

   Assert_Eq (Retrieve (X * IX), U256s.ONE);
end Tests.Modular;
