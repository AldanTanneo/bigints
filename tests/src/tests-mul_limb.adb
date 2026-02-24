procedure Tests.Mul_Limb is
   A, B     : U256;
   ResCarry : U256s.Uint_Carry;

   N : constant Natural := (if Test_Is_Github_CI then 10 ** 6 else 10 ** 5);
begin
   for I in 1 .. N loop
      A := Random_U256;
      B := Random_U256;
      B (2 .. 4) := [others => 0];

      ResCarry := U256s.Mul_Limb (A, B (1));

      declare
         Ab : constant Big_Natural := To_Big_Number (A);
         Bb : constant Big_Natural := To_Big_Number (B);
         Mb : constant Big_Natural := Ab * Bb;
         Rb : constant Big_Natural := Mb mod (2 ** 256);
         Cb : constant Big_Natural := Mb / (2 ** 256);
      begin
         Assert (Mb = To_Big_Number (ResCarry.Res) + (2 ** 256) * To_Big_Number (ResCarry.Carry));

         Assert_Eq (ResCarry.Res, To_Uint (Rb), "invalid result");
         Assert_Eq (ResCarry.Carry, To_U64 (Cb), "invalid carry");
      end;
   end loop;
end Tests.Mul_Limb;
