procedure Tests.Div_Rem is
   use U256s;

   A, B, Q, R : Uint;

   N : constant Natural := (if Test_Is_Github_CI then 10 ** 6 else 10 ** 5);
begin
   A :=
     From_Hex
       ("e26eb455e6b52676c7e35948d2ad9f718249415b821526cdf49b7d5c4c2999ed");
   B := From_Hex ("8aa2cd4b28164b88");
   Impl_Div_Rem (A, B, Q, R);
   Assert_Eq
     (Q, From_Hex ("1a21f2b3ce5eb79df1043754cb5aa5d4cb5af4e679eb19963"));
   Assert_Eq (R, From_Hex ("14be0964f5e1c55"));

   for I in 1 .. N loop
      A := Random_U256;
      B := Random_U256;
      B ((I mod 4) + 2 .. 4) := [others => 0];

      if To_Big_Number (A) < To_Big_Number (B) then
         Q := A;
         A := B;
         B := Q;
      end if;

      Impl_Div_Rem (A, B, Q, R);

      declare
         Ab : constant Big_Natural := To_Big_Number (A);
         Bb : constant Big_Natural := To_Big_Number (B);
         Qb : constant Big_Natural := Ab / Bb;
         Rb : constant Big_Natural := Ab mod Bb;
      begin
         Assert_Eq (Q, To_Uint (Qb), "[" & I'Image & "]: Q != A / B");
         Assert_Eq (R, To_Uint (Rb), "[" & I'Image & "]: R != A mod B");
      end;

   end loop;

end Tests.Div_Rem;
