with Bigints.Primitives; use Bigints.Primitives;

package body Bigints.Modular with
  SPARK_Mode => On
is

   function Create (U : Uint) return Fp is
      Prod            : constant Wide_Uint := Uints.Mul_Wide (U, R2);
      Montgomery_Form : constant Uint      :=
        Montgomery_Reduction (Prod, P, MOD_NEG_INV);
   begin
      return Fp (Montgomery_Form);
   end Create;

   function Create (U : U64) return Fp is
      Prod            : constant Uint_Carry := Uints.Mul_Limb (R2, U);
      Prod_Wide       : constant Wide_Uint  :=
        Concat (Prod.Res, [Prod.Carry, others => 0]);
      Montgomery_Form : constant Uint       :=
        Montgomery_Reduction (Prod_Wide, P, MOD_NEG_INV);
   begin
      return Fp (Montgomery_Form);
   end Create;

   function Retrieve (F : Fp) return Uint is
      Extended : constant Wide_Uint := Concat (F, ZERO);
      Reduced  : constant Uint      :=
        Montgomery_Reduction (Extended, P, MOD_NEG_INV);
   begin
      return Reduced;
   end Retrieve;

   function Sub_Mod_With_Carry
     (Lhs : Uint; Carry : U64; Rhs, P : Uint) return Uint
   is
      OutBorrow : constant Uint_Carry := Sub_Borrow (Lhs, Rhs, 0);
      Mask      : constant U64        := (not (-Carry)) and OutBorrow.Carry;
   begin
      return OutBorrow.Res + (P and Mask);
   end Sub_Mod_With_Carry;

   procedure Impl_Montgomery_Reduction
     (Value       : in out Wide_Uint; MCarry : out U64; Modulus : Uint;
      Mod_Neg_Inv :        U64)
   is
      SumCarry   : Tuple := (0, 0);
      New_Sum    : U64 renames SumCarry.Fst;
      Meta_Carry : U64 renames SumCarry.Snd;

      LimbCarry : Tuple;
      New_Limb  : U64 renames LimbCarry.Fst;
      Carry     : U64 renames LimbCarry.Snd;

      U : U64;
   begin
      for I in 1 .. N loop
         U         := Value (I) * Mod_Neg_Inv;
         LimbCarry := Mac (Value (I), U, Modulus (1), 0);

         for J in 1 .. N - 1 loop
            LimbCarry     := Mac (Value (I + J), U, Modulus (J + 1), Carry);
            Value (I + J) := New_Limb;
         end loop;

         SumCarry      := Add_Carry (Value (I + N), Carry, Meta_Carry);
         Value (I + N) := New_Sum;
      end loop;

      MCarry := Meta_Carry;
   end Impl_Montgomery_Reduction;

   function Montgomery_Reduction
     (Value : Wide_Uint; Modulus : Uint; Mod_Neg_Inv : U64) return Uint
   is
      V : Wide_Uint := Value;
      M : U64;
   begin
      Impl_Montgomery_Reduction (V, M, Modulus, Mod_Neg_Inv);
      return Sub_Mod_With_Carry (Truncate_Upper (V), M, Modulus, Modulus);
   end Montgomery_Reduction;

   overriding function "+" (A, B : Fp) return Fp is
   begin
      return Fp (Uints_Modulo.Add_Mod (Uint (A), Uint (B), P));
   end "+";

   overriding function "-" (A : Fp) return Fp is
   begin
      return Fp (Uints_Modulo.Neg_Mod (Uint (A), P));
   end "-";

   overriding function "-" (A, B : Fp) return Fp is
   begin
      return Fp (Uints_Modulo.Sub_Mod (Uint (A), Uint (B), P));
   end "-";

   overriding function "*" (A, B : Fp) return Fp is
      W : constant Wide_Uint := Mul_Wide (A, B);
   begin
      return Fp (Montgomery_Reduction (W, P, MOD_NEG_INV));
   end "*";

   overriding function Square (A : Fp) return Fp is
      W : constant Wide_Uint := Square_Wide (A);
   begin
      return Fp (Montgomery_Reduction (W, P, MOD_NEG_INV));
   end Square;

   function Pow (A : Fp; N : Uint) return Fp is
      use Const_Choice;
      Y : Fp := ONE;
      X : Fp := A;
      C : Choice;
   begin
      for I in 0 .. BITS - 1 loop
         C := Bit_Vartime (N, I);
         Y := Cond_Select (Y, X * Y, C);
         X := X * X;
      end loop;
      return Y;
   end Pow;

   function Pow_Vartime (A : Fp; N : Uint) return Fp is
      Y : Fp := ONE;
      X : Fp := A;
   begin
      for I in 0 .. BITS - 1 loop
         if Bit_Vartime (N, I) then
            Y := Y * X;
         end if;
         X := X * X;
      end loop;
      return Y;
   end Pow_Vartime;

   function "**" (A : Fp; N : Uint) return Fp is (Pow (A, N));

   function Div_By_2 (A : Fp) return Fp is
      use Const_Choice;

      Half_Carry   : constant Uint_Carry := Shr1 (A);
      Half_Modulus : constant Uint       := Shr1 (P);

      Is_Odd : constant Choice := Choice_From_Bit (Half_Carry.Carry);

      If_Even : constant Uint := Half_Carry.Res;
      If_Odd  : constant Uint := Add_Carry (If_Even, Half_Modulus, 1).Res;
   begin
      return Fp (Cond_Select (If_Even, If_Odd, Is_Odd));
   end Div_By_2;

   function Inv (A : Fp) return Fp is (Pow (A, P_MINUS_TWO));

   function Inv_Vartime (A : Fp) return Fp is (Pow_Vartime (A, P_MINUS_TWO));

   overriding function Cond_Select
     (A, B : Fp; C : Const_Choice.Choice) return Fp
   is
   begin
      return Fp (Cond_Select (Uint (A), Uint (B), C));
   end Cond_Select;

   overriding procedure CSwap (A, B : in out Fp; C : Const_Choice.Choice) is
   begin
      CSwap (Uint (A), Uint (B), C);
   end CSwap;

end Bigints.Modular;
