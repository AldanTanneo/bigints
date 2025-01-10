with Bigints.Const_Choice;
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

   function Div_By_2 (A : Fp) return Fp is
      Half_Carry   : constant Uint_Carry          := Shr1 (A);
      Is_Odd       : constant Const_Choice.Choice :=
        Const_Choice.Choice_From_Condition (Half_Carry.Carry /= 0);
      Half_Modulus : constant Uint                := Shr1 (P);
      If_Even      : constant Uint                := Half_Carry.Res;
      If_Odd       : constant Uint := Add_Carry (If_Even, Half_Modulus, 1).Res;
   begin
      return Fp (Cond_Select (If_Even, If_Odd, Is_Odd));
   end Div_By_2;

   function Inv_2k_Vartime (K : Natural) return Fp is
      Res : Fp := ONE;
   begin
      for I in 1 .. K loop
         Res := Div_By_2 (Res);
      end loop;
      return Res;
   end Inv_2k_Vartime;

   function Len (X : Uint) return Natural with
     Inline,
     Post =>
      Len'Result <= BITS
      and then ((Len'Result = 0) = (for all I in 1 .. N => X (I) = 0))
   is
   begin
      return BITS - Leading_Zeros (X);
   end Len;

   Len_P       : constant Positive := Len (P);
   K           : constant Natural  := 32;
   TwoKm1      : constant U64      := Shift_Left (1, K - 1) - 1;
   Inv_K1_2LdK : constant Fp       :=
     Inv_2k_Vartime ((K - 1) * ((2 * Len_P + K - 3) / (K - 1)));

   function Inv (Y : Fp) return Fp is
      --  Implement Optimized Extended Binary GCD
      --  (https://eprint.iacr.org/2020/972.pdf)
      use Const_Choice;

      function Mul_Add_Shift
        (A : Uint; F : U64; B : Uint; G : U64; K : Natural) return Uint with
        Pre => K in 1 .. 64
      is
         AF : constant Uint_Carry := Mul_Limb (A, F);
         BG : constant Uint_Carry := Mul_Limb (B, G);
         R  : Uint_Carry := Add_Carry (AF.Res, BG.Res, AF.Carry + BG.Carry);
      begin
         R.Res     := Shr_Vartime (R.Res, K);
         R.Res (N) := R.Res (N) or Shift_Left (R.Carry, 64 - K);
         return R.Res;
      end Mul_Add_Shift;

      function Mul_Vec_Mod (U, V : Fp; F, G : U64) return Fp is
         F_Neg : constant Choice := Choice_From_Condition (F >= 2**63);
         G_Neg : constant Choice := Choice_From_Condition (G >= 2**63);

         F_Abs : constant U64 := Cond_Select (F, -F, F_Neg);
         G_Abs : constant U64 := Cond_Select (G, -G, G_Neg);

         U_F : constant Fp := Cond_Select (U, -U, F_Neg) * Create (F_Abs);
         V_G : constant Fp := Cond_Select (V, -V, G_Neg) * Create (G_Abs);
      begin
         return U_F + V_G;
      end Mul_Vec_Mod;

      A : Uint := Retrieve (Y);
      U : Fp   := ONE;
      B : Uint := P;
      V : Fp   := ZERO;

      L      : Natural;
      A2, B2 : Uint;
      U2, V2 : Fp;

   begin
      for I in 1 .. (2 * Len_P + K - 2) / K loop
         L  := Natural'Max (Natural'Max (Len (A), Len (B)), 2 * K);
         A2 :=
           [A (1) and TwoKm1, others => 0] or Shl (Shr (A, L - K - 1), K - 1);
         B2 :=
           [B (1) and TwoKm1, others => 0] or Shl (Shr (B, L - K - 1), K - 1);
         declare
            F0                : U64 := 1;
            G0                : U64 := 0;
            F1                : U64 := 0;
            G1                : U64 := 1;
            A2_Even, A2_Lt_B2 : Choice;
            Swap, Lt_Zero     : Choice;
         begin
            for J in 1 .. K - 1 loop
               A2_Even  := Choice_From_Condition (A2 (1) mod 2 = 0);
               A2_Lt_B2 := Choice_From_Condition (A2 < B2);
               Swap     := (not A2_Even) and A2_Lt_B2;
               --  if A2 mod 2 == 0
               A2       := Cond_Select (A2, Shr1 (A2), A2_Even);
               --  else
               --    if A2 < B2
               CSwap (A2, B2, Swap);
               CSwap (F0, F1, Swap);
               CSwap (G0, G1, Swap);
               --    end if
               A2 := Cond_Select (Shr1 (A2 - B2), A2, A2_Lt_B2);
               F0 := Cond_Select (F0 - F1, F0, A2_Lt_B2);
               G0 := Cond_Select (G0 - G1, F0, A2_Lt_B2);
               --  end if
               F1 := Shift_Left (F1, 1);
               G1 := Shift_Left (G1, 1);
            end loop;
            A2 := A;
            B2 := B;
            A  := Mul_Add_Shift (A2, F0, B2, G0, K - 1);
            B  := Mul_Add_Shift (A2, F1, B2, G1, K - 1);

            Lt_Zero := Choice_From_Condition (A (N) >= 2**63); -- B < 0
            A       := Cond_Select (A, -A, Lt_Zero);
            F0      := Cond_Select (F0, -F0, Lt_Zero);
            G0      := Cond_Select (G0, -G0, Lt_Zero);
            Lt_Zero := Choice_From_Condition (B (N) >= 2**63); -- B < 0
            B       := Cond_Select (B, -B, Lt_Zero);
            F1      := Cond_Select (F1, -F1, Lt_Zero);
            G1      := Cond_Select (G1, -G1, Lt_Zero);

            U2 := U;
            V2 := V;
            U  := Mul_Vec_Mod (U2, V2, F0, G0);
            V  := Mul_Vec_Mod (U2, V2, F1, G1);
         end;
      end loop;
      return V * Inv_K1_2LdK;
   end Inv;

end Bigints.Modular;
