with Bigints.Const_Choice; use Bigints.Const_Choice;

package body Bigints.Primitives
  with SPARK_Mode => On
is
   function Low (X : U128) return U64
   with Inline, Post => U128 (Low'Result) = X mod (2 ** 64)
   is
      Y : constant U128 := X and U128 (U64'Last);
   begin
      return U64 (Y);
   end Low;

   function High (X : U128) return U64
   with Inline, Post => U128 (High'Result) = X / (2 ** 64)
   is
      Y : constant U128 := Shift_Right (X, 64);
   begin
      return U64 (Y);
   end High;

   function Mul_Wide (X, Y : U64) return Tuple is
      Res : constant U128 := U128 (X) * U128 (Y);
   begin
      return (High (Res), Low (Res));
   end Mul_Wide;

   function Add_Wide (X_Hi, X_Lo, Y_Hi, Y_Lo : U64) return Tuple is
      Res : constant U128 :=
        (Shift_Left (U128 (X_Hi), 64) or U128 (X_Lo))
        + (Shift_Left (U128 (Y_Hi), 64) or U128 (Y_Lo));
   begin
      return (High (Res), Low (Res));
   end Add_Wide;

   function Add_Carry (X, Y, Carry : U64) return Tuple is
      A   : constant U128 := U128 (X);
      B   : constant U128 := U128 (Y);
      C   : constant U128 := U128 (Carry);
      Ret : constant U128 := A + B + C;
   begin
      return (Low (Ret), High (Ret));
   end Add_Carry;

   function Overflowing_Add (X, Y : U64) return Tuple is
      Res   : constant U64 := X + Y;
      Carry : constant Boolean := X > U64'Last - Y;
   begin
      return (Res, Boolean'Pos (Carry));
   end Overflowing_Add;

   function Sub_Borrow (X, Y, Borrow : U64) return Tuple is
      A   : constant U128 := U128 (X);
      B   : constant U128 := U128 (Y);
      Bor : constant U128 := U128 (Shift_Right (Borrow, 63));
      Ret : constant U128 := A - (B + Bor);
   begin
      pragma Assert (Bor in 0 .. 1);
      return (Low (Ret), High (Ret));
   end Sub_Borrow;

   function Mac (A, B, C, Carry : U64) return Tuple is
      Ret : constant U128 := U128 (A) + (U128 (B) * U128 (C)) + U128 (Carry);
   begin
      return (Low (Ret), High (Ret));
   end Mac;

   function Short_Div
     (Dividend      : U32;
      Dividend_Bits : Natural;
      Divisor       : U32;
      Divisor_Bits  : Natural) return U32
   with
     Pre =>
       Dividend_Bits >= Divisor_Bits
       and then Dividend_Bits <= 32
       and then Divisor_Bits in 1 .. 32
   is
      --  Calculates `dividend / divisor`, given `dividend` and `divisor`
      --  along with their maximum bitsizes.
      Divd : U32 := Dividend;
      Divo : U32 := Shift_Left (Divisor, Dividend_Bits - Divisor_Bits);
      Quot : U32 := 0;
      I    : Natural := Dividend_Bits - Divisor_Bits + 1;
   begin
      while I > 0 loop
         pragma Loop_Invariant (I <= 32);
         pragma Loop_Variant (Decreases => I);
         I := I - 1;
         declare
            Bit     : constant Choice := Choice_From_Condition (Divd < Divo);
            Inv_Bit : constant Choice := not Bit;
         begin
            Divd := Cond_Select (Divd - Divo, Divd, Bit);
            Divo := Shift_Right (Divo, 1);
            Quot := Quot or Shift_Left (Lsb (Inv_Bit), I);
         end;
      end loop;
      return Quot;
   end Short_Div;

   function Reciprocal (D : U64) return U64 is
      D0    : constant U64 := D and 1;
      D9    : constant U64 := Shift_Right (D, 55);
      D40   : constant U64 := Shift_Right (D, 24) + 1;
      D63   : constant U64 := Shift_Right (D, 1) + D0;
      V0    : constant U64 :=
        U64 (Short_Div (2 ** 19 - 3 * (2 ** 8), 19, U32 (D9), 9));
      V1    : constant U64 :=
        Shift_Left (V0, 11) - Shift_Right (V0 * V0 * D40, 40) - 1;
      V2    : constant U64 :=
        Shift_Left (V1, 13) + Shift_Right (V1 * (2 ** 60 - V1 * D40), 47);
      E     : constant U64 :=
        U64'Last - V2 * D63 + 1 + Shift_Right (V2, 1) * D0;
      HiLo1 : constant Tuple := Mul_Wide (V2, E);
      V3    : constant U64 := Shift_Left (V2, 31) + Shift_Right (HiLo1.Fst, 1);
      X     : constant U64 := V3 + 1;
      HiLo2 : constant Tuple := Mul_Wide (X, D);
      Hi    : constant U64 :=
        Cond_Select (D, HiLo2.Fst, Choice_From_Condition (X /= 0));
   begin
      return V3 - Hi - D;
   end Reciprocal;

   function Create_Recip (Divisor : U64) return Recip is
      Shift          : constant Natural := Leading_Zeros (Divisor);
      Div_Normalized : constant U64 := Shift_Left (Divisor, Shift);
   begin
      return (Div_Normalized, Shift, Reciprocal (Div_Normalized));
   end Create_Recip;

   function Div2By1 (U1, U0 : U64; Re : Recip) return Tuple is
      D     : constant U64 := Re.Div_Normalized;
      Q1Q0M : constant Tuple := Mul_Wide (Re.Reciprocal, U1);
      Q1Q0A : constant Tuple := Add_Wide (Q1Q0M.Fst, Q1Q0M.Snd, U1, U0);
      Q1    : U64 := Q1Q0A.Fst + 1;
      R     : U64 := U0 - (Q1 * D);
      Cmp   : Choice;
   begin
      Cmp := Choice_From_Condition (Q1Q0A.Snd < R);
      Q1 := Cond_Select (Q1, Q1 - 1, Cmp);
      R := Cond_Select (R, R + D, Cmp);

      Cmp := Choice_From_Condition (D <= R);
      Q1 := Cond_Select (Q1, Q1 + 1, Cmp);
      R := Cond_Select (R, R - 1, Cmp);

      return (Q1, R);
   end Div2By1;

   function Div3By2 (U2, U1, U0 : U64; V1_Recip : Recip; V0 : U64) return U64
   is
      Q_Maxed   : constant Choice :=
        Choice_From_Condition (U2 = V1_Recip.Div_Normalized);
      QuoRem    : Tuple :=
        Div2By1 (Cond_Select (U2, 0, Q_Maxed), U1, V1_Recip);
      Quo       : U64 renames QuoRem.Fst;
      Remainder : U128;
   begin
      Quo := Cond_Select (Quo, U64'Last, Q_Maxed);
      Remainder :=
        Cond_Select (U128 (QuoRem.Snd), U128 (U2) + U128 (U1), Q_Maxed);
      for I in 1 .. 2 loop
         declare
            QY   : constant U128 := U128 (Quo) * U128 (V0);
            RX   : constant U128 := Shift_Left (Remainder, 64) or U128 (U0);
            Done : constant Choice :=
              Choice_From_Condition (Shift_Right (Remainder, 64) /= 0)
              or Choice_From_Condition (QY < RX);
         begin
            Quo := Cond_Select (Quo - 1, Quo, Done);
            Remainder :=
              Cond_Select
                (Remainder + U128 (V1_Recip.Div_Normalized), Remainder, Done);
         end;
      end loop;
      return Quo;
   end Div3By2;

end Bigints.Primitives;
