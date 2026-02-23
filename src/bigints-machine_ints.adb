package body Bigints.Machine_Ints is
   function Low (X : U128) return U64 is
      Y : constant U128 := X and U128 (U64'Last);
   begin
      return U64 (Y);
   end Low;

   function High (X : U128) return U64 is
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
      return (Low (Ret), High (Ret));
   end Sub_Borrow;

   function Mac (A, B, C, Carry : U64) return Tuple is
      Ret : constant U128 := U128 (A) + (U128 (B) * U128 (C)) + U128 (Carry);
   begin
      return (Low (Ret), High (Ret));
   end Mac;

   function Saturating_Sub (A, B : U64) return U64 is
      T : constant Tuple := Sub_Borrow (A, B, 0);
   begin
      return T.Fst xor (T.Snd and T.Fst);
   end Saturating_Sub;
end Bigints.Machine_Ints;
