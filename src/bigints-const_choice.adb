package body Bigints.Const_Choice
  with SPARK_Mode => On
is

   overriding
   function "not" (C : Choice) return Choice is
   begin
      return Choice (not U64 (C));
   end "not";

   overriding
   function "or" (A, B : Choice) return Choice is
   begin
      return Choice (U64 (A) or U64 (B));
   end "or";

   overriding
   function "and" (A, B : Choice) return Choice is
   begin
      return Choice (U64 (A) and U64 (B));
   end "and";

   function Lsb (C : Choice) return U32 is
   begin
      return U32 (U64 (C) and 1);
   end Lsb;
   function Lsb (C : Choice) return U64 is
   begin
      return U64 (C) and 1;
   end Lsb;
   function Lsb (C : Choice) return U128 is
   begin
      return U128 (U64 (C) and 1);
   end Lsb;

   function Choice_From_Condition (Cond : Boolean) return Choice is
      V : constant U64 := Boolean'Pos (Cond);
   begin
      pragma
        Assert
          ((V = 0 and then -V = 0) or else (V = 1 and then -V = U64'Last));
      return Choice (-V);
   end Choice_From_Condition;

   function Cond_Select (A, B : U32; C : Choice) return U32 is
      Mask : constant U32 := U32 (Shift_Right (C, 32));
   begin
      return A xor (Mask and (A xor B));
   end Cond_Select;

   function Cond_Select (A, B : U64; C : Choice) return U64 is
   begin
      return A xor (U64 (C) and (A xor B));
   end Cond_Select;

   function Cond_Select (A, B : U128; C : Choice) return U128 is
      Mask : constant U128 := Shift_Left (U128 (C), 64) or U128 (C);
   begin
      return A xor (Mask and (A xor B));
   end Cond_Select;

end Bigints.Const_Choice;
