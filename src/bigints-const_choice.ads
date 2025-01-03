with Bigints.Machine_Ints; use Bigints.Machine_Ints;

package Bigints.Const_Choice
  with SPARK_Mode => On
is
   type Choice is private;

   function To_Bool (C : Choice) return Boolean
   with Ghost;

   function "not" (C : Choice) return Choice
   with Post => To_Bool ("not"'Result) = (not To_Bool (C)), Inline_Always;

   function "or" (A, B : Choice) return Choice
   with
     Post => To_Bool ("or"'Result) = (To_Bool (A) or else To_Bool (B)),
     Inline_Always;

   function "and" (A, B : Choice) return Choice
   with
     Post => To_Bool ("and"'Result) = (To_Bool (A) and then To_Bool (B)),
     Inline_Always;

   function Lsb (C : Choice) return U32
   with
     Inline,
     Post =>
       (To_Bool (C) and then Lsb'Result = 1)
       or else (not To_Bool (C) and then Lsb'Result = 0);
   function Lsb (C : Choice) return U64
   with
     Inline,
     Post =>
       (To_Bool (C) and then Lsb'Result = 1)
       or else (not To_Bool (C) and then Lsb'Result = 0);
   function Lsb (C : Choice) return U128
   with
     Inline,
     Post =>
       (To_Bool (C) and then Lsb'Result = 1)
       or else (not To_Bool (C) and then Lsb'Result = 0);

   function Choice_From_Condition (Cond : Boolean) return Choice
   with Post => To_Bool (Choice_From_Condition'Result) = Cond;

   function Cond_Select (A, B : U32; C : Choice) return U32
   with
     Inline,
     Post =>
       (if To_Bool (C) then Cond_Select'Result = B
        else Cond_Select'Result = A);
   function Cond_Select (A, B : U64; C : Choice) return U64
   with
     Inline,
     Post =>
       (if To_Bool (C) then Cond_Select'Result = B
        else Cond_Select'Result = A);
   function Cond_Select (A, B : U128; C : Choice) return U128
   with
     Inline,
     Post =>
       (if To_Bool (C) then Cond_Select'Result = B
        else Cond_Select'Result = A);

private
   type Choice is new U64
   with
     Type_Invariant => (Choice = 0 or else U64 (Choice) = U64'Last),
     Default_Value => 0;

   function To_Bool (C : Choice) return Boolean
   is (C /= 0);

end Bigints.Const_Choice;
