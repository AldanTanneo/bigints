package Bigints.Const_Choice
  with SPARK_Mode => On, Pure
is
   type Choice is private;

   function To_Bool (C : Choice) return Boolean
   with Ghost;

   function "not" (C : Choice) return Choice
   with Post => To_Bool ("not"'Result) = (not To_Bool (C)), Inline_Always;

   function "or" (A, B : Choice) return Choice
   with Post => To_Bool ("or"'Result) = (To_Bool (A) or else To_Bool (B)), Inline_Always;

   function "and" (A, B : Choice) return Choice
   with Post => To_Bool ("and"'Result) = (To_Bool (A) and then To_Bool (B)), Inline_Always;

   function "xor" (A, B : Choice) return Choice
   with Post => To_Bool ("xor"'Result) = (To_Bool (A) xor To_Bool (B)), Inline_Always;

   function Lsb (C : Choice) return U32
   with
     Post =>
       (To_Bool (C) and then Lsb'Result = 1) or else (not To_Bool (C) and then Lsb'Result = 0),
     Inline_Always;
   function Lsb (C : Choice) return U64
   with
     Post =>
       (To_Bool (C) and then Lsb'Result = 1) or else (not To_Bool (C) and then Lsb'Result = 0),
     Inline_Always;
   function Lsb (C : Choice) return U128
   with
     Post =>
       (To_Bool (C) and then Lsb'Result = 1) or else (not To_Bool (C) and then Lsb'Result = 0),
     Inline_Always;

   function Choice_From_Bit (Bit : U64) return Choice
   with Pre => Bit in 0 .. 1, Post => To_Bool (Choice_From_Bit'Result) = (Bit = 1), Inline_Always;

   function Choice_From_Mask (Mask : U64) return Choice
   with
     Pre  => Mask = 0 or else Mask = U64'Last,
     Post => To_Bool (Choice_From_Mask'Result) = (Mask /= 0),
     Inline_Always;

   generic
      type T is mod <>;
      with function Shift_Right (Value : T; Amount : Natural) return T is <>;
   function Generic_Ct_Eq (A, B : T) return Choice
   with Post => To_Bool (Generic_Ct_Eq'Result) = (A = B), Inline_Always;

   function Ct_Eq (A, B : U32) return Choice
   with Post => To_Bool (Ct_Eq'Result) = (A = B), Inline_Always;

   function Ct_Eq (A, B : U64) return Choice
   with Post => To_Bool (Ct_Eq'Result) = (A = B), Inline_Always;

   function Ct_Eq (A, B : U128) return Choice
   with Post => To_Bool (Ct_Eq'Result) = (A = B), Inline_Always;

   --  derived helpers
   function Ct_Ne (A, B : U32) return Choice
   is (not Ct_Eq (A, B))
   with Post => To_Bool (Ct_Ne'Result) = (A /= B);

   function Ct_Ne (A, B : U64) return Choice
   is (not Ct_Eq (A, B))
   with Post => To_Bool (Ct_Ne'Result) = (A /= B);

   function Ct_Ne (A, B : U128) return Choice
   is (not Ct_Eq (A, B))
   with Post => To_Bool (Ct_Ne'Result) = (A /= B);

   function Ct_Ge (A, B : U32) return Choice
   with Post => To_Bool (Ct_Ge'Result) = (A >= B), Inline_Always;

   function Ct_Ge (A, B : U64) return Choice
   with Post => To_Bool (Ct_Ge'Result) = (A >= B), Inline_Always;

   function Ct_Gt (A, B : U128) return Choice
   with Post => To_Bool (Ct_Gt'Result) = (A > B), Inline_Always;

   pragma Warnings (Off, "actuals for this call may be in wrong order");

   --  U32 derived helpers
   function Ct_Gt (A, B : U32) return Choice
   is (not (Ct_Ge (B, A)))
   with Post => To_Bool (Ct_Gt'Result) = (A > B), Inline_Always;

   function Ct_Lt (A, B : U32) return Choice
   is (Ct_Gt (B, A))
   with Post => To_Bool (Ct_Lt'Result) = (A < B), Inline_Always;

   function Ct_Le (A, B : U32) return Choice
   is (not (Ct_Gt (A, B)))
   with Post => To_Bool (Ct_Le'Result) = (A <= B), Inline_Always;

   --  U64 derived helpers
   function Ct_Gt (A, B : U64) return Choice
   is (not (Ct_Ge (B, A)))
   with Post => To_Bool (Ct_Gt'Result) = (A > B), Inline_Always;

   function Ct_Lt (A, B : U64) return Choice
   is (Ct_Gt (B, A))
   with Post => To_Bool (Ct_Lt'Result) = (A < B), Inline_Always;

   function Ct_Le (A, B : U64) return Choice
   is (not (Ct_Gt (A, B)))
   with Post => To_Bool (Ct_Le'Result) = (A <= B), Inline_Always;

   --  U128 derived helpers
   function Ct_Ge (A, B : U128) return Choice
   is (not (Ct_Gt (B, A)))
   with Post => To_Bool (Ct_Ge'Result) = (A >= B), Inline_Always;

   function Ct_Lt (A, B : U128) return Choice
   is (Ct_Gt (B, A))
   with Post => To_Bool (Ct_Lt'Result) = (A < B), Inline_Always;

   function Ct_Le (A, B : U128) return Choice
   is (not (Ct_Gt (A, B)))
   with Post => To_Bool (Ct_Le'Result) = (A <= B), Inline_Always;

   pragma Warnings (On, "actuals for this call may be in wrong order");

   function Cond_Select (A, B : U32; C : Choice) return U32
   with
     Post => (if To_Bool (C) then Cond_Select'Result = B else Cond_Select'Result = A),
     Inline_Always;
   function Cond_Select (A, B : U64; C : Choice) return U64
   with
     Post => (if To_Bool (C) then Cond_Select'Result = B else Cond_Select'Result = A),
     Inline_Always;
   function Cond_Select (A, B : U128; C : Choice) return U128
   with
     Post => (if To_Bool (C) then Cond_Select'Result = B else Cond_Select'Result = A),
     Inline_Always;

   procedure CSwap (A, B : in out U32; C : Choice)
   with
     Post => (if To_Bool (C) then B = A'Old and then A = B'Old else A = A'Old and then B = B'Old),
     Inline_Always;

   procedure CSwap (A, B : in out U64; C : Choice)
   with
     Post => (if To_Bool (C) then B = A'Old and then A = B'Old else A = A'Old and then B = B'Old),
     Inline_Always;

   procedure CSwap (A, B : in out U128; C : Choice)
   with
     Post => (if To_Bool (C) then B = A'Old and then A = B'Old else A = A'Old and then B = B'Old),
     Inline_Always;

   TRUTHY : constant Choice;
   FALSEY : constant Choice;

private
   type Choice is new U64
   with Static_Predicate => (Choice = 0 or else Choice = 2 ** 64 - 1), Default_Value => 0;

   function To_Bool (C : Choice) return Boolean
   is (C /= 0);

   TRUTHY : constant Choice := Choice (U64'Last);
   FALSEY : constant Choice := 0;

end Bigints.Const_Choice;
