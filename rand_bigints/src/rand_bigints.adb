package body Rand_Bigints is
   function Generic_Gen (R : in out Rng) return Uints.Uint
   is ([for I in 1 .. Uints.N => R.Next]);

   function Gen_Classwide is new Generic_Gen (Rand_Core.Generators.Rng'Class);
   function Gen (R : in out Rand_Core.Generators.Rng'Class) return Uints.Uint
   renames Gen_Classwide;
end Rand_Bigints;
