procedure Tests.Leading_Zeros is
   use U256s;

   N : U256;
begin
   N := ZERO;
   Assert (U256s.Leading_Zeros (N) = 256, "clz(0) != 256");

   N := [1 => 1, others => 0];
   Assert (U256s.Leading_Zeros (N) = 255, "clz(1) != 255");

   N := [128, others => 0];
   Assert (U256s.Leading_Zeros (N) = 248, "clz(128) != 248");

   N := [U64'Last, others => 0];
   Assert (U256s.Leading_Zeros (N) = 192, "clz(2**64-1) != 192");

   N := [0, 1, 0, 0];
   Assert (U256s.Leading_Zeros (N) = 191, "clz(2**64) != 191");

   N := [0, 0, 0, 1];
   Assert (U256s.Leading_Zeros (N) = 63, "clz(2**192) != 63");

   N := [0, 0, 0, 2 ** 63];
   Assert (U256s.Leading_Zeros (N) = 0, "clz(2**255) != 0");
end Tests.Leading_Zeros;
