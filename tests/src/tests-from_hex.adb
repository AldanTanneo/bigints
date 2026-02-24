procedure Tests.From_Hex is
   A : constant String := "1234567890abcdef";
   B : constant String := "1000000000000000_0000000000000000";
   C : constant String := "00112233445566778899aabbccddeeff";
begin
   Assert_Eq (U256s.From_Hex (A), [16#1234567890abcdef#, 0, 0, 0]);

   Assert_Eq (U256s.From_Hex (B), [0, 16#1000000000000000#, 0, 0]);

   Assert_Eq (U256s.From_Hex (C), [16#8899aabbccddeeff#, 16#0011223344556677#, 0, 0]);
end Tests.From_Hex;
