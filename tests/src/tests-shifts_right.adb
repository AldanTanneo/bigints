procedure Tests.Shifts_Right is
   use U256s;

   N          : constant U256 :=
     From_Hex ("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141");
   N_2        : constant U256 :=
     From_Hex ("7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5D576E7357A4501DDFE92F46681B20A0");
   SIXTY_FOUR : constant U256 :=
     From_Hex ("0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03B");

begin
   Assert_Eq (Shr1 (N), N_2, "N >> 1 != N / 2");
   Assert_Eq (Shr (N, 1), N_2, "N >> 1 != N / 2");
   Assert_Eq (Shr_Limb (N, 1).Res, N_2, "N >> 1 != N / 2");

   Assert_Eq (Shr (N, 64), SIXTY_FOUR, "invalid N >> 64");
end Tests.Shifts_Right;
