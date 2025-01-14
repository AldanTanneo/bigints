with Bigints.Const_Choice; use Bigints.Const_Choice;

procedure Tests.Curve25519 is
   --  https://www.rfc-editor.org/rfc/rfc7748

   use F25519;

   type U8 is mod 2**8;
   for U8'Size use 8;

   type Bytes is array (1 .. 32) of U8;

   function From_Hex (S : String) return Bytes is
      Res : Bytes := [others => 0];

      function Digit (C : Character) return U8 is
      begin
         case C is
            when '0' .. '9' =>
               return Character'Pos (C) - Character'Pos ('0');
            when 'a' .. 'f' =>
               return Character'Pos (C) - Character'Pos ('a') + 10;
            when 'A' .. 'F' =>
               return Character'Pos (C) - Character'Pos ('A') + 10;
            when others =>
               raise Program_Error with "Invalid hex digit";
         end case;
      end Digit;
   begin
      for I in 1 .. 32 loop
         Res (I) := Digit (S (2 * I - 1)) * 16 + Digit (S (2 * I));
      end loop;
      return Res;
   end From_Hex;

   function Decode_Little_Endian (K : Bytes) return U256 is
      Res : U256 := U256s.ZERO;
      Pow : U64;
   begin
      for I in 1 .. 4 loop
         Pow := 1;
         for J in 1 .. 8 loop
            Res (I) := Res (I) + U64 (K (8 * (I - 1) + J)) * Pow;
            Pow     := Pow * 256;
         end loop;
      end loop;
      return Res;
   end Decode_Little_Endian;

   function Decode_U_Coordinate (U : Bytes) return Fp is
      U_List : Bytes := U;
   begin
      U_List (32) := U_List (32) and 127;
      return Create (Decode_Little_Endian (U_List));
   end Decode_U_Coordinate;

   function Encode_U_Coordinate (U : Fp) return Bytes is
      U_Int : constant U256 := Retrieve (U);
      Res   : Bytes         := [others => 0];
   begin
      for I in 1 .. 32 loop
         Res (I) :=
           U8 (Shift_Right (U_Int ((I + 7) / 8), 8 * ((I - 1) mod 8)) and 255);
      end loop;
      return Res;
   end Encode_U_Coordinate;

   function Decode_Scalar (K : Bytes) return U256 is
      K_List : Bytes := K;
   begin
      K_List (1)  := K_List (1) and 248;
      K_List (32) := K_List (32) and 127;
      K_List (32) := K_List (32) or 64;
      return Decode_Little_Endian (K_List);
   end Decode_Scalar;

   function X25519 (K : U256; U : Fp) return Fp is
      A24  : constant Fp := Create (121665);
      X_1  : constant Fp := U;
      X_2  : Fp          := ONE;
      Z_2  : Fp          := ZERO;
      X_3  : Fp          := U;
      Z_3  : Fp          := ONE;
      Swap : Choice      := FALSEY;
   begin
      for T in reverse 0 .. 255 loop
         declare
            K_T : constant Choice := U256s.Bit_Vartime (K, T);
         begin
            Swap := Swap xor K_T;
            CSwap (X_2, X_3, Swap);
            CSwap (Z_2, Z_3, Swap);
            Swap := K_T;
         end;
         declare
            A     : constant Fp := X_2 + Z_2;
            AA    : constant Fp := Square (A);
            B     : constant Fp := X_2 - Z_2;
            BB    : constant Fp := Square (B);
            E     : constant Fp := AA - BB;
            C     : constant Fp := X_3 + Z_3;
            D     : constant Fp := X_3 - Z_3;
            DA    : constant Fp := D * A;
            CB    : constant Fp := C * B;
         begin
            X_3 := Square (DA + CB);
            Z_3 := X_1 * Square (DA - CB);
            X_2 := AA * BB;
            Z_2 := E * (AA + A24 * E);
         end;
      end loop;

      CSwap (X_2, X_3, Swap);
      CSwap (Z_2, Z_3, Swap);
      return X_2 * Inv_Vartime (Z_2);
   end X25519;

   function X25519 (K : Bytes; U : Bytes) return Bytes is
      K_Decoded : constant U256 := Decode_Scalar (K);
      U_Decoded : constant Fp   := Decode_U_Coordinate (U);
   begin
      return Encode_U_Coordinate (X25519 (K_Decoded, U_Decoded));
   end X25519;

   K, U, Res : Bytes;
   B         : constant Bytes :=
     From_Hex
       ("0900000000000000000000000000000000000000000000000000000000000000");

   A_Private, A_Public, B_Private, B_Public : Bytes;
begin

   --  RFC Test vectors 1

   K   :=
     From_Hex
       ("a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4");
   U   :=
     From_Hex
       ("e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c");
   Res :=
     From_Hex
       ("c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552");
   Assert (X25519 (K, U) = Res);

   K   :=
     From_Hex
       ("4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d");
   U   :=
     From_Hex
       ("e5210f12786811d3f4b7959d0538ae2c31dbe7106fc03c3efc4cd549c715a493");
   Res :=
     From_Hex
       ("95cbde9476e8907d7aade45cb4b873f88b595a68799fa152e6f8f7647aac7957");
   Assert (X25519 (K, U) = Res);

   --  RFC Test vectors 2

   K := B;
   U := B;

   Res := X25519 (K, U);
   U   := K;
   K   := Res;

   Res :=
     From_Hex
       ("422c8e7a6227d7bca1350b3e2bb7279f7897b87bb6854b783c60e80311ae3079");
   Assert (K = Res);

   for I in 2 .. 1_000 loop
      Res := X25519 (K, U);
      U   := K;
      K   := Res;
   end loop;

   Res :=
     From_Hex
       ("684cf59ba83309552800ef566f2f4d3c1c3887c49360e3875f2eb94d99532c51");
   Assert (K = Res);

   --  RFC Diffie-Hellman test

   A_Private :=
     From_Hex
       ("77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a");
   A_Public  := X25519 (A_Private, B);
   Assert
     (A_Public =
      From_Hex
        ("8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"));

   B_Private :=
     From_Hex
       ("5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb");
   B_Public  := X25519 (B_Private, B);
   Assert
     (B_Public =
      From_Hex
        ("de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"));

   Res := X25519 (A_Private, B_Public);
   Assert (Res = X25519 (B_Private, A_Public));
   Assert
     (Res =
      From_Hex
        ("4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742"));
end Tests.Curve25519;
