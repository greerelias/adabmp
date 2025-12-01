with AUnit.Assertions; use AUnit.Assertions;
with Protocol;
with Ada.Streams;      use Ada.Streams;

package body Protocol_Tests is

   procedure Test_Encode_Basic (T : in out Test) is
      Input    : constant Stream_Element_Array := (1, 2, 3, 4);
      Expected : constant Stream_Element_Array := (5, 1, 2, 3, 4);
      Result   : constant Stream_Element_Array := Protocol.Encode (Input);
   begin
      Assert (Result = Expected, "Basic encoding failed");
   end Test_Encode_Basic;

   procedure Test_Encode_With_Zeros (T : in out Test) is
      Input    : constant Stream_Element_Array := (1, 0, 2, 0, 3);
      -- 1, 0 -> Code 2, Data 1
      -- 2, 0 -> Code 2, Data 2
      -- 3    -> Code 2, Data 3 (End)
      Expected : constant Stream_Element_Array := (2, 1, 2, 2, 2, 3);
      Result   : constant Stream_Element_Array := Protocol.Encode (Input);
   begin
      Assert (Result = Expected, "Encoding with zeros failed");
   end Test_Encode_With_Zeros;

   procedure Test_Decode_Basic (T : in out Test) is
      Input    : constant Stream_Element_Array := (5, 1, 2, 3, 4);
      Expected : constant Stream_Element_Array := (1, 2, 3, 4);
      Result   : constant Stream_Element_Array := Protocol.Decode (Input);
   begin
      Assert (Result = Expected, "Basic decoding failed");
   end Test_Decode_Basic;

   procedure Test_Decode_With_Zeros (T : in out Test) is
      Input    : constant Stream_Element_Array := (2, 1, 2, 2, 2, 3);
      Expected : constant Stream_Element_Array := (1, 0, 2, 0, 3);
      Result   : constant Stream_Element_Array := Protocol.Decode (Input);
   begin
      Assert (Result = Expected, "Decoding with zeros failed");
   end Test_Decode_With_Zeros;

   procedure Test_Round_Trip (T : in out Test) is
      Input : Stream_Element_Array (1 .. 256);
   begin
      for I in Input'Range loop
         Input (I) := Stream_Element (I mod 256);
      end loop;

      declare
         Encoded : constant Stream_Element_Array := Protocol.Encode (Input);
         Decoded : constant Stream_Element_Array := Protocol.Decode (Encoded);
      begin
         Assert (Decoded = Input, "Round trip failed");
      end;
   end Test_Round_Trip;

   procedure Test_Decode_Error_Zero_Byte (T : in out Test) is
      Input : constant Stream_Element_Array :=
        (1, 0, 2); -- 0 is invalid in COBS
   begin
      declare
         Result : constant Stream_Element_Array := Protocol.Decode (Input);
      begin
         Assert (False, "Should have raised Decode_Error");
      end;
   exception
      when Protocol.Decode_Error =>
         null; -- Expected
   end Test_Decode_Error_Zero_Byte;

   procedure Test_Decode_Error_Length (T : in out Test) is
      Input : constant Stream_Element_Array :=
        (5, 1, 2); -- Code 5 expects 4 bytes, but only 2 provided
   begin
      declare
         Result : constant Stream_Element_Array := Protocol.Decode (Input);
      begin
         Assert (False, "Should have raised Decode_Error");
      end;
   exception
      when Protocol.Decode_Error =>
         null; -- Expected
   end Test_Decode_Error_Length;

end Protocol_Tests;
