with Interfaces;            use Interfaces;
with Ada.Streams;           use Ada.Streams;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Manufacturer_Codes;    use Manufacturer_Codes;

package body Board_Info_Printer is

   procedure Print_Board_Info (Info : in Board_Info_Record_Access) is
      Raw_ID            : Interfaces.Unsigned_32;
      High, Low         : Character;
      Hex_Chars         : constant String := "0123456789ABCDEF";
      Byte              : Stream_Element;
      Manufacturer_Code : Interfaces.Unsigned_32;
      Part_Number       : Interfaces.Unsigned_32;
   begin

      Raw_ID :=
        Interfaces.Shift_Left (Interfaces.Unsigned_32 (Info.Bytes (1)), 24)
        or Interfaces.Shift_Left (Interfaces.Unsigned_32 (Info.Bytes (2)), 16)
        or Interfaces.Shift_Left (Interfaces.Unsigned_32 (Info.Bytes (3)), 8)
        or Interfaces.Unsigned_32 (Info.Bytes (4));

      Put ("IDCODE:        0x");
      for I in Info.Bytes'Range loop
         Byte := Info.Bytes (I);
         High := Hex_Chars (Integer (Byte) / 16 + 1);
         Low := Hex_Chars (Integer (Byte) mod 16 + 1);
         Put (High & Low);
      end loop;
      Ada.Text_IO.New_Line;

      Manufacturer_Code := Interfaces.Shift_Right (Raw_ID, 1) and 16#7FF#;
      Put_Line
        ("Manufacturer:  "
         & To_String (Manufacturer_Lookup (Hex_Key (Manufacturer_Code))));

      Part_Number := Shift_Right (Raw_ID, 12) and 16#FFFF#;
      Put ("Part Number:   0x");
      for Shift in reverse 0 .. 3 loop
         declare
            N : constant Interfaces.Unsigned_32 :=
              Shift_Right (Part_Number, Shift * 4) and 16#F#;
         begin
            Put (Hex_Chars (Integer (N) + 1));
         end;
      end loop;
      Ada.Text_IO.New_Line;

   end Print_Board_Info;

end Board_Info_Printer;
