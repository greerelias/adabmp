with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Bitstream_Parser is

   -- Helper to read a Big-Endian 16-bit integer (for string lengths)
   function Read_BE_16 (File : File_Type) return Unsigned_16 is
      B1, B2 : Unsigned_8;
   begin
      Unsigned_8'Read (Stream (File), B1);
      Unsigned_8'Read (Stream (File), B2);
      return Shift_Left (Unsigned_16 (B1), 8) or Unsigned_16 (B2);
   end Read_BE_16;

   -- Helper to read a Big-Endian 32-bit integer (for the bistream length)
   function Read_BE_32 (File : File_Type) return Unsigned_32 is
      B1, B2, B3, B4 : Unsigned_8;
   begin
      Unsigned_8'Read (Stream (File), B1);
      Unsigned_8'Read (Stream (File), B2);
      Unsigned_8'Read (Stream (File), B3);
      Unsigned_8'Read (Stream (File), B4);
      return
        Shift_Left (Unsigned_32 (B1), 24)
        or Shift_Left (Unsigned_32 (B2), 16)
        or Shift_Left (Unsigned_32 (B3), 8)
        or Unsigned_32 (B4);
   end Read_BE_32;

   function Parse_Header (Filename : String) return Header_Info is
      File   : File_Type;
      Info   : Header_Info;
      Key    : Character;
      Len    : Unsigned_16;
      Buffer : String (1 .. 256);
   begin
      Open (File, In_File, Filename);

      -- We scan until we find the first valid key 'a' (0x61).
      loop
         Character'Read (Stream (File), Key);
         exit when Key = 'a';
      end loop;

      -- We found 'a', so we are now inside the structured header.
      -- Loop through keys 'a', 'b', 'c', 'd'.
      loop
         -- 1. Read the length of the value (2 bytes, Big Endian)
         Len := Read_BE_16 (File);

         -- 2. Read the string value
         String'Read (Stream (File), Buffer (1 .. Integer (Len)));

         -- 3. Store based on the current Key
         case Key is
            when 'a'    =>
               Info.Design_Name :=
                 To_Unbounded_String
                   (Buffer (1 .. Integer (Len) - 1)); -- Strip null

            when 'b'    =>
               Info.Part_Name :=
                 To_Unbounded_String (Buffer (1 .. Integer (Len) - 1));

            when 'c'    =>
               Info.Date :=
                 To_Unbounded_String (Buffer (1 .. Integer (Len) - 1));

            when 'd'    =>
               Info.Time :=
                 To_Unbounded_String (Buffer (1 .. Integer (Len) - 1));

            when others =>
               null;
         end case;

         -- 4. Read the NEXT key
         Character'Read (Stream (File), Key);

         -- 5. Exit when we hit the 'e' key (Start of Bitstream Length)
         exit when Key = 'e';
      end loop;

      -- CRITICAL: Key 'e' is followed by a 4-byte length, not 2-byte.
      Info.Data_Length := Read_BE_32 (File);

      -- The current file position is now the start of the raw bitstream.
      Info.Data_Offset := Index (File);

      Close (File);
      return Info;

   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Parse_Header;

end Bitstream_Parser;
