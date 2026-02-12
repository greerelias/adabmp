with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;

package body Bitstream_Parser is

   -- Helper: Read Big-Endian 16-bit integer
   function Read_BE_16 (File : File_Type) return Unsigned_16 is
      B1, B2 : Unsigned_8;
   begin
      Unsigned_8'Read (Stream (File), B1);
      Unsigned_8'Read (Stream (File), B2);
      return Shift_Left (Unsigned_16 (B1), 8) or Unsigned_16 (B2);
   end Read_BE_16;

   -- Helper: Read Big-Endian 32-bit integer
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
      File        : File_Type;
      Info        : Header_Info;
      Key         : Character;
      Len         : Unsigned_16;
      Buffer      : String (1 .. 256);
      Found_Start : Boolean := False;
   begin
      Open (File, In_File, Filename);

      -- 1. Search for start key 'a' safely
      -- We scan byte-by-byte until we find 'a' or hit EOF.
      while not End_Of_File (File) loop
         Character'Read (Stream (File), Key);
         if Key = 'a' then
            Found_Start := True;
            exit;
         end if;
      end loop;

      if not Found_Start then
         Close (File);
         raise Format_Error with "Invalid Bitstream: Start key 'a' not found.";
      end if;

      -- 2. Parse Keys (b, c, d, ...) until we find 'e'
      loop
         -- Safety check: If file ends while expecting a key/value, it's invalid
         if End_Of_File (File) then
            Close (File);
            raise Format_Error
              with "Invalid Bitstream: File ended before payload key 'e'.";
         end if;

         -- 'a' is already consumed or we just finished the previous key.
         -- Read the Length (2 bytes)
         Len := Read_BE_16 (File);

         -- Read the Value string
         -- Note: We check if the file has enough bytes left (basic safety)
         if Index (File) + Ada.Streams.Stream_IO.Count (Len) > Size (File) + 1
         then
            Close (File);
            raise Format_Error
              with "Invalid Bitstream: String length exceeds file size.";
         end if;

         String'Read (Stream (File), Buffer (1 .. Integer (Len)));

         -- Store value based on the LAST key we read
         case Key is
            when 'a'    =>
               Info.Design_Name :=
                 To_Unbounded_String (Buffer (1 .. Integer (Len) - 1));

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
               null; -- Ignore unknown keys
         end case;

         -- Read the NEXT key
         if End_Of_File (File) then
            Close (File);
            raise Format_Error
              with "Invalid Bitstream: Unexpected EOF looking for next key.";
         end if;

         Character'Read (Stream (File), Key);

         -- Exit condition: We found the payload separator
         exit when Key = 'e';
      end loop;

      -- 3. Read Payload Length (4 bytes)
      -- Key 'e' was found, next 4 bytes are length
      Info.Data_Length := Read_BE_32 (File);

      -- 4. Mark the offset where raw data begins
      Info.Data_Offset := Index (File);

      Close (File);
      return Info;

   exception
      -- Catch standard IO errors (like reading past EOF in Read_BE_16)
      when Ada.IO_Exceptions.End_Error =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise Format_Error
           with "Invalid Bitstream: File truncated unexpectedly.";

      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Parse_Header;

end Bitstream_Parser;
