with Ada.Streams.Stream_IO;
with Interfaces;            use Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Bitstream_Parser is

   Format_Error : exception;

   type Header_Info is record
      Design_Name : Unbounded_String;
      Part_Name   : Unbounded_String;
      Date        : Unbounded_String;
      Time        : Unbounded_String;
      Data_Length : Unsigned_32; -- The size of the raw bitstream
      Data_Offset :
        Ada.Streams.Stream_IO.Count; -- File index where raw data begins
   end record;

   -- Parses the .bit file and returns the header info.
   -- Raises Format_Error if the format is invalid.
   function Parse_Header (Filename : String) return Header_Info;

end Bitstream_Parser;
