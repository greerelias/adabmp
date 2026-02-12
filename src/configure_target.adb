with Ada.Streams;           use Ada.Streams;
with Serial_Interface;
with Bitstream_Parser;      use Bitstream_Parser;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Exceptions;
with Protocol;              use Protocol;
with Packet_Formatter;      use Packet_Formatter;
with Interfaces;            use Interfaces;
with Commands;

package body Configure_Target is
   procedure Load_Bitstream
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Path    : in String;
      Success : in out Boolean)
   is
      Bitstream          : File_Type;
      Bitstream_Header   : Header_Info;
      Data               : Stream_Element_Array (1 .. 512);
      Length             : Stream_Element_Offset;
      Bitstream_Size     : Stream_Element_Array (1 .. 4);
      Bitstream_Size_U32 : Unsigned_32
      with Address => Bitstream_Size'Address;

      Rx_Buffer : Stream_Element_Array (1 .. 64);
      Rx_Last   : Stream_Element_Offset;
      package TIO renames Ada.Text_IO;

   begin
      Success := False;
      Bitstream_Header := Bitstream_Parser.Parse_Header (Path);
      -- TODO add checks for fpga part #
      Bitstream_Size_U32 := Bitstream_Header.Data_Length;

      Open (Bitstream, In_File, Path);
      Set_Index (Bitstream, Bitstream_Header.Data_Offset);

      -- Send configure target command and bitstream size
      declare
         Packet : constant Stream_Element_Array :=
           Make_Packet (Commands.Configure_Target, Bitstream_Size);
      begin
         Protocol.Send_Packet (Port, Packet);
      end;
      -- Wait for response from progammer
      if not Protocol.Receive_Ready_Packet (Port) then
         TIO.Put_Line ("Failure: Programmer not ready.");
         Close (Bitstream);
         return;
      end if;
      -- Write first 1KB
      Read (Bitstream, Data, Length);
      Port.Write (Data);
      Read (Bitstream, Data, Length);
      Port.Write (Data);

      while not End_Of_File (Bitstream) loop
         -- Wait for response from programmer to send more
         if not Protocol.Receive_Ready_Packet (Port) then
            -- Fail on no response
            TIO.Put_Line
              ("Failure: No response from programmer during write.");
            Close (Bitstream);
            return;
         end if;
         -- Write 512B
         Read (Bitstream, Data, Length);
         Port.Write (Data (1 .. Length));
      end loop;

      Close (Bitstream);
      Success := True;

   exception
      when ADA.IO_EXCEPTIONS.NAME_ERROR =>
         TIO.Put_Line ("Failure: File " & Path & " not found.");
      when E : Format_Error =>
         TIO.Put_Line (Ada.Exceptions.Exception_Message (E));
      when E : others =>
         if Is_Open (Bitstream) then
            Close (Bitstream);
         end if;
         raise;
   end Load_Bitstream;
end Configure_Target;
