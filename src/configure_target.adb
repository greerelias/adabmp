with Ada.Streams;           use Ada.Streams;
with Bitstream_Parser;      use Bitstream_Parser;
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Protocol;
with Packet_Formatter;      use Packet_Formatter;
with Interfaces;            use Interfaces;
with Commands;
with Ada.Characters.Latin_1;
with Progress_Bar;
with Board_Info;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
use type Progress_Bar.Volatile_Integer;

package body Configure_Target is
   procedure Load_Bitstream
     (Port     : in out Serial_Interface.Serial_Port'Class;
      Path     : in String;
      Success  : in out Boolean;
      Verbose  : Boolean := False;
      SPI_JTAG : Boolean := False)
   is
      Bitstream          : File_Type;
      Bitstream_Header   : Header_Info;
      Data               : Stream_Element_Array (1 .. 512);
      Length             : Stream_Element_Offset;
      Bitstream_Size     : Stream_Element_Array (1 .. 4);
      Bitstream_Size_U32 : Unsigned_32
      with Address => Bitstream_Size'Address;
      Total              : aliased Progress_Bar.Volatile_Integer;
      Bytes_Sent         : aliased Progress_Bar.Volatile_Integer := 0;
      package TIO renames Ada.Text_IO;

      Bar_Task : Progress_Bar.Bar_Task;
      Info     : Board_Info.Board_Info_Record_Access;
   begin
      Board_Info.Get_Board_Info (Port, Info, Success);
      if not Success then
         return;
      end if;
      Success := False;
      Bitstream_Header := Bitstream_Parser.Parse_Header (Path);
      Bitstream_Size_U32 := Bitstream_Header.Data_Length;
      Total := Progress_Bar.Volatile_Integer (Bitstream_Size_U32);
      Open (Bitstream, In_File, Path);
      Set_Index (Bitstream, Bitstream_Header.Data_Offset);
      -- Data packet for configure target
      -- [SOF][Configure_Target][Bitstream Size]
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
      if Verbose then
         if SPI_JTAG then
            Bar_Task.Start
              ("Configuring Target for SPI over JTAG...",
               "Configuration Complete.",
               Total'Unchecked_Access,
               Bytes_Sent'Unchecked_Access,
               False);
         else
            Bar_Task.Start
              ("Starting bitstream transfer...",
               "Transfer complete",
               Total'Unchecked_Access,
               Bytes_Sent'Unchecked_Access,
               True);
         end if;
      end if;
      -- Write first 1KB
      Read (Bitstream, Data, Length);
      Port.Write (Data (1 .. Length));
      Bytes_Sent := Progress_Bar.Volatile_Integer (Length);
      Read (Bitstream, Data, Length);
      Port.Write (Data (1 .. Length));
      Bytes_Sent := Bytes_Sent + Progress_Bar.Volatile_Integer (Length);

      while not End_Of_File (Bitstream) loop
         -- Wait for response from programmer to send more
         if not Protocol.Receive_Ready_Packet (Port) then
            -- Fail on no response
            Bar_Task.Stop (False);
            TIO.Put_Line
              (Ada.Characters.Latin_1.LF
               & "Failure: No response from programmer during write.");
            Close (Bitstream);
            return;
         end if;
         -- Write 512B
         Read (Bitstream, Data, Length);
         Port.Write (Data (1 .. Length));
         Bytes_Sent := Bytes_Sent + Progress_Bar.Volatile_Integer (Length);
      end loop;
      Close (Bitstream);
      Protocol.Receive_Packet (Port, Data, Length);
      declare
         Packet : constant Stream_Element_Array := Data (1 .. Length);
      begin
         if not Is_Valid (Packet)
           and then Get_Command (Packet) /= Commands.Configure_Target_Complete
         then
            TIO.Put_Line
              ("Failure: Did not receive configure complete from programmer");
            return;
         end if;
      end;
      Success := True;
      Bar_Task.Stop;

   exception
      when ADA.IO_EXCEPTIONS.NAME_ERROR =>
         TIO.Put_Line ("Failure: File " & Path & " not found.");
      when E : Format_Error =>
         TIO.Put_Line (Ada.Exceptions.Exception_Message (E));
      when E : others =>
         pragma Unreferenced (E);
         if Is_Open (Bitstream) then
            Close (Bitstream);
         end if;
         Bar_Task.Stop (False);
         raise;
   end Load_Bitstream;
end Configure_Target;
