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
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
with System.SPARK.Cut_Operations;

package body Configure_Target is
   procedure Load_Bitstream
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Path    : in String;
      Success : in out Boolean;
      Verbose : Boolean := False)
   is
      Bitstream          : File_Type;
      Bitstream_Header   : Header_Info;
      Data               : Stream_Element_Array (1 .. 512);
      Length             : Stream_Element_Offset;
      Bitstream_Size     : Stream_Element_Array (1 .. 4);
      Bitstream_Size_U32 : Unsigned_32
      with Address => Bitstream_Size'Address;
      Total              : Integer;
      Bytes_Sent         : Integer := 0
      with Volatile;
      Bar_Width          : constant Integer := 40;
      Rx_Buffer          : Stream_Element_Array (1 .. 64);
      Rx_Last            : Stream_Element_Offset;
      package TIO renames Ada.Text_IO;

      task Progress_Bar is
         entry Start;
         entry Stop (Success : Boolean := True);
      end Progress_Bar;

      task body Progress_Bar is
         Running : Boolean := True;
         procedure Update is
            Percent      : Float;
            Filled_Count : Integer;
            Empty_Count  : Integer;
            Bar          : String (1 .. Bar_Width);
         begin
            if Total = 0 then
               Percent := 100.0;
            else
               Percent := (Float (Bytes_Sent) / Float (Total)) * 100.0;
            end if;

            Filled_Count := Integer (Float (Bar_Width) * (Percent / 100.0));
            if Filled_Count > Bar_Width then
               Filled_Count := Bar_Width;
            end if;
            Empty_Count := Bar_Width - Filled_Count;

            if Filled_Count > 0 then
               Move
                 (Source => (1 .. Filled_Count => '='),
                  Target => Bar (1 .. Filled_Count));
               if Filled_Count < Bar_Width then
                  Bar (Filled_Count) := '>';
               end if;
            end if;

            if Empty_Count > 0 then
               Bar (Filled_Count + 1 .. Bar_Width) := (others => ' ');
            end if;

            TIO.Put
              (Ada.Characters.Latin_1.CR
               & "["
               & Bar
               & "] "
               & Integer (Percent)'Image
               & "% "
               & "("
               & Bytes_Sent'Image
               & " /"
               & Total'Image
               & " bytes)");
            TIO.Flush;
         end Update;
      begin
         select
            accept Start do
               TIO.Put_Line ("Starting bitstream transfer...");
            end Start;
         or
            accept Stop (Success : Boolean := True);
            Running := False;
         or
            terminate;
         end select;

         while Bytes_Sent <= Total and Running loop
            select
               accept Stop (Success : Boolean := True) do
                  Update; -- Ensure final status is printed
                  Running := False;
                  if Success then
                     TIO.Put_Line
                       (Ada.Characters.Latin_1.LF & "Transfer complete");
                  end if;
               end Stop;
            or
               delay 0.050;
               Update;
            end select;
         end loop;

         -- Consume any final Stop signal if we exited due to completion
         if Running then
            select
               accept Stop (Success : Boolean := True) do
                  Update; -- Ensure final status is printed
                  if Success then
                     TIO.Put_Line
                       (Ada.Characters.Latin_1.LF & "Transfer complete");
                  end if;
               end Stop;
            or
               terminate;
            end select;
         end if;
      end Progress_Bar;
   begin

      Success := False;
      Bitstream_Header := Bitstream_Parser.Parse_Header (Path);
      -- TODO add checks for fpga part #
      Bitstream_Size_U32 := Bitstream_Header.Data_Length;
      Total := Integer (Bitstream_Size_U32);
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
      -- TODO: make status message optional
      if Verbose then
         Progress_Bar.Start;
      end if;
      -- Write first 1KB
      Read (Bitstream, Data, Length);
      Port.Write (Data (1 .. Length));
      Bytes_Sent := Integer (Length);
      Read (Bitstream, Data, Length);
      Port.Write (Data (1 .. Length));
      Bytes_Sent := Bytes_Sent + Integer (Length);

      while not End_Of_File (Bitstream) loop
         -- Wait for response from programmer to send more
         if not Protocol.Receive_Ready_Packet (Port) then
            -- Fail on no response
            Progress_Bar.Stop (False);
            TIO.Put_Line
              (Ada.Characters.Latin_1.LF
               & "Failure: No response from programmer during write.");
            Close (Bitstream);
            return;
         end if;
         -- Write 512B
         Read (Bitstream, Data, Length);
         Port.Write (Data (1 .. Length));
         Bytes_Sent := Bytes_Sent + Integer (Length);
      end loop;
      --  TIO.Put_Line ("Bytes sent" & Bytes_Sent'Image);
      Close (Bitstream);
      Success := True;
      Progress_Bar.Stop;

   exception
      when ADA.IO_EXCEPTIONS.NAME_ERROR =>
         TIO.Put_Line ("Failure: File " & Path & " not found.");
      when E : Format_Error =>
         TIO.Put_Line (Ada.Exceptions.Exception_Message (E));
      when E : others =>
         if Is_Open (Bitstream) then
            Close (Bitstream);
         end if;
         Progress_Bar.Stop (False);
         raise;
   end Load_Bitstream;
end Configure_Target;
