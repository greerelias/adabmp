with Ada.Streams;           use Ada.Streams;
with Interfaces.Fortran;
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
with Ada.Directories;

package body Flash_Target is
   package TIO renames Ada.Text_IO;
   procedure Load_SPI_Over_Jtag
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Success : in out Boolean;
      Path    : in String := SPI_JTAG_BS_Path;
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
               & "% ");
            TIO.Flush;
         end Update;
      begin
         select
            accept Start do
               TIO.Put_Line ("Configuring Target for SPI over JTAG...");
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
                       (Ada.Characters.Latin_1.LF & "Configuration Complete.");
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
                       (Ada.Characters.Latin_1.LF & "Configuration Complete.");
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
      Protocol.Receive_Packet (Port, Data, Length);
      declare
         Packet : constant Stream_Element_Array := Data (1 .. Length);
      begin
         if Is_Valid (Packet)
           and then Get_Command (Packet) /= Commands.Configure_Target_Complete
         then
            Progress_Bar.Stop (False);
            TIO.Put_Line
              (Ada.Characters.Latin_1.LF
               & "Failure: Did not receive configure complete from programmer");
            return;
         end if;
      end;
      Success := True;
      Progress_Bar.Stop;

   exception
      when ADA.IO_EXCEPTIONS.NAME_ERROR =>
         TIO.Put_Line ("Failure: File " & Path & " not found.");
      when Fmt_Err : Format_Error =>
         TIO.Put_Line (Ada.Exceptions.Exception_Message (Fmt_Err));
      when Other_Err : others =>
         if Is_Open (Bitstream) then
            Close (Bitstream);
         end if;
         Progress_Bar.Stop (False);
         raise;
   end Load_SPI_Over_Jtag;

   procedure Flash
     (Port         : in out Serial_Interface.Serial_Port'Class;
      Path         : in String;
      Data_Size    : in Unsigned_32;
      Success      : in out Boolean;
      Data_Offset  : in Ada.Streams.Stream_IO.Count := 1;
      Base_Address : Unsigned_32 := 0;
      Verbose      : Boolean := False)
   is
      Input_File       : File_Type;
      Data             : Stream_Element_Array (1 .. 512);
      Length           : Stream_Element_Offset;
      Data_Size_Arr    : Stream_Element_Array (1 .. 4)
      with Address => Data_Size'Address;
      Base_Address_Arr : Stream_Element_Array (1 .. 4)
      with Address => Base_Address'Address;
      Total            : Integer;
      Bytes_Sent       : Integer := 0
      with Volatile;
      Bar_Width        : constant Integer := 40;
      Rx_Buffer        : Stream_Element_Array (1 .. 64);
      Rx_Last          : Stream_Element_Offset;

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
               TIO.Put_Line ("Flashing Target...");
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
                       (Ada.Characters.Latin_1.LF & "Flash Complete.");
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
                       (Ada.Characters.Latin_1.LF & "Flash Complete.");
                  end if;
               end Stop;
            or
               terminate;
            end select;
         end if;
      end Progress_Bar;
   begin

      Success := False;

      Total := Integer (Data_Size);
      Open (Input_File, In_File, Path);
      Set_Index (Input_File, Data_Offset);
      declare
         Payload : constant Stream_Element_Array :=
           Data_Size_Arr & Base_Address_Arr;
         Packet  : constant Stream_Element_Array :=
           Make_Packet (Commands.Flash_Target, Payload);
      begin
         Protocol.Send_Packet (Port, Packet);
         delay (0.010);
         if not Protocol.Receive_Ready_Packet (Port) then
            TIO.Put_Line ("Failure: Programmer not ready.");
            return;
         end if;
      end;
      -- TODO: make status message optional
      -- Write first 1KB
      Read (Input_File, Data, Length);
      Port.Write (Data (1 .. Length));
      Bytes_Sent := Bytes_Sent + Integer (Length);
      Read (Input_File, Data, Length);
      Port.Write (Data (1 .. Length));
      Bytes_Sent := Bytes_Sent + Integer (Length);

      TIO.Put_Line ("Starting erase...");
      delay (Get_Erase_Delay (Data_Size));
      TIO.Put_Line ("Done.");

      if not Protocol.Receive_Ready_Packet (Port) then
         TIO.Put_Line ("Failure: Error during flash erase.");
         return;
      end if;

      if Verbose then
         Progress_Bar.Start;
      end if;
      while not End_Of_File (Input_File) loop
         -- Wait for response from programmer to send more
         if not Protocol.Receive_Ready_Packet (Port) then
            -- Fail on no response
            Progress_Bar.Stop (False);
            TIO.Put_Line
              (Ada.Characters.Latin_1.LF
               & "Failure: No response from programmer during write.");
            Close (Input_File);
            return;
         end if;
         -- Write 512B
         Read (Input_File, Data, Length);
         Port.Write (Data (1 .. Length));
         Bytes_Sent := Bytes_Sent + Integer (Length);
      end loop;
      --  TIO.Put_Line ("Bytes sent" & Bytes_Sent'Image);
      Close (Input_File);
      Protocol.Receive_Packet (Port, Data, Length);
      declare
         Packet : constant Stream_Element_Array := Data (1 .. Length);
      begin
         if not Is_Valid (Packet)
           and then Get_Command (Packet) /= Commands.Flash_Target_Complete
         then
            TIO.Put_Line
              ("Failure: Did not receive flash target complete from programmer");
            return;
         end if;
      end;
      Success := True;
      Progress_Bar.Stop;

   exception
      when ADA.IO_EXCEPTIONS.NAME_ERROR =>
         TIO.Put_Line ("Failure: File " & Path & " not found.");
      when Fmt_Err : Format_Error =>
         TIO.Put_Line (Ada.Exceptions.Exception_Message (Fmt_Err));
      when Other_Err : others =>
         if Is_Open (Input_File) then
            Close (Input_File);
         end if;
         Progress_Bar.Stop (False);
         raise;
   end Flash;

   procedure Flash_Bitstream
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Path    : in String;
      Success : in out Boolean;
      Verbose : Boolean := False)
   is
      Bitstream          : File_Type;
      Bitstream_Header   : Header_Info;
      Bitstream_Size     : Stream_Element_Array (1 .. 4);
      Bitstream_Size_U32 : Unsigned_32
      with Address => Bitstream_Size'Address;
   begin
      Success := True;
      Bitstream_Header := Bitstream_Parser.Parse_Header (Path);
      Bitstream_Size_U32 := Bitstream_Header.Data_Length;
      Load_SPI_Over_Jtag
        (Port => Port, Success => Success, Verbose => Verbose);
      delay (0.010);
      if Success then
         Flash
           (Port,
            Path,
            Bitstream_Size_U32,
            Success,
            Bitstream_Header.Data_Offset,
            0,
            Verbose);
      end if;
   exception
      when ADA.IO_EXCEPTIONS.NAME_ERROR =>
         TIO.Put_Line ("Failure: File " & Path & " not found.");
      when Fmt_Err : Format_Error =>
         TIO.Put_Line (Ada.Exceptions.Exception_Message (Fmt_Err));
      when Other_Err : others =>
         if Is_Open (Bitstream) then
            Close (Bitstream);
         end if;
         raise;
   end Flash_Bitstream;

   procedure Flash_Firmware
     (Port         : in out Serial_Interface.Serial_Port'Class;
      Path         : in String;
      Success      : in out Boolean;
      Base_Address : Unsigned_32 := 0;
      Verbose      : Boolean := False)
   is
      package DIR renames Ada.Directories;
   begin
      Load_SPI_Over_Jtag
        (Port => Port, Success => Success, Verbose => Verbose);
      if Success and then DIR.Exists (Path) then
         Flash
           (Port         => Port,
            Path         => Path,
            Data_Size    => Unsigned_32 (DIR.Size (Path)),
            Success      => Success,
            Base_Address => Base_Address,
            Verbose      => Verbose);
      else
         TIO.Put_Line ("Failure: File " & Path & " not found.");
         Success := False;
      end if;
   exception
      when ADA.IO_EXCEPTIONS.NAME_ERROR =>
         TIO.Put_Line ("Failure: Invalid file name");
         Success := False;
   end Flash_Firmware;

   function Get_Erase_Delay (Size : Unsigned_32) return Duration is
      Data_Size    : Integer := Integer (Size);
      Block_Size   : constant Integer := 16#1_0000#;
      Sector_Size  : constant Integer := 16#1000#;
      -- Wait in ms times are for approximate for MX25L3233F flash chip
      -- may be different for others
      Block_Wait   : constant Integer := 325;
      Sector_Wait  : constant Integer := 28;
      Block_Delay  : constant Integer := (Data_Size / Block_Size) * Block_Wait;
      Sector_Delay : constant Float :=
        Float'Ceiling
          ((Float ((Data_Size mod Block_Size)) / Float (Sector_Size)))
        * Float (Sector_Wait);
   begin
      return Duration ((Float (Block_Delay) + Sector_Delay) * 0.001);
   end Get_Erase_Delay;
end Flash_Target;
