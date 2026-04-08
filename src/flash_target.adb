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
with Progress_Bar;
use type Progress_Bar.Volatile_Integer;


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
      Total              : aliased Progress_Bar.Volatile_Integer;
      Bytes_Sent         : aliased Progress_Bar.Volatile_Integer := 0;
      Rx_Buffer          : Stream_Element_Array (1 .. 64);
      Rx_Last            : Stream_Element_Offset;


      Bar_Task : Progress_Bar.Bar_Task;
   begin

      Success := False;
      Bitstream_Header := Bitstream_Parser.Parse_Header (Path);
      -- TODO add checks for fpga part #
      Bitstream_Size_U32 := Bitstream_Header.Data_Length;
      Total := Progress_Bar.Volatile_Integer (Bitstream_Size_U32);
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
         Bar_Task.Start
           ("Configuring Target for SPI over JTAG...",
            "Configuration Complete.",
            Total'Unchecked_Access,
            Bytes_Sent'Unchecked_Access,
            False);
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
      --  TIO.Put_Line ("Bytes sent" & Bytes_Sent'Image);
      Close (Bitstream);
      Protocol.Receive_Packet (Port, Data, Length);
      declare
         Packet : constant Stream_Element_Array := Data (1 .. Length);
      begin
         if Is_Valid (Packet)
           and then Get_Command (Packet) /= Commands.Configure_Target_Complete
         then
            Bar_Task.Stop (False);
            TIO.Put_Line
              (Ada.Characters.Latin_1.LF
               & "Failure: Did not receive configure complete from programmer");
            return;
         end if;
      end;
      Success := True;
      Bar_Task.Stop;

   exception
      when ADA.IO_EXCEPTIONS.NAME_ERROR =>
         TIO.Put_Line ("Failure: File " & Path & " not found.");
      when Fmt_Err : Format_Error =>
         TIO.Put_Line (Ada.Exceptions.Exception_Message (Fmt_Err));
      when Other_Err : others =>
         if Is_Open (Bitstream) then
            Close (Bitstream);
         end if;
         Bar_Task.Stop (False);
         raise;
   end Load_SPI_Over_Jtag;

   procedure Erase_Flash
     (Port         : in out Serial_Interface.Serial_Port'Class;
      Data_Size    : in Unsigned_32;
      Success      : in out Boolean;
      Base_Address : Unsigned_32 := 0;
      Verbose      : Boolean := False)
   is
      use Progress_Bar;
      Data             : Stream_Element_Array (1 .. 2);
      Length           : Stream_Element_Offset;
      Base_Address_Arr : Stream_Element_Array (1 .. 4)
      with Address => Base_Address'Address;
      Rx_Buffer        : Stream_Element_Array (1 .. 64);
      Rx_Last          : Stream_Element_Offset;
      Blocks_64        : Unsigned_32 := (Data_Size / Block_Size_64);
      Blocks_32        : Unsigned_32 :=
        (Data_Size mod Block_Size_64) / Block_Size_32;
      Sectors          : Unsigned_32 :=
        (((Data_Size mod Block_Size_64) mod Block_Size_32) + 4095)
        / Sector_Size;
      Sector_Arr       : Stream_Element_Array (1 .. 4)
      with Address => Sectors'Address;
      Bar_Task         : Progress_Bar.Bar_Task;
      Payload          : Stream_Element_Array (1 .. 10);
      Total            : aliased Volatile_Integer :=
        Volatile_Integer (Data_Size);
      Total_Erased     : aliased Volatile_Integer := 0;
   begin
      Success := False;
      -- Data packet needed for erase
      -- Base Address | Sectors | 32 Blocks | 64 Blocks
      Payload (1 .. 8) := Base_Address_Arr & Sector_Arr;
      Payload (9) := Stream_Element (Blocks_32);
      Payload (10) := Stream_Element (Blocks_64);
      declare
         Packet : constant Stream_Element_Array :=
           Make_Packet (Commands.Flash_Erase, Payload);
      begin
         Protocol.Send_Packet (Port, Packet);
         --  delay (0.010);
         if not Protocol.Receive_Ready_Packet (Port) then
            TIO.Put_Line ("Failure: Programmer not ready.");
            return;
         end if;
      end;
      if Verbose then
         Bar_Task.Start
           ("Starting Erase...",
            "Done.",
            Total'Unchecked_Access,
            Total_Erased'Unchecked_Access);
      end if;
      loop
         Protocol.Receive_Packet (Port, Data, Length);
         if Is_Valid (Data (1 .. Length)) then
            declare
               Cmd : Command_Id := Get_Command (Data (1 .. Length));
            begin
               case Cmd is
                  when Commands.Block64_Erase_Done   =>
                     Total_Erased :=
                       Total_Erased + Volatile_Integer (Block_Size_64);

                  when Commands.Block32_Erase_Done   =>
                     Total_Erased :=
                       Total_Erased + Volatile_Integer (Block_Size_32);

                  when Commands.Sector_Erase_Done    =>
                     Total_Erased :=
                       Total_Erased + Volatile_Integer (Sector_Size);

                  when Commands.Flash_Erase_Complete =>
                     Total_Erased := Total;
                     Bar_Task.Stop;
                     exit;

                  when others                        =>
                     TIO.Put_Line ("Failure: Error During Erase.");
                     Bar_Task.Stop (False);
                     return;
               end case;
            end;
         else
            TIO.Put_Line ("Failure: Error During Erase.");
            Bar_Task.Stop (False);
            return;
         end if;
      end loop;
      Success := True;

   exception
      when Other_Err : others =>
         Bar_Task.Stop (False);
         raise;
   end Erase_Flash;

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
      Total            : aliased Progress_Bar.Volatile_Integer;
      Bytes_Sent       : aliased Progress_Bar.Volatile_Integer := 0;
      Rx_Buffer        : Stream_Element_Array (1 .. 64);
      Rx_Last          : Stream_Element_Offset;

      Bar_Task : Progress_Bar.Bar_Task;
   begin

      Success := False;

      Total := Progress_Bar.Volatile_Integer (Data_Size);
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
      if Verbose then
         Bar_Task.Start
           ("Flashing Target...",
            "Flash Complete.",
            Total'Unchecked_Access,
            Bytes_Sent'Unchecked_Access,
            True);
      end if;
      Read (Input_File, Data, Length);
      Port.Write (Data (1 .. Length));
      Bytes_Sent := Bytes_Sent + Progress_Bar.Volatile_Integer (Length);
      Read (Input_File, Data, Length);
      Port.Write (Data (1 .. Length));
      Bytes_Sent := Bytes_Sent + Progress_Bar.Volatile_Integer (Length);
      while not End_Of_File (Input_File) loop
         -- Wait for response from programmer to send more
         if not Protocol.Receive_Ready_Packet (Port) then
            -- Fail on no response
            Bar_Task.Stop (False);
            TIO.Put_Line
              (Ada.Characters.Latin_1.LF
               & "Failure: No response from programmer during write.");
            Close (Input_File);
            return;
         end if;
         -- Write 512B
         Read (Input_File, Data, Length);
         Port.Write (Data (1 .. Length));
         Bytes_Sent := Bytes_Sent + Progress_Bar.Volatile_Integer (Length);
      end loop;
      --  TIO.Put_Line ("Bytes sent" & Bytes_Sent'Image);
      Close (Input_File);
      Protocol.Receive_Packet (Port, Data, Length);
      if Length > 0 then
         declare
            Packet : constant Stream_Element_Array := Data (1 .. Length);
         begin
            if Is_Valid (Packet)
              and then Get_Command (Packet) /= Commands.Flash_Target_Complete
            then
               Bar_Task.Stop (False);
               TIO.Put_Line
                 (Ada.Characters.Latin_1.LF
                  & "Failure: Did not receive flash target complete from programmer");
               return;
            end if;
         end;
      else
         Bar_Task.Stop (False);
         TIO.Put_Line
           (Ada.Characters.Latin_1.LF
            & "Failure: Did not receive flash target complete from programmer");
         return;
      end if;
      Success := True;
      Bar_Task.Stop;

   exception
      when ADA.IO_EXCEPTIONS.NAME_ERROR =>
         TIO.Put_Line ("Failure: File " & Path & " not found.");
      when Fmt_Err : Format_Error =>
         TIO.Put_Line (Ada.Exceptions.Exception_Message (Fmt_Err));
      when Other_Err : others =>
         if Is_Open (Input_File) then
            Close (Input_File);
         end if;
         Bar_Task.Stop (False);
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
         Erase_Flash (Port, Bitstream_Size_U32, Success, 0, True);
      end if;
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
      File_Size : Unsigned_32;
      package DIR renames Ada.Directories;
   begin
      Load_SPI_Over_Jtag
        (Port => Port, Success => Success, Verbose => Verbose);
      if Success and then DIR.Exists (Path) then
         File_Size := Unsigned_32 (DIR.Size (Path));
         Erase_Flash (Port, File_Size, Success, Base_Address, True);
         if Success then
            Flash
              (Port         => Port,
               Path         => Path,
               Data_Size    => File_Size,
               Success      => Success,
               Base_Address => Base_Address,
               Verbose      => Verbose);
         end if;
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
