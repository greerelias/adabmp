with Commands; use Commands;
with HAL.Block_Drivers;
with RP.Device;
with RP.Clock;
with RP.Uart;  use RP.Uart;
with Pico;


with HAL;                 use HAL;
with RP.GPIO;
with RP.GPIO.Interrupts;
with RP.Timer;            use RP.Timer;
with Protocol;            use Protocol;
with JTAG;
with USB_Int;             use USB_Int;
with Atomic.Unsigned_32;
with Byte_Counter;        use Byte_Counter;
with Interfaces;          use Interfaces;
with System.Machine_Code; use System.Machine_Code;

package body AdaBMP_FW is
   -- For testing w/ external button
   Last_Button_Press : Time := 0;
   Debounce_Time     : constant Time := 500_000; --500ms
   Testing           : Boolean := False;

   Device_Info_Pkt : aliased constant String :=
     Character'Val (170) & Character'Val (2) & "AdaBMP v0.1.0";
   Device_Info     : constant UInt8_Array (1 .. Device_Info_Pkt'Length)
   with Import, Convention => Ada, Address => Device_Info_Pkt'Address;

   function Reverse_Bytes (Input : UInt32) return UInt32 is
      Result : UInt32;
   begin
      -- %0 is Result, %1 is Input
      -- "=r" means write-only register, "r" means general register
      Asm
        ("rev %0, %1",
         Outputs  => UInt32'Asm_Output ("=r", Result),
         Inputs   => UInt32'Asm_Input ("r", Input),
         Volatile => False);
      return Result;
   end Reverse_Bytes;

   -- Only used for testing with external button
   procedure GPIO_Isr_Handler
     (Pin : RP.GPIO.GPIO_Pin; Trigger : RP.GPIO.Interrupt_Triggers)
   is
      pragma Unreferenced (Pin);
      pragma Unreferenced (Trigger);
      Test_Word : UInt32 := 16#20#;

      RDSR : constant UInt32 := 16#0500_0000#; -- Read Status Register
      -- Check WIP bit of status register (bit 0)
      function Wait_Write_In_Progress return Boolean is
         Status : UInt32 := 1;
      begin
         JTAG.SPI_Start_Transaction;
         -- Write RDSR and leave CS low so that status reg is sent continuously
         -- Flash chip will keep sending status aftr RDSR command
         JTAG.Write_Blocking (RDSR, 8);
         JTAG.SPI_Start_Read_Blocking (Status, 8);
         while (Status and 1) > 0 loop
            JTAG.SPI_Read_Next_Blocking (Status, 8);
         end loop;
         -- Read_Last to shift to exit-dr
         JTAG.SPI_Read_Last_Blocking (Status, 8);
         JTAG.SPI_End_Transaction;
         return (Status and 1) = 0; -- Should return false on timeout error
      end Wait_Write_In_Progress;
   begin
      if (Clock - Last_Button_Press) > Debounce_Time then
         Pico.LED.Toggle;
         JTAG.Load_JProgram;
         Last_Button_Press := Clock;
      end if;
   end GPIO_Isr_Handler;

   procedure Handle_Command (Cmd_Packet : UInt8_Array) is
      Command : constant Command_Id := Get_Command (Cmd_Packet);
   begin
      case Command is
         when Get_Programmer_Info =>
            Send_Programmer_Info;

         when Test_Connection     =>
            Run_Connection_Test;

         when Get_Board_Info      =>
            Send_Board_Info;

         when Configure_Target    =>
            Run_Configure_Target (Get_Payload (Cmd_Packet));

         when Start_UART          =>
            Run_UART;

         when Flash_Target        =>
            declare
               Payload : UInt8_Array := Get_Payload (Cmd_Packet);
            begin
               if Payload'Length >= 8 then
                  Run_Flash_Target
                    (Payload (Payload'First .. Payload'First + 3),
                     Payload (Payload'First + 4 .. Payload'First + 7));
               end if;
            end;

         when others              =>
            null;
      end case;
   end Handle_Command;

   procedure Send_Programmer_Info is
      Packet    : constant UInt8_Array := Encode (Device_Info);
      Write_Len : UInt32 := UInt32 (Packet'Length + 1);
   begin
      USB_Tx (1 .. Packet'Length) := Packet;
      USB_Tx (Packet'Length + 1) := 0;
      USB_Serial.Write (RP.Device.UDC, USB_Tx'Address, Write_Len);
   end Send_Programmer_Info;

   procedure Run_Connection_Test is
      Packet        : constant UInt8_Array :=
        Encode (Make_Packet (Ready, (1 .. 0 => 0)));
      Length        : UInt32 := UInt32 (Packet'Length + 1);
      Error_Timeout : constant Time := 20_000;
      Last          : Time := Clock;
   begin
      USB_Tx (1 .. Packet'Length) := Packet;
      USB_Tx (Packet'Length + 1) := 0;
      USB_Serial.Write (RP.Device.UDC, USB_Tx'Address, Length);
      while Clock - Last < Error_Timeout loop
         Length := USB_Rx'Length;
         USB_Serial.Read (USB_Rx'Address, Length);
         if Length > 0 then
            if USB_Rx (3) = UInt8 (Data_Packet) then
               USB_Serial.Write (RP.Device.UDC, USB_Rx'Address, Length);
               Last := Clock;
               Pico.LED.Toggle;
            else
               exit;
            end if;
         end if;
      end loop;
   end;

   procedure Send_Board_Info is
      Info       : aliased UInt32 := 0;
      Info_Bytes : UInt8_Array (1 .. 4)
      with Import, Convention => Ada, Address => Info'Address;
   begin
      JTAG.Get_Board_Info (Info);
      if Info > 0 then
         declare
            Packet : constant UInt8_Array :=
              Encode (Make_Packet (Data_Packet, Info_Bytes));
            Length : UInt32 := UInt32 (Packet'Length + 1);
         begin
            USB_Tx (1 .. Packet'Length) := Packet;
            USB_Tx (Packet'Length + 1) := 0;
            -- Send idcode Packet LSB first
            USB_Serial.Write (RP.Device.UDC, USB_Tx'Address, Length);
         end;
      else
         declare
            Packet : constant UInt8_Array :=
              Encode (Make_Packet (JTAG_Error, (1 .. 0 => 0)));
            Length : UInt32 := UInt32 (Packet'Length + 1);
         begin
            USB_Tx (1 .. Packet'Length) := Packet;
            USB_Tx (Packet'Length + 1) := 0;
            -- Send error message
            USB_Serial.Write (RP.Device.UDC, USB_Tx'Address, Length);
         end;
      end if;
   end Send_Board_Info;

   procedure Run_Configure_Target (Size : UInt8_Array) is
      Bs_Size             : UInt32
      with Address => Size'Address;
      Data                : UInt32_Array (1 .. 16);
      Bytes_Written_Total : UInt32 := 0;
      Bytes_Written       : UInt32 := 0;
      Error_Timeout       : constant Time := 1_000_000; -- 1s
      Last_Read           : Time;
      package AU renames Atomic.Unsigned_32;
      package BC renames Byte_Counter;
   begin
      -- Clear Rx buffer and byte count
      USB_Serial.Clear_Buffers;
      BC.Clear_Bytes_In;
      Send_Ready;
      -- Wait for buffer to fill
      while AU.Load (BC.Bytes_In) < 1024 loop
         null;
      end loop;
      JTAG.Setup_Configure_Target;
      Pico.Led.Clear;
      while USB_Serial.List_Ctrl_State.DTE_Is_Present
        and Bytes_Written_Total < Bs_Size
      loop
         Length := 64;
         -- Get up to 64 Bytes
         USB_Serial.Read (Data'Address, Length);
         Last_Read := Clock;
         if Length > 0 then
            for I in 1 .. Integer (Length / 4) loop
               Bytes_Written_Total := Bytes_Written_Total + 4;
               if Bytes_Written_Total = Bs_Size then
                  -- Last word requires special handling
                  JTAG.Write_Last_Blocking
                    (Reverse_Bytes (Data (I)), 32, JTAG.MSB_First);
                  Pico.LED.Set;
                  exit;
               else
                  JTAG.Write_Blocking (Reverse_Bytes (Data (I)), 32);
               end if;
               Bytes_Written := Bytes_Written + 4;
            end loop;
         end if;
         if Bytes_Written >= 512 and then Bytes_Written_Total < Bs_Size - 512
         then
            Bytes_Written := 0;
            Send_Ready; -- Tell host to send next 512 bytes

         end if;
         if Clock - Last_Read > Error_Timeout then
            return;
         end if;
      end loop;

      JTAG.Finish_Configure_Target;
      Send_Command (Configure_Target_Complete);
      USB_Serial.Clear_Buffers;
      BC.Clear_Bytes_In;
   end Run_Configure_Target;

   procedure Run_UART is
      UART      : RP.UART.UART_Port renames RP.Device.UART_0;
      Status    : UART_Status;
      Baud_Rate : RP.Hertz := RP.Hertz (USB_Serial.Coding.Bitrate);
   begin
      -- Switch to polling so USB sends nak during UART tx
      USB_Int.Disable;
      -- UART1 TX
      Pico.GP16.Configure (RP.GPIO.Output, RP.GPIO.Pull_Up, RP.GPIO.UART);
      -- UART1 RX
      Pico.GP17.Configure (RP.GPIO.Input, RP.GPIO.Floating, RP.GPIO.UART);
      Pico.LED.Configure (RP.GPIO.Output);
      UART.Configure
        (Config =>
           (Baud      => 19200,
            Word_Size => 8,
            Parity    => False,
            Stop_Bits => 1,
            others    => <>));
      Send_Ready;
      loop
         USB_Stack.Poll;
         if USB_Serial.List_Ctrl_State.DTE_Is_Present then
            Length := USB_Rx'Length;
            USB_Serial.Read (USB_Rx'Address, Length);
            if Length > 0 then
               UART.Transmit (UART_Tx (1 .. Integer (Length)), Status);
               if Status /= Ok then
                  USB_Serial.Write (RP.Device.UDC, "Uart Tx failed", Length);
               end if;
            end if;
            declare
               Rx_Status : UART_FIFO_Status := UART.Receive_Status;
            begin
               if Rx_Status = Invalid then
                  USB_Serial.Write
                    (RP.Device.UDC,
                     "Uart Rx failed with FIFO invalid",
                     Length);
               elsif Rx_Status = Empty then
                  null;
               else
                  UART.Receive (UART_Rx, Status, 50);
                  if Status = Ok then
                     Length := 1;
                     USB_Serial.Write (RP.Device.UDC, USB_Tx'Address, Length);
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Run_UART;

   procedure Run_Flash_Target
     (Size : Hal.UInt8_Array; Base_Addr : HAL.UInt8_Array)
   is
      Last_Read           : Time;
      Data_Size           : UInt32
      with Address => Size'Address;
      Address             : Uint32
      with Address => Base_Addr'Address;
      Data                : UInt32_Array (1 .. 16);
      Cur_Address         : UInt32 := Address;
      Bytes_Written_Total : UInt32 := 0;
      Bytes_Written       : UInt32 := 0;
      Page_Written        : UInt32 := 0;
      Error_Timeout       : constant Time := 1_000_000; -- 1s
      End_Address         : constant UInt32 := Address + Data_Size;
      Start_Threshold     : constant Unsigned_32 :=
        (if Data_Size > 1024 then 1024 else Unsigned_32 (Data_Size));

      package AU renames Atomic.Unsigned_32;
      package BC renames Byte_Counter;

      -- Check WIP bit of status register (bit 0)
      -- return True when WIP bit = 0,
      function Wait_Write_In_Progress return Boolean is
         Status : UInt32 := 1;
         Start  : Time;
      begin
         JTAG.SPI_Start_Transaction;
         -- Write RDSR and leave CS low so that status reg is sent continuously
         -- Flash chip will keep sending status aftr RDSR command
         JTAG.Write_Blocking (RDSR, 8);
         JTAG.SPI_Start_Read_Blocking (Status, 8);
         Start := Clock;
         while (Status and 1) > 0 loop
            JTAG.SPI_Read_Next_Blocking (Status, 8);
            exit when Clock - Start > Error_Timeout;
         end loop;
         -- Read_Last to shift to exit-dr
         JTAG.SPI_Read_Last_Blocking (Status, 8);
         JTAG.SPI_End_Transaction;
         return (Status and 1) = 0; -- Should return false on timeout error
      end Wait_Write_In_Progress;

   begin
      -- Address must be block aligned
      if (Address and (Block_Size - 1)) /= 0 then
         return;
      end if;
      USB_Serial.Clear_Buffers;
      BC.Clear_Bytes_In;
      Send_Ready;
      Pico.LED.Set;
      Pico.GP0.Set;
      JTAG.SPI_Start;
      -- Send Write Enable
      -- Erase 64kb Blocks
      if Data_Size > Block_Size then
         declare
            Blocks : constant UInt32 :=
              Data_Size - (Data_Size mod Block_Size) + Address;
         begin
            while Cur_Address < Blocks loop
               JTAG.SPI_Write_Command (WREN);
               JTAG.SPI_Write_Command (BE, Cur_Address);
               Cur_Address := Cur_Address + Block_Size;
               -- Wait for Block erase
               RP.Device.Timer.Delay_Milliseconds (BE_Wait);
               if not Wait_Write_In_Progress then
                  return;
               end if;
            end loop;
         end;
      end if;
      -- Erase 4kb sectors
      if Cur_Address < End_Address then
         declare
            Partial_Sector : constant UInt32 :=
              (End_Address - Cur_Address) mod Sector_Size;
            Sectors        : constant UInt32 :=
              (if Partial_Sector > 0
               then End_Address - Partial_Sector + Sector_Size
               else End_Address);
         begin
            while Cur_Address < Sectors loop
               JTAG.SPI_Write_Command (WREN);
               JTAG.SPI_Write_Command (SE, Cur_Address);
               Cur_Address := Cur_Address + Sector_Size;
               -- Wait for Sector Erase
               RP.Device.Timer.Delay_Milliseconds (SE_Wait);
               if not Wait_Write_In_Progress then
                  return;
               end if;
            end loop;
         end;
      end if;
      -- Tell host erase is done
      Send_Ready;
      -- Wait for buffer to full if necessary

      while AU.Load (BC.Bytes_In) < Start_Threshold loop
         null;
      end loop;

      Pico.GP0.Clear;
      Pico.LED.Clear;
      -- Initiate first page program
      Cur_Address := Address;
      JTAG.SPI_Write_Command (WREN);
      JTAG.SPI_Start_Transaction;
      JTAG.Write_Blocking (PP or Cur_Address, 32);
      while USB_Serial.List_Ctrl_State.DTE_Is_Present
        and Bytes_Written_Total < Data_Size
      loop
         if Bytes_Written >= 512 and Bytes_Written_Total < (Data_Size - 512)
         then
            Send_Ready;
            Bytes_Written := 0;
         end if;
         Length := 64;
         -- Get up to 64 Bytes
         USB_Serial.Read (Data'Address, Length);
         Last_Read := Clock;
         if Length > 0 then
            declare
               Words : constant Integer := Integer (Length / 4);
            begin
               for I in 1 .. Words loop
                  declare
                     Is_Last_Page_Word : constant Boolean :=
                       Page_Written + 4 = Page_Size;
                     Is_Last_Word      : constant Boolean :=
                       Bytes_Written_Total + 4 = Data_Size;
                  begin
                     Pico.LED.Set;
                     if Is_Last_Page_Word or Is_Last_Word then
                        -- CS must be set high on final bit of page/final word
                        JTAG.Write_Last_Blocking
                          (Reverse_Bytes (Data (I)), 32, JTAG.MSB_First);
                        JTAG.SPI_End_Transaction;
                        Pico.LED.Clear;

                        Bytes_Written_Total := Bytes_Written_Total + 4;
                        Bytes_Written := Bytes_Written + 4;
                        Page_Written := 0;

                        -- wait for page program to finish
                        if not Wait_Write_In_Progress then
                           return;
                        end if;

                        -- Start next page only when data remains
                        if Bytes_Written_Total < Data_Size then
                           Cur_Address := Cur_Address + Page_Size;
                           JTAG.SPI_Write_Command (WREN);
                           JTAG.SPI_Start_Transaction;
                           JTAG.Write_Blocking (PP or Cur_Address, 32);
                        end if;
                     else
                        JTAG.Write_Blocking (Reverse_Bytes (Data (I)), 32);
                        Pico.LED.Clear;
                        Bytes_Written_Total := Bytes_Written_Total + 4;
                        Bytes_Written := Bytes_Written + 4;
                        Page_Written := Page_Written + 4;
                     end if;
                  end;
               end loop;
            end;

         end if;
         -- Exit after timeout if no data to read
         if Clock - Last_Read > Error_Timeout then
            return;

         end if;
      end loop;

      -- Wait for final write if necessary;
      if not Wait_Write_In_Progress then
         return;
      end if;
      Pico.LED.Set;
      JTAG.SPI_Stop;
      -- Restart FPGA to load from flash
      JTAG.Load_JProgram;
      Send_Command (Flash_Target_Complete);
      USB_Serial.Clear_Buffers;
      BC.Clear_Bytes_In;
   end Run_Flash_Target;

   procedure Send_Ready is
      Packet : constant UInt8_Array :=
        Encode (Make_Packet (Ready, (1 .. 0 => 0)));
      Length : UInt32 := UInt32 (Packet'Length + 1);
   begin
      USB_Tx (1 .. Packet'Length) := Packet;
      USB_Tx (Packet'Length + 1) := 0;
      USB_Serial.Write (RP.Device.UDC, USB_Tx'Address, Length);
   end Send_Ready;

   procedure Send_Command (Command : Command_Id) is
      Packet : constant UInt8_Array :=
        Encode (Make_Packet (Command, (1 .. 0 => 0)));
      Length : UInt32 := UInt32 (Packet'Length + 1);
   begin
      USB_Tx (1 .. Packet'Length) := Packet;
      USB_Tx (Packet'Length + 1) := 0;
      USB_Serial.Write (RP.Device.UDC, USB_Tx'Address, Length);
   end Send_Command;

   procedure Run is
   begin
      RP.Clock.Initialize (Pico.XOSC_Frequency);
      RP.Clock.Enable (RP.Clock.PERI);
      RP.Device.Timer.Enable;
      RP.GPIO.Enable; -- Seems to be needed to enable USB
      Pico.GP0.Configure (RP.GPIO.Output, RP.GPIO.Pull_Down);
      Pico.LED.Configure (RP.GPIO.Output);
      USB_Int.Initialize;

      JTAG.Init;

      if Testing then
         --  Enable external switch for testing
         Pico.GP18.Configure (RP.GPIO.Input, RP.GPIO.Pull_Up);
         RP.GPIO.Interrupts.Attach_Handler
           (Pico.GP18, GPIO_Isr_Handler'Access);
         Pico.GP18.Enable_Interrupt (RP.GPIO.Falling_Edge);
         Pico.LED.Configure (RP.GPIO.Output);
         loop
            null;
         end loop;
      else
         loop
            if USB_Serial.List_Ctrl_State.DTE_Is_Present then
               Length := USB_Rx'Length;
               USB_Serial.Read (USB_Rx'Address, Length);
               if Length > 0 then
                  declare
                     Packet : UInt8_Array :=
                       Decode (USB_Rx (1 .. Integer (Length - 1)));
                  begin
                     if Is_Valid (Packet) then
                        Handle_Command (Packet);
                     end if;
                  exception
                     when Decode_Error =>
                        -- Error in packet encoding
                        USB_Serial.Clear_Buffers;
                  end;
               end if;
            end if;
         end loop;
      end if;
   end Run;
end AdaBMP_FW;
