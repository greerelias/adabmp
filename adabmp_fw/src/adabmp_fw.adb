with Commands; use Commands;
with RP.Device;
with RP.Clock;
with RP.Uart;  use RP.Uart;
with Pico;


with HAL;                 use HAL;
with RP.GPIO;
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

   begin
      if (Clock - Last_Button_Press) > Debounce_Time then
         null;
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
                    (Reverse_Bytes (Data (1)), 32, JTAG.MSB_First);
                  Pico.LED.Set;
                  exit;
               else
                  JTAG.Write_Blocking (Reverse_Bytes (Data (I)), 32);
               end if;
               Bytes_Written := Bytes_Written + 4;
            end loop;
         end if;
         if Bytes_Written >= 512 then
            Bytes_Written := 0;
            Send_Ready; -- Tell host to send next 512 bytes

         end if;
         if Clock - Last_Read > Error_Timeout then
            return;
         end if;
      end loop;

      JTAG.Finish_Configure_Target;
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

   procedure Send_Ready is
      Packet : constant UInt8_Array :=
        Encode (Make_Packet (Ready, (1 .. 0 => 0)));
      Length : UInt32 := UInt32 (Packet'Length + 1);
   begin
      USB_Tx (1 .. Packet'Length) := Packet;
      USB_Tx (Packet'Length + 1) := 0;
      USB_Serial.Write (RP.Device.UDC, USB_Tx'Address, Length);
   end Send_Ready;

   procedure Run is
   begin
      RP.Clock.Initialize (Pico.XOSC_Frequency);
      RP.Clock.Enable (RP.Clock.PERI);
      RP.Device.Timer.Enable;
      RP.GPIO.Enable; -- Seems to be needed to enable USB

      USB_Int.Initialize;

      JTAG.Init;

      if Testing then
         -- Enable external switch for testing
         -- need to switch to different GPIO if used
         -- GP16 now used of uart so revise if using
         --  Pico.GP16.Configure (RP.GPIO.Input, RP.GPIO.Pull_Up);
         --  RP.GPIO.Interrupts.Attach_Handler
         --    (Pico.GP16, GPIO_Isr_Handler'Access);

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
                        null;
                  end;
               end if;
            end if;
         end loop;
      end if;
   end Run;
end AdaBMP_FW;
