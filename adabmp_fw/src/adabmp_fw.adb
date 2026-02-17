with Commands; use Commands;
with Cortex_M.NVIC;
with RP.Device;
with RP.Clock;
with Pico;


with RP.PIO;
with RP.Reset;
with HAL;                 use HAL;
with RP.GPIO;
with RP.Timer;            use RP.Timer;
with RP.GPIO.Interrupts;
with Cortex_M.NVIC;
with Protocol;            use Protocol;
with JTAG;
with USB_Int;             use USB_Int;
with Atomic.Unsigned_32;
with Byte_Counter;        use Byte_Counter;
with Interfaces;          use Interfaces;
with System.Machine_Code; use System.Machine_Code;

package body AdaBMP_FW is
   Disabled_Msg      : constant String := "Interrupt Disabled";
   Enabled_Msg       : constant String := "Interrupt Enabled";
   Last_Button_Press : Time := 0;
   Debounce_Time     : constant Time := 500_000; --500ms
   Testing           : Boolean := True;
   Disabled          : Boolean := False;

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
         --  if Disabled then
         --     Cortex_M.NVIC.Enable_Interrupt (5);
         --     Length := Enabled_Msg'Length;
         --     USB_Serial.Write (RP.Device.UDC, Enabled_Msg'Address, Length);
         --     Disabled := False;
         --     Pico.LED.Set;
         --  else
         --     Cortex_M.NVIC.Disable_Interrupt (5);
         --     Length := Disabled_Msg'Length;
         --     USB_Serial.Write (RP.Device.UDC, Disabled_Msg'Address, Length);
         --     Disabled := True;
         --     Pico.LED.Clear;
         --  end if;
         --  JTAG.Set_TMS (False);
         --  JTAG.Write_Last_Blocking
         --    (Data => 16#B#, Length => 6, Dir => JTAG.MSB_First);
         JTAG.Write_Blocking (Test_Word, 32);
         Test_Word := Reverse_Bytes ((Test_Word));
         JTAG.Write_Last_Blocking (Test_Word, 32, JTAG.MSB_First);
         Pico.LED.Toggle;

      --  if Serial.List_Ctrl_State.DTE_Is_Present then
      --     Length := Disabled_Msg'Length;
      --     Serial.Write (RP.Device.UDC, Disabled_Msg'Address, Length);
      --  end if;
      --  if Atomic.Unsigned_32.Load (In_Packet_Counter) > 0 then
      --     Length := Rx'Length;
      --     USB_Serial.Read (Rx'Address, Length);
      --     Atomic.Unsigned_32.Sub (In_Packet_Counter, Unsigned_32 (Length));
      --     USB_Stack.Poll;
      --  end if;

      end if;
   end GPIO_Isr_Handler;

   procedure Handle_Command (Command : Command_Id) is
   begin
      case State is
         when Idle   =>
            case Command is
               when Get_Programmer_Info =>
                  Send_Programmer_Info;

               when Test_Connection     =>
                  State := Testing_Connection;
                  Start_Connection_Test;

               when Get_Board_Info      =>
                  Send_Board_Info;

               when Configure_Target    =>

                  null;

               when others              =>
                  null;
            end case;

         when others =>
            null;
      end case;
   end Handle_Command;

   procedure Send_Programmer_Info is
      Packet    : constant UInt8_Array := Encode (Device_Info);
      Write_Len : UInt32 := UInt32 (Packet'Length + 1);
   begin
      Tx (1 .. Packet'Length) := Packet;
      Tx (Packet'Length + 1) := 0;
      USB_Serial.Write (RP.Device.UDC, Tx'Address, Write_Len);
   end Send_Programmer_Info;

   procedure Start_Connection_Test is
      Packet : constant UInt8_Array :=
        Encode (Make_Packet (Ready, (1 .. 0 => 0)));
      Length : UInt32 := UInt32 (Packet'Length + 1);
   begin
      Tx (1 .. Packet'Length) := Packet;
      Tx (Packet'Length + 1) := 0;
      USB_Serial.Write (RP.Device.UDC, Tx'Address, Length);
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
            Tx (1 .. Packet'Length) := Packet;
            Tx (Packet'Length + 1) := 0;
            -- Send idcode Packet LSB first
            USB_Serial.Write (RP.Device.UDC, Tx'Address, Length);
         end;
      else
         declare
            Packet : constant UInt8_Array :=
              Encode (Make_Packet (JTAG_Error, (1 .. 0 => 0)));
            Length : UInt32 := UInt32 (Packet'Length + 1);
         begin
            Tx (1 .. Packet'Length) := Packet;
            Tx (Packet'Length + 1) := 0;
            -- Send error message
            USB_Serial.Write (RP.Device.UDC, Tx'Address, Length);
         end;
      end if;
   end Send_Board_Info;

   procedure Run_Configure_Target (Bs_Size : UInt32) is
      Data                : UInt32_Array (1 .. 16);
      -- Last/Final bytes requires special handling
      End_Bytes           : constant UInt32 := Bs_Size mod 64;
      Final_Bytes         : constant UInt32 := End_Bytes mod 4;
      End_Words           : UInt32 :=
        (if End_Bytes = 0 then 16 else End_Bytes / 4);
      Start_Bytes         : UInt32 := Bs_Size - End_Bytes;
      Bytes_Written_Total : UInt32 := 0;
      Bytes_Written       : UInt32 := 0;
      package AU renames Atomic.Unsigned_32;
      package BC renames Byte_Counter;
   begin
      -- Clear Rx buffer and byte count
      USB_Serial.Clear_Rx;
      BC.Clear_Bytes_In;
      Send_Ready;
      -- Wait for buffer to fill
      while AU.Load (BC.Bytes_In) < 1024 loop
         null;
      end loop;
      JTAG.Setup_Configure_Target;
      while Bytes_Written_Total < Start_Bytes loop
         -- Exit if serial connection is closed
         exit when not USB_Serial.List_Ctrl_State.DTE_Is_Present;
         Length := Data'Length;
         -- Get 64 Bytes
         USB_Serial.Read (Data'Address, Length);
         if Length > 0 then
            for I in 1 .. 16 loop
               JTAG.Write_Blocking (Reverse_Bytes (Data (I)), 32);
               Bytes_Written := Bytes_Written + 4;
            end loop;
         end if;
         if Bytes_Written >= 512 then
            Bytes_Written_Total := Bytes_Written_Total + Bytes_Written;
            Bytes_Written := 0;
            Pico.LED.Toggle;
            Send_Ready; -- Tell host to send next 512 bytes

         end if;
      end loop;

      -- Write final bytes
      if Bytes_Written_Total < Bs_Size then
         Length := Data'Length;
         -- Get remaining bytes
         USB_Serial.Read (Data'Address, Length);
         if Length > 0 then
            -- Write final bytes if data is word aligned
            if Final_Bytes = 0 then
               for I in 1 .. Integer (End_Words - 1) loop
                  JTAG.Write_Blocking (Reverse_Bytes (Data (I)), 32);
                  Bytes_Written := Bytes_Written + 4;
               end loop;
               JTAG.Write_Last_Blocking
                 (Reverse_Bytes (Data (Integer (End_Words))),
                  32,
                  JTAG.MSB_First);
               Bytes_Written := Bytes_Written + 4;
            else
               -- Write final bytes if end bytes are less than full word
               for I in 1 .. Integer (End_Words) loop
                  JTAG.Write_Blocking (Data (I), 32);
                  Bytes_Written := Bytes_Written + 4;
               end loop;
               -- Write last word with TMS high on final bit
               -- Should work without shifting even if less than 4 bytes
               -- since data is shifted out MSB first
               JTAG.Write_Last_Blocking
                 (Data (Integer (End_Words + 1)),
                  Final_Bytes * 8,
                  JTAG.MSB_First); -- change to jtag write last
               Bytes_Written := Bytes_Written + 4;
            end if;
         else
            return; -- Error, should never reach
         end if;
      end if;
      Pico.LED.Clear;
      JTAG.Finish_Configure_Target;
      USB_Serial.Clear_Rx;
      BC.Clear_Bytes_In;
      Pico.LED.Set;
   end Run_Configure_Target;

   procedure Send_Ready is
      Packet : constant UInt8_Array :=
        Encode (Make_Packet (Ready, (1 .. 0 => 0)));
      Length : UInt32 := UInt32 (Packet'Length + 1);
   begin
      Tx (1 .. Packet'Length) := Packet;
      Tx (Packet'Length + 1) := 0;
      USB_Serial.Write (RP.Device.UDC, Tx'Address, Length);
   end Send_Ready;

   procedure Run is
      Index : Integer := 0;
   begin
      RP.Clock.Initialize (Pico.XOSC_Frequency);
      RP.GPIO.Enable; -- Seems to be needed to enable USB

      USB_Int.Initialize;
      --  if not USB_Stack.Register_Class (USB_Serial'Access) then
      --     raise Program_Error;
      --  end if;
      --  if USB_Stack.Initialize
      --       (RP.Device.UDC'Access,
      --        USB.To_USB_String ("Ada Baremetal Programmer"),
      --        USB.To_USB_String ("AdaCore/Team 27"),
      --        USB.To_USB_String ("0001"),
      --        64)
      --    /= Ok
      --  then
      --     raise Program_Error;
      --  end if;

      --  USB_Stack.Start;

      if Testing then
         -- Enable external switch for testing
         Pico.GP16.Configure (RP.GPIO.Input, RP.GPIO.Pull_Up);
         RP.GPIO.Interrupts.Attach_Handler
           (Pico.GP16, GPIO_Isr_Handler'Access);

         Pico.LED.Configure (RP.GPIO.Output);
         Pico.LED.Set;

         JTAG.Init;
         JTAG.Set_TX_Shift_Direction (JTAG.MSB_First);

         Pico.GP16.Enable_Interrupt (RP.GPIO.Falling_Edge);

         --  RP.Device.Timer.Enable;
         loop
            --  if Atomic.Unsigned_32.Load (In_Packet_Counter) < Max_Packets then
            --     USB_Stack.Poll;
            --  end if;
            --  Stack.Poll;
            if USB_Serial.List_Ctrl_State.DTE_Is_Present then
               Length := Rx'Length;
               USB_Serial.Read (Rx'Address, Length);
               if Length > 0 then
                  declare
                     Packet  : UInt8_Array :=
                       Decode (Rx (1 .. Integer (Length - 1)));
                     Size    : UInt8_Array (1 .. 4);
                     Bs_Size : UInt32
                     with Address => Size'Address;
                  begin
                     if Is_Valid (Packet)
                       and then
                         Get_Command (Packet) = Commands.Configure_Target
                     then
                        Size := Packet_Formatting.Get_Payload (Packet);
                        Run_Configure_Target (Bs_Size);

                     end if;
                  end;
               end if;
            end if;
            null;
         end loop;
      else
         loop
            USB_Stack.Poll;
            if USB_Serial.List_Ctrl_State.DTE_Is_Present then
               Length := Rx'Length;
               USB_Serial.Read (Rx'Address, Length);
               if Length > 0 then
                  --  -- If we don't read a whole pack keep reading buffer
                  --  Index := Length + 1;
                  --  while Rx (Integer (Length)) /= 0 and then Index <= Rx'Length
                  --  loop
                  --     Serial.Read (Rx'Address (Index .. Rx'Length), Length);
                  --     Index := Index + Length;
                  --  end loop;
                  --  Index := 0;
                  case State is
                     when Idle               =>
                        declare
                           Packet : UInt8_Array :=
                             Decode (Rx (1 .. Integer (Length - 1)));
                        begin
                           if Is_Valid (Packet) then
                              Handle_Command (Get_Command (Packet));
                           end if;
                        end;

                     when Testing_Connection =>
                        if Rx (3) = UInt8 (Data_Packet) then
                           USB_Serial.Write
                             (RP.Device.UDC, Rx'Address, Length);
                        else
                           State := Idle;
                        end if;

                     when others             =>
                        null;
                  end case;
               end if;
            end if;
            null;
         end loop;
      end if;
   end Run;
end AdaBMP_FW;
