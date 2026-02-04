with Commands; use Commands;
with Cortex_M.NVIC;
with RP.Device;
with RP.Clock;
with Pico;


with RP.PIO;
with RP.Reset;
with USB.Device.Serial; use USB.Device.Serial;
with USB.Device;        use USB.Device;
with USB;
with HAL;               use HAL;
with RP.GPIO;
with RP.Timer;          use RP.Timer;
with RP.GPIO.Interrupts;
with Cortex_M.NVIC;
with Protocol;          use Protocol;
with JTAG;              use JTAG;

package body AdaBMP_FW is
   Disabled_Msg      : constant String := "Interrupt Disabled";
   Enabled_Msg       : constant String := "Interrupt Enabled";
   Last_Button_Press : Time := 0;
   Debounce_Time     : constant Time := 500_000; --500ms
   Testing           : Boolean := False;
   Disabled          : Boolean := False;

   Device_Info_Pkt : aliased constant String :=
     Character'Val (170) & Character'Val (2) & "AdaBMP v0.1.0";
   Device_Info     : constant UInt8_Array (1 .. Device_Info_Pkt'Length)
   with Import, Convention => Ada, Address => Device_Info_Pkt'Address;

   Stack           : USB.Device.USB_Device_Stack (Max_Classes => 1);
   Max_Packet_Size : constant := 64;
   Serial          :
     aliased USB.Device.Serial.Default_Serial_Class
               (TX_Buffer_Size => 512, RX_Buffer_Size => 1024);

   procedure GPIO_Isr_Handler
     (Pin : RP.GPIO.GPIO_Pin; Trigger : RP.GPIO.Interrupt_Triggers)
   is
      pragma Unreferenced (Pin);
      pragma Unreferenced (Trigger);
   begin
      if (Clock - Last_Button_Press) > Debounce_Time then
         null;
         Last_Button_Press := Clock;
         --  if Disabled then
         --     Cortex_M.NVIC.Enable_Interrupt (5);
         --     Length := Enabled_Msg'Length;
         --     USB_Int.USB_Serial.Write
         --       (RP.Device.UDC, Enabled_Msg'Address, Length);
         --     Disabled := False;
         --     Pico.LED.Set;
         --  else
         --     Cortex_M.NVIC.Disable_Interrupt (5);
         --     Length := Disabled_Msg'Length;
         --     USB_Int.USB_Serial.Write
         --       (RP.Device.UDC, Disabled_Msg'Address, Length);
         --     Disabled := True;
         --  Pico.LED.Clear;
         --  end if;
         Send_Board_Info;
         Pico.LED.Toggle;

      --  if Serial.List_Ctrl_State.DTE_Is_Present then
      --     Length := 4;
      --     Serial.Write (RP.Device.UDC, Info'Address, Length);
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

               when Flash_Target        =>
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
      Serial.Write (RP.Device.UDC, Tx'Address, Write_Len);
   end Send_Programmer_Info;

   procedure Start_Connection_Test is
      Packet : constant UInt8_Array :=
        Encode (Make_Packet (Ready, (1 .. 0 => 0)));
      Length : UInt32 := UInt32 (Packet'Length + 1);
   begin
      Tx (1 .. Packet'Length) := Packet;
      Tx (Packet'Length + 1) := 0;
      -- Send Ready Packet
      Serial.Write (RP.Device.UDC, Tx'Address, Length);
   end;

   procedure Send_Board_Info is
      Info       : aliased UInt32 := 0;
      Info_Bytes : UInt8_Array (1 .. 4)
      with Import, Convention => Ada, Address => Info'Address;
   begin
      JTAG_Get_Board_Info (Info);
      if Info > 0 then
         declare
            Packet : constant UInt8_Array :=
              Encode (Make_Packet (Data_Packet, Info_Bytes));
            Length : UInt32 := UInt32 (Packet'Length + 1);
         begin
            Tx (1 .. Packet'Length) := Packet;
            Tx (Packet'Length + 1) := 0;
            -- Send idcode Packet LSB first
            Serial.Write (RP.Device.UDC, Tx'Address, Length);
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
            Serial.Write (RP.Device.UDC, Tx'Address, Length);
         end;
      end if;
   end Send_Board_Info;

   procedure Run is
      Index : Integer := 0;
   begin
      RP.Clock.Initialize (Pico.XOSC_Frequency);
      RP.GPIO.Enable; -- Seems to be needed to enable USB

      if not Stack.Register_Class (Serial'Access) then
         raise Program_Error;
      end if;
      if Stack.Initialize
           (RP.Device.UDC'Access,
            USB.To_USB_String ("Ada Baremetal Programmer"),
            USB.To_USB_String ("AdaCore/Team 27"),
            USB.To_USB_String ("0001"),
            64)
        /= Ok
      then
         raise Program_Error;
      end if;

      JTAG.PIO_JTAG_Init;

      Stack.Start;

      if Testing then
         -- Enable external switch for testing
         Pico.GP16.Configure (RP.GPIO.Input, RP.GPIO.Pull_Up);
         RP.GPIO.Interrupts.Attach_Handler
           (Pico.GP16, GPIO_Isr_Handler'Access);
         Pico.GP16.Enable_Interrupt (RP.GPIO.Falling_Edge);

         Pico.LED.Configure (RP.GPIO.Output);
         Pico.LED.Set;

         --  RP.Device.Timer.Enable;
         loop
            Stack.Poll;
            --  if Serial.List_Ctrl_State.DTE_Is_Present then
            --     Length := Rx'Length;
            --     Serial.Read (Rx'Address, Length);
            --     --  USB_Int.Stack.Poll;
            --     if Length > 0 then
            --        Serial.Write (RP.Device.UDC, Rx'Address, Length);
            --     end if;
            --  end if;
            --  Pico.LED.Toggle;
            --  JTAG_Write (2#01011100#);
            --  RP.Device.Timer.Delay_Milliseconds (100);

            null;
         end loop;
      else
         loop
            Stack.Poll;
            if Serial.List_Ctrl_State.DTE_Is_Present then
               Length := Rx'Length;
               Serial.Read (Rx'Address, Length);
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
                           Serial.Write (RP.Device.UDC, Rx'Address, Length);
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
