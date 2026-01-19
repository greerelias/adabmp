with RP.Device;
with RP.Clock;
with Pico;

with RP.Reset;
with USB.Device.Serial;
with USB.Device;
with USB;
with HAL; use HAL;
with RP.GPIO;

with USB_Int;

procedure Adabmp_FW is
   Message : String (1 .. USB_Int.Max_Packet_Size);
   Length  : HAL.UInt32;
begin
   RP.Clock.Initialize (Pico.XOSC_Frequency);
   RP.GPIO.Enable;
   --  Pico.LED.Configure (RP.GPIO.Output);
   --  RP.Reset.Reset_Peripheral (RP.Reset.Reset_USBCTRL);
   --  See usb_int.adb for initialization and interrupt handling setup
   USB_Int.Initialize;

   loop
      if USB_Int.USB_Serial.List_Ctrl_State.DTE_Is_Present then
         USB_Int.USB_Serial.Read (Message, Length);
         if Length > 0 then
            USB_Int.USB_Serial.Write
              (RP.Device.UDC, Message (1 .. Natural (Length)), Length);
         end if;
      end if;
   end loop;
end Adabmp_FW;
