with RP.Device;
with RP.Clock;
with Pico;

with RP.PIO;
with RP.Reset;
with USB.Device.Serial;
with USB.Device;
with USB;
with HAL;      use HAL;
with RP.GPIO;
with RP.Timer; use RP.Timer;
with RP.GPIO.Interrupts;

with USB_Int;

package body AdaBMP_FW is
   Rx                : String (1 .. USB_Int.Max_Packet_Size);
   Message           : constant String := "Test";
   Length            : HAL.UInt32;
   Last_Button_Press : Time := 0;
   Debounce_Time     : constant Time := 20_000; --20ms

   procedure GPIO_Isr_Handler
     (Pin : RP.GPIO.GPIO_Pin; Trigger : RP.GPIO.Interrupt_Triggers)
   is
      pragma Unreferenced (Pin);
      pragma Unreferenced (Trigger);

   begin
      if (Clock - Last_Button_Press) > Debounce_Time then
         Last_Button_Press := Clock;
         if USB_Int.USB_Serial.List_Ctrl_State.DTE_Is_Present then
            --  USB_Int.USB_Serial.Read (Rx, Length);
            --  USB_Int.USB_Stack.Poll;
            --  if Length > 0 then
            USB_Int.USB_Serial.Write (RP.Device.UDC, Message, Length);
         --  USB_Int.USB_Stack.Poll;
         --  USB_Int.USB_Stack.Poll;

         end if;
         RP.GPIO.Toggle (Pico.LED);
      end if;
   end GPIO_Isr_Handler;

   procedure Run is

   begin
      RP.Clock.Initialize (Pico.XOSC_Frequency);
      RP.GPIO.Enable;
      Pico.GP16.Configure (RP.GPIO.Input, RP.GPIO.Pull_Up);

      RP.GPIO.Interrupts.Attach_Handler (Pico.GP16, GPIO_Isr_Handler'Access);
      Pico.GP16.Enable_Interrupt (RP.GPIO.Falling_Edge);
      --  Pico.LED.Configure (RP.GPIO.Output);
      --  RP.Reset.Reset_Peripheral (RP.Reset.Reset_USBCTRL);
      --  See usb_int.adb for initialization and interrupt handling setup   Pico.LED.Configure (Output);
      Pico.LED.Configure (RP.GPIO.Output);
      Pico.LED.Set;
      USB_Int.Initialize;
      loop
         null;
         --  if USB_Int.USB_Serial.List_Ctrl_State.DTE_Is_Present then
         --     USB_Int.USB_Serial.Read (Rx, Length);
         --     USB_Int.USB_Stack.Poll;
         --     --  if Length > 0 then
         --     USB_Int.USB_Serial.Write (RP.Device.UDC, Message, Length);
         --     USB_Int.USB_Stack.Poll;
         --     --  end if;

         --     Timer.Delay_Milliseconds (100);

         --  end if;
      end loop;
   end Run;
end AdaBMP_FW;
