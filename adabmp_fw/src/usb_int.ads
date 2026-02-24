--
--  Copyright 2022 (C) Jeremy Grosser
--
--  SPDX-License-Identifier: BSD-3-Clause
--

with Atomic.Unsigned_8;
with USB.Device.Serial;
with Atomic;
with USB.Device.AdaBMP_Serial;
with Interfaces; use Interfaces;

package USB_Int is

   USB_Stack       : USB.Device.USB_Device_Stack (Max_Classes => 1);
   Max_Packet_Size : constant := 64;
   USB_Serial      :
     aliased USB.Device.AdaBMP_Serial.Default_Serial_Class
               (TX_Buffer_Size => 256, RX_Buffer_Size => 1088);

   USB_Event : aliased Atomic.Flag := Atomic.Init (False);

   procedure Initialize;
   procedure Enable;
   procedure Disable;

end USB_Int;
