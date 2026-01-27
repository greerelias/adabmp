with JTAG.PIO;
with RP.Device; use RP.Device;

package body JTAG is
   procedure PIO_JTAG_Init is
   begin
      Init_Pins;
      P.Enable;
      P.Load (JTAG.PIO.Jtag_Tdo_Program_Instructions, Program_Offset);

      Set_Out_Pins (Config, TDI.Pin, 1);  -- TDI is output to target
      --  Set_In_Pins (Config, TDO.Pin);      -- TDO is input from target
      Set_Sideset_Pins
        (Config,
         TCK.Pin); -- Clock can be set/cleared simulaneously with TDO/TDI
      Set_Sideset (Config, 1, False, False);
      Set_Out_Shift (Config, False, True, 8); -- Auto pull in 8 bits
      --  Set_In_Shift (Config, True, True, 32);  -- Auto push out 8 bits
      Set_Wrap
        (Config      => Config,
         Wrap_Target => JTAG.PIO.Jtag_Tdo_Wrap_Target,
         Wrap        => JTAG.PIO.Jtag_Tdo_Wrap);
      Set_Clock_Frequency (Config, 200_000);

      --  INPUT_SYNC_BYPASS.PROC_IN_SYNC_BYPASS :=
      --    16#800#; -- Bypass input sync for TDO

      P.SM_Initialize (SM, Program_Offset, Config); -- Init state machine
      P.Set_Pin_Direction (SM, TCK.Pin, Output);
      P.Set_Pin_Direction (SM, TDI.Pin, Output);
      P.Set_Pin_Direction (SM, TDO.Pin, Input);
      P.Set_Enabled (SM, True);

   end PIO_JTAG_Init;

   -- Configure GPIO pins
   procedure Init_Pins is
   begin
      TCK.Configure
        (Mode      => Output,
         Pull      => Pull_Down,
         Slew_Fast => True,
         Func      => P.GPIO_Function);
      TDO.Configure (Mode => Input, Pull => Pull_Down, Slew_Fast => True);
      TDI.Configure
        (Mode      => Output,
         Pull      => Pull_Up,
         Slew_Fast => True,
         Func      => P.GPIO_Function);
      TMS.Configure (Mode => Output, Pull => Pull_Up, Slew_Fast => True);
      RST.Configure (Mode => Input, Pull => Pull_Up, Slew_Fast => True);
      TRST.Configure (Mode => Output, Pull => Pull_Up, Slew_Fast => True);
   end Init_Pins;

   procedure JTAG_Write (Data : HAL.UInt8) is
      Success : Boolean;
      Input   : HAL.UInt32;
   begin
      P.Put (SM, 7);
      P.Put (SM, 16#AAAAAAAA#);
      --  P.Put (SM, 16#FFFFFFFF#);
      --  P.Put (SM, HAL.UInt32 (Data));
      --  P.Try_Put (SM, 7, Success);
      --  P.Try_Put (SM, HAL.UInt32 (Data), Success);
      --  P.Try_Get (SM, Input, Success);
      --  P.Get (SM, Input);
   end JTAG_Write;
end JTAG;
