package body JTAG is
   procedure PIO_JTAG_Init is
   begin
      Init_Pins;
      P.Enable;
      P.Load (JTAG.PIO.Jtag_Tdo_Program_Instructions, Program_Offset);

      Set_Out_Pins (Config, TDI, 1);  -- TDI is output to target
      Set_In_Pins (Config, TDO);      -- TDO is input from target
      Set_Sideset_Pins
        (Config, TCK); -- Clock can be set/cleared simulaneously with TDO/TDI

      Set_Out_Shift (Config, False, True, 8); -- Auto pull in 8 bits
      Set_In_Shift (Config, False, True, 8);  -- Auto push out 8 bits

      Set_Clkdiv_Int_Frac (Config, Div_Int => 31, 0); -- Around 1Mhz

      P.Set_Pin_Direction (SM, TCK, Output);
      P.Set_Pin_Direction (SM, TDI, Output);
      P.Set_Pin_Direction (SM, TDO, Input);

      INPUT_SYNC_BYPASS (11) := 1; -- Bypass input sync for TDO

      P.SM_Initialize (SM, Program_Offset, Config); -- Init state machine
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

   procedure JTAG_Write (Data : UInt8) is
      Success : Boolean;
   begin
      P.Try_Put (SM, UInt32 (Data), Success);
   end JTAG_Write;
end JTAG;
