with JTAG.PIO;
with RP.Device; use RP.Device;
with System;
with RP.GPIO;   use RP.GPIO;
with USB.Lang;

package body JTAG is
   procedure PIO_JTAG_Init is
   begin
      Init_Pins;
      P.Enable;
      P.Load (JTAG.PIO.Jtag_Tdo_Program_Instructions, Program_Offset);

      Set_Out_Pins (Config, TDI.Pin, 1);  -- TDI is output to target
      Set_In_Pins (Config, TDO.Pin);      -- TDO is input from target
      Set_Sideset_Pins
        (Config,
         TCK.Pin); -- Clock can be set/cleared simulaneously with TDO/TDI
      Set_Sideset (Config, 1, False, False);
      Set_Out_Shift
        (Config, False, True, 32); -- Auto pull in 8 bits, left shift
      Set_In_Shift
        (Config, True, False, 32);  -- Auto push out 32 bits/shift right

      -- Bypass Input synchronizer for TDO
      PIO0_Reg.INPUT_SYNC_BYPASS := 16#800#;
      Set_Wrap
        (Config      => Config,
         Wrap_Target => JTAG.PIO.Jtag_Tdo_Wrap_Target,
         Wrap        => JTAG.PIO.Jtag_Tdo_Wrap);
      Set_Clock_Frequency (Config, 200_000);

      P.SM_Initialize (SM, Program_Offset, Config); -- Init state machine
      P.Set_Pin_Direction (SM, TCK.Pin, Output);
      P.Set_Pin_Direction (SM, TDI.Pin, Output);
      P.Set_Pin_Direction (SM, TDO.Pin, Input);
      P.Set_Enabled (SM, True);
      P.Clear_FIFOs (SM);

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
      Input   : UInt32;
   begin
      --  TX_FIFO := 8;
      --  TX_FIFO := 16#AA#;
      --  P.Clear_FIFOs (SM);
      P.Put (SM, 19);
      P.Put (SM, 16#EAAAAAEA#);
      --  P.Put (SM, 16#AAAAAAAA#);
      if not P.RX_FIFO_Empty (SM) then
         P.Get (SM, Input);
      end if;
      --  P.Put (SM, 16#FFFFFFFF#);
      --  P.Put (SM, UInt32 (Data));
      --  P.Try_Put (SM, 7, Success);
      --  P.Try_Put (SM, UInt32 (Data), Success);
      --  P.Try_Get (SM, Input, Success);
      --  P.Get (SM, Input);
   end JTAG_Write;

   procedure JTAG_Read_Blocking (Length : UInt32; Data : in out UInt32) is
      Len : constant UInt32 := Length - 1;
   begin
      P.Force_SM_IRQ (0);
      P.Put (SM, Len);
      P.Put (SM, 0);
      while P.SM_IRQ_Status (0) loop
         null;
      end loop;
      --  if not P.RX_FIFO_Empty (SM) then
      P.Get (SM, Data);
      --  end if;
   end JTAG_Read_Blocking;

   procedure JTAG_Get_Board_Info (Data : in out UInt32) is
      Last : UInt32;
   begin
      JTAG_Set_TMS (True);
      JTAG_Strobe_Blocking (5);
      JTAG_Set_TMS (False);
      JTAG_Strobe_Blocking (1);
      JTAG_Set_TMS (True);
      JTAG_Strobe_Blocking (1);
      JTAG_Set_TMS (False);
      JTAG_Strobe_Blocking (2);
      JTAG_Read_Blocking (31, Data);
      JTAG_Set_TMS (True);
      JTAG_Read_Blocking (1, Data);
      JTAG_Strobe_Blocking (1);
      JTAG_Set_TMS (False);
      JTAG_Strobe_Blocking (1);
      Data := UInt32 (Shift_Right (Data, 1));
      Last := Last and 1;
      Data := Data or Last;
   end JTAG_Get_Board_Info;

   procedure JTAG_Strobe_Blocking (Count : UInt32) is
      Cnt   : constant UInt32 := Count - 1;
      Input : UInt32;
   begin
      P.Force_SM_IRQ (0);
      P.Put (SM, Cnt);
      P.Put (SM, 0);
      if not P.RX_FIFO_Empty (SM) then
         P.Get (SM, Input);
      end if;
      while P.SM_IRQ_Status (0) loop
         null;
      end loop;
   end JTAG_Strobe_Blocking;

   procedure JTAG_Set_TMS (Value : Boolean) is
   begin
      if Value then
         TMS.Set;
      else
         TMS.Clear;
      end if;
   end JTAG_Set_TMS;

end JTAG;
