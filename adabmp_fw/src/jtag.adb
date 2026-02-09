with JTAG.PIO;
with RP.Device; use RP.Device;
with System;
with RP.GPIO;   use RP.GPIO;
with USB.Lang;

package body JTAG is
   procedure Init is
   begin
      Init_Pins;
      P.Enable;
      P.Load (JTAG.PIO.Jtag_Tdo_Program_Instructions, Program_Offset);

      Set_Out_Pins (Config, TDI.Pin, 1);  -- TDI is output to target
      Set_In_Pins (Config, TDO.Pin);      -- TDO is input from target
      -- TCK(clock) can be set/cleared simulaneously with TDO/TDI
      Set_Sideset_Pins (Config, TCK.Pin);
      Set_Sideset (Config, 1, False, False);
      -- Shift out data LSB(right shift), auto pull 32 bits
      Set_Out_Shift (Config, True, True, 32);
      -- Shift in data LSB(right shift), no auto push
      Set_In_Shift (Config, True, False, 32);

      -- Bypass Input synchronizer for TDO
      PIO0_Reg.INPUT_SYNC_BYPASS := 16#800#;
      Set_Wrap
        (Config      => Config,
         Wrap_Target => JTAG.PIO.Jtag_Tdo_Wrap_Target,
         Wrap        => JTAG.PIO.Jtag_Tdo_Wrap);
      -- 100KHz: 4 instructions in loop; 1 clock each instruction
      -- TCK = Clk_F/4
      Set_Clock_Frequency (Config, 400_000);

      P.SM_Initialize (SM, Program_Offset, Config); -- Init state machine
      P.Set_Pin_Direction (SM, TCK.Pin, Output);
      P.Set_Pin_Direction (SM, TDI.Pin, Output);
      P.Set_Pin_Direction (SM, TDO.Pin, Input);
      P.Set_Enabled (SM, True);
      P.Clear_FIFOs (SM);

   end Init;

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

   procedure Write (Data : UInt8) is
      Success : Boolean;
      Input   : UInt32;
   begin
      --  TX_FIFO := 8;
      --  TX_FIFO := 16#AA#;
      --  P.Clear_FIFOs (SM);
      P.Put (SM, 31);
      P.Put (SM, 16#EAAAAAAA#);
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
   end Write;

   procedure Read_Blocking (Length : UInt32; Data : in out UInt32) is
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
   end Read_Blocking;

   -- Shift last word with TMS on final bit
   procedure Read_Last_Blocking (Length : UInt32; Data : in out UInt32) is
      Last : UInt32;
   begin
      Read_Blocking (Length - 1, Data);
      Set_TMS (True);
      Read_Blocking (1, Last);
      Data := UInt32 (Shift_Right (Data, 1));
      Last := Last and 1;
      Data := Data or Last;
   end Read_Last_Blocking;

   procedure Get_Board_Info (Data : in out UInt32) is
   begin
      TAP_Reset;
      Set_TMS (True);
      Strobe_Blocking (1); -- Select-DR-Scan
      Set_TMS (False);
      Strobe_Blocking (2); -- Shift-DR
      Read_Last_Blocking (32, Data); -- Exit1-DR
      Strobe_Blocking (1); --Update-DR
      Set_TMS (False);
      Strobe_Blocking (1); --Run-Test-Idle
   end Get_Board_Info;

   procedure Strobe_Blocking (Count : UInt32) is
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
   end Strobe_Blocking;

   procedure Set_TMS (Value : Boolean) is
   begin
      if Value then
         TMS.Set;
      else
         TMS.Clear;
      end if;
   end Set_TMS;

   -- Puts TAP in Run-Test/Idle state
   procedure TAP_Reset is
   begin
      Set_TMS (True);
      Strobe_Blocking (5);
      Set_TMS (False);
      Strobe_Blocking (1);
   end TAP_Reset;

   procedure Set_TX_Shift_Direction (Dir : Shift_Direction) is
   begin
      P.Set_Enabled (SM, False);
      case Dir is
         when MSB_First =>
            Set_Out_Shift (Config, True, True, 32);

         when LSB_First =>
            Set_Out_Shift (Config, False, True, 32);
      end case;
      P.Set_Enabled (SM, True);
      P.Clear_FIFOs (SM);
   end Set_TX_Shift_Direction;

end JTAG;
