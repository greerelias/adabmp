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

      -- Shift out data LSB(left shift), auto pull 32 bits
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
      Set_Clock_Frequency (Config, 4_000_000);

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

   procedure Write_Blocking (Data : UInt32; Length : UInt32) is
      Input : UInt32;
   begin
      P.Force_SM_IRQ (0);
      P.Put (SM, Length - 1);
      P.Put (SM, Data);
      while P.SM_IRQ_Status (0) loop
         null;
      end loop;
      -- Clear RX FIFO
      if not P.RX_FIFO_Empty (SM) then
         P.Get (SM, Input);
      end if;
   end Write_Blocking;

   procedure Write_Last_Blocking
     (Data : UInt32; Length : UInt32; Dir : Shift_Direction)
   is
      Len      : constant Integer := Integer (Length - 1);
      Last_Bit : constant UInt32 :=
        UInt32
          (if Dir = MSB_First
           then Shift_Left (Data, Len)
           else Shift_Right (Data, Len));
   begin
      Write_Blocking (Data, Length - 1);
      Set_TMS (True);
      Write_Blocking (Last_bit, 1);
   end Write_Last_Blocking;

   procedure Read_Blocking (Data : in out UInt32; Length : UInt32) is
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
   procedure Read_Last_Blocking (Data : in out UInt32; Length : UInt32) is
      Last : UInt32;
   begin
      Read_Blocking (Data, Length - 2);
      Set_TMS (True);
      Read_Blocking (Last, 1);
      Data := UInt32 (Shift_Right (Data, 1));
      Last := Last and 1;
      Data := Data or Last;
   end Read_Last_Blocking;

   procedure Get_Board_Info (Data : in out UInt32) is
   begin
      TAP_Reset;
      Set_TMS (False); -- Enter Run/Test -Idle
      Strobe_Blocking (1);
      Set_TMS (True);
      Strobe_Blocking (1); -- Select-DR-Scan
      Set_TMS (False);
      Strobe_Blocking (2); -- Shift-DR
      Read_Last_Blocking (Data, 32); -- Exit1-DR
      Strobe_Blocking (1); --Update-DR
      Set_TMS (False);
      Strobe_Blocking (1); --Run-Test-Idle
   end Get_Board_Info;

   -- Strobe up to 32 clocks
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

   -- Puts TAP in Test Logic Reset
   procedure TAP_Reset is
   begin
      Set_TMS (True);
      Strobe_Blocking (5);
   end TAP_Reset;

   procedure Set_TX_Shift_Direction (Dir : Shift_Direction) is
   begin
      P.Set_Enabled (SM, False);
      case Dir is
         when LSB_First =>
            Set_Out_Shift (Config, True, True, 32);

         when MSB_First =>
            Set_Out_Shift (Config, False, True, 32);
      end case;
      P.SM_Initialize (SM, Program_Offset, Config); -- Init state machine
      P.Set_Enabled (SM, True);
      P.Clear_FIFOs (SM);
   end Set_TX_Shift_Direction;

   procedure Setup_Configure_Target is
      JProgram : UInt32 := 16#B#;
      CFG_IN   : UInt32 := 16#5#;
   begin
      Set_TX_Shift_Direction (LSB_First);
      TAP_Reset;
      Set_TMS (False);
      Strobe_Blocking (1); -- RTI
      Set_TMS (True);
      Strobe_Blocking (2); -- Select-IR
      Set_TMS (False);
      Strobe_Blocking (2); -- Shift-IR
      Write_Last_Blocking (JProgram, 6, LSB_First); -- Write JProgram
      TAP_Reset;
      Set_TMS (False);
      -- Clock for atleast 10ms in RTI
      for I in 1 .. 32 loop
         Strobe_Blocking (32);
      end loop;
      Set_TMS (True);
      Strobe_Blocking (2); -- Select-IR
      Set_TMS (False);
      Strobe_Blocking (2); -- Shift-IR
      Write_Last_Blocking (CFG_IN, 6, LSB_First); -- Write CFG_IN
      Set_TMS (True);
      Strobe_Blocking (2); -- Select-DR
      Set_TMS (False);
      Strobe_Blocking (2); -- Shift-DR
      Set_TX_Shift_Direction (MSB_First);
      -- FPGA is set up to receive bitstream
   end Setup_Configure_Target;

   procedure Finish_Configure_Target is
      JStart : UInt32 := 16#C#;
   begin
      -- Bitstream is finished writing TMS should be high
      Set_TX_Shift_Direction (LSB_First);
      Strobe_Blocking (1); -- Update-DR
      Set_TMS (False);
      Strobe_Blocking (1); -- RTI
      Set_TMS (True);
      Strobe_Blocking (2); -- Select-IR
      Set_TMS (False);
      Strobe_Blocking (2); -- Shift-IR
      Write_Last_Blocking (JStart, 6, LSB_First); -- Write JStart
      Strobe_Blocking (1); -- Update-IR
      Set_TMS (False);
      -- Clock for atleast 2ms in RTI
      for I in 1 .. 7 loop
         Strobe_Blocking (32);
      end loop;
      TAP_Reset;
   end Finish_Configure_Target;
end JTAG;
