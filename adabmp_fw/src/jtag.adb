with JTAG.PIO;
with RP.Device; use RP.Device;
with System;
with RP.GPIO;   use RP.GPIO;
with USB.Lang;
with RP.Timer;
with Pico_Jtag_Device;
with Pico_Jtag;
with Jtag_Types; use Jtag_Types;
with Interfaces; use Interfaces;
with MT; use type MT.Bit;



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
      Set_Clock_Frequency (Config, 24_000_000);

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
      TDO.Configure (Mode => Input, Pull => Floating, Slew_Fast => True);
      TDI.Configure
        (Mode      => Output,
         Pull      => Pull_Up,
         Slew_Fast => True,
         Func      => P.GPIO_Function);
      TMS.Configure (Mode => Output, Pull => Pull_Up, Slew_Fast => True);
   end Init_Pins;

   procedure Write_Blocking (Data : UInt32; Length : UInt32) is
   begin
      P.Force_SM_IRQ (0);
      P.Put (SM, Length - 1);
      P.Put (SM, Data);
      while P.SM_IRQ_Status (0) loop
         null;
      end loop;
      P.Clear_FIFOs (SM);
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
      P.Get (SM, Data);
      P.Clear_FIFOs (SM);
   end Read_Blocking;

   -- Shift last word with TMS high on final bit
   procedure Read_Last_Blocking (Data : in out UInt32; Length : UInt32) is
      Last_Bit   : UInt32;
      Last_Shift : Integer := 32 - Integer (Length);
      Shift      : Integer := 32 - (Integer (Length) - 1);
   begin

      Read_Blocking (Data, Length - 1);
      Set_TMS (True);
      -- Get last bit
      Read_Blocking (Last_Bit, 1);
      -- Data is shifted in from left to right, align to LSB
      Data := UInt32 (Shift_Right (Data, Shift));
      -- Shift Last bit into position
      Last_Bit := UInt32 (Shift_Left (Last_Bit, Last_Shift));
      -- Insert last bit
      Data := Data or Last_Bit;
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
      Cnt : constant UInt32 := Count - 1;
   begin
      P.Force_SM_IRQ (0);
      P.Put (SM, Cnt);
      P.Put (SM, 0);
      while P.SM_IRQ_Status (0) loop
         null;
      end loop;
      P.Clear_FIFOs (SM);
   end Strobe_Blocking;

   procedure Set_TMS (Value : Boolean) is
   begin
      if Value then
         TMS.Set;
      else
         TMS.Clear;
      end if;
   end Set_TMS;

   
   procedure Set_TDI (Value : Boolean) is
   begin
      if Value then
         TDI.Set;
      else
         TDI.Clear;
      end if;
   end Set_TDI;

   procedure Set_TCK (Value : Boolean) is
   begin
      if Value then
         TCK.Set;
      else
         TCK.Clear;
      end if;
   end Set_TCK;

   function Get_TDO return Boolean is
   begin
      return TDO.Get;
   end Get_TDO;

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

   procedure Set_RX_Shift_Direction (Dir : Shift_Direction) is
   begin
      P.Set_Enabled (SM, False);
      case Dir is
         when LSB_First =>
            Set_In_Shift (Config, True, False, 32);

         when MSB_First =>
            Set_In_Shift (Config, False, False, 32);
      end case;
      P.SM_Initialize (SM, Program_Offset, Config); -- Init state machine
      P.Set_Enabled (SM, True);
      P.Clear_FIFOs (SM);
   end Set_RX_Shift_Direction;

   procedure Setup_Configure_Target is
      JProgram : constant UInt32 := 16#B#;
      CFG_IN   : constant UInt32 := 16#5#;
   begin
      Set_TX_Shift_Direction (LSB_First);
      Load_JProgram;
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

   -- Clears FPGA configuration SRAM
   procedure Load_JProgram is
      JProgram : constant UInt32 := 16#B#;
   begin
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
      -- Clock 10,000 cycles in RTI
      for I in 1 .. 314 loop
         Strobe_Blocking (32);
      end loop;
   end Load_JProgram;

   procedure Finish_Configure_Target is
      JStart : constant UInt32 := 16#C#;
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
      -- Clock 2000 cycles in RTI
      for I in 1 .. 63 loop
         Strobe_Blocking (32);
      end loop;
      TAP_Reset;
   end Finish_Configure_Target;

   procedure SPI_Write_Command (Cmd : UInt32) is
      Cmd_Shifted : UInt32 := UInt32 (Shift_Left (Cmd, 24));
   begin
      SPI_Start_Transaction;
      Write_Last_Blocking (Cmd_Shifted, 8, MSB_First);
      SPI_End_Transaction;
   end SPI_Write_Command;

   procedure SPI_Write_Command (Cmd : UInt32; Address : UInt32) is
      Cmd_Shifted : UInt32 := UInt32 (Shift_Left (Cmd, 24));
      Cmd_Addr    : UInt32 := Cmd_Shifted or Address;
   begin
      SPI_Start_Transaction;
      Write_Last_Blocking (Cmd_Addr, 32, MSB_First);
      SPI_End_Transaction;
   end SPI_Write_Command;

   procedure SPI_Write_Read_Command
     (Cmd : UInt32; Data : in out UInt32; Length : UInt32)
   is
      Cmd_Shifted : UInt32 := UInt32 (Shift_Left (Cmd, 24));
   begin
      SPI_Start_Transaction;
      Write_Blocking (Cmd_Shifted, 8);
      SPI_Read_Once_Blocking (Data, Length);
      SPI_End_Transaction;
   end SPI_Write_Read_Command;

   procedure SPI_Read_Register
     (Cmd : UInt32; Data : in out UInt32; Length : UInt32) is
   begin
      Set_TMS (False);
      Write_Blocking (Cmd, 8);
      Read_Blocking (Data, Length);
      Set_TMS (True);
   end SPI_Read_Register;

   procedure SPI_Start is
   begin
      JTAG.TAP_Reset;
      JTAG.Set_TMS (False);
      JTAG.Strobe_Blocking (1); -- RTI
      JTAG.Set_TMS (True);
      JTAG.Strobe_Blocking (2); -- Select-IR
      JTAG.Set_TMS (False);
      JTAG.Strobe_Blocking (2); -- Shift-IR
      -- Send USER2 code to start SPI
      JTAG.Write_Last_Blocking (16#02#, 6, JTAG.LSB_First);
      JTAG.Strobe_Blocking (1); -- Update-IR
      JTAG.Set_TMS (False);
      JTAG.Strobe_Blocking (1); -- RTI
      JTAG.Set_TX_Shift_Direction (JTAG.MSB_First);
      JTAG.Set_RX_Shift_Direction (JTAG.MSB_First);
      -- Ready to shift data
   end SPI_Start;

   procedure SPI_Stop is
   begin
      Set_TX_Shift_Direction (LSB_First);
      Set_RX_Shift_Direction (LSB_First);
      JTAG.TAP_Reset;
   end SPI_Stop;

   -- Assume state is RTI and SPI_Start has been called
   -- Ends in Shift-Dr
   procedure SPI_Start_Transaction is
   begin
      JTAG.Set_TMS (True);
      JTAG.Strobe_Blocking (1); -- Select-DR-Scan
      JTAG.Set_TMS (False);
      JTAG.Strobe_Blocking (2); -- Shift-Dr
   end SPI_Start_Transaction;

   -- Assume state is Exit-DR and SPI_Start has been called
   -- Ends in RTI
   procedure SPI_End_Transaction is
   begin
      JTAG.Strobe_Blocking (1); -- Update-DR
      JTAG.Set_TMS (False);
      JTAG.Strobe_Blocking (1); -- RTI
   end SPI_End_Transaction;

   -- Assume state is Shift-Dr and SPI_Start has been called
   procedure SPI_Start_Read_Blocking (Data : in out UInt32; Length : UInt32) is
      Fs   : constant Unsigned_32 := 16#FFFF_FFFF#;
      Mask : constant UInt32 := not UInt32 (Shift_Left (Fs, Integer (Length)));
   begin
      P.Force_SM_IRQ (0);
      P.Put (SM, Length);
      P.Put (SM, 0);
      while P.SM_IRQ_Status (0) loop
         null;
      end loop;
      P.Get (SM, Data);
      P.Clear_FIFOs (SM);
      -- SPI SO is delayed one TCK so dont want MS
      Data := Data and Mask;
   end SPI_Start_Read_Blocking;

   -- Use only after SPI_Start_Read_Blocking to account for SO delay
   procedure SPI_Read_Next_Blocking (Data : in out UInt32; Length : UInt32) is
   begin
      Read_Blocking (Data, Length);
   end SPI_Read_Next_Blocking;

   -- Use to finish read after SPI_Start_Read_Blocking or SPI_Read_Next_Blocking
   procedure SPI_Read_Last_Blocking (Data : in out UInt32; Length : UInt32) is
      Last_Bit : UInt32;
   begin
      Read_Blocking (Data, Length - 1);
      Set_TMS (True);
      Read_Blocking (Last_Bit, 1);
      Data := UInt32 (Shift_Left (Data, 1));
      Data := Data or Last_Bit;
   end SPI_Read_Last_Blocking;

   -- Use for single read up to full word
   procedure SPI_Read_Once_Blocking (Data : in out UInt32; Length : UInt32) is
      Last_Bit : UInt32;
   begin
      SPI_Start_Read_Blocking (Data, Length - 1);
      Set_TMS (True);
      Read_Blocking (Last_Bit, 1);
      Data := UInt32 (Shift_Left (Data, 1));
      Data := Data or Last_Bit;
   end SPI_Read_Once_Blocking;

   -- Check WIP bit of status register (bit 0)
   -- return True when WIP bit = 0,
   -- Timer must be enabled in main
   function SPI_Wait_Write_In_Progress return Boolean is
      Status : UInt32 := 1;
      Start  : Time;
   begin
      JTAG.SPI_Start_Transaction;
      -- Write RDSR and leave CS low so that status reg is sent continuously
      -- Flash chip will keep sending status aftr RDSR command
      JTAG.Write_Blocking (RDSR, 8);
      JTAG.SPI_Start_Read_Blocking (Status, 8);
      Start := Clock;
      while (Status and 1) > 0 loop
         JTAG.SPI_Read_Next_Blocking (Status, 8);
         exit when Clock - Start > Error_Timeout;
      end loop;
      -- Read_Last to shift to exit-dr
      JTAG.SPI_Read_Last_Blocking (Status, 8);
      JTAG.SPI_End_Transaction;
      return (Status and 1) = 0; -- Should return false on timeout error
   end SPI_Wait_Write_In_Progress;

      --Debugging functions and procedures
   
   function Build_DMI_DR
   (Req : DMI_Request) return Bit_Array
   is
      DR : Bit_Array (0 .. 40) := (others => MT.Zero);

      function To_Op_Bits (Op : DMI_Op) return Interfaces.Unsigned_32 is
      begin
         case Op is
            when DMI_NOP   => return 0;
            when DMI_Read  => return 1;
            when DMI_Write => return 2;
         end case;
      end To_Op_Bits;

      Op_Bits : constant Interfaces.Unsigned_32 := To_Op_Bits (Req.Op);

   begin
      ------------------------------------------------------------------
      -- op bits [1:0]
      ------------------------------------------------------------------
      if (Op_Bits and 1) /= 0 then
         DR (0) := MT.One;
      end if;

      if (Op_Bits and 2) /= 0 then
         DR (1) := MT.One;
      end if;

      ------------------------------------------------------------------
      -- data bits [33:2]
      ------------------------------------------------------------------
      for I in 0 .. 31 loop
         if (Req.Data and Interfaces.Shift_Left (Interfaces.Unsigned_32 (1), I)) /= 0 then
            DR (I + 2) := MT.One;
         end if;
      end loop;

      ------------------------------------------------------------------
      -- address bits [40:34]
      ------------------------------------------------------------------
      for I in 0 .. 6 loop
         if (Req.Address and Interfaces.Shift_Left (Interfaces.Unsigned_32 (1), I)) /= 0 then
            DR (I + 34) := MT.One;
         end if;
      end loop;

      return DR;
   end Build_DMI_DR;



   -- Use for setting dm to active on target device

   procedure jtag_halt (Dev : in out Pico_Jtag_Device.JTAG_Device) is

      -- DMI IR = 10001
      IR_In  : Bit_Array (0 .. 4) :=
      (0 => MT.One,
         1 => MT.Zero,
         2 => MT.Zero,
         3 => MT.Zero,
         4 => MT.One);

      IR_Out : Bit_Array (0 .. 4);

      DR_In  : Bit_Array (0 .. 40);
      DR_Out : Bit_Array (0 .. 40);

      DMCONTROL_Addr : constant Interfaces.Unsigned_32 := 16#10#;

      Req : DMI_Request;

   begin
      Pico_Jtag.Reset (Dev);

      -- Select DMI IR
      Pico_Jtag.Scan_IR (Dev, IR_In, IR_Out);

      ------------------------------------------------------------------
      -- 1. Enable debug (dmactive = 1)
      ------------------------------------------------------------------
      Req :=
      (Op      => DMI_Write,
         Data    => 16#0000_0001#,
         Address => DMCONTROL_Addr);

      DR_In := Build_DMI_DR (Req);
      Pico_Jtag.Scan_DR (Dev, DR_In, DR_Out);

      ------------------------------------------------------------------
      -- 2. Halt core (dmactive + haltreq)
      ------------------------------------------------------------------
      Req :=
      (Op      => DMI_Write,
         Data    => 16#8000_0001#,
         Address => DMCONTROL_Addr);

      DR_In := Build_DMI_DR (Req);
      Pico_Jtag.Scan_DR (Dev, DR_In, DR_Out);

   end jtag_halt;

   procedure jtag_resume (Dev : in out Pico_Jtag_Device.JTAG_Device) is

      -- DMI IR = 10001
      IR_In  : Bit_Array (0 .. 4) :=
      (0 => MT.One,
         1 => MT.Zero,
         2 => MT.Zero,
         3 => MT.Zero,
         4 => MT.One);

      IR_Out : Bit_Array (0 .. 4);

      DR_In  : Bit_Array (0 .. 40);
      DR_Out : Bit_Array (0 .. 40);

      DMCONTROL_Addr : constant Interfaces.Unsigned_32 := 16#10#;

      Req : DMI_Request;

   begin
      Pico_Jtag.Reset (Dev);

      -- Select DMI IR
      Pico_Jtag.Scan_IR (Dev, IR_In, IR_Out);

      ------------------------------------------------------------------
      -- Resume core (dmactive + resumereq)
      ------------------------------------------------------------------
      Req :=
      (Op      => DMI_Write,
         Data    => 16#4000_0001#,
         Address => DMCONTROL_Addr);

      DR_In := Build_DMI_DR (Req);
      Pico_Jtag.Scan_DR (Dev, DR_In, DR_Out);

   end jtag_resume;

   procedure DMI_Read_Register
   (Dev      : in out Pico_Jtag_Device.JTAG_Device;
      Address  : in     Interfaces.Unsigned_32;
      Data_Out : out    Interfaces.Unsigned_32;
      Op_Out   : out    Interfaces.Unsigned_32)
   is
      -- DMI IR = 10001
      IR_In  : Bit_Array (0 .. 4) :=
      (0 => MT.One,
         1 => MT.Zero,
         2 => MT.Zero,
         3 => MT.Zero,
         4 => MT.One);

      IR_Out : Bit_Array (0 .. 4);

      DR_In  : Bit_Array (0 .. 40);
      DR_Out : Bit_Array (0 .. 40);

      Read_Req : DMI_Request :=
      (Op      => DMI_Read,
         Data    => 0,
         Address => Address);

      Nop_Req : DMI_Request :=
      (Op      => DMI_NOP,
         Data    => 0,
         Address => 0);

      function Bits_To_U32
      (Arr   : Bit_Array;
         First : Natural;
         Last  : Natural) return Interfaces.Unsigned_32
      is
         Result : Interfaces.Unsigned_32 := 0;
         Shift  : Natural := 0;
      begin
         for I in First .. Last loop
            if Arr (I) = MT.One then
               Result :=
               Result or Interfaces.Shift_Left (Interfaces.Unsigned_32 (1), Shift);
            end if;
            Shift := Shift + 1;
         end loop;

         return Result;
      end Bits_To_U32;

   begin
      Data_Out := 0;
      Op_Out   := 0;

      -- Select DMI IR
      Pico_Jtag.Scan_IR (Dev, IR_In, IR_Out);

      -- First scan: submit read request
      DR_In := Build_DMI_DR (Read_Req);
      Pico_Jtag.Scan_DR (Dev, DR_In, DR_Out);

      -- Second scan: NOP to receive prior read result
      DR_In := Build_DMI_DR (Nop_Req);
      Pico_Jtag.Scan_DR (Dev, DR_In, DR_Out);

      -- Decode returned response
      Op_Out   := Bits_To_U32 (DR_Out, 0, 1);
      Data_Out := Bits_To_U32 (DR_Out, 2, 33);
   end DMI_Read_Register;

   procedure Read_DMSTATUS
   (Dev    : in out Pico_Jtag_Device.JTAG_Device;
      Status : out Interfaces.Unsigned_32;
      Op     : out Interfaces.Unsigned_32)
   is
      DMSTATUS_Addr : constant Interfaces.Unsigned_32 := 16#11#;
   begin
      DMI_Read_Register
      (Dev      => Dev,
         Address  => DMSTATUS_Addr,
         Data_Out => Status,
         Op_Out   => Op);
   end Read_DMSTATUS;


   procedure Read_DMSTATUS_Value
   (Dev    : in out Pico_Jtag_Device.JTAG_Device;
      Status : out Interfaces.Unsigned_32)
   is
      Op : Interfaces.Unsigned_32;
   begin
      Read_DMSTATUS (Dev, Status, Op);
   end Read_DMSTATUS_Value;

   
   -- Use fot halting the cpu of the target
   --procedure halt_cpu is
   --begin 

   --end halt_cpu;
   
   
   
   -- Use for resuming after a halt
   --procedure resume_halt is
   --begin

   --end resume_halt;


end JTAG;
