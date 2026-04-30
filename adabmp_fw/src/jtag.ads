with RP.PIO;            use RP.PIO;
with RP.GPIO;           use RP.GPIO;
with Pico;
with RP.Device;
with RP2040_SVD.SYSCFG; use RP2040_SVD.SYSCFG;
with HAL;               use HAL;
with System;
with Interfaces;        use Interfaces;
with RP2040_SVD.PIO;
with RP.Timer;          use RP.Timer;
with Pico_Jtag_Device;
with Jtag_types; use Jtag_Types;

package JTAG is

   type Shift_Direction is (MSB_First, LSB_First);
   procedure Init;

   procedure Write_Blocking (Data : UInt32; Length : UInt32);
   procedure Write_Last_Blocking
     (Data : UInt32; Length : Uint32; Dir : Shift_Direction);
   procedure Read_Last_Blocking (Data : in out UInt32; Length : UInt32);
   procedure Read_Blocking (Data : in out UInt32; Length : UInt32);

   procedure Get_Board_Info (Data : in out UInt32);

   procedure Strobe_Blocking (Count : UInt32);
   procedure Set_TMS (Value : Boolean);
   procedure Set_TDI (Value : Boolean);
   procedure Set_TCK (Value : Boolean);
   function Get_TDO return Boolean;

   procedure TAP_Reset;

   procedure Set_TX_Shift_Direction (Dir : Shift_Direction);

   procedure Setup_Configure_Target;

   procedure Finish_Configure_Target;

   procedure Load_JProgram;

   procedure SPI_Read_Register
     (Cmd : UInt32; Data : in out UInt32; Length : UInt32);

   -- Write command only, CS set HIGH after write
   procedure SPI_Write_Command (Cmd : UInt32);
   -- Write command w/ address
   procedure SPI_Write_Command (Cmd : UInt32; Address : UInt32);
   -- Write command then read in one transaction
   procedure SPI_Write_Read_Command
     (Cmd : UInt32; Data : in out UInt32; Length : UInt32);
   -- Send USER2 Code to entry SPI mode
   procedure SPI_Start;
   procedure SPI_Stop;
   -- Assume state is RTI and SPI_Start has been called
   procedure SPI_Start_Transaction;
   -- Assume state is Exit-DR and SPI_Start has been called
   procedure SPI_End_Transaction;
   -- Assume state is Shift-Dr and SPI_Start has been called
   -- Initiate a read were a single command results in multiple bytes returned
   procedure SPI_Start_Read_Blocking (Data : in out UInt32; Length : UInt32);
   -- Use only after SPI_Start_Read_Blocking to keep reading
   procedure SPI_Read_Next_Blocking (Data : in out UInt32; Length : UInt32);
   -- Use to finish read after SPI_Start_Read_Blocking or SPI_Read_Next_Blocking
   procedure SPI_Read_Last_Blocking (Data : in out UInt32; Length : UInt32);
   -- Use for single read up to full word
   procedure SPI_Read_Once_Blocking (Data : in out UInt32; Length : UInt32);
   -- Use for setting dm to active on target device



   --for debugging purposes

   type DMI_Op is
     (DMI_NOP,
      DMI_Read,
      DMI_Write);


   type DMI_Request is record
      Op      : DMI_Op;
      Data    : Interfaces.Unsigned_32;
      Address : Interfaces.Unsigned_32;
   end record;

   type DMI_Response is record
      Op      : Interfaces.Unsigned_32;
      Data    : Interfaces.Unsigned_32;
      Address : Interfaces.Unsigned_32;
   end record;

   function Build_DMI_DR
     (Req : DMI_Request) 
     return Bit_Array;


   procedure jtag_halt (Dev : in out Pico_Jtag_Device.JTAG_Device);
   procedure jtag_resume (Dev : in out Pico_Jtag_Device.JTAG_Device);
   
   procedure DMI_Read_Register
      (Dev      : in out Pico_Jtag_Device.JTAG_Device;
      Address  : in     Interfaces.Unsigned_32;
      Data_Out : out    Interfaces.Unsigned_32;
      Op_Out   : out    Interfaces.Unsigned_32);

   procedure Read_DMSTATUS
      (Dev    : in out Pico_Jtag_Device.JTAG_Device;
      Status : out Interfaces.Unsigned_32;
      Op     : out Interfaces.Unsigned_32);

   procedure Read_DMSTATUS_Value
      (Dev    : in out Pico_Jtag_Device.JTAG_Device;
      Status : out Interfaces.Unsigned_32);

   function SPI_Wait_Write_In_Progress return Boolean;
private
   Program_Offset : constant PIO_Address := 0;
   SM             : constant PIO_SM := 0;
   Config         : PIO_SM_Config := Default_SM_Config;
   P              : PIO_Device renames RP.Device.PIO_0;

   TCK : GPIO_Point renames Pico.GP10;
   TDO : GPIO_Point renames Pico.GP11;
   TDI : GPIO_Point renames Pico.GP12;
   TMS : GPIO_Point renames Pico.GP13;

   -- For SPI_Wait_Write_In_Progress
   -- RDSR right padded full word because we are using
   -- JTAG.Write_Blocking so we can continously read the status reg
   -- ie, keeping CS low after sending cmd
   RDSR          : constant UInt32 := 16#0500_0000#; -- Read status
   Error_Timeout : constant Time := 1_000_000; -- 1s

   TX_FIFO : aliased UInt32_Array (1 .. 4)
   with Volatile, Import, Address => P.TX_FIFO_Address (SM);

   RX_FIFO : aliased UInt32_Array (1 .. 4)
   with Volatile, Import, Address => P.RX_FIFO_Address (SM);

   PIO0_Reg : RP2040_SVD.PIO.PIO_Peripheral renames RP2040_SVD.PIO.PIO0_Periph;
   procedure Init_Pins;


end JTAG;
