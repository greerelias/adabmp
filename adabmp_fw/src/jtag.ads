with RP.PIO;            use RP.PIO;
with RP.GPIO;           use RP.GPIO;
with Pico;
with RP.Device;
with RP2040_SVD.SYSCFG; use RP2040_SVD.SYSCFG;
with HAL;               use HAL;
with System;
with Interfaces;        use Interfaces;
with RP2040_SVD.PIO;

package JTAG is

   type Shift_Direction is (MSB_First, LSB_First);

   procedure Init;

   procedure Write (Data : UInt8);

   procedure Read_Blocking (Length : UInt32; Data : in out UInt32);

   procedure Get_Board_Info (Data : in out UInt32);

   procedure Strobe_Blocking (Count : UInt32);
   procedure Set_TMS (Value : Boolean);
   procedure TAP_Reset;

   procedure Set_TX_Shift_Direction (Dir : Shift_Direction);
private
   Program_Offset : constant PIO_Address := 0;
   SM             : constant PIO_SM := 0;
   Config         : PIO_SM_Config := Default_SM_Config;
   P              : PIO_Device renames RP.Device.PIO_0;

   TCK  : GPIO_Point renames Pico.GP10;
   TDO  : GPIO_Point renames Pico.GP11;
   TDI  : GPIO_Point renames Pico.GP12;
   TMS  : GPIO_Point renames Pico.GP13;
   RST  : GPIO_Point renames Pico.GP14;
   TRST : GPIO_Point renames Pico.GP15;

   TX_FIFO : aliased UInt32_Array (1 .. 4)
   with Volatile, Import, Address => P.TX_FIFO_Address (SM);

   RX_FIFO : aliased UInt32_Array (1 .. 4)
   with Volatile, Import, Address => P.RX_FIFO_Address (SM);

   PIO0_Reg : RP2040_SVD.PIO.PIO_Peripheral renames RP2040_SVD.PIO.PIO0_Periph;
   procedure Init_Pins;
end JTAG;
