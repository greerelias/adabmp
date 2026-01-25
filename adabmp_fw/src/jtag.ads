with RP.PIO;            use RP.PIO;
with JTAG.PIO;
with RP.GPIO;           use RP.GPIO;
with Pico;
with RP.Device;
with RP2040_SVD.SYSCFG; use RP2040_SVD.SYSCFG;

package JTAG is

   procedure PIO_JTAG_Init;

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

   INPUT_SYNC_BYPASS : PROC_IN_SYNC_BYPASS_Register renames
     SYSCFG_Peripheral.PROC_IN_SYNC_BYPASS;

   procedure Init_Pins;
end JTAG;
