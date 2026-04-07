with HAL;                      use HAL;
with HAL.UART;                 use HAL.UART;
with Atomic;
with Packet_Formatting;        use Packet_Formatting;
with USB.Device.AdaBMP_Serial; use USB.Device.AdaBMP_Serial;
with USB.Device;               use USB.Device;
with USB;

package AdaBMP_FW is

   -- Flash Memory Commands for Macronix MX25L3233FMI-08G
   WREN        : constant UInt32 := 16#06#; -- Write Enable
   BE          : constant UInt32 := 16#D8#; -- Block Erase
   SE          : constant UInt32 := 16#20#; -- Sector Erase
   Block_Size  : constant UInt32 := 16#1_0000#; -- 64KB
   Sector_Size : constant UInt32 := 16#1000#; -- 4KB
   Page_Size   : constant UInt32 := 16#100#; -- 256 bytes
   -- RDSR & PP right padded full word because we are using
   -- JTAG.Write_Blocking so we can continously read the status reg
   -- ie, keeping CS low after sending cmd
   RDSR        : constant UInt32 := 16#0500_0000#; -- Read status
   PP          : constant UInt32 := 16#0200_0000#; -- Page Program
   -- Erase/program wait times in ms for Macronix MX25L3233FMI-08G
   BE_Wait     : constant Integer := 325;
   SE_Wait     : constant Integer := 27;

   Info   : HAL.UInt32
   with Volatile, Export, Convention => C, External_Name => "db_info";
   USB_Tx : HAL.UInt8_Array (1 .. 64)
   with Volatile, Export, Convention => C, External_Name => "_tx";

   UART_Rx : HAL.UART.UART_Data_8b (1 .. 1)
   with Address => USB_Tx'Address;
   USB_Rx  : HAL.UInt8_Array (1 .. 64)
   with Volatile, Export, Convention => C, External_Name => "_rx";
   UART_Tx : HAL.UART.UART_Data_8b (1 .. 64)
   with Address => USB_Rx'Address;
   Length  : HAL.UInt32
   with Volatile, Export, Convention => C, External_Name => "_l";
   procedure Run;
   Data    : HAL.UInt32
   with Address => USB_Rx'Address;

   File_Size       : HAL.UInt8_Array (1 .. 4);
   Flash_Base_Addr : HAL.UInt8_Array (1 .. 4);
private
   procedure Send_Programmer_Info;

   procedure Run_Connection_Test;

   procedure Send_Board_Info;

   procedure Run_Configure_Target;
   procedure Run_Configure_Target_C1;
   procedure Run_Flash_Target;
   procedure Run_Flash_Target_C1;
   procedure Send_Ready;
   procedure Send_Command (Command : Command_Id);
   procedure Handle_Command (Cmd_Packet : HAL.UInt8_Array);
   procedure Run_UART;
end AdaBMP_FW;
