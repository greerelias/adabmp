with HAL;
with HAL.UART;                 use HAL.UART;
with Atomic;
with Packet_Formatting;        use Packet_Formatting;
with USB.Device.AdaBMP_Serial; use USB.Device.AdaBMP_Serial;
with USB.Device;               use USB.Device;
with USB;

package AdaBMP_FW is

   type Programmer_State is
     (Idle,
      Testing_Connection,
      Getting_Board_Info,
      Configuring_Target,
      Debugging_Target);

   --  USB_Stack       : USB.Device.USB_Device_Stack (Max_Classes => 1);
   --  Max_Packet_Size : constant := 64;
   --  USB_Serial      :
   --    aliased USB.Device.AdaBMP_Serial.Default_Serial_Class
   --              (TX_Buffer_Size => 512, RX_Buffer_Size => 1024);
   State  : Programmer_State := Idle;
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

private
   procedure Send_Programmer_Info;

   procedure Run_Connection_Test;

   procedure Send_Board_Info;

   procedure Run_Configure_Target (Size : HAL.UInt8_Array);

   procedure Send_Ready;
   procedure Handle_Command (Cmd_Packet : HAL.UInt8_Array);
   procedure Run_UART;
end AdaBMP_FW;
