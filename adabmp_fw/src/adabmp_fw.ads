with HAL;
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
      Flashing_Target,
      Debugging_Target);

   --  USB_Stack       : USB.Device.USB_Device_Stack (Max_Classes => 1);
   --  Max_Packet_Size : constant := 64;
   --  USB_Serial      :
   --    aliased USB.Device.AdaBMP_Serial.Default_Serial_Class
   --              (TX_Buffer_Size => 512, RX_Buffer_Size => 1024);
   State : Programmer_State := Idle;
   Info  : HAL.UInt32
   with Volatile, Export, Convention => C, External_Name => "db_info";
   Tx    : HAL.UInt8_Array (1 .. 256)
   with Volatile, Export, Convention => C, External_Name => "_tx";

   Rx     : HAL.UInt8_Array (1 .. 4)
   with Volatile, Export, Convention => C, External_Name => "_rx";
   Length : HAL.UInt32
   with Volatile, Export, Convention => C, External_Name => "_l";
   procedure Run;
   Data   : HAL.UInt32
   with Address => Rx'Address;

private
   procedure Send_Programmer_Info;

   procedure Start_Connection_Test;

   procedure Handle_Command (Command : Command_Id);
end AdaBMP_FW;
