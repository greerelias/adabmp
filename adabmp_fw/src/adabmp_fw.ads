with HAL;
with Atomic;
with Packet_Formatting; use Packet_Formatting;

package AdaBMP_FW is

   type Programmer_State is
     (Idle,
      Testing_Connection,
      Getting_Board_Info,
      Flashing_Target,
      Debugging_Target);

   State : Programmer_State := Idle;
   Tx    : HAL.UInt8_Array (1 .. 256)
   with Volatile, Export, Convention => C, External_Name => "_tx";

   Rx     : HAL.UInt8_Array (1 .. 256)
   with Volatile, Export, Convention => C, External_Name => "_rx";
   Length : HAL.UInt32
   with Volatile, Export, Convention => C, External_Name => "_l";
   procedure Run;

private
   procedure Send_Programmer_Info;

   procedure Start_Connection_Test;

   procedure Handle_Command (Command : Command_Id);
end AdaBMP_FW;
