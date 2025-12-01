with Interfaces; use Interfaces;
with Packet_Formatter;

package Commands is
   Get_Info        : constant Packet_Formatter.Command_Id := 1;
   Data_Packet     : constant Packet_Formatter.Command_Id := 2;
   Test_Connection : constant Packet_Formatter.Command_Id := 3;
   Ready           : constant Packet_Formatter.Command_Id := 4;
end Commands;
