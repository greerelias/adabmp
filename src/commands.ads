with Interfaces;       use Interfaces;
with Packet_Formatter; use Packet_Formatter;

package Commands is
   Get_Info        : constant Command_Id := 1;
   Data_Packet     : constant Command_Id := 2;
   Test_Connection : constant Command_Id := 3;
   Ready           : constant Command_Id := 4;
   End_Test        : constant Command_Id := 5;
end Commands;
