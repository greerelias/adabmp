with Interfaces;       use Interfaces;
with Packet_Formatter; use Packet_Formatter;

package Commands is
   Get_Programmer_Info : constant Command_Id := 1;
   Data_Packet         : constant Command_Id := 2;
   Test_Connection     : constant Command_Id := 3;
   Ready               : constant Command_Id := 4;
   End_Test            : constant Command_Id := 5;
   Start_JTAG          : constant Command_Id := 6;
   JTAG_Command        : constant Command_Id := 7;
   Get_Board_Info      : constant Command_Id := 8;
   Flash_Target        : constant Command_Id := 9;
   Configure_Target    : constant Command_Id := 10;
end Commands;
