with Packet_Formatter; use Packet_Formatter;

package Commands is
   Get_Programmer_Info       : constant Command_Id := 1;
   Data_Packet               : constant Command_Id := 2;
   Test_Connection           : constant Command_Id := 3;
   Ready                     : constant Command_Id := 4;
   End_Test                  : constant Command_Id := 5;
   Start_JTAG                : constant Command_Id := 6;
   JTAG_Command              : constant Command_Id := 7;
   Get_Board_Info            : constant Command_Id := 8;
   Flash_Target              : constant Command_Id := 9;
   Configure_Target          : constant Command_Id := 10;
   JTAG_Error                : constant Command_Id := 11;
   Configure_Target_Complete : constant Command_Id := 12;
   Start_UART                : constant Command_Id := 13;
   Flash_Target_Complete     : constant Command_Id := 14;
   Flash_Erase               : constant Command_Id := 15;
   Block64_Erase_Done        : constant Command_Id := 16;
   Block32_Erase_Done        : constant Command_Id := 17;
   Sector_Erase_Done         : constant Command_Id := 18;
   Flash_Erase_Complete      : constant Command_Id := 19;
end Commands;
