with Atomic.Unsigned_32;
with HAL; use HAL;
with Interfaces;

package Packet_Manager is

   In_Packet_Counter  : aliased Atomic.Unsigned_32.Instance :=
     Atomic.Unsigned_32.Init (0);
   Out_Packet_Counter : aliased Atomic.Unsigned_32.Instance :=
     Atomic.Unsigned_32.Init (0);

   Max_Packets : constant Interfaces.Unsigned_32 := 15;
end Packet_Manager;
