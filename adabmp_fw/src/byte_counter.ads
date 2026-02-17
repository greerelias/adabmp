with Atomic.Unsigned_32;
with HAL; use HAL;
with Interfaces;

package Byte_Counter is

   Bytes_In  : aliased Atomic.Unsigned_32.Instance :=
     Atomic.Unsigned_32.Init (0);
   Bytes_Out : aliased Atomic.Unsigned_32.Instance :=
     Atomic.Unsigned_32.Init (0);

   procedure Clear_Bytes_In;
end Byte_Counter;
