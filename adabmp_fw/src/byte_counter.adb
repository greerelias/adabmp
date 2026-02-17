package body Byte_Counter is
   procedure Clear_Bytes_In is
   begin
      Atomic.Unsigned_32.Store (Bytes_In, 0);
   end Clear_Bytes_In;
end Byte_Counter;
