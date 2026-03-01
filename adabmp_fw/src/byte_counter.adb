package body Byte_Counter is
   procedure Clear_Bytes_In is
   begin
      Atomic.Unsigned_32.Store (Bytes_In, 0);
   end Clear_Bytes_In;
   procedure Clear_Bytes_Out is
   begin
      Atomic.Unsigned_32.Store (Bytes_Out, 0);
   end Clear_Bytes_Out;
   procedure Clear_Bytes_Out_Total is
   begin
      Atomic.Unsigned_32.Store (Bytes_Out_Total, 0);
   end Clear_Bytes_Out_Total;
end Byte_Counter;
