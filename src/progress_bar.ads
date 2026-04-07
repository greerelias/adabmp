package Progress_Bar is

   type Volatile_Integer is new Integer;
   pragma Volatile (Volatile_Integer);
   
   type Volatile_Integer_Access is access all Volatile_Integer;

   task type Bar_Task is
      entry Start
        (Start_Message : String;
         Stop_Message  : String;
         Total_Bytes   : Volatile_Integer_Access;
         Bytes_Sent    : Volatile_Integer_Access;
         Show_Bytes    : Boolean := False);
      entry Stop (Success : Boolean := True);
   end Bar_Task;

end Progress_Bar;
