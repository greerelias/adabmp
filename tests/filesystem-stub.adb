package body Filesystem.Stub is

   function Read_File (Self : Mock_Filesystem; Path : String) return String is
      Key : constant Unbounded_String := To_Unbounded_String (Path);
   begin
      if Self.Files.Contains (Key) then
         return To_String (Self.Files (Key));
      else
         return "";
      end if;
   end Read_File;

   function Exists (Self : Mock_Filesystem; Path : String) return Boolean is
      Key : constant Unbounded_String := To_Unbounded_String (Path);
   begin
      return Self.Files.Contains (Key) or else Self.Dirs.Contains (Key);
   end Exists;

   function List_Directory
     (Self : Mock_Filesystem; Path : String) return Filesystem.String_Array
   is
      Key : constant Unbounded_String := To_Unbounded_String (Path);
   begin
      if Self.Dirs.Contains (Key) then
         return Self.Dirs (Key);
      else
         return (1 .. 0 => <>);
      end if;
   end List_Directory;

   procedure Add_File
     (Self : in out Mock_Filesystem; Path : String; Content : String) is
   begin
      Self.Files.Include
        (To_Unbounded_String (Path), To_Unbounded_String (Content));
   end Add_File;

   procedure Add_Directory (Self : in out Mock_Filesystem; Path : String) is
      Key : constant Unbounded_String := To_Unbounded_String (Path);
   begin
      if not Self.Dirs.Contains (Key) then
         Self.Dirs.Insert (Key, (1 .. 0 => <>));
      end if;
   end Add_Directory;

   procedure Add_Directory_Entry
     (Self : in out Mock_Filesystem; Dir : String; Name : String)
   is
      Key       : constant Unbounded_String := To_Unbounded_String (Dir);
      New_Entry : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if Self.Dirs.Contains (Key) then
         declare
            Old_Arr : constant Filesystem.String_Array := Self.Dirs (Key);
            New_Arr : Filesystem.String_Array (1 .. Old_Arr'Length + 1);
         begin
            New_Arr (1 .. Old_Arr'Length) := Old_Arr;
            New_Arr (New_Arr'Last) := New_Entry;
            Self.Dirs.Replace (Key, New_Arr);
         end;
      else
         Self.Dirs.Insert (Key, (1 => New_Entry));
      end if;
   end Add_Directory_Entry;

end Filesystem.Stub;
