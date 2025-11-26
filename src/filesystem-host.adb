with Ada.Directories;
with Ada.Text_IO;
with Ada.Containers.Vectors;

package body Filesystem.Host is

   function Read_File (Self : Host_Filesystem; Path : String) return String is
      File : Ada.Text_IO.File_Type;
      Line : String (1 .. 100);
      Last : Natural;
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Path);
      Ada.Text_IO.Get_Line (File, Line, Last);
      Ada.Text_IO.Close (File);
      return Line (1 .. Last);
   exception
      when others =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
         return "";
   end Read_File;

   function Exists (Self : Host_Filesystem; Path : String) return Boolean is
   begin
      return Ada.Directories.Exists (Path);
   end Exists;

   function List_Directory
     (Self : Host_Filesystem; Path : String) return Filesystem.String_Array
   is
      use Ada.Directories;
      Search  : Search_Type;
      Dir_Ent : Directory_Entry_Type;

      package String_Vectors is new
        Ada.Containers.Vectors
          (Index_Type   => Positive,
           Element_Type => Ada.Strings.Unbounded.Unbounded_String,
           "="          => Ada.Strings.Unbounded."=");

      Vec : String_Vectors.Vector;
   begin
      if not Exists (Path) then
         return (1 .. 0 => <>);
      end if;

      Start_Search (Search, Path, "");
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Dir_Ent);
         Vec.Append
           (Ada.Strings.Unbounded.To_Unbounded_String (Simple_Name (Dir_Ent)));
      end loop;

      declare
         Result : Filesystem.String_Array (1 .. Integer (Vec.Length));
      begin
         for I in Result'Range loop
            Result (I) := Vec.Element (I);
         end loop;
         return Result;
      end;
   end List_Directory;

end Filesystem.Host;
