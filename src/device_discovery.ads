package Device_Discovery is

   Device_Not_Found : exception;

   --  Returns the path to the serial device (e.g., "/dev/ttyACM0")
   --  Raises Device_Not_Found if no matching device is found.
   function Find_Device (Vendor_ID, Product_ID : String) return String;

end Device_Discovery;
