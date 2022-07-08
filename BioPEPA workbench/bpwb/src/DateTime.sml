(* For MLj only *)
structure DateTime = 
struct

  type Date = "java.lang.Date"

  fun now () = 
      let val date = _new Date ()
       in _invoke "toString" date
      end
end;
