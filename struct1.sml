
use "Bn1.sml";
open BIGNAT ;
functor BigInt (Bn: B) :
 
  sig
      datatype bigint= mypair of (bignat * bool);
  val normalize : bigint -> bigint     (* *)
  val ++ : bigint * bigint -> bigint 
  val fromString: string -> bigint     (* *)
 val toString: bigint -> string	       (* *)		
  val -- : bigint * bigint -> bigint    (* *)				 
  val ** : bigint * bigint -> bigint    (* *)
  val %% : bigint * bigint -> (bigint * bigint)    (* *)
  val bigzero : bigint                  (* *)
  val compare : bigint * bigint -> order   (* *)
  val bigint : int -> bigint            (* *)
  val int : bigint -> int option        (* *)
  val abs : bigint -> bigint		(* *)	  
  val succ : bigint -> bigint           (* *)
  val min : bigint * bigint -> bigint   (* *)
  val max : bigint * bigint -> bigint   (* *)
  val sign : bigint -> int 	        (* *)
  val << : bigint * bigint -> bool       (* *)
  val <<= : bigint * bigint -> bool       (* *)
  val >> : bigint * bigint -> bool        (* *)
  val >>= : bigint * bigint -> bool      (* *)
  val == : bigint * bigint -> bool       (* *)
  val len : bigint -> int               (* *)
  val lenCompare : bigint * bigint -> order    (* *)
  val lenLt : bigint * bigint -> bool       (* *)
  val lenLeq : bigint * bigint -> bool       (* *)
  val lenGt : bigint * bigint -> bool      (* *)
  val lenGeq : bigint * bigint -> bool      (* *)
  val lenEq : bigint * bigint -> bool    (* *)
  val pred : bigint -> bigint            (* *)
  val quo : bigint * bigint -> bigint     (* *)
  val rem : bigint * bigint -> bigint	 (* *)
  val ~~ : bigint -> bigint    (* *)				   
  end	
  =
struct
	

	datatype bigint = mypair of (bignat * bool)
	;

(*	  fun string_to_lists (mypair(L,true)) = 
		string_to_list(L)  
	  | string_to_lists (mypair(L,false)) =
	    (~(hd (string_to_list(String.substring(L,1,String.size(L)-1))))
	    )::(tl (string_to_list(String.substring(L,1,String.size(L)-1))))
		   
*)
     fun signs (stri) =
	let val fir = ord(hd(String.explode(stri)))
  	in 
	    if(fir = 45)
	  then 	false
	    else
		true
       end


     fun sign(mypair(X,_)) =
	 if signs(X) then 1
	 else
	     ~1
    	    
     val  bigzero = mypair("0",true);
  
     	
     fun fromString (x:string ) =
	         mypair(x,signs(x))       (*  string_to_lists(fromStrings("-3453"));  *)

     fun abs (mypair(X,_)) =		       
              if signs(X) then mypair(X,true)
	      else fromString(String.substring(X,1,String.size(X)-1))
    		       
    fun toString (mypair(X,_))=
		  X     


    fun ~~ (mypair(X,true)) =
	    mypair("-"^X,false)
      | ~~ (mypair(X,false)) =
	     mypair(String.substring(X,1,String.size(X)-1),true)
		      
     fun normalize (mypair(X,true)) =
               fromString(BIGNAT.normalize(X)) 
       | normalize (mypair(X,false)) =
            fromString( "-"^BIGNAT.normalize(String.substring(X,1,String.size(X)-1)))
	    
    fun len (mypair(X,_)) =
        if signs(X) then String.size(BIGNAT.normalize(X))
	else
	    String.size(BIGNAT.normalize(String.substring(X,1,String.size(X)-1)))
    
    fun lenCompare (mypair(X,_),mypair(Y,_)) =
	let val a = len(fromString(X))
	    val	 b =  len(fromString(Y))
    in	if a>b then GREATER
	else  (if a = b then EQUAL else
               LESS
 	      )
   end
	fun lenLt (P,Q) =
	    if lenCompare(P,Q)  = LESS then true
	    else
		false

      	fun lenGt (P,Q) =
	    if lenCompare(P,Q)  = GREATER then true
	    else
		false

	fun lenEq (P,Q) =
	    if lenCompare(P,Q)  = EQUAL then true
	    else
		false

	fun lenLeq (P,Q) =
	    if lenCompare(P,Q)  = GREATER then false
	    else
		true
       
     	fun lenGeq (P,Q) =
	    if lenCompare(P,Q)  = LESS  then false
	    else
		true
       

		    
     fun fromStringdiv (x:string,y:string ) =
	        (mypair(x,signs(x)),mypair(y,signs(y)))       (*  string_to_lists(fromStrings("-3453"));  *)

		    
     fun bigint (x) =
	 fromString( Int.toString(x))

     fun int (mypair(X,_)) =
 	        Int.fromString(X)
     fun compared (x:string ,y:string) =
	 if ((signs(x) andalso signs(y)) orelse  (not(signs(x)) andalso not(signs(y))))
	 then
	     (if signs(x) then BIGNAT.compare(x,y)
	      else BIGNAT.compare(String.substring(y,1,String.size(y)-1),
					     String.substring(x,1,String.size(x)-1))
             )
	 else
	     ( if signs(x) then GREATER
	       else
		   LESS
	     )
		 
     fun compare (mypair(X,_),mypair(Y,_)) =
	 compared(X,Y)

     fun min (mypair(X,_),mypair(Y,_)) =
			  if compared(X,Y) = LESS
			  then fromString(X)
			  else
			      fromString(Y)
		 
      fun max (mypair(X,_),mypair(Y,_)) =
			  if compared(X,Y) = GREATER
			  then fromString(X)
			  else
			      fromString(Y)


      fun << (mypair(X,_),mypair(Y,_)) =
	  if compared(X,Y) = LESS then true
	  else
	      false

       fun >> (mypair(X,_),mypair(Y,_)) =
	  if compared(X,Y) = GREATER then true
	  else
	      false

        fun <<= (mypair(X,_),mypair(Y,_)) =
	  if compared(X,Y) = GREATER then false
	  else
	      true
    
  	fun >>= (mypair(X,_),mypair(Y,_)) =
	  if compared(X,Y) = LESS then false
	  else
	      true
    
   	fun == (mypair(X,_),mypair(Y,_)) =
	  if compared(X,Y) = EQUAL then true
	  else
	      false
    
  	   
    
		  
     fun comparemagnitude (x:string,y:string) =
	 if ((signs(x) andalso signs(y)) orelse  (not(signs(x)) andalso not(signs(y))))
	 then
	     (if signs(x) then BIGNAT.compare(x,y)
	      else BIGNAT.compare(String.substring(x,1,String.size(x)-1),
					     String.substring(y,1,String.size(y)-1))
             )
	 else
	     ( if signs(x) then
		         BIGNAT.compare(x, String.substring(y,1,String.size(y)-1))
		   
	       else
		   BIGNAT.compare(String.substring(x,1,String.size(x)-1),y)
	     )
		       
     fun ++ (mypair(X,true),mypair(Y,true)) =
	    fromString(BIGNAT.++(X,Y))
	  | ++ (mypair(X,false),mypair(Y,false))  =
	   fromString("-"^ BIGNAT.++(String.substring(X,1,String.size(X)-1),
		      String.substring(Y,1,String.size(Y)-1))) 
	  | ++ (mypair(X,_),mypair(Y,_)) =
	   ( if(comparemagnitude(X,Y)=EQUAL)
	    then fromString("0")
	     else
		( if (comparemagnitude(X,Y)=GREATER)
	         then ( if signs(X) then
		       fromString(BIGNAT.++(X, String.substring(Y,1,String.size(Y)-1)))
		       else
		     fromString("-"^BIGNAT.++(String.substring(X,1,String.size(X)-1),Y))                         )

		  else
 		      ( if signs(X) then
		       fromString("-"^BIGNAT.++(String.substring(Y,1,String.size(Y)-1),X))
		       else
			  fromString(BIGNAT.++(Y,String.substring(X,1,String.size(X)-1))))
		)
	   )
	       
     fun succ(mypair(X,_)) =
	 ++(mypair(X,signs(X)),mypair("1",true))
	       
     fun -- (mypair(X,true),mypair(Y,true)) =
	     ++ (mypair(X,true),mypair("-"^Y,false))
       | -- (mypair(X,false),mypair(Y,false)) =
	 ++ (mypair(X,false),mypair(String.substring(Y,1,String.size(Y)-1),true))
       | -- (mypair(X,true),mypair(Y,false)) =
	 ++ (mypair(X,true),mypair(String.substring(Y,1,String.size(Y)-1),true))
       | -- (mypair(X,false),mypair(Y,true)) =
	 ++ (mypair(X,false),mypair("-"^Y,false))
	       
      fun pred(mypair(X,_)) =
	 --(mypair(X,signs(X)),mypair("1",true))
	    
     fun ** (mypair(X,true),mypair(Y,true)) =
	    fromString(BIGNAT.**(X,Y))
	  | ** (mypair(X,false),mypair(Y,false))  =
	   fromString(BIGNAT.**(String.substring(X,1,String.size(X)-1),
		      String.substring(Y,1,String.size(Y)-1))) 
	  | ** (mypair(X,_),mypair(Y,_)) =
	   ( if signs(X)
		   then  fromString("-"^BIGNAT.**(X,
		      String.substring(Y,1,String.size(Y)-1)))
             else
		 fromString("-"^BIGNAT.**(Y,
		      String.substring(X,1,String.size(X)-1))) 
    
           )
	       
     fun %% (mypair(X,true),mypair(Y,true)) =
	    fromStringdiv(BIGNAT.%%(X,Y))
       | %% (mypair(X,false),mypair(Y,false))  =
	  ( let val (p,q) =  BIGNAT.%%(String.substring(X,1,String.size(X)-1),
		                           String.substring(Y,1,String.size(Y)-1))
	    in	( if q <> "0" then
		      fromStringdiv(BIGNAT.++(p,"1"),
				         BIGNAT.--(toString(abs(fromString(Y))),q))
		  else
		      fromStringdiv(p,q)
		)
		end

		 )
	  
	  | %% (mypair(X,_),mypair(Y,_)) =
	   ( if signs(X)
	     then
		 ( let val (p,q) = BIGNAT.%%(X,String.substring(Y,1,String.size(Y)-1))
		   in		fromStringdiv("-"^p,q)
					     end

		 )
	     else
                 ( let val (p,q) = BIGNAT.%%(String.substring(X,1,String.size(X)-1),Y)
		   in	(if q <> "0"   then
		           fromStringdiv("-"^BIGNAT.++(p,"1"),
				     BIGNAT.--(Y,q))
			 else
			      fromStringdiv("-"^p,q)
				     
                        )
		   end

		 )
           )


     fun quo (mypair(X,_),mypair(Y,_)) =
	 let val (m,n) = %% (fromString(X),fromString(Y))
	 in
             m
	 end

     fun rem (mypair(X,_),mypair(Y,_)) =
	 let val (m,n) = %% (fromString(X),fromString(Y))
	 in
             n
	 end
     
	       

     
	       
	
			     

end

structure name = BigInt(BIGNAT);

open name ;

