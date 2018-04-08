signature B = 

sig
    type bignat
    exception underflow
    exception	division_by_zero
    exception overflow		    
	exception	emptyList	    
         val ++ : (bignat * bignat) -> bignat   (* *)
        val normalize : bignat -> bignat       (* *)
	val fromString : string -> bignat      (* *)
	val toString: bignat -> string	       (* *)		       
        val -- : bignat * bignat -> bignat     (* *) 		   
        val compare: bignat * bignat -> order  (* *) 
        val ** : bignat * bignat -> bignat     (* *)
	val zero: bignat                       (* *)
(*	val compares: bignat * bignat -> int   *)
        val succ : bignat -> bignat       	(* *)				     
	val pred : bignat -> bignat	        (* *)
	val min : bignat * bignat -> bignat     (* *)
 	val max : bignat * bignat -> bignat     (* *)
	val << : bignat * bignat -> bool        (* *)
        val <<= : bignat * bignat -> bool       (* *)
        val >> : bignat * bignat -> bool        (* *)
        val >>= : bignat * bignat -> bool       (* *)
        val == : bignat * bignat -> bool        (* *)
        val len : bignat -> int                 (* *)
        val lenCompare : bignat * bignat -> order    (* *)
	val lenLt : bignat * bignat -> bool    (* *) 
	val lenLeq : bignat * bignat -> bool   (* *)
	val lenGt : bignat * bignat -> bool    (* *)
	val lenGeq : bignat * bignat -> bool   (* *)
	val lenEq : bignat * bignat -> bool    (* *)
	val quo : bignat * bignat -> bignat    (* *)
        val rem : bignat * bignat -> bignat    (* *)
	val %% : bignat * bignat -> (bignat * bignat) (* *)				 
end


structure BIGNAT:B=

	struct 

	type bignat =(string) 

      
      fun to_int_list clst =
        case clst of
	[]=>[]
      | xs::ys => ord(xs)-ord(#"0"):: to_int_list ys

						  
    fun string_to_list numstr =
	(to_int_list (explode numstr))


    fun to_char_list ilst =
    case ilst of
	[]=>[]
      | xs::ys => chr(xs+ord(#"0"))::to_char_list ys

    val  zero = "0"; 

						  
	fun fromString (x:string) =
	                             x

	fun toString (x) =
	                    x

fun reverse lst =
    if null lst 
    then []
    else reverse (tl lst) @ [hd lst]

fun string_to_list_rev numstr =
    reverse (to_int_list (explode numstr))

fun list_to_string_rev ilist =
    implode(to_char_list (reverse ilist))


fun string_to_list numstr =
    (to_int_list (explode numstr))

fun list_to_string ilist =
    implode(to_char_list ( ilist))

fun padding_zero ( x : string,n) =
    if n = 0 then x
    else
	padding_zero ("0"^x,n-1)

		     
fun padding_zeroback ( x : string,n) =
    if n <= 0 then x
    else
	padding_zeroback (x^"0",n-1)

fun removesignificantzero (xs:int list)=
    if null xs then [0]
    else
(   if hd xs = 0
    then
	removesignificantzero(tl xs)
    else
	xs
)
   
fun remzero (x: string) =
    
   list_to_string(removesignificantzero(string_to_list(x)))

fun normalize (x:string) =                           (*  normalize *)
      remzero(x) 
		 

fun sumlist (xs1:int list,xs2:int list,
	 xs3:int list,n,carry) =
    if n=0 then  list_to_string(carry::xs3)
    else
      (
	  if (hd xs1 + hd xs2 + carry)>=10
	then
	    sumlist(tl xs1,tl xs2,
  (((hd xs1 + hd xs2 + carry) mod 10) :: xs3),n-1,1)
	else
	    sumlist(tl xs1,tl xs2,
	((hd xs1 + hd xs2 + carry)::xs3),n-1,0)
      )

exception emptyList
fun sum (x: string ,y:string) =
    if (x ="") orelse (y="") then raise emptyList
    else
	(
          let val diff = String.size(x)-String.size(y)
        in ( if(diff >0)
          then
             sumlist(string_to_list_rev(x) ,
     string_to_list_rev(padding_zero(y,diff)),[],String.size(x),0)
   	  else
	      sumlist(string_to_list_rev(padding_zero(x,~diff)) ,
     string_to_list_rev(y),[],String.size(y),0)
)	end	     
	)
	    
fun ++ (x: string ,y:string) =
    remzero(sum(x,y))

fun succ (x:string) =
    ++(x,"1")
	   
fun minuslist (xs1:int list,xs2:int list,
	 xs3:int list,n,carry) =
    if n=0 then  list_to_string(carry::xs3)
    else
      (
	  if (hd xs1 -hd xs2 -carry)>=0
	then
	    minuslist(tl xs1,tl xs2,
  (((hd xs1- hd xs2 -carry)) :: xs3),n-1,0)
	else
	    minuslist(tl xs1,tl xs2,
	((hd xs1 -hd xs2 -carry+10)::xs3),n-1,1)
      )


fun minus (x: string ,y:string) =
    if (x ="") orelse (y="") then raise emptyList
    else
	(
          let val diff = String.size(x)-String.size(y)
        in ( if(diff >0)
          then
             minuslist(string_to_list_rev(x) ,
     string_to_list_rev(padding_zero(y,diff)),[],String.size(x),0)
   	  else
	      minuslist(string_to_list_rev(padding_zero(x,~diff)) ,
     string_to_list_rev(y),[],String.size(y),0)
)	end	     
	)
 	    

fun Comparehelper (xs1: int list,xs2: int list)=
    if null xs1 then 0
    else
	if(hd xs1 > hd xs2)
	then 1
	else
	   (	if(hd xs1 < hd xs2)
		then ~1
		else
		    Comparehelper(tl xs1,tl xs2)
			 )

fun compares (x:string ,y:string)=
let val diffs = String.size(x)-String.size(y)
  in  ( if (diffs>0)
	 then  Comparehelper(string_to_list(x),
			 string_to_list(padding_zero(y,diffs)))
			 else
	 Comparehelper(string_to_list(padding_zero(x,~diffs)),
		       string_to_list(y))
      )
	  
end
    
exception underflow ;
exception overflow ;	
fun -- (x: string ,y:string) =
    if compares(x,y) = ~1 then raise underflow
    else
 remzero(minus(x,y))

 fun pred (x:string) =
    --(x,"1")


    
fun min (x:string ,y:string) =
    let val chec = compares(x,y)
    in  if(chec =1 ) then y
	else
	    x
    end
	
fun max (x:string ,y:string) =
    let val chec = compares(x,y)
    in  if(chec =1 ) then x
	else
	    y
    end

fun << (x:string ,y:string) =
    let val chec = compares(x,y)
    in  if(chec = ~1 ) then true 
	else
	    false
    end	


fun <<= (x:string ,y:string) =
    let val chec = compares(x,y)
    in  if(chec = 1 ) then false 
	else
	    true
    end	
	
fun >> (x:string ,y:string) =
    let val chec = compares(x,y)
    in  if(chec = 1 ) then true 
	else
	    false
    end	


fun >>= (x:string ,y:string) =
    let val chec = compares(x,y)
    in  if(chec = ~1 ) then false 
	else
	    true
    end	

fun == (x:string ,y:string) =
    let val chec = compares(x,y)
    in  if(chec = 0 ) then true 
	else
	    false
    end
	
fun len (x:string) =
    String.size(remzero(x))
	       

fun lenCompare (x:string,y:string) =
    let val xle = len(x)
	val yle = len(y)
    in
	if(xle > yle ) then GREATER
	else 
	    (if xle < yle then  LESS
	     else
		 EQUAL

	    )
	end


fun lenLt (x:string,y:string) =
    let val xle = len(x)
	val yle = len(y)
    in
	if(xle < yle ) then true
	else false
	end


fun lenGt (x:string,y:string) =
    let val xle = len(x)
	val yle = len(y)
    in
	if(xle > yle ) then true
	else false
	end
	

fun lenEq (x:string,y:string) =
    let val xle = len(x)
	val yle = len(y)
    in
	if(xle = yle ) then true
	else false
	end

fun lenLeq (x:string,y:string) =
    let val xle = len(x)
	val yle = len(y)
    in
	if(xle > yle ) then false
	else true
	end

fun lenGeq (x:string,y:string) =
    let val xle = len(x)
	val yle = len(y)
    in
	if(xle < yle ) then false
	else true
	end
	
fun productone (xs1:int list,xs2:int list,carry,p)=
    if null xs1 then carry::xs2
    else
	productone(tl xs1,((hd xs1*p) +carry) mod 10 ::xs2
		   ,((hd xs1*p)+carry) div 10,p)

fun productlist (xs3:int list,xs1:int list,
		 xs2:int list,xs_2:int list,n) =
    if null xs3
    then (sum(padding_zeroback(list_to_string(xs2),n),list_to_string(xs_2)))
    else
	productlist(tl xs3,xs1,productone(xs1,[],0,hd xs3),
string_to_list(sum(padding_zeroback(list_to_string(xs2),n),list_to_string(xs_2))),n+1)
	
 fun ** (x: string ,y:string) =
    if (x ="") orelse (y="") then raise emptyList
    else
	 
	      (
          let val diff = String.size(x)-String.size(y)
        in ( if(diff >0)
          then
              remzero(productlist(string_to_list_rev(x) ,
     string_to_list_rev(padding_zero(y,diff)),[0],[0],~1))
   	  else
    remzero(productlist(string_to_list_rev(padding_zero(x,~diff)) ,
     string_to_list_rev(y),[0],[0],~1))
)	end	     
	      )

exception division_by_zero 
fun dividesearch (xs1:int list ,xs2:int list ,n )=
    if compares(list_to_string(xs2),"0") = 0 orelse n =10
    then raise  division_by_zero
    else
	(  let val remain = minus(list_to_string(xs1),
	      ** (list_to_string(xs2),Int.toString(n)))
       in
      (	   if( compares(remain,list_to_string(xs2)) = ~1)
	   then
	      
	       (n,remain)
	   else
               dividesearch(xs1,xs2,n+1)
      )  
end
	)
 
fun Dividehelp (xs_1:int list ,xs1:int list ,xs2:int list ,
	   rem: string, q:string ) =
    if null xs_1 then (remzero(q),remzero(rem))
    else
	( let val (q1,r1) = dividesearch(xs1,xs2,0)
	  in
(* print("\n"^r1^" --  "^q^Int.toString(q1)^"\n");
	      print(r1^(Int.toString(hd xs_1))); *)
	     
	      Dividehelp(tl xs_1,string_to_list(r1^(Int.toString(hd xs_1))),
			 xs2, r1  ,q^Int.toString(q1))
	    
	 end   
	)
	    
fun divpart (xs_1:int list,n,xs1:int list) =
    if n =0 then (xs_1,reverse(xs1))
    else
	divpart(tl xs_1,n-1,hd xs_1::xs1)
    







fun %% (x:string ,y:string ) =
    if compares(x,y)= ~1 then ("0",remzero(x))
    else
	
   ( let
	val (back,front) = divpart(string_to_list(remzero(x)),
				   String.size(remzero(y)),[])
	    
    in
	Dividehelp(back@[0],front,string_to_list(remzero(y)),"0","0")
   end
   )

fun quo (x:string ,y:string) =
    let val  (s,t)  = %% (x,y)
    in
	s
    end
	
fun rem (x:string ,y:string) =
    let val  (s,t)  = %% (x,y)
    in
	t
    end

fun compare (x:string ,y:string) =
     if (x ="") orelse (y="") then raise emptyList
    else
   ( let val che = compares (x,y)
    in
	if(che = 0) then EQUAL
	else
	    ( if (che = 1) then GREATER
	      else
		  LESS
	)		       
	end
       
)
	

       
       
       

  

end;
