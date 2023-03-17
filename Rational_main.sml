signature BIGINT =
  sig
    type bigint
    exception bigint_error
    val zero : bigint
    val make_big : int list list -> bigint
    val break_big : bigint -> int list list
    val fromInt : int -> bigint
    val fromString : string -> bigint
    val toString : bigint -> string
    val toInt : bigint -> int
    val sign : bigint -> int
    val negate : bigint -> bigint
    val abs : bigint -> bigint
    val equal : bigint * bigint -> bool
    val less : bigint * bigint -> bool
    val add : bigint * bigint -> bigint
    val subtract : bigint * bigint -> bigint
    val multiply : bigint * bigint -> bigint
    val divide : bigint * bigint -> bigint list
    val quotient : bigint * bigint -> bigint
    val remainder : bigint * bigint -> bigint
    val reduce : bigint * bigint -> bigint list
end;

structure BIGINT : BIGINT =
  struct
    type bigint = int list list
    exception bigint_error
    local

    fun digit(l) = case l of
                        x :: xs => x
                      | [] => 0;
    (*gives the first element for a non empty list and 0 for an empty list*)

    fun headlesslist(l)= case l of
                              x::xs => xs
                            | [] => [];
    (*gives the tail for a non empty list and an empty list for an empty list*)

    fun add_digits(x,y,c)= (x+y+c) mod 10;
    (*gives the unit digit of the sum of x,y and c*)

    fun carry_in_addition(x,y,c)= (x+y+c) div 10;
    (*gives the tens digit of the sum of x,y and c*)

    fun add_(l1,l2,c)=if length(l1) + length(l2) <> 0 then
      add_digits(digit(l1),digit(l2),c) :: add_( headlesslist(l1), headlesslist(l2) ,
      carry_in_addition(digit(l1),digit(l2),c)) 
                      else if c<>0 then [c] else [] ;
    (*recursively adds two numbers represented as lists by adding heads of the list and passing
    rest of the elements of the lists along with a carry to itself until both lists are empty*)

    fun subtract_digits(x,y,c)= (x-y+c) mod 10;
    (*gives the unit digit when subtracting y from x and also accounting for carry
    given and carry borrowed*)

    fun carry_in_subtraction(x,y,c)= (x-y+c) div 10;
    (*gives the carry borrowed when subtracting y from x*)

    fun dirty_subtract(l1,l2,c)=if length(l1) + length(l2) <> 0 then
      subtract_digits(digit(l1),digit(l2),c) :: dirty_subtract( headlesslist(l1) ,
      headlesslist(l2), carry_in_subtraction(digit(l1),digit(l2),c)) 
                                else [] ;
    (*recursively subtracts two numbers represented as lists by subtracting heads of the list and passing
    rest of the elements of the lists along with a carry to itself until both lists are empty*)
    
    fun clean(l)=if length(l)=0 then []
                else let val r=clean(tl(l)) in if (length(r)=0) andalso (hd(l)=0) then [] 
                      else hd(l) :: r end;
    (*removes unnecessary zeros from the end of the list recursively until the last element of
    the list is is non-zero*)

    fun subtract_(l1,l2)=clean(dirty_subtract(l1,l2,0));
    (*subtracts two numbers represented as lists and also removes unnecessary zeros*)

    fun compare_adv(l1,l2)= case l1 of 
                                x :: xs => let val i=compare_adv(tl(l1),tl(l2)) in
                                  if i=2 then 
                                  if hd(l1)>hd(l2) then 0
                                  else if hd(l1)<hd(l2) then 1 else 2 else i end
                              | [] => 2;
    (*compares two numbers represented by lists of equal length recursively by comparing the
    later digits first and if they are equal then comparing the first elements.
    0: l1>l2 , 1: l1<l2 , 2: l1=l2 *)

    fun compare(l1,l2) = let val l_1=clean(l1)
      val l_2=clean(l2) in if length(l_1) > length(l_2) then true
                        else if length(l_1) < length(l_2) then false 
                              else compare_adv(l_1,l_2)=0 end;
    (*compares two numbers represented as lists and gives the value of l1>l2*) 

    fun multiply_digits(x,n,c)= (x*n+c) mod 10;
    (*gives the unit digit when muntiplying x and y and adding the carry c*)

    fun carry_in_multiplication(x,n,c) = (x*n+c) div 10;
    (*gives the tens digit when muntiplying x and y and adding the carry c*)

    fun multiply(l,n,c)= if length(l) <> 0 then
      multiply_digits(hd(l),n,c) ::
      multiply(tl(l),n,carry_in_multiplication(hd(l),n,c)) 
                        else if c <> 0 then [c] else [] ;
    (*multiplies a number represented as a list and a single digit number
    recursively by multiplying the first element and passing rest of the digits to
    itself along with a carry*)

    fun multiply_lists(l1,l2)= if length(l1) <> 0 then
      add_(multiply(l2,hd(l1),0),multiply_lists(tl(l1),0::l2),0) 
                              else [] ;
    (*multiplies two numbers represented as lists by multiplying the first element of the*)
    (*second list with the first list and passing the rest of the elements of the second*)
    (*list to itself along with a carry*)

    fun division_last_digit(x,y,c)= if compare(y,x) then c else division_last_digit(subtract_(x,y),y,c+1);
    (*gives the last digit of the quotient when dividing x by y*)

    fun divide_integer_part(x,y,n)= if length(y)<n then [[],x] else let val l= division_last_digit(x,y,0) val r= divide_integer_part(subtract_(x,multiply(y,l,0)),tl(y),n) in [l::hd(r),hd(tl(r))] end;
    (*gives the integer part of the quotient when dividing x by y*)

    fun split(l,n)= if n=0 then [tl(l),[hd(l)]] else let val r=split(tl(l),n-1) in [hd(r),hd(l)::hd(tl(r))] end;
    (*splits a list into two lists at the nth position*)

    fun index(x,l)= if l=[] then ~1 else if compare(hd(l),x)=false andalso compare(x,hd(l))=false then 0 else let val i=index(x,tl(l)) in if i= ~1 then i else 1+i end;
    (*gives the index of the first occurence of x in l*)

    fun divide_decimal_part(x,y,rem,ans)= if clean(x)=[] then [ans,[]] else let val l=division_last_digit(0::x,y,0) val remainder=clean(subtract_(0::x,multiply(y,l,0))) val i=index(remainder,rem) in
    if(i= ~1) then divide_decimal_part(remainder,y,remainder::rem,l::ans) else split(l::ans,i) end;
    (*gives the decimal part of the quotient when dividing x by y*)

    fun add_length(l,n)= if length(l)>=n then l else add_length(0::l,n);
    (*adds zeros to the end of a list until the length of the list is equal to n*)

    fun divide_(x,y)= let  val k=add_length(y,length(x)) val l=divide_integer_part(x,k,length(y)) val d=divide_decimal_part(hd(tl(l)),y,[hd(tl(l))],[]) in [clean(rev(hd(l))),hd(d),hd(tl(d))] end;
    (*divides two numbers represented as lists and gives the quotient and remainder*)

    fun convert_to_list(x)= if x=0 then [] else (x mod 10)::convert_to_list(x div 10);
    (*converts a number to a list*)

    fun convert_to_number(l)= if l=[] then 0 else (hd(l) + 10*convert_to_number(tl(l)));
    (*converts a list to a number*)

    fun con l= if l=[] then [] else if hd(l)= #"-" then con(tl(l)) else ord(hd(l))-48::con(tl(l));
    (*converts a list of characters to a list of integers*)

    fun string_to_list(s)= if s="" then [] else con(explode(s));
    (*converts a string to a list*)

    fun list_to_string(l)= if l=[] then "" else Char.toString(chr(hd(l)+48))^list_to_string(tl(l));
    (*converts a list to a string*)

    in

    val zero = [[0],[]]

    fun make_big(x)= [hd(x),hd(tl(x))]

    fun break_big(x)= [hd(x),hd(tl(x))]

    fun fromInt x = if x<0 then [[1],clean(convert_to_list(~x))] else [[0],clean(convert_to_list(x))]

    fun fromString s = if(hd(explode(s))= #"~") then [[1],clean(rev(string_to_list(s)))] else [[0],clean(rev(string_to_list(s)))]

    fun toString l = if hd(hd(l))=1 then "~"^list_to_string(rev(hd(tl(l)))) else list_to_string(rev(hd(tl(l))))

    fun toInt l = if hd(hd(l))=1 then ~1 * convert_to_number(hd(tl(l))) else convert_to_number(hd(tl(l)))

    fun sign l = hd(hd(l))

    fun negate l = if hd(hd(l))=1 then [[0],hd(tl(l))] else [[1],hd(tl(l))]

    fun abs(l) = [[0],hd(tl(l))]

    fun equal (x,y) = if hd(hd(x))=hd(hd(y)) then (compare(hd(tl(x)),hd(tl(y))) orelse compare(hd(tl(y)),hd(tl(x))))=false else if clean(hd(tl(x)))=[] andalso clean(hd(tl(y)))=[] then true else false

    fun less (x,y) = if hd(hd(x))=1 then if hd(hd(y))=1 then compare(hd(tl(x)),hd(tl(y))) else true else if hd(hd(y))=1 then false else compare(hd(tl(y)),hd(tl(x))) 

    fun add (x,y) = if hd(hd(x))=hd(hd(y)) then [hd(x),clean(add_(hd(tl(x)),hd(tl(y)),0))] else if compare(hd(tl(x)),hd(tl(y))) then [hd(x),clean(subtract_(hd(tl(x)),hd(tl(y))))] else [hd(y),clean(subtract_(hd(tl(y)),hd(tl(x))))]

    fun subtract (x,y) = if hd(hd(x))=hd(hd(y)) then if compare(hd(tl(x)),hd(tl(y))) then [hd(x),clean(subtract_(hd(tl(x)),hd(tl(y))))] else [hd(y),clean(subtract_(hd(tl(y)),hd(tl(x))))] else [hd(x),clean(add_(hd(tl(x)),hd(tl(y)),0))]

    fun multiply (x,y) = if hd(hd(x))=hd(hd(y)) then [[0],clean(multiply_lists(hd(tl(x)),hd(tl(y))))] else [[1],clean(multiply_lists(hd(tl(x)),hd(tl(y))))]

    fun divide (x,y) = (*if clean(hd(tl(y)))=[] then raise bigint_error else*) let val l=divide_(hd(tl(x)),hd(tl(y))) in if hd(hd(x))=hd(hd(y)) then [[[0],clean(hd(l))],[clean(hd(tl(l))),clean(hd(tl(tl(l))))]] else [[[1],clean(hd(l))],[clean(hd(tl(l))),clean(hd(tl(tl(l))))]] end

    fun quotient (x,y) = (*if clean(hd(tl(y)))=[] then raise bigint_error else*) let val l=divide_integer_part(hd(tl(x)),add_length(hd(tl(y)),length(hd(tl(x)))),length(hd(tl(y)))) in if hd(hd(x))=hd(hd(y)) then [[0],clean(rev(hd(l)))] else if hd(hd(x))=1 then [[1],add_([1],clean(rev(hd(l))),0)] else [[0],clean(rev(hd(l)))] end

    fun remainder (x,y)= (*if clean(hd(tl(y)))=[] then raise bigint_error else*) let val l=divide_integer_part(hd(tl(x)),add_length(hd(tl(y)),length(hd(tl(x)))),length(hd(tl(y)))) in if hd(hd(x))=hd(hd(y)) then [hd(y),clean(hd(tl(l)))] else [hd(y),subtract_(hd(tl(y)),clean(hd(tl(l))))] end

    fun reduce (x,y) = let fun gcd (x,y) = if equal(y,zero) then x else gcd(y,remainder(x,y))
    fun hcf (x,y) = if less(x,y) then gcd(y,x) else gcd(x,y)
     val z=hcf(abs(x),abs(y)) in if sign(y)=1 then [negate(quotient(x,z)),negate(quotient(y,z))] else [quotient(x,z),quotient(y,z)] end
    end
  end;

signature RATIONAL =
  sig
    type bigint
    type rational
    exception rat_error
    val make_rat: bigint * bigint -> rational option
    val rat: bigint -> rational option
    val reci: bigint -> rational option
    val neg: rational -> rational
    val inverse : rational -> rational option
    val equal : rational * rational -> bool (* equality *)
    val less : rational * rational -> bool (* less than *)
    val add : rational * rational -> rational (* addition *)
    val subtract : rational * rational -> rational (* subtraction *)
    val multiply  : rational * rational -> rational (* multiplication *)
    val divide : rational * rational -> rational option (* division *)
    val showRat : rational -> string
    val showDecimal : rational -> string
    val fromDecimal : string -> rational
    val toDecimal : rational -> string
end;

functor Rational (BIGINT : BIGINT ) : RATIONAL =
  struct

    type bigint = BIGINT.bigint
    type rational = bigint list
    exception rat_error

    fun make_rat (x,y) = if BIGINT.equal(y,BIGINT.zero) then NONE else SOME(BIGINT.reduce(x,y))

    fun rat x = SOME([x,BIGINT.make_big([[0],[1]])])

    fun reci (x) = if BIGINT.equal(x,BIGINT.zero) then NONE else SOME([BIGINT.make_big([[0],[1]]),x])

    fun neg (x) = if BIGINT.equal(hd(x),BIGINT.zero) then BIGINT.reduce(hd(x),hd(tl(x))) else BIGINT.reduce(BIGINT.negate(hd(x)),hd(tl(x)))
    
    fun inverse (x) = if BIGINT.equal(hd(x),BIGINT.zero) then NONE else SOME(BIGINT.reduce(hd(tl(x)),hd(x)))
    fun equal (x,y) = let val x1=BIGINT.reduce(hd(x),hd(tl(x))) val y1=BIGINT.reduce(hd(y),hd(tl(y))) in BIGINT.equal(hd(x1),hd(y1)) andalso BIGINT.equal(hd(tl(x1)),hd(tl(y1))) end

    fun less (x,y) = let val x1=BIGINT.reduce(hd(x),hd(tl(x))) val y1=BIGINT.reduce(hd(y),hd(tl(y))) in if BIGINT.sign(hd(x1))>BIGINT.sign(hd(y1)) then true else if BIGINT.sign(hd(x1))<BIGINT.sign(hd(y1)) then false else let val z=BIGINT.multiply(hd(x1),hd(tl(y1))) val w=BIGINT.multiply(hd(y1),hd(tl(x1))) in if BIGINT.sign(hd(x1))=1 then BIGINT.less(w,z) else BIGINT.less(z,w) end end

    fun add (x,y) = let val x1=BIGINT.reduce(hd(x),hd(tl(x))) val y1=BIGINT.reduce(hd(y),hd(tl(y))) val z=BIGINT.add(BIGINT.multiply(hd(x1),hd(tl(y1))),BIGINT.multiply(hd(y1),hd(tl(x1)))) val w=BIGINT.multiply(hd(tl(x1)),hd(tl(y1))) in BIGINT.reduce(z,w) end

    fun subtract (x,y) = let val x1=BIGINT.reduce(hd(x),hd(tl(x))) val y1=BIGINT.reduce(hd(y),hd(tl(y))) val z=BIGINT.subtract(BIGINT.multiply(hd(x1),hd(tl(y1))),BIGINT.multiply(hd(y1),hd(tl(x1)))) val w=BIGINT.multiply(hd(tl(x1)),hd(tl(y1))) in BIGINT.reduce(z,w) end

    fun multiply (x,y) = let val x1=BIGINT.reduce(hd(x),hd(tl(x))) val y1=BIGINT.reduce(hd(y),hd(tl(y))) val z=BIGINT.multiply(hd(x1),hd(y1)) val w=BIGINT.multiply(hd(tl(x1)),hd(tl(y1))) in BIGINT.reduce(z,w) end

    fun divide (x,y) = let val x1=BIGINT.reduce(hd(x),hd(tl(x))) val y1=BIGINT.reduce(hd(y),hd(tl(y))) val z=BIGINT.multiply(hd(x1),hd(tl(y1))) val w=BIGINT.multiply(hd(y1),hd(tl(x1))) in if BIGINT.equal(w,BIGINT.zero) then NONE else SOME(BIGINT.reduce(z,w)) end

    fun showRat (x) = BIGINT.toString(hd(x)) ^ "/" ^ BIGINT.toString(hd(tl(x)))

    fun showDecimal (x) = let val x1=BIGINT.reduce(hd(x),hd(tl(x))) val y=BIGINT.divide(hd(x1),hd(tl(x1))) val d= if hd(tl(BIGINT.break_big(hd(tl(y)))))=[] then "(0)" else "("^BIGINT.toString(BIGINT.make_big([[0],hd(tl(BIGINT.break_big(hd(tl(y)))))]))^")"  in BIGINT.toString(hd(y)) ^ "." ^ BIGINT.toString(BIGINT.make_big([[0],hd(BIGINT.break_big(hd(tl(y))))]))^ d end

    fun fromDecimal x = let fun add_zeros (x,y) = if y<=0 then x else add_zeros(0::x,y-1) fun num (x,l) = if x=[] orelse hd(x)= #" " orelse hd(x)= #")" then rev(l) else if hd(x)= #"." orelse hd(x)= #"(" then num(tl(x),[]::l) else num(tl(x),((ord(hd(x))-48)::hd(l))::tl(l))
    fun seprate (x) = if hd(x)= #"~" then let val n=num(tl(x),[[]]) in [[[1],hd(n)],[hd(tl(n)),hd(tl(tl(n)))]] end else if hd(x)= #"+" then let val n=num(tl(x),[[]]) in [[[0],hd(n)],[hd(tl(n)),hd(tl(tl(n)))]] end else let val n=num(x,[[]]) in [[[0],hd(n)],[hd(tl(n)),hd(tl(tl(n)))]] end 
    val n=seprate(String.explode(x^"()")) val x1= if length(hd(tl(hd(tl(n)))))<> 0 then BIGINT.subtract(BIGINT.make_big([[0],add_zeros([1],length(hd(hd(tl(n))))+length(hd(tl(hd(tl(n))))))]),BIGINT.make_big([[0],add_zeros([1],length(hd(hd(tl(n)))))])) else BIGINT.make_big([[0],add_zeros([1],length(hd(hd(tl(n)))))]) val y= if length(hd(tl(hd(tl(n))))) <> 0 then BIGINT.subtract(BIGINT.make_big([[0],hd(tl(hd(tl(n)))) @ hd(hd(tl(n))) @ hd(tl(hd(n)))]),BIGINT.make_big([[0],hd(hd(tl(n))) @ hd(tl(hd(n)))])) else BIGINT.make_big([[0], hd(hd(tl(n))) @ hd(tl(hd(n)))]) in BIGINT.reduce(y,x1) end

    fun toDecimal x = let val x1=BIGINT.reduce(hd(x),hd(tl(x))) val y=BIGINT.divide(hd(x1),hd(tl(x1))) val s=if BIGINT.sign(hd(y))=1 then "~" else "" 
    val d= if hd(tl(BIGINT.break_big(hd(tl(y)))))=[] then "(0)" else "("^BIGINT.toString(BIGINT.make_big([[0],hd(tl(BIGINT.break_big(hd(tl(y)))))]))^")" in s^BIGINT.toString(BIGINT.make_big([[0],hd(tl(BIGINT.break_big(hd(y))))]))^"."^BIGINT.toString(BIGINT.make_big([[0],hd(BIGINT.break_big(hd(tl(y))))]))^d end

  end;
  
structure Rational= Rational(BIGINT)