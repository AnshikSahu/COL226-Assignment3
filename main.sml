(*Note: Lists with varriable names ending in "__" are to be read from left to
 right and the rest from right to left*)

(*Arthematic funcions---------------------------------------------------------------------------------*)

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

fun add(l1,l2,c)=if length(l1) + length(l2) <> 0 then
  add_digits(digit(l1),digit(l2),c) :: add( headlesslist(l1), headlesslist(l2) ,
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

fun subtract(l1,l2,c)=clean(dirty_subtract(l1,l2,0));
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
  add(multiply(l2,hd(l1),0),multiply_lists(tl(l1),0::l2),0) 
                           else [] ;
(*multiplies two numbers represented as lists by multiplying the first element of the*)
(*second list with the first list and passing the rest of the elements of the second*)
(*list to itself along with a carry*)

fun division_last_digit(x,y,c)= if compare(y,x) then c else division_last_digit(subtract(x,y),y,c+1);
(*gives the last digit of the quotient when dividing x by y*)


fun divide_integer_part(x,y)= if compare(y,x) then [[0],x] else let val l= division_last_digit(x,y,1) val r= divide_integer_part(subtract(x,multiply(y,l,0)),tl(y)) in [l::hd(r),hd(tl(r))] end;
(*gives the integer part of the quotient when dividing x by y*)

fun split(l,n)= if n=0 then [[],l] else let val r=split(tl(l),n-1) in [hd(l)::hd(r),hd(tl(r))] end;
(*splits a list into two lists at the nth position*)

fun index(l,x)= if l=[] then -1 else if hd(l)=x then 0 else 1+index(tl(l),x);
(*gives the index of the first occurence of x in l*)

fun divide_decimal_part(x,y,rem,ans)= if clean(x)==[] then [ans,[]] else let l=division_last_digit(0::x,y,0) val remainder=clean(subtract(x,multiply(y,l,0))) val i=index(remainder,rem) in
if(i=-1) then divide_decimal_part(remainder,y,remainder::rem,l::ans) else split(ans,i) end;
(*gives the decimal part of the quotient when dividing x by y*)

fun add_length(l,n)= if len(l)>=n then l else add_length(0::l,n);
(*adds zeros to the end of a list until the length of the list is equal to n*)

fun divide(x,y)= let  val k=add_length(y,len(x)) val l=divide_integer_part(x,k) val d=divide_decimal_part(hd(tl(l)),y,[]) in [hd(l),hd(d),hd(tl(d))] end;
(*divides two numbers represented as lists and gives the quotient and remainder*)