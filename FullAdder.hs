import Lava

halfAdder (a,b) = (sum,carry)
    where   sum = xor2 (a, b)
            carry = and2 (a, b)

fullAdder (carryin, ab@(a,b)) = (sum, carryOut)
    where   (sum1, carry1) = halfAdder ab
            (sum, carry2)  = halfAdder (sum1, carryin)
            carryOut       = xor2 (carry2, carry1)

swap (a,b) = (b,a)

copy (a,b) = ((a,a),(b,b))

sorter (a,b) = (c,d)
    where   c = or2 (a,b)
            d = and2 (a,b)

alwaysHigh () = (high)

multiplexer2 sig (a,b) =    if sig == low
                            then a
                            else b


