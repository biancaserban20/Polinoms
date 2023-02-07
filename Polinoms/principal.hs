import Polinom

--Checking normalization of Polynoms with some edge cases

s1 = "0xy - y^2 + y - 100x + 6z-   0xz  -      6y^2   + 8y  ^ 2  + x^2-yx   + 101 x - 6z"
s2 = "-0 + 1-5x^200y^30  + 2 + 3 - 4x -4x^10y^30 +4abcdef+ 5x^200y^30 - 4 - 2 + 5x- 4abcdef + 4x^10y^30"
s3 = "-x+x-y+2y + 1 + abcd - 3abcd +1 + 2abcd +xyz"
s4 = "0"
s5 = "2xyz + 10"
s6 = "1 + x + x^2 + x^3 + x^4"


s7 = "10x + 20y + 30z + 200xy + 300xz + 600yx + 6000xyz"
s8 = "a^2 - b^2 + xyz"
s9 = "-x + y -z + a - b"
s10= "x^100 - 100y^2 + 2xy^2z^3 - 3a^2b^3c^4d^3"
s12= "abc"
s13= "x^2 + y^2 - abcd + 6"

m0 = "0x + 0"
m1 = "2x^0y^3"
m2 = "-2x^3y^5z + 6a^3b^2"
m3 = "5x^3y^0z^3 - 3a^3b^2c^0 + 36"
m4 = "12"
-----Normalization and Parsing Testing-------

p1  = normalizePolinom $ fromString s1  -- should be x+ y + xy + x^2 + y^2
p2  = normalizePolinom $ fromString s2  -- should be x
p3  = normalizePolinom $ fromString s3  -- should be xyz + y + 2


--------------------------------------------

p4  =  normalizePolinom $ fromString s4
p5  =  normalizePolinom $ fromString s5
p6  =  normalizePolinom $ fromString s6

p7  =  normalizePolinom $ fromString s7
p8  =  normalizePolinom $ fromString s8
p9  =  normalizePolinom $ fromString s9
p10 =  normalizePolinom $ fromString s10

pm0  =  normalizePolinom $ fromString m0
pm1  =  normalizePolinom $ fromString m1
pm2  =  normalizePolinom $ fromString m2
pm3  =  normalizePolinom $ fromString m3
pm4  =  normalizePolinom $ fromString m4

---Sum Tests------------

sum12 = addPolinoms p1 p2  -- should be 2x + y + xy + x^2 + y^2
sum21 = addPolinoms p2 p1

sum13 = addPolinoms p1 p3  -- should be 2 + x + 2y + xy + x^2 + y^2 + xyz
sum31 = addPolinoms p3 p1

sum23 = addPolinoms p2 p3 -- shuld be x + y + 2 + xyz
sum32 = addPolinoms p3 p2


-------Multiplying Tests------------------
prod01 = multiplyPolinoms pm0 pm1  -- should be 0
prod10 = multiplyPolinoms pm1 pm0

prod12 = multiplyPolinoms pm1 pm2  -- should be 12a^3b^2y^3 - 4x^3y^8z
prod21 = multiplyPolinoms pm2 pm1

prod14 = multiplyPolinoms pm1 pm4  -- should be 24y^3
prod41 = multiplyPolinoms pm4 pm1

prod23 = multiplyPolinoms pm2 pm3  -- should be -18a^6b^4  + 216a^3b^2 + 6a^3b^2x^3y^5z + 30a^3b^2x^3x^3 - 10x^6y^5z^4 - 72x^3y^5z
prod32 = multiplyPolinoms pm3 pm2


------Derivative Tests----------------
der0x = derivatePolinom 'x' pm0  -- should be 0

der1y = derivatePolinom 'y' pm1  -- should be 6y^2
der1x = derivatePolinom 'x' pm1  -- should be 0

der2x = derivatePolinom 'x' pm2  -- should be -6x^2y^5z
der2y = derivatePolinom 'y' pm2  -- should be -10x^3y^4z
der2z = derivatePolinom 'z' pm2  -- should be -2x^3y^5
der2a = derivatePolinom 'a' pm2  -- should be 18a^2b^2
der2b = derivatePolinom 'b' pm2  -- should be 12a^3b
der2c = derivatePolinom 'c' pm2  -- should be 0 

der4c = derivatePolinom 'c' pm4  -- should be 0