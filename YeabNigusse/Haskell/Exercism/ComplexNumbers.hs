module ComplexNumbers  
(Complex,
 conjugate,
 abs,
 exp,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs, exp)

-- Data definition -------------------------------------------------------------
data Complex a = MyComplex a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (x, y)= MyComplex x y

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (MyComplex x y) = MyComplex x (-y)

abs :: Floating a => Complex a -> a
abs (MyComplex x y) = sqrt (x^2 + y^2)

real :: Num a => Complex a -> a
real (MyComplex x _) = x

imaginary :: Num a => Complex a -> a
imaginary (MyComplex _ y) = y

exp :: Floating a => Complex a -> Complex a
exp (MyComplex x y) = MyComplex (exp x * cos y) (exp x * sin y)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (MyComplex x y) (MyComplex z w) = MyComplex (x*z - y*w)  (y*z + x*w)

add :: Num a => Complex a -> Complex a -> Complex a
add (MyComplex x y) (MyComplex z w) = MyComplex (x+z) (y+w)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (MyComplex x y) (MyComplex z w) = MyComplex (x-z) (y-w)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (MyComplex x y) (MyComplex z w) = MyComplex ((x*z + y*w)/z^2 + w^2) ((y*z - x*w)/z^2 + w^2)
