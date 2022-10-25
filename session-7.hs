-- letrec natural = \l x . if x = 0 then 0 else plus x (natural (minus x 1)) in natural 10

data LExp = X String | L String LExp | LET String LExp LExp | IF LExp LExp LExp | LETREC String LExp LExp | C String